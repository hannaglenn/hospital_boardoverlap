# load libraries 
library(dplyr)
library(fuzzyjoin)
library(stringdist)
library(tidyr)
library(stringr)
library(readr)
library(data.table)
library(purrr)
library(igraph)
library(future.apply)

source("./Scripts/paths.R")

# functions ------------------------------------------------------------------------------------------------------

combine_fuzzy_matches <- function(data, max_dist) {
  setDT(data)  # Convert to data.table for efficiency
  
  # Split by EIN to avoid excessive joins
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  # Function to process each firm's data
  process_firm <- function(firm_data) {
    if (nrow(firm_data) <= 1) return(firm_data)  # Skip firms with only 1 name
    
    unique_names <- unique(firm_data[, .(name_cleaned)])  # Reduce size
    
    # Compute approximate name matches only within this firm
    matches <- stringdist_inner_join(
      unique_names, unique_names, 
      by = "name_cleaned", 
      max_dist = max_dist, 
      method = "osa"
    ) %>%
      filter(name_cleaned.x < name_cleaned.y) %>%
      group_by(name_cleaned.y) %>%
      summarise(correct_name = first(name_cleaned.x), .groups = "drop")
    
    # Merge corrected names
    setDT(matches)
    firm_data <- merge(firm_data, matches, by.x = "name_cleaned", by.y = "name_cleaned.y", all.x = TRUE)
    firm_data[, name_cleaned := fcoalesce(correct_name, name_cleaned)]
    firm_data[, correct_name := NULL]
    
    return(firm_data)
  }
  
  # Process firms in parallel (reduces RAM usage)
  corrected_list <- lapply(firm_list, process_firm)
  
  # Recombine data
  corrected_data <- rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
  
  return(corrected_data)
}

# nicknames mapping
nickname_map <- list(
  "jeff" = "jeffrey",
  "bob" = "robert",
  "rob" = "robert",
  "liz" = "elizabeth",
  "kate" = "katherine",
  "mike" = "michael",
  "tom" = "thomas",
  "tim" = "timothy",
  "pat" = "patricia",
  "chris" = "christopher",
  "ed" = "edward",
  "ron" = "ronald",
  "aly" = "alison",
  "tricia" = "patricia",
  "angeline" = "angie",
  "mike" = "michael",
  "ken" = "kenneth",
  "bob" = "robert",
  "patty" = "patricia",
  "les" = "leslie",
  "cort" = "cortland",
  "greg" = "gregory",
  "art" = "arthur",
  "dick" = "richard"
  # Add more as needed
)

normalize_nicknames <- function(data, nickname_map){
  setDT(data)
  data[, name_cleaned := tolower(name_cleaned)]
  
  # Replace nicknames using the map
  
  # Replace nicknames using the map
  for (nick in names(nickname_map)) {
    full <- nickname_map[[nick]]
    data[, name_cleaned := gsub(paste0("\\b", nick, "\\b"), full, name_cleaned)]
  }
  
  return(data)
}

combine_token_matches <- function(data, min_shared_tokens = 2) {
  setDT(data)
  
  # Split by firm
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  process_firm <- function(firm_data) {
    unique_names <- unique(firm_data[, .(name_cleaned)])
    n <- nrow(unique_names)
    
    # Build edges for graph
    edges <- list()
    for (i in 1:(n - 1)) {
      name_i <- unique_names$name_cleaned[i]
      tokens_i <- unlist(strsplit(tolower(gsub("[^a-z ]", "", name_i)), "\\s+"))
      
      for (j in (i + 1):n) {
        name_j <- unique_names$name_cleaned[j]
        tokens_j <- unlist(strsplit(tolower(gsub("[^a-z ]", "", name_j)), "\\s+"))
        
        if (length(intersect(tokens_i, tokens_j)) >= min_shared_tokens) {
          edges <- append(edges, list(c(name_i, name_j)))
        }
      }
    }
    
    if (length(edges) > 0) {
      g <- graph_from_edgelist(do.call(rbind, edges), directed = FALSE)
      comps <- components(g)$membership
      
      # Map each name to its cluster's canonical name
      canonical_map <- data.table(
        name_cleaned = names(comps),
        group = comps
      )[, .(canonical_name = name_cleaned[which.min(nchar(name_cleaned))]), by = group]
      
      mapping <- merge(data.table(name_cleaned = names(comps), group = comps),
                       canonical_map, by = "group")[, .(name_cleaned, canonical_name)]
      
      firm_data <- merge(firm_data, mapping, by = "name_cleaned", all.x = TRUE)
      firm_data[, name_cleaned := fcoalesce(canonical_name, name_cleaned)]
      firm_data[, canonical_name := NULL]
    }
    
    return(firm_data)
  }
  
  corrected_list <- lapply(firm_list, process_firm)
  corrected_data <- rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
  
  return(corrected_data)
}

assign_person_id <- function(data) {
  
  # Step 1: Normalize and tokenize names, remove single-letter tokens
  data <- people %>%
    mutate(tokens = str_split(str_to_lower(str_replace_all(name_cleaned, "[^a-z ]", "")), "\\s+")) %>%
    mutate(tokens = map(tokens, ~ .x[nchar(.x) > 1]))  # remove single-letter tokens
  
  # Step 2: Explode tokens into rows
  token_map <- data %>%
    select(name_cleaned, tokens) %>%
    unnest(tokens)
  
  # Step 3: Group by token to find candidate names
  candidates <- token_map %>%
    group_by(tokens) %>%
    summarise(names = list(unique(name_cleaned)), .groups = "drop")
  
  # Step 4: Generate candidate pairs
  unique_names <- data %>%
    distinct(name_cleaned, tokens)
  
  name_lookup <- setNames(unique_names$tokens, unique_names$name_cleaned)
  
  candidate_pairs <- map_df(candidates$names, ~ {
    if (length(.x) > 1) {
      expand.grid(i = .x, j = .x, stringsAsFactors = FALSE) %>%
        filter(i < j)
    } else {
      tibble(i = character(), j = character())
    }
  }) %>%
    distinct() %>%
    rowwise() %>%
    filter({
      tokens_i <- name_lookup[[i]]
      tokens_j <- name_lookup[[j]]
      shared <- length(intersect(tokens_i, tokens_j))
      if (length(tokens_i) >= 3 && length(tokens_j) >= 3) {
        shared >= 3
      } else {
        shared >= 2
      }
    }) %>%
    ungroup()
  
  # Step 5: Filter candidate pairs to same first_name
  candidate_pairs <- candidate_pairs %>%
    left_join(data %>% select(name_cleaned, first_name), by = c("i" = "name_cleaned")) %>%
    rename(first_i = first_name) %>%
    left_join(data %>% select(name_cleaned, first_name), by = c("j" = "name_cleaned")) %>%
    rename(first_j = first_name) %>%
    filter(first_i == first_j) %>%
    select(i, j) %>%
    distinct()
  
  # Step 6: Build graph and find connected components
  if (nrow(candidate_pairs) > 0) {
    g <- graph_from_data_frame(candidate_pairs, directed = FALSE)
    comps <- components(g)
    
    cluster_map <- tibble(
      name_cleaned = names(comps$membership),
      person_id = comps$membership
    )
  } else {
    cluster_map <- tibble(name_cleaned = character(), person_id = integer())
  }
  
  # Step 7: Handle isolated names
  isolated <- data %>%
    distinct(name_cleaned) %>%
    filter(!name_cleaned %in% cluster_map$name_cleaned) %>%
    mutate(person_id = seq(max(cluster_map$person_id, 0) + 1,
                           max(cluster_map$person_id, 0) + n()))
  
  cluster_map <- bind_rows(cluster_map, isolated)
  
  # Step 8: Merge back to original data
  result <- data %>%
    select(-tokens) %>%
    left_join(cluster_map, by = "name_cleaned")
  
  return(result)
}

identify_common_members <- function(data) {
  # Collect all EINs for each name within the same year
  ein_mapping <- data %>%
    group_by(person_id, TaxYr) %>%
    summarise(all_eins = list(unique(Filer.EIN)), .groups = "drop")
  
  # Merge back and correctly compute other EINs within the same year
  df_with_others <- data %>%
    left_join(ein_mapping, by = c("person_id", "TaxYr")) %>%
    rowwise() %>%
    mutate(other_eins = paste(setdiff(unlist(all_eins), Filer.EIN), collapse = ", ")) %>%
    ungroup() %>%
    select(-all_eins)  # Drop intermediate column
  
  return(df_with_others)
}








# Clean up the names ----------------------------------------------------------------------------------------

# Read in cleaned up version of names found in 990 tax forms
people <- read_rds(paste0(created_data_path, "cleaned_people_data.rds"))

# map nicknames to longer versions
people <- normalize_nicknames(people, nickname_map)

# remove initials
people <- people %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "\\b[a-z]\\b")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# match names within the same EIN that are similar based on token overlap
people <- combine_token_matches(people, min_shared_tokens = 2)

# replace dashes with a space
people <- people %>%
  mutate(name_cleaned = str_replace_all(name_cleaned, "-", " ")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# order each name alphabetically
people <- people %>%
  mutate(name_cleaned = map_chr(name_cleaned, ~ {
    words <- str_split(.x, "\\s+")[[1]]
    sorted_words <- sort(words)
    str_c(sorted_words, collapse = " ")
  }))

# standardize names within the same EIN
people <- combine_fuzzy_matches(people, max_dist = 2)

# misc name fixes
people <- people %>%
  mutate(name_cleaned = ifelse(name_cleaned=="sabotka tj", "sabotka thomas", name_cleaned),
         name_cleaned = ifelse(name_cleaned=="jim sabetta", "james sabetta", name_cleaned),
         name_cleaned = ifelse(name_cleaned=="morr robert", "morrisey robert", name_cleaned)) 

# look at distinct names to check validity
# observe <- people %>%
#   distinct(Filer.EIN, name_cleaned)
# write.csv(observe, paste0(created_data_path, "observe.csv"))

# assign unique identifiers to names that share at least 2 tokens across firm, years
people <- assign_person_id(people)

# Create variables of board members connected to multiple EINs ---------------------------------------------------
connections <- identify_common_members(people)

# only keep EINs that are present in enough years
ein_keep <- connections %>%
  distinct(Filer.EIN, TaxYr) %>%
  mutate(count=ifelse(TaxYr %in% c(2017,2018,2019,2020,2021),1,0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(count)>=5) %>%
  ungroup() %>%
  distinct(Filer.EIN)

connections <- connections %>%
  filter(Filer.EIN %in% unique(ein_keep$Filer.EIN))

# pivot longer
connections <- connections %>%
  separate_wider_delim(cols = "other_eins", delim=",", names_sep = "", too_few = "align_start")

connections <- connections %>%
  pivot_longer(cols=starts_with("other_eins"), names_to="other", values_to="other_ein")

# Remove columns with NA, only keep actual connections
connections <- connections %>%
  mutate(other_ein = str_trim(other_ein)) %>%
  filter(!is.na(other_ein))

# calculate initial overlap of boards for hospital pairs
filer_board <- connections %>%
  group_by(TaxYr, Filer.EIN) %>%
  summarise(board = list(unique(name_cleaned)))

connections <- connections %>%
  left_join(filer_board, by=c("TaxYr", "Filer.EIN")) %>%
  rename(filer_board = board)
connections <- connections %>%
  left_join(filer_board, by=c("TaxYr", "other_ein"="Filer.EIN")) %>%
  rename(other_board = board)

connections <- connections %>%
  rowwise() %>%
  mutate(overlap_count = length(intersect(filer_board, other_board)),
         filer_size = length(filer_board)) %>%
  mutate(overlap_percent = overlap_count / filer_size * 100) %>%
  ungroup()

connections <- connections %>%
  select(-filer_board, -other_board)

ein_positions <- people %>%
  mutate(position = ifelse(position=="board", "board", "exec")) %>%
  distinct(TaxYr, person_id, position) %>%
  group_by(TaxYr, person_id) %>%
  summarise(positions = list(position)) %>%
  ungroup() %>%
  mutate(person_positions = ifelse(positions=="board", "board only", NA)) %>%
  mutate(person_positions = ifelse(positions=="exec", "exec only", person_positions)) %>%
  mutate(person_positions = ifelse(is.na(person_positions), "board and exec", person_positions)) %>%
  distinct(TaxYr, person_id, person_positions)

# join to connections data
connections <- connections %>%
  left_join(ein_positions)


# Join each EIN to their AHA
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds"))
connections <- connections %>%
  left_join(cw, by="Filer.EIN") %>%
  rename(Filer.ID = ID)
connections <- connections %>%
  left_join(cw, by=c("other_ein"="Filer.EIN")) %>%
  rename(other.id = ID)

# create hospital level data set to merge back to later
hospital_data <- connections %>%
  distinct(TaxYr, Filer.EIN) %>%
  left_join(cw) %>%
  rename(Filer.ID=ID)

# Get rid of duplicates
connections <- connections %>%
  distinct()


# join AHA information to the filer and other id
AHA <- read_csv(paste0(raw_data_path, "\\AHAdata_20052023.csv")) 

AHA_hrr <- AHA %>%
  select(ID, YEAR, HRRCODE, SYSID, NETNAME, MNAME, SERV, LAT, LONG) %>%
  filter(YEAR>=2015 & YEAR<=2023) %>%
  mutate(YEAR = as.character(YEAR)) %>%
  distinct(ID, YEAR, .keep_all = TRUE)

# complete AHA data for variables we can extrapolate
AHA_hrr <- AHA_hrr %>%
  group_by(ID) %>%
  mutate(YEAR=as.numeric(YEAR)) %>%
  complete(YEAR = c(2015:2023)) %>%
  fill(HRRCODE, MNAME, SERV, LAT, LONG, .direction="downup") %>%
  mutate(YEAR=as.character(YEAR))

connections <- connections %>%
  left_join(AHA_hrr, by=c("Filer.ID"="ID", "TaxYr"="YEAR")) %>%
  rename(filer.hrr=HRRCODE, filer.sysid=SYSID, filer.name=MNAME, filer.type=SERV, filer.lat=LAT, filer.long=LONG, filer.net = NETNAME)

connections <- connections %>%
  left_join(AHA_hrr, by=c("other.id"="ID", "TaxYr"="YEAR")) %>%
  rename(other.hrr=HRRCODE, other.sysid=SYSID, other.name=MNAME, other.type=SERV, other.lat=LAT, other.long=LONG, other.net=NETNAME)

# Create classifications for each affiliation type
# "formal" means they are in the same system or network
# "informal" means they share a large portion of exec and board team (management consolidation)
# "partnership" means they just share a small number of board members
# the categories are mutually exclusive

# pick up on shared systems using name distance metric
connections <- connections %>%
  mutate(name_dist = stringdist::stringdist(str_extract(filer.name,"[A-Za-z]+\\s"), str_extract(other.name,"[A-Za-z]+\\s"), method = "jw")) 

# create categories
connections <- connections %>%
  mutate(affiliation = ifelse(filer.sysid==other.sysid | filer.net==other.net | name_dist==0, "formal", NA)) %>%
  mutate(affiliation = ifelse(is.na(affiliation) & overlap_percent>=70, "informal", affiliation)) %>%
  mutate(affiliation = ifelse(is.na(affiliation) & overlap_percent>0, "partnership", affiliation)) %>%
  mutate(affiliation = ifelse(is.na(affiliation), "None", affiliation))

# look at frequencies:
aff_freq <- connections %>%
  group_by(TaxYr) %>%
  count(affiliation)

# create connection indicators for same or different HRR
create_affiliation_indicators <- function(data, affiliation_col, filer_hrr_col, other_hrr_col, categories) {
  for (cat in categories) {
    same_col <- paste0(cat, "_sameHRR")
    diff_col <- paste0(cat, "_diffHRR")
    
    data[[same_col]] <- ifelse(data[[affiliation_col]] == cat & data[[filer_hrr_col]] == data[[other_hrr_col]], 1, 0)
    data[[diff_col]] <- ifelse(data[[affiliation_col]] == cat & data[[filer_hrr_col]] != data[[other_hrr_col]], 1, 0)
  }
  return(data)
}

# Example usage:
categories <- c("formal", "informal", "partnership")
connections <- create_affiliation_indicators(connections, "affiliation", "filer.hrr", "other.hrr", categories)

people_connections <- connections %>%
  group_by(TaxYr, Filer.EIN, person_id) %>%
  summarise(
    doctor = max(doctor, na.rm = TRUE),
    nurse = max(nurse, na.rm = TRUE),
    other_doctor = max(other_doctor, na.rm = TRUE),
    ha = max(ha, na.rm = TRUE),
    nonvoting = max(nonvoting, na.rm = TRUE),
    position = first(position),  # or paste(unique(position), collapse = ";")
    gender = first(gender),
    person_positions = first(person_positions),
    formal_sameHRR = max(formal_sameHRR),
    formal_diffHRR = max(formal_diffHRR),
    informal_sameHRR = max(informal_sameHRR),
    informal_diffHRR = max(informal_diffHRR),
    partnership_sameHRR = max(partnership_sameHRR),
    partnership_diffHRR = max(partnership_diffHRR)
  )

# save connections data
saveRDS(people_connections, file=paste0(created_data_path, "people_connections_boardandexec.rds"))

# summarize connections at the hospital level -----------------------------------------------
# create hospital-level connections data set for making a map showing the connections
hospital_connections <- connections %>%
  distinct(TaxYr, Filer.EIN, Filer.ID, filer.hrr, filer.sysid, filer.name, filer.type, filer.lat, filer.long, filer.net, 
           other_ein, other.id, other.hrr, other.sysid, other.name, other.type, other.lat, other.long, other.net,
           formal_sameHRR, formal_diffHRR, informal_sameHRR, informal_diffHRR, partnership_sameHRR, partnership_diffHRR, 
           overlap_count, overlap_percent) 
saveRDS(hospital_connections, file=paste0(created_data_path, "hospital_connections_boardandexec.rds"))

hospital_connections <- connections %>%
  filter(other_ein!="") %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(any_formal_sameHRR = max(formal_sameHRR),
         any_formal_diffHRR = max(formal_diffHRR),
         any_informal_sameHRR = max(informal_sameHRR),
         any_informal_diffHRR = max(informal_diffHRR),
         any_partnership_sameHRR = max(partnership_sameHRR),
         any_partnership_diffHRR = max(partnership_diffHRR)) %>%
  ungroup() %>%
  distinct(TaxYr, Filer.EIN,
           any_formal_sameHRR, any_formal_diffHRR,
           any_informal_sameHRR, any_informal_diffHRR,
           any_partnership_sameHRR, any_partnership_diffHRR)


# join back to hospital data
hospital_data <- hospital_data %>%
  left_join(hospital_connections, by=c("TaxYr", "Filer.EIN"))

# Change NAs to zeros
hospital_data <- hospital_data %>%
  mutate(any_formal_sameHRR = ifelse(is.na(any_formal_sameHRR),0,any_formal_sameHRR),
         any_formal_diffHRR = ifelse(is.na(any_formal_diffHRR),0,any_formal_diffHRR),
         any_informal_sameHRR = ifelse(is.na(any_informal_sameHRR),0,any_informal_sameHRR),
         any_informal_diffHRR = ifelse(is.na(any_informal_diffHRR),0,any_informal_diffHRR),
         any_partnership_sameHRR = ifelse(is.na(any_partnership_sameHRR),0,any_partnership_sameHRR),
         any_partnership_diffHRR = ifelse(is.na(any_partnership_diffHRR),0,any_partnership_diffHRR))


# find the first year the hospital gained each type of connections (0 if not connected in the relevant way)
minyr_sameHRR_partnership <- hospital_data %>%
  filter(any_partnership_sameHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_sameHRR_part = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_sameHRR_part)
minyr_diffHRR_partnership <- hospital_data %>%
  filter(any_partnership_diffHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_diffHRR_part = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_diffHRR_part)

# merge back to data
hospital_data <- hospital_data %>%
  left_join(minyr_sameHRR_partnership, by="Filer.EIN") %>%
  left_join(minyr_diffHRR_partnership, by="Filer.EIN") 

hospital_data <- hospital_data %>%
  ungroup() %>%
  mutate(minyr_sameHRR_part = ifelse(is.na(minyr_sameHRR_part),0,minyr_sameHRR_part)) %>%
  mutate(minyr_diffHRR_part = ifelse(is.na(minyr_diffHRR_part),0,minyr_diffHRR_part))

# Merge outcome variables
AHA_variables <- AHA %>%
  select(YEAR, ID, SYSID, HRRCODE, LAT, LONG, MAPP5, MCRNUM, SERV,
         GENBD, PEDBD, OBBD, MSICBD, CICBD, NICBD, NINTBD, PEDICBD, SYSID,
         BRNBD, SPCICBD, REHABBD, OTHICBD, ACULTBD, ALCHBD, PSYBD, SNBD88, ICFBD88,
         OTHLBD94, OTHBD94, HOSPBD,
         FTMT, FTRNTF,
         ICLABHOS, ACLABHOS,
         MCDDC, MCRDC)
hospital_data <- hospital_data %>%
  mutate(TaxYr=as.numeric(TaxYr)) %>%
  left_join(AHA_variables, by = c("TaxYr"="YEAR", "Filer.ID"="ID"))

hospital_data <- hospital_data %>%
  mutate(bed_conc = (GENBD/HOSPBD)^2 + (PEDBD/HOSPBD)^2 + (OBBD/HOSPBD)^2 + (MSICBD/HOSPBD)^2 + (CICBD/HOSPBD)^2 + (NICBD/HOSPBD)^2 + (NINTBD/HOSPBD)^2 + 
           (PEDICBD/HOSPBD)^2 + (BRNBD/HOSPBD)^2 + (SPCICBD/HOSPBD)^2 + (OTHICBD/HOSPBD)^2 + (REHABBD/HOSPBD)^2 + (ALCHBD/HOSPBD)^2 + 
           (PSYBD/HOSPBD)^2 + (SNBD88/HOSPBD)^2 + (ICFBD88/HOSPBD)^2 + (ACULTBD/HOSPBD)^2 + (OTHLBD94/HOSPBD)^2 + (OTHBD94/HOSPBD)^2)


# read in HCRIS data
hcris <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) 

hcris <- hcris %>%
  select(provider_number, year, tot_discharges, mcare_discharges, mcaid_discharges, build_purch, fixedequipment_purch,
         movableequipment_purch, medrecords_expenses, HIT_purch, tot_pat_rev, tot_operating_exp, cost_to_charge)
hospital_data <- hospital_data %>%
  left_join(hcris, by = c("MCRNUM"="provider_number", "TaxYr"="year"))

# Create variables for percent of discharges medicare and medicaid from HCRIS
hospital_data <- hospital_data %>%
  mutate(perc_mcare = ifelse(!is.na(tot_discharges) & tot_discharges > 0, mcare_discharges / tot_discharges, NA),
         perc_mcaid = ifelse(!is.na(tot_discharges) & tot_discharges > 0, mcaid_discharges / tot_discharges, NA)) 

observe <- hospital_data %>%
  filter(is.na(tot_discharges))

# save hospital data
saveRDS(hospital_data, file=paste0(created_data_path, "hospital_data_boardandexec.rds"))

