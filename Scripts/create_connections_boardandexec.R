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
people <- read_rds(paste0(created_data_path, "cleaned_people_data_new.rds")) %>%
  filter(TaxYr!=2025)

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

saveRDS(people, paste0(created_data_path, "people.rds"))

# Create variables of board members connected to multiple EINs ---------------------------------------------------
connections <- identify_common_members(people)

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
  select(-other) %>%
  distinct() 

# join AHA information to the filer and other id
AHA <- read_csv(paste0(raw_data_path, "/AHAdata_20052023.csv")) 

AHA_hrr <- AHA %>%
  select(ID, YEAR, HRRCODE, SYSID, NETNAME, MNAME, SERV, LAT, LONG) %>%
  filter(YEAR>=2013 & YEAR<=2023) %>%
  distinct(ID, YEAR, .keep_all = TRUE)

# complete AHA data for variables we can extrapolate
AHA_hrr <- AHA_hrr %>%
  group_by(ID) %>%
  mutate(YEAR=as.numeric(YEAR)) %>%
  complete(YEAR = c(2013:2023)) %>%
  fill(HRRCODE, MNAME, SERV, LAT, LONG, .direction="downup") %>%
  ungroup() %>%
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
  # I checked that using name_dist==0 is a good choice!

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

categories <- c("formal", "partnership")
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
    partnership_sameHRR = max(partnership_sameHRR),
    partnership_diffHRR = max(partnership_diffHRR)
  )

# save connections data
saveRDS(people_connections, file=paste0(created_data_path, "people_connections_boardandexec.rds"))

# summarize connections at the hospital level -----------------------------------------------
# create hospital-level connections data set for making a map showing the connections
hospital_connections <- connections %>%
  group_by(Filer.EIN) %>%
  mutate(overlap_type = list(unique(person_positions))) %>%
  ungroup() %>%
  distinct(TaxYr, Filer.EIN, Filer.ID, filer.hrr, filer.sysid, filer.name, filer.type, filer.lat, filer.long, filer.net, 
           other_ein, other.id, other.hrr, other.sysid, other.name, other.type, other.lat, other.long, other.net,
           formal_sameHRR, formal_diffHRR, partnership_sameHRR, partnership_diffHRR, 
           overlap_count, overlap_percent) 
saveRDS(hospital_connections, file=paste0(created_data_path, "hospital_connections_boardandexec.rds"))

hospital_connections <- connections %>%
  filter(other_ein!="") %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(any_formal_sameHRR = max(formal_sameHRR),
         any_formal_diffHRR = max(formal_diffHRR),
         any_partnership_sameHRR = max(partnership_sameHRR),
         any_partnership_diffHRR = max(partnership_diffHRR)) %>%
  ungroup() %>%
  distinct(TaxYr, Filer.EIN,
           any_formal_sameHRR, any_formal_diffHRR,
           any_partnership_sameHRR, any_partnership_diffHRR)


# join back to hospital data
hospital_data <- hospital_data %>%
  left_join(hospital_connections, by=c("TaxYr", "Filer.EIN"))

# Change NAs to zeros
hospital_data <- hospital_data %>%
  mutate(any_formal_sameHRR = ifelse(is.na(any_formal_sameHRR),0,any_formal_sameHRR),
         any_formal_diffHRR = ifelse(is.na(any_formal_diffHRR),0,any_formal_diffHRR),
         any_partnership_sameHRR = ifelse(is.na(any_partnership_sameHRR),0,any_partnership_sameHRR),
         any_partnership_diffHRR = ifelse(is.na(any_partnership_diffHRR),0,any_partnership_diffHRR))


# find the first year the hospital gained each type of connections (0 if not connected in the relevant way)
minyr_sameHRR_partnership <- hospital_data %>%
  filter(any_partnership_sameHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_sameHRR_part = min(TaxYr)) %>%
  mutate(maxyr_sameHRR_part = max(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_sameHRR_part, maxyr_sameHRR_part)
minyr_diffHRR_partnership <- hospital_data %>%
  filter(any_partnership_diffHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_diffHRR_part = min(TaxYr)) %>%
  mutate(maxyr_diffHRR_part = max(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_diffHRR_part, maxyr_diffHRR_part)

# do the same for formal affiliation
minyr_sameHRR_formal <- hospital_data %>%
  filter(any_formal_sameHRR==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_sameHRR_formal = max(TaxYr)) %>%
  mutate(maxyr_sameHRR_formal = max(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_sameHRR_formal, maxyr_sameHRR_formal)

# merge back to data
hospital_data <- hospital_data %>%
  left_join(minyr_sameHRR_partnership, by="Filer.EIN") %>%
  left_join(minyr_diffHRR_partnership, by="Filer.EIN") %>%
  left_join(minyr_sameHRR_formal, by="Filer.EIN") 

hospital_data <- hospital_data %>%
  ungroup() %>%
  mutate(minyr_sameHRR_part = ifelse(is.na(minyr_sameHRR_part),0,minyr_sameHRR_part)) %>%
  mutate(minyr_diffHRR_part = ifelse(is.na(minyr_diffHRR_part),0,minyr_diffHRR_part)) %>%
  mutate(maxyr_sameHRR_part = ifelse(is.na(maxyr_sameHRR_part),0,maxyr_sameHRR_part)) %>%
  mutate(maxyr_diffHRR_part = ifelse(is.na(maxyr_diffHRR_part),0,maxyr_diffHRR_part)) %>%
  mutate(minyr_sameHRR_formal = ifelse(is.na(minyr_sameHRR_formal),0,minyr_sameHRR_formal)) %>%
  mutate(maxyr_sameHRR_formal = ifelse(is.na(maxyr_sameHRR_formal),0,maxyr_sameHRR_formal)) 

# complete the data but keep leadership variables NA
hospital_data <- hospital_data %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  group_by(Filer.EIN) %>%
  complete(TaxYr = c(2013:2023)) %>%
  fill(Filer.ID, .direction="downup") %>%
  ungroup()

# Merge outcome and control variables
AHA_hosp <- AHA %>%
  select(YEAR, ID, SYSID, HRRCODE, LAT, LONG, MAPP5, MCRNUM, SERV,
         ends_with("BD")|ends_with("BD94")|ends_with("BD88"),
         FTMT, FTRNTF,
         ICLABHOS, ACLABHOS,
         MCDDC, MCRDC,
         MAPP3, MAPP5, MAPP8, BDTOT,
         JNTMD,
         ends_with("HOS")) 

# create hospital types  
AHA_hosp <- AHA_hosp %>%
  mutate(
    # hosptype1: equivalent to type1_aha
    hosptype1 = dplyr::case_when(
      SERV == 10 ~ 1L,
      SERV %in% c(11, 12, 13, 22, 33, 41, 42, 44, 45, 46, 47, 48, 49) ~ 2L,
      SERV == 18 ~ 3L,
      SERV == 50 ~ 4L,
      SERV %in% c(51, 52, 53, 55, 56, 57, 58, 59, 90, 91) ~ 5L,
      SERV %in% c(62, 80, 82) ~ 6L,
      TRUE ~ NA_integer_
    ),
    hosptype1 = factor(
      hosptype1, levels = 1:6,
      labels = c(
        "General medical/surgical",
        "Specialty",
        "Rural",
        "Children's general medical/surgical",
        "Children's specialty",
        "Other"
      )
    ),
    #hosptype2: equivalent to type2_aha, based on hosptype1
    hosptype2 = dplyr::case_when(
      hosptype1 %in% c("General medical/surgical",
                       "Children's general medical/surgical") ~ 1L,
      hosptype1 %in% c("Specialty",
                       "Children's specialty") ~ 2L,
      hosptype1 %in% c("Rural", "Other") ~ 3L,
      TRUE ~ NA_integer_
    ),
    hosptype2 = factor(
      hosptype2, levels = 1:3,
      labels = c(
        "General medical/surgical",
        "Specialty",
        "Other"
      )
    )
  )

# create indicator for system
AHA_hosp <- AHA_hosp %>%
  mutate(independent = ifelse(is.na(SYSID),1,0))

# create teaching hospital indicators
AHA_hosp <- AHA_hosp %>%
  mutate(
    # teaching1_aha: start at 3 ("Not Teaching"), then update
    teaching1_aha = 3L,
    teaching1_aha = ifelse(MAPP8 == 1, 1L, teaching1_aha),
    teaching1_aha = ifelse(
      teaching1_aha == 3L & (MAPP3 == 1 | MAPP5 == 1),
      2L,
      teaching1_aha
    ),
    teaching1_aha = factor(
      teaching1_aha,
      levels = c(1L, 2L, 3L),
      labels = c("Major Teaching", "Minor Teaching", "Not Teaching")
    ),
    
    # teaching2_aha: 0/1 then factor with labels
    teaching2_aha = 0L,
    teaching2_aha = ifelse(
      teaching1_aha %in% c("Major Teaching", "Minor Teaching"),
      1L,
      teaching2_aha
    ),
    teaching2_aha = factor(
      teaching2_aha,
      levels = c(0L, 1L),
      labels = c("Not Teaching", "Teaching")
    )
  )

# Only keep hospital type 2 for now, but can go back and change later
AHA_hosp <- AHA_hosp %>%
  mutate(type_factor = as.factor(hosptype2)) %>%
  mutate(teaching = ifelse(teaching2_aha=="Teaching",1,0)) %>%
  mutate(general = ifelse(hosptype2=="General medical/surgical",1,0)) 

observe <- AHA_hosp %>%
  filter(is.na(GENBD))

# List all specialty bed columns once
bed_cols <- c("GENBD","PEDBD","OBBD","MSICBD","CICBD","NICBD","NINTBD","PEDICBD","BRNBD","SPCICBD","OTHICBD","REHABBD","ALCHBD",  "PSYBD","SNBD88","ICFBD88","ACULTBD","OTHLBD94","OTHBD94")

AHA_hosp <- AHA_hosp %>%
  # Do NOT replace NAs in bed columns; only ensure HOSPBD is non-missing
  mutate(HOSPBD = tidyr::replace_na(HOSPBD, 0)) %>%
  # Compute concentration safely: if BDTOT > 0, else NA
  mutate(
    bed_conc = if_else(
      BDTOT > 0,
      rowSums((as.matrix(select(., all_of(bed_cols))) / BDTOT)^2, na.rm = TRUE),
      NA_real_
    )
  ) %>%
  arrange(ID, YEAR) %>%
  group_by(ID) %>%
  # Create lags for each bed category (keep NAs)
  mutate(
    across(all_of(bed_cols), ~ dplyr::lag(.x, 1), .names = "{.col}_lag")
  ) 

AHA_hosp <- AHA_hosp %>%
  mutate(bed_conc = ifelse(bed_conc==0,NA,bed_conc))

# Figure out if they start a new service based on the "HOS" columns
AHA_hosp <- AHA_hosp %>%
  ungroup() %>%
  mutate(num_services = rowSums(as.matrix(select(.,ends_with("HOS"))), na.rm=T)) %>%
  mutate(num_services = ifelse(num_services==0,NA,num_services))




# Only keep the columns I need
AHA_hosp <- AHA_hosp %>%
  select(-ends_with("VEN"), -ends_with("BD"), -ends_with("lag"), -ends_with("HOS"),
         -MAPP5, -MAPP3, -MAPP8, -SERV, -ends_with("BD88"), -ends_with("BD94"), -ends_with("started"),
         -teaching1_aha, -teaching2_aha) 


hospital_data <- hospital_data %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(AHA_hosp, by=c("TaxYr"="YEAR", "Filer.ID"="ID"))

# Fill MCRNUM and get rid of units with no mcrnum
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  fill(MCRNUM, .direction="downup") %>%
  ungroup() %>%
  filter(!is.na(MCRNUM))


# read in HCRIS data
hcris <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) 

hcris <- hcris %>%
  select(provider_number, year, tot_discharges, mcare_discharges, mcaid_discharges, build_purch, fixedequipment_purch,
         movableequipment_purch, land_purch, HIT_purch, labor_costs, tot_operating_exp, cost_to_charge) %>%
  mutate(build_purch = abs(build_purch),
         fixedequipment_purch = abs(fixedequipment_purch),
         movableequipment_purch = abs(movableequipment_purch),
         land_purch = abs(land_purch)) %>%
  rowwise() %>%
  mutate(any_purch = sum(build_purch, fixedequipment_purch, movableequipment_purch, land_purch, na.rm=T)) %>%
  ungroup() 


hospital_data <- hospital_data %>%
  left_join(hcris, by = c("MCRNUM"="provider_number", "TaxYr"="year"))

# Create variables for percent of discharges medicare and medicaid from HCRIS
hospital_data <- hospital_data %>%
  mutate(perc_mcare = ifelse(!is.na(tot_discharges) & tot_discharges > 0, mcare_discharges / tot_discharges, NA),
         perc_mcaid = ifelse(!is.na(tot_discharges) & tot_discharges > 0, mcaid_discharges / tot_discharges, NA)) 


# Get rid of units where the bed size is too small
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  filter(min(BDTOT, na.rm=T)>15) %>%
  ungroup()

summary(hospital_data)

# Figure out how to represent hospitals that are in the data for the whole time versus drop out for good
# Make hospital-level indicators
indicators <- hospital_data %>%
  arrange(Filer.EIN, TaxYr) %>%
  group_by(Filer.EIN) %>%
  summarise(
    # 1) Non-missing any_partnership_sameHRR in ALL years (2013-2023)
    any_sameHRR_nonmiss_all = all(!is.na(any_partnership_sameHRR)),
    
    # 2) Non-missing any_partnership_sameHRR in 2015-2020
    any_sameHRR_nonmiss_2014_2021 = {
      in_window <- TaxYr >= 2014 & TaxYr <= 2021
      all(!is.na(any_partnership_sameHRR[in_window]))
    },
    
    # 3) Dropout indicator: ≥3 consecutive non-missing for BOTH vars, then missing for the rest
    dropout_both_ind = {
      x <- !is.na(any_partnership_sameHRR) & !is.na(BDTOT)  # TRUE when both non-missing
      if (!any(x)) {
        FALSE
      } else {
        r <- rle(x)                          # run-length encoding of TRUE/FALSE
        ends <- cumsum(r$lengths)            # end positions of runs
        true_runs <- which(r$values)         # indices of TRUE runs
        if (length(true_runs) == 0) {
          FALSE
        } else {
          last_true_run <- tail(true_runs, 1)
          last_true_end <- ends[last_true_run]
          run_len <- r$lengths[last_true_run]
          # Dropout requires: final TRUE run does NOT reach last panel year,
          # and that terminal TRUE run has length ≥ 3
          last_true_end < length(x) && run_len >= 3L
        }
      }
    },
    
    # Dropout year: first year after the terminal non-missing streak (NA if no dropout)
    dropout_year = {
      x <- !is.na(any_partnership_sameHRR) & !is.na(BDTOT)
      if (!any(x)) {
        NA_integer_
      } else {
        r <- rle(x)
        ends <- cumsum(r$lengths)
        true_runs <- which(r$values)
        if (length(true_runs) == 0) {
          NA_integer_
        } else {
          last_true_run <- tail(true_runs, 1)
          last_true_end <- ends[last_true_run]
          run_len <- r$lengths[last_true_run]
          if (last_true_end < length(x) && run_len >= 3L) {
            TaxYr[last_true_end] + 1L    # first missing year after the streak
          } else {
            NA_integer_
          }
        }
      }
    },
    .groups = "drop"
  )

# If you want these indicators attached back to each row of the panel:
hospital_data <- hospital_data %>%
  left_join(indicators, by = "Filer.EIN")

# save hospital data
saveRDS(hospital_data, file=paste0(created_data_path, "hospital_data_boardandexec.rds"))

