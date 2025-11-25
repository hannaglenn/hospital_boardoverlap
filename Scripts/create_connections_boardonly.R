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
  data <- board_people %>%
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

# only keep relevant variables and board members
board_people <- people %>%
  filter(position=="board") 

# map nicknames to longer versions
board_people <- normalize_nicknames(board_people, nickname_map)

# remove initials
board_people <- board_people %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "\\b[a-z]\\b")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# match names within the same EIN that are similar based on token overlap
board_people <- combine_token_matches(board_people, min_shared_tokens = 2)

# replace dashes with a space
board_people <- board_people %>%
  mutate(name_cleaned = str_replace_all(name_cleaned, "-", " ")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# order each name alphabetically
board_people <- board_people %>%
  mutate(name_cleaned = map_chr(name_cleaned, ~ {
    words <- str_split(.x, "\\s+")[[1]]
    sorted_words <- sort(words)
    str_c(sorted_words, collapse = " ")
  }))

# standardize names within the same EIN
board_people <- combine_fuzzy_matches(board_people, max_dist = 2)

# misc name fixes
board_people <- board_people %>%
  mutate(name_cleaned = ifelse(name_cleaned=="sabotka tj", "sabotka thomas", name_cleaned),
         name_cleaned = ifelse(name_cleaned=="jim sabetta", "james sabetta", name_cleaned),
         name_cleaned = ifelse(name_cleaned=="morr robert", "morrisey robert", name_cleaned)) 

# look at distinct names to check validity
# observe <- board_people %>%
#   distinct(Filer.EIN, name_cleaned)
# write.csv(observe, paste0(created_data_path, "observe.csv"))

# assign unique identifiers to names that share at least 2 tokens across firm, years
board_people <- assign_person_id(board_people)

# Create variables of board members connected to multiple EINs ---------------------------------------------------
connections <- identify_common_members(board_people)

# separate other_eins into multiple columns
connections <- connections %>%
  separate_wider_delim(other_eins, delim = ",", names_sep = "", too_few="align_start")

# wide to long in other_eins
connections <- connections %>%
  pivot_longer(cols = starts_with("other_eins"), names_to = "num_board", values_to = "other_ein")

# Remove columns with NA
connections <- connections %>%
  mutate(other_ein = str_trim(other_ein)) %>%
  filter(!is.na(other_ein)) %>%
  select(TaxYr, name_cleaned, gender, position, doctor, nurse, other_doctor, ha, nonvoting, Filer.EIN, other_ein)

# Join each EIN to their AHA
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds"))
connections <- connections %>%
  left_join(cw, by="Filer.EIN") %>%
  rename(Filer.ID = ID)
connections <- connections %>%
  left_join(cw, by=c("other_ein"="Filer.EIN")) %>%
  rename(other.id = ID)

# create hospital level data set to merge back to later
hospital_data <- board_people %>%
  distinct(TaxYr, Filer.EIN) %>%
  left_join(cw) %>%
  rename(Filer.ID=ID)

# Get rid of duplicates
connections <- connections %>%
  distinct()


# join AHA information to the filer and other id
AHA <- read_csv(paste0(raw_data_path, "\\AHAdata_20052023.csv")) 

AHA_hrr <- AHA %>%
  select(ID, YEAR, HRRCODE, SYSID, MNAME, SERV) %>%
  filter(YEAR>=2015 & YEAR<=2023) %>%
  mutate(YEAR = as.character(YEAR))

connections <- connections %>%
  left_join(AHA_hrr, by=c("Filer.ID"="ID", "TaxYr"="YEAR")) %>%
  rename(filer.hrr=HRRCODE, filer.sysid=SYSID, filer.name=MNAME, filer.type=SERV)

# fill missing variables when possible
connections <- connections %>%
  group_by(Filer.ID) %>%
  fill(filer.hrr, filer.type, filer.name, .direction="downup") %>%
  ungroup()

connections <- connections %>%
  left_join(AHA_hrr, by=c("other.id"="ID", "TaxYr"="YEAR")) %>%
  rename(other.hrr=HRRCODE, other.sysid=SYSID, other.name=MNAME, other.type=SERV)

# fill missing variables when possible
connections <- connections %>%
  group_by(other.id) %>%
  fill(other.hrr, other.type, other.name, .direction="downup") %>%
  ungroup()

# Create classifications for each connection type

# define competitor vs. non-competitor (like hospitals, regardless of location)
connections <- connections %>%
  mutate(name_dist = stringdist::stringdist(str_extract(filer.name,"[A-Za-z]+\\s"), str_extract(other.name,"[A-Za-z]+\\s"), method = "jw")) %>%
  mutate(competitor = ifelse(name_dist>0 & (filer.sysid!=other.sysid | is.na(filer.sysid) | is.na(other.sysid)), 1, 0)) %>%
  mutate(missing_hrr_info = ifelse(is.na(competitor),1,0))

# Define indicators to describe the 4 types of connections
connections <- connections %>%
  mutate(sameHRR_competing = ifelse(filer.hrr==other.hrr & competitor==1, 1, 0),
         diffHRR_competing = ifelse(filer.hrr!=other.hrr & competitor==1, 1, 0),
         sameHRR_noncompeting = ifelse(filer.hrr==other.hrr & competitor==0, 1, 0),
         diffHRR_noncompeting = ifelse(filer.hrr!=other.hrr & competitor==0, 1, 0))

people_connections <- connections %>%
  group_by(TaxYr, Filer.EIN, name_cleaned) %>%
  summarise(
    doctor = max(doctor, na.rm = TRUE),
    nurse = max(nurse, na.rm = TRUE),
    other_doctor = max(other_doctor, na.rm = TRUE),
    ha = max(ha, na.rm = TRUE),
    nonvoting = max(nonvoting, na.rm = TRUE),
    position = first(position),  # or paste(unique(position), collapse = ";")
    gender = first(gender),
    sameHRR_competing = max(sameHRR_competing, na.rm = TRUE),
    diffHRR_competing = max(diffHRR_competing, na.rm = TRUE),
    sameHRR_noncompeting = max(sameHRR_noncompeting, na.rm = TRUE),
    diffHRR_noncompeting = max(diffHRR_noncompeting, na.rm = TRUE)
  )

people_connections <- people_connections %>%
  ungroup() %>%
  mutate(sameHRR_competing = ifelse(sameHRR_competing=="-Inf",0,sameHRR_competing),
         sameHRR_noncompeting = ifelse(sameHRR_noncompeting=="-Inf",0,sameHRR_noncompeting),
         diffHRR_competing = ifelse(diffHRR_competing=="-Inf",0,diffHRR_competing),
         diffHRR_noncompeting = ifelse(diffHRR_noncompeting=="-Inf",0,diffHRR_noncompeting))

# save connections data
saveRDS(people_connections, file=paste0(created_data_path, "people_connections_boardonly.rds"))

# summarize connections at the hospital level -----------------------------------------------
hospital_connections <- connections %>%
  filter(!is.na(competitor)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(any_sameHRR_competing = max(sameHRR_competing, na.rm=T),
         perc_sameHRR_competing = mean(sameHRR_competing, na.rm=T)) %>%
  mutate(any_diffHRR_competing = max(diffHRR_competing, na.rm=T),
         perc_diffHRR_competing = mean(diffHRR_competing, na.rm=T)) %>%
  mutate(any_sameHRR_noncompeting = max(sameHRR_noncompeting, na.rm=T),
         perc_sameHRR_noncompeting = mean(sameHRR_noncompeting, na.rm=T)) %>%
  mutate(any_diffHRR_noncompeting = max(diffHRR_noncompeting, na.rm=T),
         perc_diffHRR_noncompeting = mean(diffHRR_noncompeting, na.rm=T)) %>%
  ungroup() %>%
  distinct(TaxYr, Filer.EIN, 
           any_sameHRR_competing, perc_sameHRR_competing,
           any_diffHRR_competing, perc_diffHRR_competing,
           any_sameHRR_noncompeting, perc_sameHRR_noncompeting,
           any_diffHRR_noncompeting, perc_diffHRR_noncompeting)


# join back to hospital data
hospital_data <- hospital_data %>%
  left_join(hospital_connections, by=c("TaxYr", "Filer.EIN")) %>%
  mutate(any_sameHRR_competing = ifelse(is.na(any_sameHRR_competing),0,any_sameHRR_competing),
         perc_sameHRR_competing = ifelse(is.na(perc_sameHRR_competing),0,perc_sameHRR_competing),
         any_sameHRR_noncompeting = ifelse(is.na(any_sameHRR_noncompeting),0,any_sameHRR_noncompeting),
         perc_sameHRR_noncompeting = ifelse(is.na(perc_sameHRR_noncompeting),0,perc_sameHRR_noncompeting),
         any_diffHRR_competing = ifelse(is.na(any_diffHRR_competing),0,any_diffHRR_competing),
         perc_diffHRR_competing = ifelse(is.na(perc_diffHRR_competing),0,perc_diffHRR_competing),
         any_diffHRR_noncompeting = ifelse(is.na(any_diffHRR_noncompeting),0,any_diffHRR_noncompeting),
         perc_diffHRR_noncompeting = ifelse(is.na(perc_diffHRR_noncompeting),0,perc_diffHRR_noncompeting))
  

# only keep hospitals present in 2017-2021
hospital_data <- hospital_data %>%
  mutate(count = ifelse(TaxYr %in% c(2017:2021),1,0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(count)==5) %>%
  ungroup() %>%
  mutate(TaxYr = as.numeric(TaxYr))


# find the first year the hospital gained each type of connections (0 if not connected in the relevant way)
minyr_sameHRR_comp <- hospital_data %>%
  filter(any_sameHRR_competing==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_sameHRR_comp = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_sameHRR_comp)
minyr_diffHRR_comp <- hospital_data %>%
  filter(any_diffHRR_competing==1) %>%
  group_by(Filer.EIN) %>%
  mutate(minyr_diffHRR_comp = min(TaxYr)) %>%
  ungroup() %>%
  distinct(Filer.EIN, minyr_diffHRR_comp)

# merge back to data
hospital_data <- hospital_data %>%
  left_join(minyr_sameHRR_comp, by="Filer.EIN") %>%
  left_join(minyr_diffHRR_comp, by="Filer.EIN") 

hospital_data <- hospital_data %>%
  mutate(minyr_sameHRR_comp = ifelse(is.na(minyr_sameHRR_comp),0,minyr_sameHRR_comp),
         minyr_diffHRR_comp = ifelse(is.na(minyr_diffHRR_comp),0,minyr_diffHRR_comp))

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
saveRDS(hospital_data, file=paste0(created_data_path, "hospital_data_boardonly.rds"))


  
# create data set for comparison of hosp who become connected in same HRR vs. diff HRR
reg1_data <- hospital_data %>%
  filter(TaxYr!=2016 & TaxYr!=2022) %>%
  group_by(Filer.EIN) %>%
  arrange(TaxYr) %>%
  mutate(lag_treat = dplyr::lag(any_sameHRR_competing)) %>%
  mutate(lead_treat = dplyr::lead(any_sameHRR_competing)) %>%
  mutate(any_sameHRR_competing = ifelse(lag_treat==1 & lead_treat==1,1,any_sameHRR_competing)) %>%
  mutate(drop = ifelse(lag_treat==1 & any_sameHRR_competing==0,1,NA)) %>%
  fill(drop, .direction="downup") %>%
  ungroup() %>%
  filter(is.na(drop)) %>%
  filter(minyr_sameHRR_comp!=0 | minyr_diffHRR_comp!=0) 

# only observations that don't have multiple events or the same board connection happens later
reg1_data <- reg1_data %>%
  filter(minyr_diffHRR_comp==0 | minyr_sameHRR_comp==0 | minyr_sameHRR_comp>minyr_diffHRR_comp)

# create "group" for whether same HRR or different HRR
reg1_data <- reg1_data %>%
  mutate(group = ifelse(minyr_sameHRR_comp==0, "Different HRR", "Same HRR"))

# get rid of those who were already connected in 2016
reg1_data <- reg1_data %>%
  filter(minyr_sameHRR_comp!=2016 & minyr_sameHRR_comp!=2020 & !(group=="Different HRR" & minyr_diffHRR_comp==2016))

# create relative year variable
reg1_data <- reg1_data %>%
  mutate(TaxYr=as.numeric(TaxYr),
         minyr_sameHRR_comp = as.numeric(minyr_sameHRR_comp),
         minyr_diffHRR_comp = as.numeric(minyr_diffHRR_comp)) %>%
  mutate(rel_year = ifelse(group=="Same HRR", minyr_sameHRR_comp-TaxYr, minyr_diffHRR_comp-TaxYr))


# summarize means over time for each group
rawmeans_reg1 <- reg1_data %>%
  group_by(rel_year, group) %>%
  summarise_at(c("perc_mcare", "perc_mcaid", "bed_conc"),
               list(m=mean), na.rm=T)

rawmeans_reg1 <- rawmeans_reg1 %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = ends_with("_m"),
               names_to = "variable",
               values_to = "mean")

ggplot(rawmeans_reg1, aes(x = rel_year, y = mean, colour = group, linetype = variable)) +
  geom_line(aes(group = interaction(group, variable)), size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Raw Means by Group and Variable Over Event Time",
    x = "Years Relative to Event",
    y = "Mean Value",
    colour = "Group",
    linetype = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

table(reg1_data$minyr_sameHRR_comp)
reg1_data %>%
  filter(minyr_sameHRR_comp!=0) %>%
  distinct(Filer.EIN) %>%
  nrow()

