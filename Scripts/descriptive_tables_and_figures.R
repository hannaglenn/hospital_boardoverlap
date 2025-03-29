# load libraries 
library(dplyr)
library(fuzzyjoin)
library(stringdist)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(readr)
library(data.table)
library(kableExtra)
library(purrr)
library(ggpubr)
library(did)

created_data_path <- "CreatedData/"


## SCRIPT CREATING SUMMARY STATISTICS FOR THIRD CHAPTER ################

# Part 1: Clean the Data ---------------------------------------------------------
# Read in cleaned up version of names found in 990 tax forms
people <- read_rds(paste0(created_data_path, "cleaned_people_data.rds"))
# Read is crosswalk of AHA ID - EIN, only keeping those who match to a standalone hospital
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds")) 



# only keep people whose EIN is in the crosswalk as a standalone hospital
people <- people %>% filter(Filer.EIN %in% cw$Filer.EIN)

# only keep people who are board members
board_people <- people %>%
  filter(board_member==1) %>%
  select(TaxYr, Filer.EIN, name_cleaned)

# Only keep EINs present in 2017-2021
ein_keep <- board_people %>%
  distinct(TaxYr, Filer.EIN) %>%
  mutate(count = ifelse(TaxYr %in% 2017:2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  summarise(count = sum(count)) %>%
  filter(count>=5)

board_people <- board_people %>%
  filter(Filer.EIN %in% ein_keep$Filer.EIN)

rm(ein_keep)

# clean up extras in names
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "deceased")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\seff$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scsjp$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scisa$")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "^mrs\\s")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "^reverend\\s")
board_people$name_cleaned <- str_remove(board_people$name_cleaned, "\\scns$")
board_people$name_cleaned <- str_remove_all(board_people$name_cleaned, "january|february|march|june|july|august|september|october|november|december|dec\\b")

# get rid of empty names
board_people <- board_people %>%
  filter(name_cleaned!="" & !str_detect(name_cleaned, "ceo|cfo"))

# define function to normalize the name columns to take care of different order of first, last
normalize_name <- function(name) {
  # Split by spaces
  parts <- unlist(strsplit(name, " "))
  
  # Remove single-letter initials (optional)
  parts <- parts[nchar(parts) > 1]
  
  # Sort non-initial parts alphabetically
  paste(sort(parts), collapse = " ")
}

board_people$name_cleaned <- sapply(board_people$name_cleaned, normalize_name)

# remove empty names and one-word names
board_people <- board_people %>%
  filter(str_detect(name_cleaned, "[a-zA-Z]")) %>%
  filter(str_count(name_cleaned, " ")>0)

# join AHA data to get HRR and state
AHA <- read_csv("RawData/AHAdata_20052023.csv") 

AHA_hrr <- AHA %>%
  select(ID, YEAR, HRRCODE, SYSID, MNAME, NETWRK, NETNAME) %>%
  filter(YEAR>=2015 & YEAR<=2023) %>%
  mutate(YEAR = as.character(YEAR))

board_people <- board_people %>%
  left_join(cw, by = c("Filer.EIN")) %>%
  left_join(AHA_hrr, by = c("ID", "TaxYr"="YEAR"))

# fill HRR code
board_people <- board_people %>%
  group_by(Filer.EIN) %>%
  fill(HRRCODE, .direction = "downup") %>%
  ungroup()

# how many IDs?
length(unique(board_people$ID))
#1520

# run the script titled "function1_standardize_names"
source("Scripts//function1_standardize_names.R")

# combine names that have a slight misspelling using the standardize names function
# these have to be in the same EIN to be combined
board_people <- standardize_names_optimized(board_people, max_dist = 3)

# also standardize names that are a very close match within the same HRR (more strict matches)
board_people <- board_people %>%
  standardize_names_by_hrr(., max_dist = 2)


# run the script titled "function2_find_common_board_members"
source("Scripts//function2_identify_common_members.R")

# identify common members on the board of directors
board_people <- identify_common_members(board_people)

# remove variables we don't need
board_people <- board_people %>%
  select(Filer.EIN, TaxYr, name_cleaned, other_eins)

# separate other_eins into multiple columns
board_people <- board_people %>%
  separate_wider_delim(other_eins, delim = ",", names_sep = "", too_few="align_start")

# distinct
board_people <- board_people %>%
  distinct(Filer.EIN, TaxYr, name_cleaned, other_eins1, other_eins2, other_eins3)

# wide to long in other_eins
board_people <- board_people %>%
  pivot_longer(cols = starts_with("other_eins"), names_to = "num_board", values_to = "other_ein")

# remove NA values in other_ein
board_people <- board_people %>%
  filter(!(num_board=="other_eins2" & is.na(other_ein))) %>%
  filter(!(num_board=="other_eins3" & is.na(other_ein)))

# trim the white space on other_ein
board_people$other_ein <- str_trim(board_people$other_ein)

# Merge in information about the root and connected hospital
AHA_geog <- AHA %>%
  select(ID, YEAR, SYSID, NETNAME, FSTCD, LONG, LAT, HRRCODE, MNAME) %>%
  filter(YEAR>=2016 & YEAR<=2022)
  # no duplicates in ID, YEAR

# Merge AHA data to the AHA - EIN crosswalk
cw <- cw %>%
  left_join(AHA_geog, by = "ID") 

# Merge AHA data to the board_people data
board_people <- board_people %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(cw, by = c("Filer.EIN", "TaxYr"="YEAR"))

# Rename variables to show that they are the filer geographic information
board_people <- board_people %>%
  rename(filer_id=ID, filer_sysid = SYSID, filer_stcd = FSTCD, filer_long = LONG, filer_lat = LAT, filer_hrrcode = HRRCODE,
         filer_name = MNAME, filer_net = NETNAME)

# Merge AHA data to the other_ein data
board_people <- board_people %>%
  left_join(cw, by = c("other_ein" = "Filer.EIN", "TaxYr"="YEAR"))

# Rename variables to show that they are the other_ein geographic information
board_people <- board_people %>%
  rename(other_id=ID, other_sysid = SYSID, other_stcd = FSTCD, other_long = LONG, other_lat = LAT, other_hrrcode = HRRCODE,
         other_name = MNAME, other_net = NETNAME)

# Remove any observations with NA for ID in the relevant years
board_people <- board_people %>%
  filter(TaxYr %in% 2017:2022) %>%
  mutate(missing = ifelse(is.na(filer_id), 1, 0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(missing)==0) %>%
  ungroup() %>%
  select(-missing)

# change all other_ variables to NA if the connection is in the same HRR
board_people <- board_people %>%
  mutate(other_sysid = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_sysid),
         other_stcd = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_stcd),
         other_long = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_long),
         other_lat = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_lat),
         other_name = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_name),
         other_net = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_net),
         other_ein = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), "", other_ein),
         other_id = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_id),
         other_hrrcode = ifelse(filer_hrrcode==other_hrrcode| is.na(other_hrrcode), NA, other_hrrcode))

# do the same for connections in the same system
board_people <- board_people %>%
  mutate(other_sysid = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_sysid),
         other_stcd = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_stcd),
         other_long = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_long),
         other_lat = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_lat),
         other_name = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_name),
         other_net = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_net),
         other_ein = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), "", other_ein),
         other_id = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_id),
         other_hrrcode = ifelse(filer_sysid==other_sysid| is.na(other_sysid)| is.na(filer_sysid), NA, other_hrrcode))


# get rid of connections where the name indicates system affiliation even if system id is missing
board_people <- board_people %>%
  mutate(name_dist = stringdist::stringdist(str_extract(filer_name,"[A-Za-z]+\\s"), str_extract(other_name,"[A-Za-z]+\\s"), method = "jw")) %>%
  mutate(other_sysid = ifelse(name_dist==0 | is.na(name_dist), NA, other_sysid),
         other_stcd = ifelse(name_dist==0 | is.na(name_dist), NA, other_stcd),
         other_long = ifelse(name_dist==0 | is.na(name_dist), NA, other_long),
         other_lat = ifelse(name_dist==0 | is.na(name_dist), NA, other_lat),
         other_name = ifelse(name_dist==0 | is.na(name_dist), NA, other_name),
         other_net = ifelse(name_dist==0 | is.na(name_dist), NA, other_net),
         other_ein = ifelse(name_dist==0 | is.na(name_dist), "", other_ein),
         other_id = ifelse(name_dist==0 | is.na(name_dist), NA, other_id),
         other_hrrcode = ifelse(name_dist==0 | is.na(name_dist), NA, other_hrrcode)) %>%
  select(-name_dist)

# get rid rows where other_ein is 2 or higher and missing
board_people <- board_people %>%
  filter(!(num_board=="other_eins2" & is.na(other_ein))) %>%
  filter(!(num_board=="other_eins3" & is.na(other_ein)))

# create a variable for the number of people on the board in each Filer.EIN, year
board_people <- board_people %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(num_people = n()) %>%
  ungroup()

# create a data set aggregating to the hospital level
hospital_connections <- board_people %>%
  distinct(TaxYr, Filer.EIN, other_ein, filer_id, filer_sysid, filer_stcd, filer_long, filer_lat, filer_hrrcode,filer_name, filer_net,
           other_id, other_sysid, other_stcd, other_long, other_lat, other_hrrcode, other_name, other_net, num_people)

# If a hospital has a connection in a given year, remove the empty other_ein row
hospital_connections <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="" | is.na(other_ein), 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  filter(!((other_ein=="" | is.na(other_ein)) & connected==1)) %>%
  distinct() %>%
  select(-connected)

rm(AHA_geog, AHA_hrr)

AHA_variables <- AHA %>%
  select(YEAR, ID, SYSID, HRRCODE, LAT, LONG, MAPP5, MCRNUM, SERV,
         GENBD, PEDBD, OBBD, MSICBD, CICBD, NICBD, NINTBD, PEDICBD, SYSID,
         BRNBD, SPCICBD, REHABBD, OTHICBD, ACULTBD, ALCHBD, PSYBD, SNBD88, ICFBD88,
         OTHLBD94, OTHBD94, HOSPBD,
         FTMT, FTRNTF,
         ICLABHOS, ACLABHOS,
         MCDDC, MCRDC)





## Part 2: create tables and figures ---------------------------------------------------------
# table 1: general summary statistics of the whole sample 
filer_data <- hospital_connections %>%
  mutate(connected = ifelse(other_ein=="" | is.na(other_ein), 0, 1)) %>%
  distinct(TaxYr, filer_id, connected, num_people) 

# join to AHA variables
filer_data <- filer_data %>%
  left_join(AHA_variables, by = c("TaxYr"="YEAR", "filer_id"="ID"))

# Read in Medicare files 
for (year in 2017:2021) {
  assign(paste0("util", year), read_csv(paste0("RawData/MedicareHospitalUtilization/Medicare_IP_Hospitals_by_Provider_",year,".csv")) %>% mutate(year=year))
}

mcare <- rbind(util2017, util2018, util2019, util2020, util2021) %>%
  select(year, Rndrng_Prvdr_CCN, Rndrng_Prvdr_RUCA_Desc, Tot_Benes:Bene_Avg_Age, Bene_Race_Wht_Cnt:Bene_Race_Othr_Cnt, Bene_CC_PH_Cancer6_V2_Pct, Bene_CC_PH_CKD_V2_Pct,
         Bene_CC_PH_COPD_V2_Pct, Bene_CC_PH_IschemicHeart_V2_Pct, Bene_Avg_Risk_Scre)
rm(util2017, util2018, util2019, util2020, util2021)

# join to filer_data
filer_data <- filer_data %>%
  left_join(mcare, by = c("MCRNUM"="Rndrng_Prvdr_CCN", "TaxYr"="year"))

# create indicator variables
filer_data <- filer_data %>%
  mutate(general = ifelse(SERV==10,1,0),
         specialty = ifelse(SERV %in% c(13,22,33,41,42,44,45,46,47,48,49),1,0),
         adult = ifelse(SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49),1,0),
         child = ifelse(SERV %in% 50:59,1,0),
         sys = ifelse(is.na(SYSID),0,1),
         academic = ifelse(MAPP5==1,1,0),
         metro = ifelse(str_detect(Rndrng_Prvdr_RUCA_Desc, "Metro"),1,0)
  )

# filter to sufficient number of beds
filer_data <- filer_data %>%
  filter(HOSPBD>=10)

observe <- filer_data %>%
  select(TaxYr, filer_id, HOSPBD, general, specialty, adult, child, sys, academic, metro, num_people, connected)

tab1_data <- filer_data %>%
  summarise_at(c("Number Beds" = "HOSPBD",
                 "General" = "general",
                 "Specialty" = "specialty",
                 "Adult" = "adult",
                 "Child" = "child",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "In Metrop. Area"="metro",
                 "Number of Board Members" = "num_people",
                 "Overlapping Board"="connected"), 
               list(m=mean,min=min,max=max,sd=sd), na.rm=T) %>%
  mutate_if(is.numeric, ~ifelse(abs(.)==Inf,NA,.))  %>%
  gather(key=var,value=value) %>%
  extract(col="var",into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>%
  relocate(variable,m,min,max,sd) %>%
  add_row(variable = "Number of Hospitals", m = distinct(filer_data, filer_id) %>% nrow()) %>%
  mutate(m = ifelse(m>9, round(m,0), round(m,2)),
         min = ifelse(min>9, round(min,0), round(min,2)),
         max = ifelse(max>9, round(max,0), round(max,2)),
         sd = ifelse(sd>9, round(sd,0), round(sd,2))) 

knitr::kable(tab1_data[c(7,4,10,2,3,1,5,6,8,9,11),],
               format = "latex",
               col.names = c("Variable", "Mean", "Min", "Max", "SD"),
               caption = "Full Sample Summary Statistics\\label{all_sumstats}",
               row.names = FALSE,
               table.envir="table",
               digits=2,
               booktabs=TRUE,
               escape=F,
               align=c("l","c","c","c","c"),
               position="ht!") %>%
  pack_rows("Characteristics", 1, 8) %>%
  pack_rows("Board Characteristics", 9, 10) %>%
  write("Objects//all_summarystats.tex")
