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


###### PART 1: CLEAN DATA ###############################################################################################################################

# Read in cleaned up version of names found in 990 tax forms
people <- read_rds(paste0(created_data_path, "cleaned_people_data.rds"))

# Read is crosswalk of AHA ID - EIN, only keeping those who match to a standalone hospital
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds")) 

# Clean up the people data 
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

# create variable for number of people on board
number_board_people <- board_people %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(num_board = n()) %>%
  ungroup()


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

# Merge in AHA geographic information
AHA_geog <- AHA %>%
  select(ID, YEAR, SYSID, NETNAME, FSTCD, LONG, LAT, HRRCODE, MNAME) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# Are there any duplicates in ID, YEAR?
AHA_geog %>% 
  group_by(ID, YEAR) %>%
  summarise(n = n()) %>%
  filter(n > 1)

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

# Remove any observations with NA for ID at any point
board_people <- board_people %>%
  mutate(missing = ifelse(is.na(filer_id) & TaxYr %in% 2017:2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(missing)==0) %>%
  filter(TaxYr!=2015)



# Create hospital-level connections data
hospital_connections <- board_people %>%
  distinct(TaxYr, Filer.EIN, other_ein, filer_id, filer_sysid, filer_stcd, filer_long, filer_lat, filer_hrrcode,filer_name, filer_net,
           other_id, other_sysid, other_stcd, other_long, other_lat, other_hrrcode, other_name, other_net) 

# only keep connections in the same HRR
hospital_connections <- hospital_connections %>%
  filter(filer_hrrcode==other_hrrcode | is.na(other_hrrcode))

# only keep connections with hospitals not affiliated by system
hospital_connections <- hospital_connections %>%
  filter(is.na(filer_sysid) | is.na(other_sysid) | filer_sysid!=other_sysid)

# get rid of connections where the name indicates system affiliation even if system id is missing
hospital_connections <- hospital_connections %>%
  mutate(name_dist = stringdist::stringdist(str_extract(filer_name,"[A-Za-z]+\\s"), str_extract(other_name,"[A-Za-z]+\\s"), method = "jw")) %>%
  filter(name_dist!=0 | is.na(name_dist))


# If a hospital has a connection in a given year, remove the empty other_ein row
hospital_connections <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="", 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  filter(!(other_ein=="" & connected==1)) %>%
  distinct() %>%
  select(-connected)

# complete hospital connections to make sure we have every year
hospital_connections <- hospital_connections %>%
  complete(TaxYr=2017:2022, Filer.EIN) %>%
  group_by(Filer.EIN) %>%
  fill(filer_id, filer_sysid, filer_stcd, filer_long, filer_lat, filer_hrrcode,filer_name, 
       .direction = "downup") %>%
  ungroup()

rm(AHA_geog, AHA_hrr, ein_keep)

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
  mutate(connected = ifelse(other_ein!="" & !is.na(other_ein), 1, NA)) %>%
  group_by(TaxYr, filer_id) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%
  distinct(TaxYr, filer_id, connected, .keep_all = TRUE) 

# join number of people people
filer_data <- filer_data %>%
  mutate(TaxYr = as.character(TaxYr)) %>%
  left_join(number_board_people, by = c("TaxYr"="TaxYr", "Filer.EIN"="Filer.EIN"))


# join to AHA variables
filer_data <- filer_data %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
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

tab1_data <- filer_data %>%
  summarise_at(c("Number Beds" = "HOSPBD",
                 "General" = "general",
                 "Specialty" = "specialty",
                 "Adult" = "adult",
                 "Child" = "child",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "In Metrop. Area"="metro",
                 "Number of Board Members" = "num_board",
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

# graph the percent of hospitals that are connected in each year
num_conn_list <- filer_data %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected)) %>%
  mutate(n_connected = paste("n = ", n_connected))

filer_data %>%
  group_by(TaxYr) %>%
  summarise(m_connected = mean(connected)) %>%
  ggplot(aes(x=TaxYr)) +
  geom_line(aes(y=m_connected)) +
  labs(title = "",
       x = "\nYear",
       y = "Percent of Hospitals\n") +
  theme_minimal() + xlim(2017,2022) + ylim(0,.5) +
  labs(color='') +
  # add labels for the raw number of hospitals at each point using num_conn_list
  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3) +
  theme(text = element_text(size=15))

ggsave("Objects//connected_percent.pdf", width=6, height=3)

# how many HRRs are connected in each year?
num_conn_list <- filer_data %>%
  distinct(TaxYr, filer_hrrcode, connected) %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(connected)) %>%
  mutate(n_connected = paste("n = ", n_connected))


filer_data %>%
  group_by(TaxYr, filer_hrrcode) %>%
  summarise(connected = sum(connected)) %>%
  mutate(connected = ifelse(connected>0, 1, 0)) %>%
  group_by(TaxYr) %>%
  summarise(m_connected = mean(connected)) %>%
  ggplot(aes(x=TaxYr)) +
  geom_line(aes(y=m_connected)) +
  labs(title = "",
       x = "\nYear",
       y = "Percent of HRRs\n") +
  theme_minimal() + xlim(2017,2022) + ylim(0,.5) +
  labs(color='') +
  # add labels for the raw number of hospitals at each point using num_conn_list
  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3) +
  theme(text = element_text(size=15))

ggsave("Objects//connected_HRR_percent.pdf", width=6, height=3)

# plot geographically the hospitals that are connected
# read in map data
us_states <- map_data("state")

hospital_pairs <- hospital_connections %>%
  mutate(connected = ifelse(other_ein!="" & !is.na(other_ein), 1, NA)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  fill(connected, .direction = "downup") %>%
  ungroup() %>%
  mutate(connected = ifelse(is.na(connected), 0, connected))

plot_map <- function(df, year) {
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify hospitals with at least one connection
  connected_hospitals <- df_year %>%
    filter(connected==1) %>%
    select(Filer.EIN) %>%
    distinct()
  
  # Create nodes (all hospitals, even unconnected ones)
  nodes <- df_year %>%
    select(Filer.EIN, filer_lat, filer_long) %>%
    distinct(Filer.EIN, .keep_all = TRUE) %>%
    mutate(is_connected = ifelse(Filer.EIN %in% connected_hospitals$Filer.EIN, "Connected", "Unconnected"))
  
  
  # Filter hospitals with board connections
  df_filtered <- df_year %>%
    filter(connected==1)
  
  
  # Generate the plot
  ggplot() +
    # Draw U.S. state boundaries
    geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    # Draw connection lines
    geom_segment(data = df_filtered, 
                 aes(x = filer_long, y = filer_lat, xend = other_long, yend = other_lat),
                 color = "blue", alpha = 1, size = 0.75) +
    # Plot all hospitals as red dots
    geom_point(data = nodes, aes(x = filer_long, y = filer_lat, color = is_connected), 
               size = 0.01) +
    scale_color_manual(values = c("Connected" = "blue", "Unconnected" = "red")) + 
    theme_void() +
    labs(title = paste0("           ", year)) +
    coord_fixed(1.3)  +
    ylim(20,50) + xlim(-130,-60) +
    theme(legend.position = "none")
  
}


# 1. Geographic map of connection within the same HRR excluding systems
plot_map(hospital_pairs, 2017)
ggsave("Objects//connected_map_2017.pdf", width = 5, height = 3, units = "in")

# Generate maps for each year (2017-2022)
plots <- lapply(2017:2022, function(year) plot_map(hospital_pairs, year))

# Combine plots into a single graphic
# combine legends into one 
combined_plot <-  wrap_plots(plots, ncol = 2) 

# save combined plot
ggsave("Objects//connected_maps.pdf", width = 6, height = 6.5, units = "in")


# Summarise hospital pairs
hospital_connected_pairs <- hospital_pairs %>%
  select(TaxYr, filer_id, other_id) %>%
  filter(!is.na(filer_id) & !is.na(other_id))

AHA_pair <- AHA %>%
  select(YEAR, ID, SERV, BDTOT, MNAME, SYSID) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# join by filer_id
hospital_connected_pairs <- hospital_connected_pairs %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  rename(filer_SERV = SERV, filer_BDTOT = BDTOT, filer_name = MNAME, filer_sysid = SYSID) %>%
  left_join(AHA_pair, by=c("other_id"="ID", "TaxYr"="YEAR")) %>%
  rename(other_SERV = SERV, other_BDTOT = BDTOT, other_name = MNAME, other_sysid = SYSID)

# create variables capturing type of relationship (general - general/ general - specialty/specialty - specialty)
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(gen_gen = ifelse(filer_SERV==10 & other_SERV==10,1,0),
         gen_spec = ifelse((filer_SERV==10 & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) | (other_SERV==10 & filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)),1,0),
         spec_spec = ifelse(filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49),1,0),
         adult_child = ifelse((filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% 50:59) | (other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & filer_SERV %in% 50:59),1,0),
         adult_adult = ifelse((filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49)) | (other_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49)),1,0),
         child_child = ifelse((filer_SERV %in% 50:59 & other_SERV %in% 50:59) | (other_SERV %in% 50:59 & filer_SERV %in% 50:59),1,0))

# create variables capturing ownership relationship (ind - ind/sys - sys/ind - sys)
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(ind_ind = ifelse(is.na(filer_sysid) & is.na(other_sysid), 1, 0),
         sys_sys = ifelse(!is.na(filer_sysid) & !is.na(other_sysid), 1, 0),
         ind_sys = ifelse((is.na(filer_sysid) & !is.na(other_sysid)) | (!is.na(filer_sysid) & is.na(other_sysid)), 1, 0))


# put these values in a table 
pair_table <- hospital_connected_pairs %>%
  filter(gen_gen==1 | gen_spec==1 | spec_spec==1 | adult_child==1 | ind_ind==1 | sys_sys==1 | ind_sys==1) %>%
  summarise("General - General" = mean(gen_gen),
            "General - Specialty" = mean(gen_spec),
            "Specialty - Specialty" = mean(spec_spec),
            "Adult - Childrens" = mean(adult_child),
            "Adult - Adult" = mean(adult_adult),
            "Childrens - Childrens" = mean(child_child),
            "Ind. - Ind." = mean(ind_ind),
            "Sys. - Sys." = mean(sys_sys),
            "Ind. - Sys." = mean(ind_sys))

# pivot longer
pair_table <- pair_table %>%
  pivot_longer(cols = `General - General`:`Ind. - Sys.`, names_to = "variable", values_to = "value")

# add row for total number of connected hospitals
pair_table <- pair_table %>%
  add_row(variable = "Total Connected Hospitals", value = nrow(distinct(hospital_connected_pairs, filer_id)))

knitr::kable(pair_table, format = "latex",
             col.names = c("Type of Connected Pair", "Percent"),
             caption = "Types of Hospital Connections",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c"),
             position="ht!") %>%
  kable_styling(full_width=F) %>%
  write("Objects//hospital_pair_types.tex")

# Summarise different types of hospital connections

# Summarise the different types of hospitals
gen_connected <- hospital_connected_pairs %>%
  filter(gen_gen==1) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "General Connected to General")
n_gen_connected <- nrow(distinct(gen_connected, filer_id))

gen_connected_spec <- hospital_connected_pairs %>%
  filter(gen_spec==1) %>%
  filter(filer_SERV==10) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "General Connected to Specialty")
n_gen_connected_spec <- nrow(distinct(gen_connected_spec, filer_id))

spec_connected_gen <- hospital_connected_pairs %>%
  filter(gen_spec==1) %>%
  filter(other_SERV==10) %>%
  distinct(TaxYr, filer_id) %>%
  mutate(group = "Specialty Connected to General")
n_spec_connected_gen <- nrow(distinct(spec_connected_gen, filer_id))

gen_unconnected_sys <- hospital_pairs %>%
  filter(other_ein==""|is.na(other_ein)) %>%
  filter(!is.na(filer_sysid)) %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV==10) %>%
  mutate(group = "General Unconnected, Part of System") %>%
  select(TaxYr, filer_id, group)
n_gen_unconnected_sys <- nrow(distinct(gen_unconnected_sys, filer_id))

gen_unconnected <- hospital_pairs %>%
  filter(other_ein==""|is.na(other_ein)) %>%
  filter(is.na(filer_sysid)) %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV==10) %>%
  mutate(group = "General Unconnected") %>%
  select(TaxYr, filer_id, group)
n_gen_unconnected <- nrow(distinct(gen_unconnected, filer_id))

spec_unconnected_sys <- hospital_pairs %>%
  filter(other_ein==""|is.na(other_ein)) %>%
  filter(!is.na(filer_sysid)) %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) %>%
  mutate(group = "Specialty Unconnected, Part of System") %>%
  select(TaxYr, filer_id, group)
n_spec_unconnected_sys <- nrow(distinct(spec_unconnected_sys, filer_id))

spec_unconnected <- hospital_pairs %>%
  filter(other_ein==""|is.na(other_ein)) %>%
  filter(is.na(filer_sysid)) %>%
  distinct(TaxYr, filer_id) %>%
  left_join(AHA_pair, by=c("filer_id"="ID", "TaxYr"="YEAR")) %>%
  filter(SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) %>%
  mutate(group = "Specialty Unconnected") %>%
  select(TaxYr, filer_id, group)
n_spec_unconnected <- nrow(distinct(spec_unconnected, filer_id))

# combine the data
gen_hosp_connections <- bind_rows(gen_connected, gen_connected_spec, gen_unconnected_sys,
                                  gen_unconnected, spec_connected_gen, spec_unconnected, spec_unconnected_sys)

# get AHA variables I want to summarise in this table
AHA_gen <- AHA %>%
  select(YEAR, ID, GENBD, PEDBD, OBBD, MSICBD, CICBD, NICBD, NINTBD, PEDICBD, SYSID,
         BRNBD, SPCICBD, REHABBD, OTHICBD, ACULTBD, ALCHBD, PSYBD, SNBD88, ICFBD88,
         OTHLBD94, OTHBD94, HOSPBD, FTMT, FTRNTF, ICLABHOS, MCDDC, MCRDC, HRRCODE, MAPP5, MCRNUM, ACLABHOS)

gen_hosp_connections <- gen_hosp_connections %>%
  left_join(AHA_gen, by=c("filer_id"="ID", "TaxYr"="YEAR"))

gen_hosp_connections <- gen_hosp_connections %>%
  group_by(filer_id) %>%
  fill(MCRNUM, .direction="downup") %>%
  ungroup()

# how many non-NAs for MCRNUM? 
gen_hosp_connections %>%
  filter(!is.na(MCRNUM)) %>%
  distinct(filer_id) %>%
  nrow()

# Read in CMS provider patient utilization data
for (year in 2017:2021) {
  assign(paste0("util", year), read_csv(paste0("RawData/MedicareHospitalUtilization/Medicare_IP_Hospitals_by_Provider_",year,".csv")) %>% mutate(year=year))
}

util <- rbind(util2017, util2018, util2019, util2020, util2021) %>%
  select(year, Rndrng_Prvdr_CCN, Rndrng_Prvdr_RUCA_Desc, Tot_Benes:Bene_Avg_Age, Bene_Race_Wht_Cnt:Bene_Race_Othr_Cnt, Bene_CC_PH_Cancer6_V2_Pct, Bene_CC_PH_CKD_V2_Pct,
         Bene_CC_PH_COPD_V2_Pct, Bene_CC_PH_IschemicHeart_V2_Pct, Bene_Avg_Risk_Scre)

# join to gen_hosp_connections
gen_hosp_connections <- gen_hosp_connections %>%
  left_join(util, by=c("MCRNUM"="Rndrng_Prvdr_CCN", "TaxYr"="year"))

gen_hosp_connections <- gen_hosp_connections %>%
  filter(TaxYr %in% 2017:2021)

observe <- gen_hosp_connections %>%
  filter(!is.na(Rndrng_Prvdr_RUCA_Desc)) 

# fill missing variables when applicable
gen_hosp_connections <- gen_hosp_connections %>%
  group_by(filer_id) %>%
  fill(GENBD:OTHBD94, HRRCODE, .direction = "downup") %>%
  fill(Rndrng_Prvdr_RUCA_Desc, .direction = "downup") %>%
  ungroup()

# create variable for how concentrated services are (hhi from AHA beds)
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(hhi = (GENBD/HOSPBD)^2 + (PEDBD/HOSPBD)^2 + (OBBD/HOSPBD)^2 + (MSICBD/HOSPBD)^2 + (CICBD/HOSPBD)^2 + (NICBD/HOSPBD)^2 + (NINTBD/HOSPBD)^2 + 
           (PEDICBD/HOSPBD)^2 + (BRNBD/HOSPBD)^2 + (SPCICBD/HOSPBD)^2 + (OTHICBD/HOSPBD)^2 + (REHABBD/HOSPBD)^2 + (ALCHBD/HOSPBD)^2 + 
           (PSYBD/HOSPBD)^2 + (SNBD88/HOSPBD)^2 + (ICFBD88/HOSPBD)^2 + (ACULTBD/HOSPBD)^2 + (OTHLBD94/HOSPBD)^2 + (OTHBD94/HOSPBD)^2)

# create variable for how concentrated patients are in cancer, chronic kidney, COPD, and heart failure from the CMS data
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(cancer = Bene_CC_PH_Cancer6_V2_Pct,
         kidney = Bene_CC_PH_CKD_V2_Pct,
         copd = Bene_CC_PH_COPD_V2_Pct,
         heart = Bene_CC_PH_IschemicHeart_V2_Pct) %>%
  mutate(hhi_cms = (cancer)^2 + (kidney)^2 + (copd)^2 + (heart)^2)

# create new variables for whether each service is offered or not
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(GEN = ifelse(GENBD>0, 1, 0),
         PED = ifelse(PEDBD>0, 1, 0),
         OB = ifelse(OBBD>0, 1, 0),
         MSIC = ifelse(MSICBD>0, 1, 0),
         CIC = ifelse(CICBD>0, 1, 0),
         NIC = ifelse(NICBD>0, 1, 0),
         NINT = ifelse(NINTBD>0, 1, 0),
         PEDIC = ifelse(PEDICBD>0, 1, 0),
         BRN = ifelse(BRNBD>0, 1, 0),
         SPCIC = ifelse(SPCICBD>0, 1, 0),
         OTHIC = ifelse(OTHICBD>0, 1, 0),
         REHAB = ifelse(REHABBD>0, 1, 0),
         ALCH = ifelse(ALCHBD>0, 1, 0),
         PSY = ifelse(PSYBD>0, 1, 0),
         SN = ifelse(SNBD88>0, 1, 0),
         ICF = ifelse(ICFBD88>0, 1, 0),
         ACULT = ifelse(ACULTBD>0, 1, 0),
         OTHL = ifelse(OTHLBD94>0, 1, 0),
         OTH = ifelse(OTHBD94>0, 1, 0))

# same but for CMS
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(cancer = ifelse(!is.na(cancer) & cancer>0, 1, 0),
         kidney = ifelse(!is.na(kidney) & kidney>0, 1, 0),
         copd = ifelse(!is.na(copd) & copd>0, 1, 0),
         heart = ifelse(!is.na(heart) & heart>0, 1, 0))

gen_hosp_connections <- gen_hosp_connections %>%
  mutate(academic = ifelse(MAPP5==1,1,0)) %>%
  mutate(metro = ifelse(str_detect(Rndrng_Prvdr_RUCA_Desc, "Metropolitan"),1,0),
         sys = ifelse(is.na(SYSID),0,1))   

# Create variables for medicare and medicaid patients per bed
gen_hosp_connections <- gen_hosp_connections %>%
  mutate(Medicare_Per_Bed = ifelse(HOSPBD>0, MCRDC/HOSPBD, NA),
         Medicaid_Per_Bed = ifelse(HOSPBD>0, MCDDC/HOSPBD, NA))

# create general summary stats for all hospitals


# create summary stats table for connected vs. unconnected hospitals
table_data <- gen_hosp_connections %>%
  group_by(group) %>%
  summarise("Number of Beds"=mean(HOSPBD,na.rm=T), 
            "Number of Medicare Patients (CMS)"=mean(Tot_Benes,na.rm=T), 
            "Avg Medicare Patient Age"=mean(Bene_Avg_Age,na.rm=T), 
            "Academic Medical Center"=mean(academic,na.rm=T), 
            "In Metropolitan Area"=mean(metro,na.rm=T), 
            "Has a NICU"=mean(NIC,na.rm=T), 
            "Has a Cath Lab"=mean(ACLABHOS,na.rm=T), 
            "Service Concentration (beds)"=mean(hhi,na.rm=T), 
            "Service Concentration (conditions)"=mean(hhi_cms,na.rm=T),
            "Avg Patient Risk Score"=mean(Bene_Avg_Risk_Scre,na.rm=T), 
            "Medicare Patients per Bed (AHA)"=mean(Medicare_Per_Bed,na.rm=T),
            "Medicaid Patients per Bed (AHA)"=mean(Medicaid_Per_Bed,na.rm=T),
            "In System"=mean(sys,na.rm=T)) %>%
  t() %>%
  as.data.frame() %>%
  # make row names into its own column
  tibble::rownames_to_column("Variable") 
colnames(table_data) <- table_data[1,]
table_data <- table_data %>%
  filter(group!="group") %>%
  mutate_at(vars(-group), as.numeric) %>%
  add_row(group = "Num. Hospitals", 
          `General Connected to General` = n_gen_connected,
          `General Connected to Specialty` = n_gen_connected_spec,
          `Specialty Connected to General` = n_spec_connected_gen,
          `General Unconnected` = n_gen_unconnected,
          `Specialty Unconnected` = n_spec_unconnected,
          `General Unconnected, Part of System` = n_gen_unconnected_sys,
          `Specialty Unconnected, Part of System` = n_spec_unconnected_sys)

gen_table_data <- table_data %>%
  select(group, `General Connected to General`, `General Unconnected, Part of System`, `General Unconnected`) 

# one table devoted to general hospitals   
kable(gen_table_data[c(1,4,5,13,12,11,2,3,10,7,6,8,9,14),], 
      format = "latex",
      col.names = c("Variable", "Affiliated w/ General", "Affiliated w/ Specialty", "Unaffiliated, Part of System", "Unaffiliated, Not Part of System"),
      caption = "Summary Statistics of General Hospitals",
      row.names = FALSE,
      table.envir="table",
      digits=2,
      booktabs=TRUE,
      escape=F,
      align=c("l","c","c","c","c","c"),
      position="ht!") %>%
  pack_rows("Characteristics", 1, 4) %>%
  pack_rows("Patients", 5, 9) %>%
  pack_rows("Services", 10, 13) %>%
  write("Objects//hospital_general_summarystats.tex")

# separate table for specialty hospitals
spec_table_data <- table_data %>%
  select(group, `Specialty Connected to General`, `Specialty Unconnected, Part of System`, `Specialty Unconnected`)
kable(spec_table_data[c(1,4,5,13,12,11,2,3,10,7,6,8,9,14),],
      format = "latex",
      col.names = c("Variable", "Affiliated w/ General", "Unaffiliated, Part of System", "Unaffiliated, Not Part of System"),
      caption = "Summary Statistics of Specialty Hospitals",
      row.names = FALSE,
      table.envir="table",
      digits=2,
      booktabs=TRUE,
      escape=F,
      align=c("l","c","c","c"),
      position="ht!") %>%
  pack_rows("Characteristics", 1, 4) %>%
  pack_rows("Patients", 5, 9) %>%
  pack_rows("Services", 10, 13) %>%
  write("Objects//hospital_specialty_summarystats.tex")

