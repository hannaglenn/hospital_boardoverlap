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

source("./Scripts/paths.R")

people_data <- readRDS(paste0(created_data_path, "people_connections_boardandexec.rds"))
hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))
hospital_connections <- readRDS(paste0(created_data_path, "hospital_connections_boardandexec.rds"))
AHA <- read_csv(paste0(raw_data_path, "\\AHAdata_20052023.csv")) 

# filter out hospitals that turn their overlap on and off
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  arrange(TaxYr) %>%
  mutate(lag_treat = dplyr::lag(any_partnership_sameHRR)) %>%
  mutate(lead_treat = dplyr::lead(any_partnership_sameHRR)) %>%
  mutate(any_partnership_sameHRR = ifelse(!is.na(lag_treat) & !is.na(lead_treat) & lag_treat==1 & lead_treat==1,1,any_partnership_sameHRR)) %>%
  mutate(drop = ifelse(lag_treat==1 & any_partnership_sameHRR==0,1,NA)) %>%
  fill(drop, .direction="downup") %>%
  ungroup() %>%
  filter(is.na(drop)) 

# drop 2016 because the sample of hospitals is smaller
hospital_data <- hospital_data %>%
  filter(TaxYr!=2016)

# create variables for always, become, and never connected in the same and different HRRs (for partnerships)
hospital_data <- hospital_data %>%
  mutate(always_same = ifelse(minyr_sameHRR_part==2017,1,0),
         become_same = ifelse(minyr_sameHRR_part>2017,1,0),
         never_same = ifelse(minyr_sameHRR_part==0,1,0)) %>%
  mutate(always_diff = ifelse(minyr_diffHRR_part==2017,1,0),
         become_diff = ifelse(minyr_diffHRR_part>2017,1,0),
         never_diff = ifelse(minyr_diffHRR_part==0,1,0))

# save a list of EINs that stay in the analysis data set to verify the connections
final_ein_list <- hospital_data %>%
  distinct(TaxYr, Filer.EIN)
saveRDS(final_ein_list, file=paste0(created_data_path, "final_ein_list_boardandexec.rds"))


# limit people data only to those in the hospital_data sample
people_data <- people_data %>%
  ungroup() %>%
  filter(Filer.EIN %in% unique(hospital_data$Filer.EIN)) %>%
  mutate(female = ifelse(gender=="female",1,0)) %>%
  filter(TaxYr!=2016) 

# functions -----------------------------------------------------------------------
count_obs <- function(data, filter_col) {
  data %>%
    filter(.data[[filter_col]] == 1) %>%
    distinct(name_cleaned) %>%
    nrow()
}



# table: summary stats of individual characteristics for board members and hospital board teams --------------------------------------------------------
n_board_people <- people_data %>%
  filter(position=="board") %>%
  distinct(person_id) %>%
  nrow()
n_board_people <- format(round(n_board_people,0), big.mark = ",", scientific = FALSE, trim = TRUE)
n_exec_people <- people_data %>%
  filter(position!="board") %>%
  distinct(person_id) %>%
  nrow()
n_exec_people <- format(round(n_exec_people,0), big.mark = ",", scientific = FALSE, trim = TRUE)
n_hosp_board <- people_data %>%
  filter(position=="board") %>%
  mutate(count=1) %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(num = sum(count)) %>%
  ungroup() %>%
  summarise_at(c("num"), list(mean))
n_hosp_board <- format(round(n_hosp_board$num[[1]],0), big.mark = ",", scientific = FALSE, trim = TRUE)
n_hosp_exec <- people_data %>%
  filter(position!="board") %>%
  mutate(count=1) %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(num = sum(count)) %>%
  ungroup() %>%
  summarise_at(c("num"), list(mean))
n_hosp_exec <- format(round(n_hosp_exec$num[[1]],0), big.mark = ",", scientific = FALSE, trim = TRUE)


board_people_stats <- people_data %>%
  filter(position=="board") %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  mutate(sample = "Board Members",
         n = n_board_people) %>%
  select(sample, n, doctor, nurse, ha, female)
exec_people_stats <- people_data %>%
  filter(position!="board") %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  mutate(sample = "Executives",
         n = n_exec_people) %>%
  select(sample, n, doctor, nurse, ha, female)
board_hosp_stats <- people_data %>%
  filter(position=="board") %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  ungroup() %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  mutate(sample = "Individual Hospital Boards",
         n = n_hosp_board) %>%
  select(sample, n, doctor, nurse, ha, female)
exec_hosp_stats <- people_data %>%
  filter(position!="board") %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  ungroup() %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  mutate(sample = "Individual Hospital Boards",
         n = n_hosp_exec) %>%
  select(sample, n, doctor, nurse, ha, female)

stats <- rbind(board_people_stats, exec_people_stats, board_hosp_stats, exec_hosp_stats)

knitr::kable(stats,
             format = "latex",
             col.names = c("Sample", "Num. People", "Doctor", "Nurse", "Health Admin.", "Female"),
             caption = "Characteristics of Individuals and Hospital Teams\\label{tab:people}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c", "c", "c"),
             position="ht!",
             linesep = "") %>%
  add_header_above(c(" "=2, "Percent" =5)) %>%
  pack_rows(group_label = "Individuals", start_row = 1, end_row = 2) %>%
  pack_rows(group_label = "Hospital Teams", start_row = 3, end_row = 4) %>%
  write(file = paste0(objects_path, "boardandexec_people.tex"))


# graph: percent of hospitals/HRRs that are connected in each year--------------------------------------------
hosp <- hospital_data %>%
  group_by(TaxYr) %>%
  summarise(m = mean(any_partnership_sameHRR)) %>%
  mutate(group="Hospitals (by Board/Exec)")
hrr <- hospital_data %>%
  group_by(TaxYr, HRRCODE) %>%
  summarise(connected = sum(any_partnership_sameHRR)) %>%
  mutate(connected = ifelse(connected>0, 1, 0)) %>%
  group_by(TaxYr) %>%
  summarise(m= mean(connected)) %>%
  mutate(group = "HRRs")
both <- rbind(hosp, hrr)

ggplot(both, aes(x=TaxYr, y=m, group=group, colour = group)) +
  geom_line() + geom_point() +
  labs(title = "",
       x = "\nYear",
       y = "Percent Connected\n") +
  theme_minimal() + xlim(2017,2022) + ylim(0,.5) +
  labs(color ="") +
  # add labels for the raw number of hospitals at each point using num_conn_list
#  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3) +
  theme(text = element_text(size=13, family = "serif"))

ggsave(paste0(objects_path, "//connected_percent.pdf"), width=6, height=3)


# graph: maps with connected hospitals in blue ---------------------------------------------------------
# read in map data
us_states <- map_data("state")

plot_map <- function(df, year) {
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify hospitals with at least one connection
  connected_hospitals <- df_year %>%
    filter(partnership_sameHRR==1) %>%
    select(Filer.EIN) %>%
    distinct()
  
  # Create nodes (all hospitals, even unconnected ones)
  nodes <- df_year %>%
    select(Filer.EIN, filer.lat, filer.long) %>%
    distinct(Filer.EIN, .keep_all = TRUE) %>%
    mutate(is_connected = ifelse(Filer.EIN %in% connected_hospitals$Filer.EIN, "Connected", "Unconnected"))
  
  
  # Filter hospitals with connections
  df_filtered <- df_year %>%
    filter(partnership_sameHRR==1)
  
  
  # Generate the plot
  ggplot() +
    # Draw U.S. state boundaries
    geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    # Draw connection lines
    geom_segment(data = df_filtered, 
                 aes(x = filer.long, y = filer.lat, xend = other.long, yend = other.lat),
                 color = "blue", alpha = 1, size = 0.75) +
    # Plot all hospitals as red dots
    geom_point(data = nodes, aes(x = filer.long, y = filer.lat, color = is_connected), 
               size = 0.01) +
    scale_color_manual(values = c("Connected" = "blue", "Unconnected" = "red")) + 
    theme_void() +
    labs(title = paste0("     ", year)) +
    coord_cartesian(xlim = c(-130, -60), ylim = c(20, 50), expand = FALSE) +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0)) +
    theme(element_text(family = "serif", size=13))
  
}

# Generate maps for each year (2017-2022)
plots <- lapply(2017:2022, function(year) plot_map(hospital_connections, year))

# combine legends into one 
combined_plot <-  wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0)) + theme(element_text(family = "serif", size=13))

# save combined plot
ggsave("Objects//connected_maps.pdf", width = 6, height = 6.5, units = "in")


# graph: HRRs with connected hospitals within them -------------------------------------------------
# read in HRR shape file
hrr_shapes <- sf::st_read(paste0(raw_data_path, "\\HRR_Bdry__AK_HI_unmodified\\HRR_Bdry__AK_HI_unmodified\\hrr-shapefile\\Hrr98Bdry_AK_HI_unmodified.shp"))

plot_hrr_map <- function(df, year, hrr_shapes) {
  # Filter for the year
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify HRRs with connected hospitals
  connected_hrrs <- df_year %>%
    filter(partnership_sameHRR == 1) %>%
    select(filer.hrr) %>%
    distinct()
  
  # Mark HRRs as connected or not
  hrr_shapes <- hrr_shapes %>%
    mutate(is_connected = ifelse(hrrnum %in% connected_hrrs$filer.hrr, "Connected", "Unconnected"))
  
  # Plot using geom_sf
  ggplot() +
    geom_sf(data = hrr_shapes, aes(fill = is_connected), color = "white", size = 0.2) +
    scale_fill_manual(values = c("Connected" = "blue", "Unconnected" = "lightgray")) +
    theme_void() +
    labs(title = paste("     ", year)) +
    theme(legend.position = "none") + 
    coord_sf(xlim = c(-130, -60), ylim = c(20, 50), expand = FALSE) +
    theme(element_text(family = "serif", size=13))
}

plots <- lapply(2017:2022, function(year) plot_hrr_map(hospital_connections, year, hrr_shapes))

combined_plot <- wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(element_text(family = "serif", size=13))
ggsave("Objects//connected_HRR_maps.pdf", width = 6, height = 6.5, units = "in")


# table: Summarise hospital pairs-----------------------------------------------------------------------
hospital_connected_pairs <- hospital_connections %>%
  filter(Filer.EIN %in% unique(hospital_data$Filer.EIN)) %>%
  filter(partnership_sameHRR==1) %>%
  select(TaxYr, Filer.ID, other.id) %>%
  filter(!is.na(Filer.ID) & !is.na(other.id))
hospital_connected_pairs_diff <- hospital_connections %>%
  filter(Filer.EIN %in% unique(hospital_data$Filer.EIN)) %>%
  filter(partnership_diffHRR==1) %>%
  select(TaxYr, Filer.ID, other.id) %>%
  filter(!is.na(Filer.ID) & !is.na(other.id))

# create variable for number of connections
hospital_connected_pairs <- hospital_connected_pairs %>%
  group_by(TaxYr, Filer.ID) %>%
  mutate(n_connections = n_distinct(other.id)) %>%
  ungroup()
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  group_by(TaxYr, Filer.ID) %>%
  mutate(n_connections = n_distinct(other.id)) %>%
  ungroup()

AHA_pair <- AHA %>%
  select(YEAR, ID, SERV, BDTOT, MNAME, SYSID, LAT, LONG) %>%
  filter(YEAR>=2016 & YEAR<=2022)

# join by filer_id
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(AHA_pair, by=c("Filer.ID"="ID", "TaxYr"="YEAR")) %>%
  rename(filer_SERV = SERV, filer_BDTOT = BDTOT, filer_name = MNAME, filer_sysid = SYSID, filer_lat=LAT, filer_long=LONG) %>%
  left_join(AHA_pair, by=c("other.id"="ID", "TaxYr"="YEAR")) %>%
  rename(other_SERV = SERV, other_BDTOT = BDTOT, other_name = MNAME, other_sysid = SYSID, other_lat=LAT, other_long=LONG)
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  mutate(TaxYr = as.numeric(TaxYr)) %>%
  left_join(AHA_pair, by=c("Filer.ID"="ID", "TaxYr"="YEAR")) %>%
  rename(filer_SERV = SERV, filer_BDTOT = BDTOT, filer_name = MNAME, filer_sysid = SYSID, filer_lat=LAT, filer_long=LONG) %>%
  left_join(AHA_pair, by=c("other.id"="ID", "TaxYr"="YEAR")) %>%
  rename(other_SERV = SERV, other_BDTOT = BDTOT, other_name = MNAME, other_sysid = SYSID, other_lat=LAT, other_long=LONG)

# fill variables 
hospital_connected_pairs <- hospital_connected_pairs %>%
  group_by(Filer.ID) %>%
  fill(filer_SERV, filer_lat, filer_long, .direction="downup") %>%
  ungroup() %>%
  group_by(other.id) %>%
  fill(other_SERV, other_lat, other_long, .direction="downup") %>%
  ungroup()
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  group_by(Filer.ID) %>%
  fill(filer_SERV, filer_lat, filer_long, .direction="downup") %>%
  ungroup() %>%
  group_by(other.id) %>%
  fill(other_SERV, other_lat, other_long, .direction="downup") %>%
  ungroup()

# create variables capturing type of relationship (general - general/ general - specialty/specialty - specialty)
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(gen_gen = ifelse(filer_SERV==10 & other_SERV==10,1,0),
         gen_spec = ifelse((filer_SERV==10 & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)) | (other_SERV==10 & filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49)),1,0),
         spec_spec = ifelse(filer_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49),1,0),
         adult_child = ifelse((filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% 50:59) | (other_SERV %in% c(13,22,33,41,42,44,45,46,47,48,49) & filer_SERV %in% 50:59),1,0),
         adult_adult = ifelse((filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & other_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49)) | (other_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49) & filer_SERV %in% c(10,13,22,33,41,42,44,45,46,47,48,49)),1,0),
         child_child = ifelse((filer_SERV %in% 50:59 & other_SERV %in% 50:59) | (other_SERV %in% 50:59 & filer_SERV %in% 50:59),1,0))
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
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
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  mutate(ind_ind = ifelse(is.na(filer_sysid) & is.na(other_sysid), 1, 0),
         sys_sys = ifelse(!is.na(filer_sysid) & !is.na(other_sysid), 1, 0),
         ind_sys = ifelse((is.na(filer_sysid) & !is.na(other_sysid)) | (!is.na(filer_sysid) & is.na(other_sysid)), 1, 0))

# create variables for small vs. large hospitals
# median bed size of all hospitals
median_bed_size <- median(hospital_data$HOSPBD, na.rm = TRUE)

hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(small_small = ifelse(filer_BDTOT < median_bed_size & other_BDTOT < median_bed_size, 1, 0),
         small_large = ifelse((filer_BDTOT < median_bed_size & other_BDTOT >= median_bed_size) | (filer_BDTOT >= median_bed_size & other_BDTOT < median_bed_size), 1, 0),
         large_large = ifelse(filer_BDTOT >= median_bed_size & other_BDTOT >= median_bed_size, 1, 0))
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  mutate(small_small = ifelse(filer_BDTOT < median_bed_size & other_BDTOT < median_bed_size, 1, 0),
         small_large = ifelse((filer_BDTOT < median_bed_size & other_BDTOT >= median_bed_size) | (filer_BDTOT >= median_bed_size & other_BDTOT < median_bed_size), 1, 0),
         large_large = ifelse(filer_BDTOT >= median_bed_size & other_BDTOT >= median_bed_size, 1, 0))

# calculate the geographic distance between hospitals in the pair
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(distance = ifelse(!is.na(filer_lat) & !is.na(other_lat) & !is.na(filer_long) & !is.na(other_long),
                          geosphere::distHaversine(cbind(filer_long, filer_lat), cbind(other_long, other_lat)), NA)) %>%
  mutate(distance = ifelse(distance > 0, distance / 1000, NA)) %>% # convert to kilometers
  mutate(distance_miles = ifelse(!is.na(distance), distance * 0.621371, NA)) # convert to miles
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  mutate(distance = ifelse(!is.na(filer_lat) & !is.na(other_lat) & !is.na(filer_long) & !is.na(other_long),
                           geosphere::distHaversine(cbind(filer_long, filer_lat), cbind(other_long, other_lat)), NA)) %>%
  mutate(distance = ifelse(distance > 0, distance / 1000, NA)) %>% # convert to kilometers
  mutate(distance_miles = ifelse(!is.na(distance), distance * 0.621371, NA)) # convert to miles

# create variable for difference in number of beds
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(bed_diff = abs(filer_BDTOT - other_BDTOT)) %>%
  mutate(group = "Same")
hospital_connected_pairs_diff <- hospital_connected_pairs_diff %>%
  mutate(bed_diff = abs(filer_BDTOT - other_BDTOT)) %>%
  mutate(group = "Different")

data <- rbind(hospital_connected_pairs, hospital_connected_pairs_diff)

# put these values in a table 
pair_table <- data %>%
  group_by(group) %>%
  summarise("General - General" = mean(gen_gen, na.rm=T),
            "General - Specialty" = mean(gen_spec, na.rm=T),
            "Specialty - Specialty" = mean(spec_spec, na.rm=T),
            "Adult - Childrens" = mean(adult_child, na.rm=T),
            "Adult - Adult" = mean(adult_adult, na.rm=T),
            "Childrens - Childrens" = mean(child_child, na.rm=T),
            "Independent - Independent" = mean(ind_ind, na.rm=T),
            "In System - In System" = mean(sys_sys, na.rm=T),
            "Independent - In System" = mean(ind_sys, na.rm=T),
            "Small - Small" = mean(small_small, na.rm=T),
            "Small - Large" = mean(small_large, na.rm=T),
            "Large - Large" = mean(large_large, na.rm=T),
            "Bed Difference" = mean(bed_diff, na.rm=T),
            "Distance (km)" = mean(distance, na.rm=T),
            "Distance (miles)" = mean(distance_miles, na.rm=T),
            "Number of Connections" = mean(n_connections)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(variable = rownames(.)) %>%
  filter(variable!="group") %>%
  mutate(V1 = as.numeric(V1), V2 = as.numeric(V2)) %>%
  select(variable, V2, V1) %>%
  mutate(V1 = ifelse(V1 > 9, round(V1, digits = 0), V1),
         V2 = ifelse(V2 > 9, round(V2, digits = 0), V2)) %>%
  mutate(V1 = as.character(round(V1,2)), V2 = as.character(round(V2,2)))


# add row for total number of connected hospitals
pair_table <- pair_table %>%
  add_row(variable = "Total Connected Hospitals", 
          V2 = format(nrow(distinct(hospital_connected_pairs, Filer.ID)), big.mark = ",", scientific = FALSE, trim = TRUE), 
          V1 = format(nrow(distinct(hospital_connected_pairs_diff, Filer.ID)), big.mark = ",", scientific = FALSE, trim = TRUE))

knitr::kable(pair_table, format = "latex",
             col.names = c("Pair Characteristic", "Pairs in Same HRR", "Pairs in Diff. HRR"),
             caption = "Characteristics of Hospitals with Overlapping Leadership\\label{hospital_pair_types}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c", "c"),
             position="ht!") %>%
  kable_styling(full_width=F) %>%
  pack_rows("", 1, 3, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 4, 6, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 7, 9, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 10, 13, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 14, 15, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 16, 16, indent = FALSE, latex_gap_space = "-0.3em") %>%
  pack_rows("", 17,17, indent=FALSE, latex_gap_space = "-0.3em") 
  write("Objects//hospital_pair_types.tex")

# table: characteristics of connected in same/diff HRR and not connected ------------------------------
# add indicators
hospital_data <- hospital_data %>%
  mutate(general = ifelse(SERV==10,1,0),
         sys = ifelse(is.na(SYSID),0,1),
         academic = ifelse(MAPP5==1,1,0))

n_become_same <- hospital_data %>%
  filter(become_same==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_always_same <- hospital_data %>%
  filter(always_same==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_become_diff <- hospital_data %>%
  filter(become_diff==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_always_diff <- hospital_data %>%
  filter(always_diff==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_not <- hospital_data %>%
  filter(never_same==1 & never_diff==1) %>%
  distinct(Filer.EIN) %>%
  nrow()

stat_become_same <- hospital_data %>%
  filter(become_same==1) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Same HRR",
         n = n_become_same) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Academic`, `Num. Physicians`, `Num. Nurses`)
stat_always_same <- hospital_data %>%
  filter(always_same==1) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Same HRR",
         n = n_always_same) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Academic`, `Num. Physicians`, `Num. Nurses`)
stat_become_diff <- hospital_data %>%
  filter(become_diff==1) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Different HRR",
         n = n_become_diff) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Academic`, `Num. Physicians`, `Num. Nurses`)
stat_always_diff <- hospital_data %>%
  filter(always_diff==1) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Different HRR",
         n = n_always_diff) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Academic`, `Num. Physicians`, `Num. Nurses`)
stat_not <- hospital_data %>%
  filter(never_same==1 & never_diff==1) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Never Connected",
         n = n_not) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Academic`, `Num. Physicians`, `Num. Nurses`)
  
stat <- rbind(stat_become_same, stat_become_diff,stat_always_same, stat_always_diff, stat_not) %>%
  mutate(n=as.character(n),
         `Num. Beds` = as.character(format(round(`Num. Beds`), big.mark = ",", scientific = FALSE, trim = TRUE)),
         `Num. Physicians` = as.character(format(round(`Num. Physicians`), big.mark = ",", scientific = FALSE, trim = TRUE)),
         `Num. Nurses` = as.character(format(round(`Num. Nurses`), big.mark = ",", scientific = FALSE, trim = TRUE)))

knitr::kable(stat,
             format = "latex",
             col.names = c("Sample of Hosp.", "N", "Num. Beds", "General", "In System", "Academic", "Num. Phys.", "Num. Nurses"),
             caption = "Hospital Characteristics by Connections\\label{tab:hosp_group_stats}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c","c","c","c"),
             position="ht!",
             linesep = "") %>%
  pack_rows(group_label = "Become Connected", start_row = 1, end_row = 2) %>%
  pack_rows(group_label = "Always Connected", start_row = 3, end_row = 4) %>%
  pack_rows("", 5, 5, indent = FALSE, latex_gap_space = "-0.3em") %>%
  write(file = paste0(objects_path, "hosp_group_stats.tex"))

# graph: outcome variables by group------------------------------------------------------
# define the groups
hospital_data <- hospital_data %>%
  mutate(group = ifelse(minyr_sameHRR_part>2017, "Become Connected Same", NA)) %>%
  mutate(group = ifelse(is.na(group) & minyr_diffHRR_part>2017, "Become Connected Different", group)) %>%
  mutate(group = ifelse(is.na(group) & always_diff==1, "Always Connected Different", group)) %>%
  mutate(group = ifelse(is.na(group) & never_same==1 & never_diff==1, "Never Connected", group))

# create relative year variable
hospital_data <- hospital_data %>%
  mutate(TaxYr = as.numeric(TaxYr),
         minyr_sameHRR_part = as.numeric(minyr_sameHRR_part),
         minyr_diffHRR_part = as.numeric(minyr_diffHRR_part)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Same", minyr_sameHRR_part-TaxYr, NA)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Different", minyr_diffHRR_part-TaxYr, rel_year))

treat_stats <- hospital_data %>%
  filter(!is.na(rel_year) & minyr_sameHRR_part!=2023 & minyr_diffHRR_part!=2023) %>%
  group_by(rel_year, group) %>%
  summarise_at(c("perc_mcaid", "perc_mcare", "tot_discharges", "bed_conc", "build_purch", "fixedequipment_purch",
                 "movableequipment_purch", "tot_pat_rev", "tot_operating_exp"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean")

treat_stats %>%
  filter(variable %in% c("tot_operating_exp_m")) %>%
  ggplot(aes(x=rel_year, y=mean, color=group, linetype=variable)) + geom_line() +
  theme_minimal() + xlim(-3,3) 
treat_stats %>%
  filter(variable %in% c("perc_mcaid_m", "perc_mcare_m", "bed_conc_m")) %>%
  ggplot(aes(x=rel_year, y=mean, color=group, linetype=variable)) + geom_line() +
  theme_minimal() + xlim(-3,3) 
treat_stats %>%
  filter(variable %in% c("fixedequipment_purch_m", "build_purch_m", "movableequipment_purch_m")) %>%
  ggplot(aes(x=rel_year, y=mean, color=group, linetype=variable)) + geom_line() +
  theme_minimal() + xlim(-3,3)
treat_stats %>%
  filter(variable %in% c("tot_pat_rev_m")) %>%
  ggplot(aes(x=rel_year, y=mean, color=group, linetype=variable)) + geom_line() +
  theme_minimal() + xlim(-3,3)

# from this, there are several variables where it seems like something is going on! I need to really pin
# down my story though and not just throw darts to see what sticks. 
# think about pre-trends because it could be hospitals who are financially struggling that
# make the choice to form partnerships with other hospitals. 

stats <- hospital_data %>%
  filter(group %in% c("Never Connected", "Always Connected Different")) %>%
  group_by(TaxYr, group) %>%
  summarise_at(c("perc_mcaid", "perc_mcare", "tot_discharges", "bed_conc", "build_purch", "fixedequipment_purch",
                 "movableequipment_purch", "tot_pat_rev", "tot_operating_exp", "cost_to_charge"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean")

stats %>%
  filter(variable %in% c("perc_mcaid_m", "perc_mcare_m")) %>%
  ggplot() + geom_line(aes(x=TaxYr, y=mean, color=variable, linetype=group)) +
  theme_minimal()
  
  
  
  
  
  

