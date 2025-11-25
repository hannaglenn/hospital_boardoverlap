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

people_data_board <- readRDS(paste0(created_data_path, "people_connections_boardonly.rds"))
people_data <- readRDS(paste0(created_data_path, "people_connections_boardandexec.rds"))
hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))
hospital_connections <- readRDS(paste0(created_data_path, "hospital_connections_boardandexec.rds"))

# table: summary stats of the people who are connected versus not connected --------------------------------------------------------
# I think I should do this for a snapshot in time
n_conn <- people_data %>%
  filter(TaxYr==2019 & sameHRR_competing==1) %>%
  distinct(name_cleaned) %>%
  nrow()
n_unconn <- people_data %>%
  filter(TaxYr==2019 & sameHRR_competing==0) %>%
  distinct(name_cleaned) %>%
  nrow()

people_sumstats <- people_data %>%
  filter(TaxYr==2019) %>%
  mutate(female = ifelse(gender=="female",1,0),
         board = ifelse(position=="board",1,0),
         exec = ifelse(position=="board",0,1)) %>%
  group_by(sameHRR_competing) %>%
  summarise_at(c("doctor", "nurse", "ha", "nonvoting", "female", "board", "exec"),
               list(m=mean), na.rm=T) %>%
  t() %>%
  as.data.frame() %>%
  mutate(variable = rownames(.)) %>%
  rename(Connected = V1, `Not Connected` = V2) %>%
  filter(variable!="sameHRR_competing") %>%
  select(variable, Connected, `Not Connected`) %>%
  mutate(Connected = as.character(round(Connected,2)), `Not Connected` = as.character(round(`Not Connected`,2))) %>%
  add_row(variable = "N", 
          Connected = format(round(n_conn), big.mark = ",", scientific = FALSE, trim = TRUE), 
          `Not Connected` = format(round(n_unconn), big.mark = ",", scientific = FALSE, trim = TRUE)) %>%
  mutate(variable = str_remove(variable, "_m")) %>%
  mutate(variable = paste0(toupper(substr(variable, 1, 1)), substr(variable, 2, nchar(variable)))) %>%
  mutate(variable = ifelse(variable=="Ha", "Health Admin. Degree", variable))

knitr::kable(people_sumstats,
             format = "latex",
             col.names = c("Variable", "Connected", "Not Connected"),
             caption = "Hospital Board Members and Executives in 2019\\label{tab:people_2019}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c"),
             position="ht!",
             linesep = "") %>%
  write(file = paste0(objects_path, "people_2019.tex"))


# graph: percent of hospitals that are connected in each year--------------------------------------------
num_conn_list <- hospital_data %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(any_sameHRR_competing)) %>%
  mutate(n_connected = paste("n = ", n_connected))

hospital_data %>%
  group_by(TaxYr) %>%
  summarise(m_connected = mean(any_sameHRR_competing)) %>%
  ggplot(aes(x=TaxYr)) +
  geom_line(aes(y=m_connected)) +
  labs(title = "",
       x = "\nYear",
       y = "Percent of Hospitals\n") +
  theme_minimal() + xlim(2016,2022) + ylim(0,.5) +
  labs(color='') +
  # add labels for the raw number of hospitals at each point using num_conn_list
  geom_label(aes(x=TaxYr, y=m_connected, label=num_conn_list$n_connected), vjust=-1, size = 3) +
  theme(text = element_text(size=15))

ggsave(paste0(objects_path, "//connected_percent.pdf"), width=6, height=3)

# graph: percent of HRRs with hospitals connected in each year---------------------------------
num_conn_list <- hospital_data %>%
  distinct(TaxYr, HRRCODE, any_sameHRR_competing) %>%
  group_by(TaxYr) %>%
  summarise(n_connected = sum(any_sameHRR_competing)) %>%
  mutate(n_connected = paste("n = ", n_connected))

hospital_data %>%
  group_by(TaxYr, HRRCODE) %>%
  summarise(connected = sum(any_sameHRR_competing)) %>%
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

ggsave(paste0(objects_path, "connected_HRR_percent.pdf"), width=6, height=3)

# graph: maps with connected hospitals in blue ---------------------------------------------------------
# read in map data
us_states <- map_data("state")

plot_map <- function(df, year) {
  df_year <- df %>% filter(TaxYr == year)
  
  # Identify hospitals with at least one connection
  connected_hospitals <- df_year %>%
    filter(sameHRR_competing==1) %>%
    select(Filer.EIN) %>%
    distinct()
  
  # Create nodes (all hospitals, even unconnected ones)
  nodes <- df_year %>%
    select(Filer.EIN, filer.lat, filer.long) %>%
    distinct(Filer.EIN, .keep_all = TRUE) %>%
    mutate(is_connected = ifelse(Filer.EIN %in% connected_hospitals$Filer.EIN, "Connected", "Unconnected"))
  
  
  # Filter hospitals with connections
  df_filtered <- df_year %>%
    filter(sameHRR_competing==1)
  
  
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
    labs(title = paste0("           ", year)) +
    coord_cartesian(xlim = c(-130, -60), ylim = c(20, 50), expand = FALSE) +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0)) 
  
}

# Generate maps for each year (2017-2022)
plots <- lapply(2017:2022, function(year) plot_map(hospital_connections, year))

# combine legends into one 
combined_plot <-  wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0))

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
    filter(sameHRR_competing == 1) %>%
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
    coord_sf(xlim = c(-130, -60), ylim = c(20, 50), expand = FALSE)
}

plots <- lapply(2017:2022, function(year) plot_hrr_map(hospital_connections, year, hrr_shapes))

combined_plot <- wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0))
ggsave("Objects//connected_HRR_maps.pdf", width = 6, height = 6.5, units = "in")


# table: Summarise hospital pairs-----------------------------------------------------------------------
hospital_connected_pairs <- hospital_connections %>%
  filter(sameHRR_competing==1) %>%
  select(TaxYr, Filer.ID, other.id) %>%
  filter(!is.na(Filer.ID) & !is.na(other.id))

# create variable for number of connections
hospital_connected_pairs <- hospital_connected_pairs %>%
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

# fill variables 
hospital_connected_pairs <- hospital_connected_pairs %>%
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

# create variables capturing ownership relationship (ind - ind/sys - sys/ind - sys)
hospital_connected_pairs <- hospital_connected_pairs %>%
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

# calculate the geographic distance between hospitals in the pair
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(distance = ifelse(!is.na(filer_lat) & !is.na(other_lat) & !is.na(filer_long) & !is.na(other_long),
                          geosphere::distHaversine(cbind(filer_long, filer_lat), cbind(other_long, other_lat)), NA)) %>%
  mutate(distance = ifelse(distance > 0, distance / 1000, NA)) %>% # convert to kilometers
  mutate(distance_miles = ifelse(!is.na(distance), distance * 0.621371, NA)) # convert to miles

# create variable for difference in number of beds
hospital_connected_pairs <- hospital_connected_pairs %>%
  mutate(bed_diff = abs(filer_BDTOT - other_BDTOT))


# put these values in a table 
pair_table <- hospital_connected_pairs %>%
  summarise("General - General" = mean(gen_gen, na.rm=T),
            "General - Specialty" = mean(gen_spec, na.rm=T),
            "Specialty - Specialty" = mean(spec_spec, na.rm=T),
            "Adult - Childrens" = mean(adult_child, na.rm=T),
            "Adult - Adult" = mean(adult_adult, na.rm=T),
            "Childrens - Childrens" = mean(child_child, na.rm=T),
            "Ind. - Ind." = mean(ind_ind, na.rm=T),
            "Sys. - Sys." = mean(sys_sys, na.rm=T),
            "Ind. - Sys." = mean(ind_sys, na.rm=T),
            "Small - Small" = mean(small_small, na.rm=T),
            "Small - Large" = mean(small_large, na.rm=T),
            "Large - Large" = mean(large_large, na.rm=T),
            "Bed Difference" = mean(bed_diff, na.rm=T),
            "Distance (km)" = mean(distance, na.rm=T),
            "Distance (miles)" = mean(distance_miles, na.rm=T),
            "Number of Connections" = mean(n_connections))

# pivot longer
pair_table <- pair_table %>%
  pivot_longer(cols = `General - General`:`Number of Connections`, names_to = "variable", values_to = "value")

# add row for total number of connected hospitals
pair_table <- pair_table %>%
  add_row(variable = "Total Connected Hospitals", value = nrow(distinct(hospital_connected_pairs, Filer.ID)))

pair_table <- pair_table %>%
  mutate(value = ifelse(value>9, round(value,0), round(value, 2))) %>%
  mutate(value = as.character(value))

knitr::kable(pair_table, format = "latex",
             col.names = c("Type of Connected Pair", "Average"),
             caption = "Types of Hospital Connections\\label{hospital_pair_types}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c"),
             position="ht!") %>%
  kable_styling(full_width=F) %>%
  group_rows("", 1, 3, indent = FALSE) %>%
  group_rows("", 4, 6, indent = FALSE) %>%
  group_rows("", 7, 9, indent = FALSE) %>%
  group_rows("", 10, 13, indent = FALSE) %>%
  group_rows("", 14, 15, indent = FALSE) %>%
  group_rows("", 16, 16, indent = FALSE) %>%
  group_rows("", 17,17, indent=FALSE) %>%
  write("Objects//hospital_pair_types.tex")



# create data set that would be used for analysis to make summary stats table and graphs --------------
analysis_data_boardandexec <- hospital_data %>%
  filter(TaxYr!=2016 & TaxYr!=2022) %>%
  group_by(Filer.EIN) %>%
  arrange(TaxYr) %>%
  mutate(lag_treat = dplyr::lag(any_sameHRR_competing)) %>%
  mutate(lead_treat = dplyr::lead(any_sameHRR_competing)) %>%
  mutate(any_sameHRR_competing = ifelse(!is.na(lag_treat) & !is.na(lead_treat) & lag_treat==1 & lead_treat==1,1,any_sameHRR_competing)) %>%
  mutate(drop = ifelse(lag_treat==1 & any_sameHRR_competing==0,1,NA)) %>%
  fill(drop, .direction="downup") %>%
  ungroup() %>%
  filter(is.na(drop)) 

# create "groups" for whether connected in same HRR, diff HRR, or not connected to competitors
analysis_data_boardandexec <- analysis_data_boardandexec %>%
  mutate(group = ifelse(minyr_sameHRR_comp!=0, "Connected Same HRR", NA)) %>%
  mutate(group = ifelse(is.na(group) & minyr_diffHRR_comp%in%c(2016:2017), "Connected Different HRR", group)) %>%
  mutate(group = ifelse(is.na(group) & minyr_diffHRR_comp==0, "Not Connected", group)) %>%
  filter(!is.na(group))

# get rid of those who were already connected in 2016
analysis_data_boardandexec <- analysis_data_boardandexec %>%
  filter(minyr_sameHRR_comp!=2016)

# only observations that don't have multiple events or the same board connection happens later
analysis_data_boardandexec <- analysis_data_boardandexec %>%
  filter(minyr_diffHRR_comp==0 | minyr_sameHRR_comp==0 | minyr_sameHRR_comp>minyr_diffHRR_comp)

# create relative year variable
analysis_data_boardandexec <- analysis_data_boardandexec %>%
  mutate(TaxYr=as.numeric(TaxYr),
         minyr_sameHRR_comp = as.numeric(minyr_sameHRR_comp),
         minyr_diffHRR_comp = as.numeric(minyr_diffHRR_comp)) %>%
  mutate(rel_year = ifelse(group=="Connected Same HRR", minyr_sameHRR_comp-TaxYr, minyr_diffHRR_comp-TaxYr))

# add indicators
analysis_data_boardandexec <- analysis_data_boardandexec %>%
  mutate(general = ifelse(SERV==10,1,0),
         sys = ifelse(is.na(SYSID),0,1),
         academic = ifelse(MAPP5==1,1,0))

# table: characteristics of connected in same/diff HRR and not connected ------------------------------
n_same <- analysis_data_boardandexec %>%
  filter(group=="Connected Same HRR") %>%
  distinct(Filer.EIN) %>%
  nrow()
n_diff <- analysis_data_boardandexec %>%
  filter(group=="Connected Different HRR") %>%
  distinct(Filer.EIN) %>%
  nrow()
n_none <- analysis_data_boardandexec %>%
  filter(group=="Not Connected") %>%
  distinct(Filer.EIN) %>%
  nrow()

stats <- analysis_data_boardandexec %>%
  group_by(group) %>%
  summarise_at(c("Num. Beds" = "HOSPBD",
                 "General" = "general",
                 "In System" = "sys",
                 "Academic" = "academic",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  t() %>%
  as.data.frame() %>%
  mutate(variable = rownames(.)) %>%
  filter(variable!="group") %>%
  mutate(V1 = as.numeric(V1), V2 = as.numeric(V2), V3=as.numeric(V3)) %>%
  select(variable, V2, V1, V3) %>%
  mutate(V1 = as.character(round(V1,2)), V2 = as.character(round(V2,2)),
         V3 = as.character(round(V3,2))) %>%
  add_row(variable = "N", 
          V1 = format(round(n_diff), big.mark = ",", scientific = FALSE, trim = TRUE), 
          V2 = format(round(n_same), big.mark = ",", scientific = FALSE, trim = TRUE),
          V3 = format(round(n_none), big.mark = ",", scientific = FALSE, trim = TRUE))

knitr::kable(stats,
             format = "latex",
             col.names = c("Variable", "Connected in Same HRR", "Connected in Diff HRR", "Not Connected"),
             caption = "Hospital Characteristics by Connections\\label{tab:hosp_group_stats}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c"),
             position="ht!",
             linesep = "") %>%
  write(file = paste0(objects_path, "hosp_group_stats.tex"))

# graph: outcome variables by group------------------------------------------------------
perc_graph <- analysis_data_boardandexec %>%
  group_by(rel_year, group) %>%
  summarise_at(c("perc_mcare", "perc_mcaid"),
               list(m=mean), na.rm=T)
  
  
  
  
  
  

