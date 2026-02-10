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
library(readxl)
library(usmap)
library(magick)


source("./Scripts/paths.R")

options(knitr.kable.NA = '')

people_data <- readRDS(paste0(created_data_path, "people_connections_boardandexec.rds"))
hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))
hospital_connections <- readRDS(paste0(created_data_path, "hospital_connections_boardandexec.rds"))
AHA <- read_csv(paste0(raw_data_path, "AHAdata_20052023.csv")) 


# First make generic graph for presentation showing that hospital concentration has increased over time
hcris <- read_csv(paste0(raw_data_path, "/final_hcris_data.csv")) 

conc_data <- AHA %>%
  filter(!is.na(MCRNUM) & !is.na(HSACODE) & SERV==10) 
conc_data <- conc_data %>% 
  left_join(hcris, by=c("MCRNUM"="provider_number", "YEAR"="year")) 

# calculate HHI in each health service area 
conc_data <- conc_data %>%
  group_by(HRRCODE, YEAR) %>%
  mutate(hrr_tot_discharges = sum(tot_discharges, na.rm=T)) %>%
  ungroup() %>%
  mutate(disch_share = (tot_discharges/hrr_tot_discharges)^2) %>%
  group_by(HRRCODE, YEAR) %>%
  mutate(hrr_hhi = sum(disch_share, na.rm=T)) %>%
  ungroup() %>%
  distinct(YEAR, HRRCODE, hrr_hhi)

# calculate percent of HRRs with HHI above .18
conc_data <- conc_data %>%
  mutate(high_HHI = ifelse(hrr_hhi>=.18,1,0)) %>%
  group_by(YEAR) %>%
  mutate(perc_high = mean(high_HHI)) %>%
  ungroup() %>%
  distinct(YEAR, perc_high)

ggplot(conc_data, aes(x=YEAR, y=perc_high)) + geom_point() + geom_line() +
  ylim(0,1) + theme_minimal() +
  xlab("\nyear") + ylab("Percent\n")
ggsave(plot=get_last_plot(), filename=paste0(objects_path, "hhi_graph.pdf"),
       width = 5, height = 4, units="in")


# generic filtering
hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))

# Filter to either being present in 2014-2021 at least or dropping out of the data
# this gets rid of hospitals who come in and out of the data due to missing tax records
hospital_data <- hospital_data %>%
  filter(any_sameHRR_nonmiss_2014_2021 | dropout_both_ind) %>%
  group_by(Filer.EIN) %>%
  fill(minyr_sameHRR_part, minyr_diffHRR_part, maxyr_sameHRR_part, maxyr_diffHRR_part, .direction="downup") %>%
  ungroup()

# add indicator for the time the hospital closes
hospital_data <- hospital_data %>%
  mutate(closure = ifelse(TaxYr>=dropout_year,1,0)) %>%
  mutate(closure = ifelse(is.na(closure),0,closure))

# convert variables to numeric
hospital_data <- hospital_data %>%
  mutate(minyr_sameHRR_part = as.numeric(minyr_sameHRR_part),
         minyr_diffHRR_part = as.numeric(minyr_diffHRR_part),
         maxyr_sameHRR_part = as.numeric(maxyr_sameHRR_part),
         maxyr_diffHRR_part = as.numeric(maxyr_diffHRR_part),
         minyr_sameHRR_formal = as.numeric(minyr_sameHRR_formal),
         maxyr_sameHRR_formal = as.numeric(maxyr_sameHRR_formal),
         Filer.EIN = as.numeric(Filer.EIN),
         TaxYr = as.numeric(TaxYr))

# create variables for always, become, and never connected in the same and different HRRs
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  mutate(always_same = ifelse(minyr_sameHRR_part==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_sameHRR_part==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_same = ifelse(minyr_sameHRR_part>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_same = ifelse(minyr_sameHRR_part==0,1,0),
         lose_same = ifelse(always_same==0 & never_same==0 & become_same==0,1,0)) %>%
  mutate(always_diff = ifelse(minyr_diffHRR_part==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_diffHRR_part==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_diff = ifelse(minyr_diffHRR_part>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_diff = ifelse(minyr_diffHRR_part==0,1,0),
         lose_diff = ifelse(always_diff==0 & never_diff==0 & become_diff==0,1,0)) %>%
  mutate(always_formal_same = ifelse(minyr_sameHRR_formal==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_sameHRR_formal==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_formal_same = ifelse(minyr_sameHRR_formal>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_formal_same = ifelse(minyr_sameHRR_formal==0,1,0)) %>%
  ungroup()



# create different data sets based on the control and treatment groups I want to use -------------------------------
hospital_data_closures <- hospital_data %>%
  mutate(maxyr_sameHRR_part = ifelse(dropout_both_ind & maxyr_sameHRR_part==dropout_year-1,NA,maxyr_sameHRR_part),
         maxyr_diffHRR_part = ifelse(dropout_both_ind & maxyr_diffHRR_part==dropout_year-1,NA,maxyr_diffHRR_part))

hospital_data_behaviors <- hospital_data %>%
  filter(any_sameHRR_nonmiss_2014_2021) %>%
  filter(minyr_sameHRR_part>0 | maxyr_sameHRR_part>0 | always_diff==1 | abs(minyr_diffHRR_part - minyr_sameHRR_part)>2) %>%   # filter control group to connected in different HRR
  filter(always_formal_same==1 | never_formal_same==1 | abs(minyr_sameHRR_formal - minyr_sameHRR_part)>2) %>% # filter out changes to in-system connectedness
  group_by(Filer.EIN) %>%
  filter(sum(independent, na.rm=T)==n() | sum(independent, na.rm=T)==0) %>% # filter out those who switch being affiliated with a system
  filter(minyr_sameHRR_part!=min(TaxYr)) %>% # filter out those treated in the first period
  ungroup() %>%
  filter(minyr_sameHRR_part==0 | maxyr_sameHRR_part - minyr_sameHRR_part>2)  # make sure they stay connected at least 2 years


# save a list of EINs that stay in the analysis data set to verify the connections
final_ein_list <- hospital_data %>%
  distinct(TaxYr, Filer.EIN)
saveRDS(final_ein_list, file=paste0(created_data_path, "final_ein_list_boardandexec.rds"))


# limit people data only to those in the hospital_data sample
people_data <- people_data %>%
  ungroup() %>%
  filter(Filer.EIN %in% unique(hospital_data$Filer.EIN)) %>%
  mutate(female = ifelse(gender=="female",1,0)) 

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
  mutate(sample = "Hospital Boards",
         n = n_hosp_board) %>%
  select(sample, n, doctor, nurse, ha, female)
exec_hosp_stats <- people_data %>%
  filter(position!="board") %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  ungroup() %>%
  summarise_at(c("doctor", "nurse", "ha", "female"), list(mean), na.rm=T) %>%
  mutate(sample = "Hospital Executive Teams",
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
  pack_rows(group_label = "Individuals", start_row = 1, end_row = 2) %>%
  pack_rows(group_label = "Hospital Teams", start_row = 3, end_row = 4) %>%
  write(file = paste0(objects_path, "boardandexec_people.tex"))


# graph: percent of hospitals/HRRs that are connected in each year--------------------------------------------
hosp <- hospital_data %>%
  group_by(TaxYr) %>%
  summarise(m = mean(any_partnership_sameHRR, na.rm=T)) %>%
  mutate(group="Hospitals")
hrr <- hospital_data %>%
  group_by(TaxYr, HRRCODE) %>%
  summarise(connected = sum(any_partnership_sameHRR, na.rm=T)) %>%
  mutate(connected = ifelse(connected>0, 1, 0)) %>%
  group_by(TaxYr) %>%
  summarise(m= mean(connected)) %>%
  mutate(group = "HRRs")
sys <- hospital_data %>%
  group_by(TaxYr) %>%
  mutate(in_sys = ifelse(independent==1,0,1)) %>%
  summarise(m=mean(in_sys, na.rm=T)) %>%
  mutate(group="Hospitals In-System")
both <- rbind(hosp, hrr, sys) %>%
  mutate(TaxYr=as.numeric(TaxYr))

ggplot(both, aes(x=TaxYr, y=m, group=group, colour = group)) +
  geom_line() + geom_point() +
  labs(title = "",
       x = "\nYear",
       y = "Fraction Connected or In-System\n") +
  theme_minimal() + xlim(2014,2023) + ylim(0,.75) +
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
plots <- lapply(2014:2021, function(year) plot_map(hospital_connections, year))

# combine legends into one 
combined_plot <-  wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0)) + theme(element_text(family = "serif", size=13))

# save combined plot
ggsave("Objects//connected_maps.pdf", width = 6, height = 8, units = "in")


# ----- Build frames and GIF -----

# Define the years you want (use your current range or detect from the data)
years <- intersect(2014:2021, sort(unique(hospital_connections$TaxYr)))

# Output folders and files
frames_dir <- "./Objects/frames_hosp_conn_gif"
gif_file   <- "./Objects/hospital_connections_2014_2021.gif"
dir.create(frames_dir, showWarnings = FALSE, recursive = TRUE)

# Generate and save a PNG per year (ensure consistent aspect and size)
# 10x6 inches at 200 dpi ≈ 2000x1200 px (crisp for slides)
walk2(
  years,
  seq_along(years),
  ~{
    p <- plot_map(hospital_connections, .x)
    out <- file.path(frames_dir, sprintf("frame_%02d.png", .y))
    ggsave(
      filename = out, plot = p,
      width = 10, height = 6, units = "in", dpi = 200, bg = "white"
      # Use bg = "transparent" if you prefer transparent background in PPT
    )
  }
)

# Read frames in correct order
pngs <- list.files(frames_dir, pattern = "\\.png$", full.names = TRUE)
# Natural sort by embedded numbers
pngs <- pngs[order(pngs)]

# Optional: hold the last frame for a short pause at the end (e.g., 12 extra frames)
hold <- 12
pngs_with_hold <- c(pngs, rep(tail(pngs, 1), hold))

# Build and write the GIF
img <- image_read(pngs_with_hold)            # read all frames
anim <- image_animate(img, fps = 2, loop = 0)  # fps controls speed; loop=0 means infinite loop
image_write(anim, gif_file)

message("GIF written to: ", normalizePath(gif_file))



# graph: HRRs with connected hospitals within them -------------------------------------------------
# read in HRR shape file
hrr_shapes <- sf::st_read(paste0(raw_data_path, "/HRR_Bdry__AK_HI_unmodified/HRR_Bdry__AK_HI_unmodified/hrr-shapefile/Hrr98Bdry_AK_HI_unmodified.shp"))

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

plots <- lapply(2014:2021, function(year) plot_hrr_map(hospital_connections, year, hrr_shapes))

combined_plot <- wrap_plots(plots, ncol = 2) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  # change legend title to empty
  theme(legend.title = element_blank()) +
  # change size of dots on legend only
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(plot.margin = margin(0,0,0,0)) +
  theme(element_text(family = "serif", size=13))
ggsave("Objects//connected_HRR_maps.pdf", width = 6, height = 6.5, units = "in")

# ----- Build frames and GIF -----

# Define the years you want (use your current range or detect from the data)
years <- intersect(2014:2021, sort(unique(hospital_connections$TaxYr)))

# Output folders and files
frames_dir <- "./Objects/frames_hrr_conn_gif"
gif_file   <- "./Objects/hrr_connections_2014_2021.gif"
dir.create(frames_dir, showWarnings = FALSE, recursive = TRUE)

# Generate and save a PNG per year (ensure consistent aspect and size)
# 10x6 inches at 200 dpi ≈ 2000x1200 px (crisp for slides)
walk2(
  years,
  seq_along(years),
  ~{
    p <- plot_hrr_map(hospital_connections, .x, hrr_shapes)
    out <- file.path(frames_dir, sprintf("frame_%02d.png", .y))
    ggsave(
      filename = out, plot = p,
      width = 10, height = 6, units = "in", dpi = 200, bg = "white"
      # Use bg = "transparent" if you prefer transparent background in PPT
    )
  }
)

# Read frames in correct order
pngs <- list.files(frames_dir, pattern = "\\.png$", full.names = TRUE)
# Natural sort by embedded numbers
pngs <- pngs[order(pngs)]

# Optional: hold the last frame for a short pause at the end (e.g., 12 extra frames)
hold <- 12
pngs_with_hold <- c(pngs, rep(tail(pngs, 1), hold))

# Build and write the GIF
img <- image_read(pngs_with_hold)            # read all frames
anim <- image_animate(img, fps = 2, loop = 0)  # fps controls speed; loop=0 means infinite loop
image_write(anim, gif_file)

message("GIF written to: ", normalizePath(gif_file))


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
  filter(YEAR>=2014 & YEAR<=2022)

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
median_bed_size <- median(hospital_data$BDTOT, na.rm = TRUE)

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
  pack_rows("", 17,17, indent=FALSE, latex_gap_space = "-0.3em") %>%
  write("Objects//hospital_pair_types.tex")

# table: characteristics of connected in same/diff HRR and not connected ------------------------------
# add indicators
hospital_data <- hospital_data %>%
  mutate(sys = ifelse(is.na(SYSID),0,1))

n_become_same <- hospital_data %>%
  filter(become_same==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_always_same <- hospital_data %>%
  filter(always_same==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_lose_same <- hospital_data %>%
  filter(lose_same==1) %>%
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
n_lose_diff <- hospital_data %>%
  filter(lose_diff==1) %>%
  distinct(Filer.EIN) %>%
  nrow()
n_not <- hospital_data %>%
  filter(never_same==1 & never_diff==1) %>%
  distinct(Filer.EIN) %>%
  nrow()

stat_become_same <- hospital_data %>%
  filter(become_same==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Same HRR",
         n = n_become_same) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_always_same <- hospital_data %>%
  filter(always_same==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Same HRR",
         n = n_always_same) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_lose_same <- hospital_data %>%
  filter(lose_same==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Same HRR",
         n = n_lose_same) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_become_diff <- hospital_data %>%
  filter(become_diff==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Different HRR",
         n = n_become_diff) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_always_diff <- hospital_data %>%
  filter(always_diff==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Different HRR",
         n = n_always_diff) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_lose_diff <- hospital_data %>%
  filter(lose_diff==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Different HRR",
         n = n_lose_diff) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
stat_not <- hospital_data %>%
  filter(never_same==1 & never_diff==1) %>%
  summarise_at(c("Num. Beds" = "BDTOT",
                 "General" = "general",
                 "In System" = "sys",
                 "Teaching" = "teaching",
                 "Num. Physicians" = "FTMT",
                 "Num. Nurses" = "FTRNTF"), list(mean), na.rm=T) %>%
  mutate(group = "Never Connected",
         n = n_not) %>%
  select(group, n, `Num. Beds`, General, `In System`, `Teaching`, `Num. Physicians`, `Num. Nurses`)
  
stat <- rbind(stat_become_same, stat_become_diff,stat_lose_same, stat_lose_diff, stat_always_same, stat_always_diff, stat_not) %>%
  mutate(n=as.character(n),
         `Num. Beds` = as.character(format(round(`Num. Beds`), big.mark = ",", scientific = FALSE, trim = TRUE)),
         `Num. Physicians` = as.character(format(round(`Num. Physicians`), big.mark = ",", scientific = FALSE, trim = TRUE)),
         `Num. Nurses` = as.character(format(round(`Num. Nurses`), big.mark = ",", scientific = FALSE, trim = TRUE))) %>%
  select(-`Num. Physicians`)

knitr::kable(stat,
             format = "latex",
             col.names = c("Sample of Hosp.", "N", "Num. Beds", "General", "In System", "Academic", "Num. Nurses"),
             caption = "Hospital Characteristics by Overlap Status\\label{tab:hosp_group_stats}",
             row.names = FALSE,
             table.envir="table",
             digits=2,
             booktabs=TRUE,
             escape=F,
             align=c("l","c","c","c","c","c","c","c","c"),
             position="ht!",
             linesep = "") %>%
  pack_rows(group_label = "Become Connected", start_row = 1, end_row = 2) %>%
  pack_rows(group_label = "Lose Connection", start_row = 3, end_row = 4) %>%
  pack_rows(group_label = "Always Connected", start_row = 5, end_row = 6) %>%
  pack_rows("", 5, 5, indent = FALSE, latex_gap_space = "-0.3em") %>%
  write(file = paste0(objects_path, "hosp_group_stats.tex"))

# table and graphs: outcome variables by group broken down by pre- and post------------------------------------------------------

# define the groups
hospital_data <- hospital_data %>%
  mutate(group = ifelse(become_same==1, "Become Connected Same", NA)) %>%
  mutate(group = ifelse(never_same==1 & never_diff==1, "Never Connected", group)) %>%
  mutate(group = ifelse(become_diff==1 & is.na(group), "Become Connected Different", group)) %>%
  mutate(group = ifelse(always_diff==1 & is.na(group), "Always Connected Different", group))

# set desired order of groups
desired_order <- c("Become Connected Same", "Become Connected Different", "Always Connected Different", "Never Connected")

# create relative year variable
outcome_table <- hospital_data %>%
  filter(!is.na(group)) %>%
  mutate(TaxYr = as.numeric(TaxYr),
         minyr_sameHRR_part = as.numeric(minyr_sameHRR_part),
         minyr_diffHRR_part = as.numeric(minyr_diffHRR_part)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Same", minyr_sameHRR_part-TaxYr, NA)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Different", minyr_diffHRR_part-TaxYr, rel_year))

p_values_relative <- outcome_table %>%
  filter(!is.na(rel_year)) %>%
  filter(rel_year < 4 & rel_year > -4) %>%
  pivot_longer(
    cols = c("mcaid_discharges", "mcare_discharges", "tot_discharges",
             "bed_conc", "any_purch", 
             "tot_operating_exp", "num_services",
             "closure", "independent"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # ensure variable names match the *_m keys in outcome_table_relative
  mutate(variable = paste0(variable, "_m")) %>%
  mutate(timing = ifelse(rel_year < -1, "Pre", "Post")) %>%
  group_by(group, variable) %>%
  summarise(
    p_value = {
      pre  <- value[timing == "Pre"]
      post <- value[timing == "Post"]
      if (length(pre) > 1 && length(post) > 1) {
        suppressWarnings(t.test(pre, post, var.equal = FALSE)$p.value)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

outcome_table_relative <- outcome_table %>%
  filter(minyr_sameHRR_part!=2023 & minyr_diffHRR_part!=2023) %>%
  filter(!is.na(rel_year)) %>%
  group_by(rel_year, group) %>%
  summarise_at(c("mcaid_discharges", "mcare_discharges", "tot_discharges",
                 "bed_conc", "any_purch", 
                 "tot_operating_exp", "num_services",
                 "closure", "independent"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean") %>%
  filter(rel_year<4 & rel_year>-4) %>%
  mutate(timing = ifelse(rel_year < -1, "Pre", "Post")) %>%
  group_by(group, variable, timing) %>%
  summarise(mean=mean(mean)) %>%
  pivot_wider(id_cols = c(group, variable), names_from = timing, values_from = mean) %>%
  mutate(difference = Post-Pre) %>%
  left_join(p_values_relative, by = c("group", "variable"))
outcome_table_other <- outcome_table %>%
  filter(is.na(rel_year)) %>%
  group_by(TaxYr, group) %>%
  summarise_at(c("mcaid_discharges", "mcare_discharges", "tot_discharges",
                 "bed_conc", "any_purch", 
                 "tot_operating_exp", "num_services",
                 "closure", "independent"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean") %>%
  group_by(group, variable) %>%
  summarise(Pre=mean(mean))

outcome_table <- bind_rows(outcome_table_relative, outcome_table_other) %>%
  select(variable, group, Pre, Post, difference, p_value) %>%
  mutate(Pre = ifelse(abs(Pre)>10000, Pre/100000, Pre),
         Post = ifelse(abs(Post)>10000, Post/100000, Post),
         difference = ifelse(abs(difference)>10000, difference/100000, difference)) %>%
  mutate(group = factor(group, levels = c("Become Connected Same", "Become Connected Different", "Always Connected Different", "Never Connected"))) %>%
  arrange(group) %>%
  arrange(variable)

outcome_table %>% ungroup() %>%
  select(-variable) %>%
  knitr::kable(format = "latex",
               col.names = c("Sample of Hospitals", "Avg Pre", "Avg Post", "Post - Pre", "P-Value"),
               caption = "Outcome Variables Over Time\\label{tab:outcome_tab}",
               row.names = FALSE,
               table.envir="table",
               digits=2,
               booktabs=TRUE,
               escape=F,
               align=c("l","c","c","c","c","c","c","c","c"),
               position="hp!",
               linesep = "") %>%
  pack_rows(group_label = "Any Purchases (/100k)", start_row = 1, end_row = 4) %>%
  pack_rows(group_label = "Bed Concentration", start_row = 5, end_row = 8) %>%
  pack_rows(group_label = "Closure", start_row = 9, end_row = 12) %>%
  pack_rows(group_label = "Independent", start_row = 13, end_row = 16) %>%
  pack_rows(group_label = "Medicaid Discharges", start_row = 17, end_row = 20) %>%
  pack_rows(group_label = "Medicare Discharges", start_row = 21, end_row = 24) %>%
  pack_rows(group_label = "Number of Services", start_row = 25, end_row = 28) %>%
  pack_rows(group_label = "Total Discharges", start_row = 29, end_row = 32) %>%
  pack_rows(group_label = "Total Operating Expenses (/100k)", start_row = 33, end_row = 36) %>%
  write(file = paste0(objects_path, "hosp_outcomes_table.tex"))

# now make graphs of each of the outcomes 
outcome_graphs <- hospital_data %>%
  filter(!is.na(group)) %>%
  mutate(TaxYr = as.numeric(TaxYr),
         minyr_sameHRR_part = as.numeric(minyr_sameHRR_part),
         minyr_diffHRR_part = as.numeric(minyr_diffHRR_part)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Same", minyr_sameHRR_part-TaxYr, NA)) %>%
  mutate(rel_year = ifelse(group=="Become Connected Different", minyr_diffHRR_part-TaxYr, rel_year))

outcome_graphs_relative <- outcome_graphs %>%
  filter(!is.na(rel_year)) %>%
  group_by(rel_year, group) %>%
  summarise_at(c("mcaid_discharges", "mcare_discharges", "tot_discharges",
                 "bed_conc", "any_purch", 
                 "tot_operating_exp", "num_services",
                 "closure", "independent"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean") %>%
  filter(rel_year<=4 & rel_year>=-4) 
outcome_graphs_other <- outcome_graphs %>%
  filter(is.na(rel_year)) %>%
  group_by(TaxYr, group) %>%
  summarise_at(c("mcaid_discharges", "mcare_discharges", "tot_discharges",
                 "bed_conc", "any_purch", 
                 "tot_operating_exp", "num_services",
                 "closure", "independent"), list(m=mean), na.rm=T) %>%
  pivot_longer(cols = ends_with("_m"), names_to = "variable", values_to = "mean") 

graphs <- bind_rows(outcome_graphs_relative, outcome_graphs_other)


# graph for operating expenses
expenses_graph <- graphs %>%
  filter(variable %in% c("tot_operating_exp_m", "any_purch_m")) %>%
  mutate(mean = mean/100000) %>%
  mutate(variable = ifelse(variable == "tot_operating_exp_m", "Total Operating Expenses", "Investments (Building/Equipment)")) %>%
  mutate(
    axis_type = case_when(
      group %in% c("Become Connected Same", "Become Connected Different") ~ "Relative Year",
      group %in% c("Never Connected", "Always Connected Different") ~ "Year",
      TRUE ~ "Unknown axis"
    ),
    x_display = dplyr::if_else(axis_type == "Relative Year", rel_year, TaxYr)
  ) %>%
  filter(!is.na(x_display)) %>%
  ggplot(aes(x = x_display, y = mean, color = group, linetype = variable, shape = variable)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ axis_type, scales = "free_x", nrow = 1) +
  # Legends: color (Group) on top; shape + linetype (Patient type) below
  scale_color_brewer(name = "Group", palette = "Dark2", breaks = desired_order) +
  scale_shape_manual(
    name = "Expenses",
    values = c("Total Operating Expenses" = 16, "Investments (Building/Equipment)" = 17)  # circle, triangle
  ) +
  scale_linetype_manual(
    name = "Expenses",
    values = c("Total Operating Expenses" = "solid", "Investments (Building/Equipment)" = "dashed")
  ) +
  labs(
    x = NULL,                       # strip labels communicate the axis
    y = "Expenses (/100k)\n"
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",     
    legend.direction = "vertical",
    strip.text = element_text(face = "bold"),
    legend.key.width  = unit(18, "pt"),
    legend.key.height = unit(14, "pt")
  ) +
  ylim(0, 5000) +
  # vertical marker (only in Relative Year facet)
  geom_segment(
    data = data.frame(axis_type = "Relative Year", x = -0.5, xend = -0.5, y = 0, yend = 5000),
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dashed", color = "gray", size = 1, inherit.aes = FALSE
  ) +
  # Legend order: color first, then shape & linetype
  guides(
    color    = guide_legend(order = 1),
    shape    = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  ) +
  scale_x_continuous( breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2),    
                      labels = scales::number_format(accuracy = 1))

# graph for discharges
disch_graph <- graphs %>%
  filter(variable %in% c("mcaid_discharges_m", "mcare_discharges_m", "tot_discharges_m")) %>%
  mutate(variable = ifelse(variable == "mcaid_discharges_m", "Medicaid Discharges", variable)) %>%
  mutate(variable = ifelse(variable == "mcare_discharges_m", "Medicare Discharges", variable)) %>%
  mutate(variable = ifelse(variable == "tot_discharges_m", "Total Discharges", variable)) %>%
  mutate(
    axis_type = case_when(
      group %in% c("Become Connected Same", "Become Connected Different") ~ "Relative Year",
      group %in% c("Never Connected", "Always Connected Different") ~ "Year",
      TRUE ~ "Unknown axis"
    ),
    x_display = dplyr::if_else(axis_type == "Relative Year", rel_year, TaxYr)
  ) %>%
  filter(!is.na(x_display)) %>%
  ggplot(aes(x = x_display, y = mean, color = group, linetype = variable, shape = variable)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ axis_type, scales = "free_x", nrow = 1) +
  # Legends: color (Group) on top; shape + linetype (Patient type) below
  scale_color_brewer(name = "Group", palette = "Dark2", breaks = desired_order) +
  scale_shape_manual(
    name = "Patient type",
    values = c("Medicare Discharges" = 16, "Medicaid Discharges" = 17, "Total Discharges"=18)  # circle, triangle
  ) +
  scale_linetype_manual(
    name = "Patient type",
    values = c("Medicare Discharges" = "dotted", "Medicaid Discharges" = "dashed", "Total Discharges"="solid")
  ) +
  labs(
    x = NULL,                       # strip labels communicate the axis
    y = "Discharges\n"
  ) +
  theme_minimal(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",     
    legend.direction = "vertical",
    strip.text = element_text(face = "bold"),
    legend.key.width  = unit(18, "pt"),
    legend.key.height = unit(14, "pt")
  ) +
  ylim(0, 10000) +
  # vertical marker (only in Relative Year facet)
  geom_segment(
    data = data.frame(axis_type = "Relative Year", x = -0.5, xend = -0.5, y = 0, yend = 10000),
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dashed", color = "gray", size = 1, inherit.aes = FALSE
  ) +
  # Legend order: color first, then shape & linetype
  guides(
    color    = guide_legend(order = 1),
    shape    = guide_legend(order = 2),
    linetype = guide_legend(order = 2)
  ) +
  scale_x_continuous( breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2),    
                      labels = scales::number_format(accuracy = 1))
                         

# graph for number of services
num_services_graph <- graphs %>%
    filter(variable=="num_services_m") %>%
    mutate(
      axis_type = case_when(
        group %in% c("Become Connected Same", "Become Connected Different") ~ "Relative Year",
        group %in% c("Never Connected", "Always Connected Different") ~ "Year",
        TRUE ~ "Unknown axis"
      ),
      x_display = if_else(axis_type == "Relative Year", rel_year, TaxYr)
    ) %>%
    filter(!is.na(x_display)) %>%
    ggplot(aes(x = x_display, y = mean, color = group)) +
    geom_line() +
    geom_point(size = 2) +
    facet_wrap(~ axis_type, scales = "free_x", nrow = 1) +
    scale_color_brewer(name = "Group", palette = "Dark2", breaks = desired_order) +
    labs(
      x = NULL,                       # Strip labels will communicate the axis
      y = "Number of Services\n",
      color = "Group",
    ) +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(
      legend.position = "bottom",     
      legend.direction = "vertical",
      strip.text = element_text(face = "bold"),
      legend.key.width  = unit(18, "pt"),
      legend.key.height = unit(14, "pt")
    ) + ylim(0,200) + theme(element_text(family = "serif", size=13)) +
    geom_segment(
      data = data.frame(axis_type = "Relative Year", x = -0.5, xend = -0.5, y = 0, yend = 200),
      aes(x = x, xend = xend, y = y, yend = yend),
      linetype = "dashed", color = "gray", inherit.aes = FALSE
    )
  

# group patient and service outcomes together for the paper
# 1. Extract patient type legend only (remove group legend completely)
disch_legend <- get_legend(
  disch_graph +
    guides(color = "none") +                # drop group legend
    theme(legend.position = "bottom",
          legend.direction = "horizontal")  # force horizontal
)

# 2. Remove patient type legend AND group legend from num_services itself
disch_no_legends <- disch_graph +
  guides(color = "none", shape = "none", linetype = "none") +
  theme(legend.position = "none")

# 3. Stack disch_graph with its legend
disch_with_legend <- cowplot::plot_grid(
  disch_no_legends, disch_legend,
  ncol = 1, rel_heights = c(1, 0.15)
)

exp_legend <- get_legend(
  expenses_graph +
    guides(color = "none") +                # drop group legend
    theme(legend.position = "bottom",
          legend.direction = "horizontal")  # force horizontal
)

# 2. Remove patient type legend AND group legend from num_services itself
exp_no_legends <- expenses_graph +
  guides(color = "none", shape = "none", linetype = "none") +
  theme(legend.position = "none")

# 3. Stack disch_graph with its legend
exp_with_legend <- cowplot::plot_grid(
  exp_no_legends, exp_legend,
  ncol = 1, rel_heights = c(1, 0.15)
)

# 4. Arrange all three plots with a common group legend to the right
final_plot <- ggarrange(
  num_services_graph + theme(legend.position="none"),
  disch_with_legend,
  exp_with_legend,
  nrow = 3, ncol = 1,
  common.legend = TRUE,
  legend = "bottom"   # group legend stacked vertically
)

ggsave(final_plot, filename = paste0(objects_path, "outcome_desc_graph.pdf"), width = 7, height=10, units = "in")
