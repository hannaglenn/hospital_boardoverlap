# load libraries 
library(tidyr)
library(readr)
library(did)
library(purrr)
library(MatchIt)

source("./Scripts/paths.R")

options(knitr.kable.NA = '')

hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))

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

# filter out hospitals who are only connected for one year
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  filter(sum(any_partnership_sameHRR)!=1 | minyr_sameHRR_part==0)

# create variables for always, become, and never connected in the same and different HRRs (for partnerships)
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  mutate(always_same = ifelse(minyr_sameHRR_part==min(TaxYr),1,0),
         become_same = ifelse(minyr_sameHRR_part>min(TaxYr),1,0),
         never_same = ifelse(minyr_sameHRR_part==0,1,0)) %>%
  mutate(always_diff = ifelse(minyr_diffHRR_part==min(TaxYr),1,0),
         become_diff = ifelse(minyr_diffHRR_part>min(TaxYr),1,0),
         never_diff = ifelse(minyr_diffHRR_part==0,1,0)) %>%
  ungroup()

hospital_data <- hospital_data %>%
  mutate(minyr_sameHRR_part = as.numeric(minyr_sameHRR_part),
         minyr_diffHRR_part = as.numeric(minyr_diffHRR_part),
         Filer.EIN = as.numeric(Filer.EIN))

# create variables for treat and control group to make filtering easier in the estimation
hospital_data <- hospital_data %>%
  mutate(treat = ifelse(minyr_sameHRR_part>0 & (always_diff==1|never_diff==1),1,0),
         control_diff = ifelse(always_diff==1,1,0)) %>%
  mutate(TaxYr = as.numeric(TaxYr))

observe <- hospital_data %>%
  filter(treat==1 & minyr_sameHRR_part!=2015 & minyr_sameHRR_part!=2022)
observe %>%
  distinct(Filer.EIN) %>%
  nrow()

# Run Callaway and Sant'Anna estimation for staggered difference in differences
varlist <- list("perc_mcaid", "perc_mcare", "tot_discharges", "bed_conc", "any_purch",
                "tot_pat_rev", "tot_operating_exp")

models <- lapply(varlist, function(x) {
  all <- att_gt(yname = x,                # LHS Variable
                gname = "minyr_sameHRR_part",             # First year a unit is treated. (set to 0 if never treated)
                idname = "Filer.EIN",               # ID
                tname = "TaxYr",                  # Time Variable
                xformla = NULL,     
                data= hospital_data,
                est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                control_group = "nevertreated", # Set the control group to notyettreated or nevertreated
                clustervars = "Filer.EIN",          # Cluster Variables          
                anticipation=0,
                base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  diff_HRR <- att_gt(yname = x,                # LHS Variable
                    gname = "minyr_sameHRR_part",             # First year a unit is treated. (set to 0 if never treated)
                    idname = "Filer.EIN",               # ID
                    tname = "TaxYr",                  # Time Variable
                    xformla = NULL,     
                    data= filter(hospital_data, (treat==1|control_diff==1)),
                    est_method = "dr",               # dr is for doubly robust. can also use "ipw" (inverse probability weighting) or "reg" (regression)
                    control_group = "nevertreated", # Set the control group to notyettreated or nevertreated
                    clustervars = "Filer.EIN",          # Cluster Variables          
                    anticipation=0,
                    base_period = "varying" # can set a number of years to account for anticipation effects
  )
  
  list(all=all, diff_HRR=diff_HRR)
})

# Aggregate the previous results
Models_agg <- lapply(models, function(x){
  agg_all <- aggte(x$all, type = "dynamic", na.rm=T)
  p_all <- x$all$Wpval
  
  agg_diff <- aggte(x$diff_HRR, type = "dynamic", na.rm=T)
  p_diff <- x$diff_HRR$Wpval
  
  list(agg_all=agg_all, p_all=p_all, agg_diff=agg_diff, p_diff=p_diff)
})

plots <- lapply(Models_agg, function(x){
  all <- ggdid(x[["agg_all"]])
  diff <- ggdid(x[["agg_diff"]])
  
  list(all=all, diff=diff)
})


# try the analysis with coarsened exact matching
shared_outcomes <- c("perc_mcaid", "tot_operating_exp")

# --- Helper: build lean pre features for ONE outcome ---
build_pre_basic <- function(df, y,                            
                            id = "Filer.EIN", time = "TaxYr", g = "minyr_sameHRR_part") {  
  df %>%    filter(.data[[time]] < .data[[g]] | .data[[g]] == 0L) %>%   
    # pre-period rows    
    filter(!is.na(.data[[y]])) %>%    
    group_by(.data[[id]]) %>%    
    summarize(last_pre = .data[[y]][which.max(.data[[time]])],      
              mean_pre = mean(.data[[y]], na.rm = TRUE),      
              .groups = "drop") %>%    
    rename_with(~ paste0(y, "_", .x), -all_of(id))}

hospital_data_filter <- hospital_data %>%
  filter((become_same==1 & (never_diff==1 | always_diff==1)) | (never_same==1 & become_diff==1)) 
# --- Build and join pre features for the shared outcomes ---
pre_tbls <- lapply(shared_outcomes, function(v) build_pre_basic(hospital_data_filter, y = v))
pre_all  <- Reduce(function(a, b) dplyr::full_join(a, b, by = "Filer.EIN"), pre_tbls)

# --- CEM input with treatment flag (ever-treated vs never-treated) ---
cem_df <- hospital_data_filter %>%  
  distinct(Filer.EIN, minyr_sameHRR_part) %>%  
  mutate(ever_treated = as.integer(minyr_sameHRR_part > 0)) %>%  
  left_join(pre_all, by = "Filer.EIN") %>%  
  mutate(across(-c(Filer.EIN, minyr_sameHRR_part, ever_treated),                
                ~ tidyr::replace_na(.x, 0)))

hospital_data_filter %>%
  filter(become_same==1) %>%
  distinct(Filer.EIN) %>%
  nrow()

#--- Coarse cutpoints: 3 bins per feature (increase overlap) ---
feature_names <- setdiff(names(cem_df), 
                         c("Filer.EIN","minyr_sameHRR_part","ever_treated"))
cuts <- as.list(rep(4, length(feature_names)))
names(cuts) <- feature_names

m_primary <- matchit(ever_treated ~ . - Filer.EIN - minyr_sameHRR_part,  
                     data = cem_df,  
                     method    = "cem",  
                     estimand  = "ATT",  
                     cutpoints = cuts,  
                     verbose   = TRUE)
print(summary(m_primary)) 

#--- Extract matched IDs and weights ---
cem_w_primary <- tibble(Filer.EIN = cem_df$Filer.EIN, 
                        cem_w_primary = m_primary$weights)
matched_ids_primary <- cem_w_primary %>% 
  filter(cem_w_primary > 0) %>% 
  pull(Filer.EIN)

#--- Pre-selected long panel for analysis (primary sample) ---
hospital_matched_primary <- hospital_data %>%  
  semi_join(tibble(Filer.EIN = matched_ids_primary), by = "Filer.EIN") %>%  
  left_join(cem_w_primary, by = "Filer.EIN") %>%  
  mutate(cem_w_primary = replace_na(cem_w_primary, 0))

# Outcomes to estimate
varlist <- c("perc_mcaid","perc_mcare","tot_discharges","bed_conc","any_purch",
             "tot_pat_rev","tot_operating_exp")

# Core estimator for one outcome, using CEM weights
run_attgt <- function(df, y) {
  att_gt(
    yname   = y,
    gname   = "minyr_sameHRR_part",  # 0 for never-treated; >0 first treatment year
    idname  = "Filer.EIN",
    tname   = "TaxYr",
    xformla = NULL,                   # add covariates here if you want conditional trends
    data    = df,
    est_method    = "dr",             # doubly robust first-stage
    control_group = "nevertreated",  # recommended in staggered timing
    clustervars   = "Filer.EIN",      # cluster at unit level
    anticipation  = 0,                # drop leads if you think anticipation exists
    base_period   = "varying",        # default; aligns event-time construction
    weightsname   = "cem_w_primary"   # << CEM weights
  )
}

# Run for each outcome
models <- setNames(
  map(varlist, ~ run_attgt(hospital_matched_primary, .x)),
  varlist
)


# Aggregate to event-time (event study) and plot
agg <- map(models, ~ aggte(.x, type = "dynamic", min_e = -3, max_e = 3, na.rm = TRUE))
plots <- map(agg, ggdid)

# Example: show/print one outcome's results (e.g., bed_conc)
print(summary(agg[["tot_operating_exp"]]))


# Tidy dynamic estimates for each outcome into one data frame
tidy_dyn <- map2_df(agg, names(agg), function(a, y) {
  tibble(
    outcome   = y,
    e         = a$egt,        # event time (relative to treatment)
    att       = a$att.egt,    # ATT at event time e
    se        = a$se.egt,
    pval      = a$pt.egt,
    cilo      = a$att.egt - qnorm(0.975)*a$se.egt,
    cihi      = a$att.egt + qnorm(0.975)*a$se.egt
  )
})

# Overall ATT (e.g., "simple" aggregation across groups/times)
agg_overall <- map(models, ~ aggte(.x, type = "simple", na.rm = TRUE))
tidy_overall <- map2_df(agg_overall, names(agg_overall), function(a,y) {
  tibble(outcome = y, att = a$overall.att, se = a$overall.se, pval = a$overall.pvalue)
})

  

