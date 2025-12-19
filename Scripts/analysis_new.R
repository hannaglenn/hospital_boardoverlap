source("./Scripts/paths.R")

library(tidyr)
library(dplyr)
library(purrr)
library(fixest)
library(ggplot2)
library(did)
library(forcats)

# ----------------------------------------------------------------------------------
#                                     Helper Functions
# ----------------------------------------------------------------------------------
# Pre period is fixed at g-1; Post period is g + post_offset (event time k).
build_stack <- function(df, treat, pre_offset = -1, post_offset = 0) {
  stopifnot(is.character(treat), length(treat) == 1)  
  if (!treat %in% names(df)) stop("`treat` column not found in `df`.")  
  if (!is.numeric(dplyr::pull(df, .data[[treat]]))) stop("`treat` must be numeric.")  
  
  cohort_years <- df %>%    
    filter(.data[[treat]] > 0) %>%    
    distinct(.data[[treat]]) %>%    
    pull() %>%    
    sort()
  
  stacked <- bind_rows(lapply(cohort_years, function(g) {
    pre_year  <- g + pre_offset       # g-1
    post_year <- g + post_offset      # g + k
    
    df %>%
      filter(TaxYr %in% c(pre_year, post_year)) %>%
      mutate(
        Treated       = as.integer(.data[[treat]] == g),
        clean_control = (.data[[treat]] == 0 | .data[[treat]] > g),
        Post          = as.integer(TaxYr == post_year),
        stack_id      = g
      ) %>%
      filter(Treated == 1 | clean_control) %>%
      group_by(Filer.EIN) %>%
      # keep units present in both pre and post within this slice
      filter(n_distinct(Post) == 2) %>%
      ungroup()
  }))
  
  stacked %>%
    mutate(
      id_stack   = interaction(stack_id, Filer.EIN, drop = TRUE, lex.order = TRUE),
      time_stack = interaction(stack_id, TaxYr,     drop = TRUE, lex.order = TRUE)
    )
}

# ------------------ Helper: pooled DiD with dynamic outcome ------------------
estimate_stacked <- function(stacked_df, outcome_var, cluster_var = "Filer.EIN") {
  if (nrow(stacked_df) == 0) {
    return(tibble(att = NA_real_, se = NA_real_, n_units = 0L, n_rows = 0L))
  }
  
  fml   <- as.formula(paste(outcome_var, "~ Treated:Post | id_stack + time_stack"))
  cl_fm <- as.formula(paste0("~ ", cluster_var))
  
  m <- feols(fml, data = stacked_df, cluster = cl_fm)
  
  mf <- model.frame(m) 
  n_treat <- n_distinct(mf$Filer.EIN[mf$Treated == 1])
  
  tibble(
    att     = unname(coef(m)[["Treated:Post"]]),
    se      = unname(sqrt(diag(vcov(m)))[["Treated:Post"]]),
    n_units = n_distinct(stacked_df$Filer.EIN),
    n_rows  = nrow(stacked_df),
    n_treat = n_treat
  )
}

# ------------------ Run stacked event-study for k in {-3,-2,0,1,2,3,4} ------------------
run_event_study <- function(df, treat, outcome_var,
                            ks = c(-3,-2, 0, 1, 2, 3, 4),
                            pre_offset = -1, cluster_var = "Filer.EIN") {
  map_df(ks, function(k) {
    stk <- build_stack(df, treat, pre_offset = pre_offset, post_offset = k)
    est <- estimate_stacked(stk, outcome_var = outcome_var, cluster_var = cluster_var)
    est %>%
      mutate(
        event_time = k,
        ci_low  = att - qnorm(0.95) * se,
        ci_high = att + qnorm(0.95) * se
      )
  }) %>%
    arrange(event_time)
}

create_side_by_side_graph <- function(es_plot, x){
  x <- if (!is.null(outcome_labels) && x %in% names(outcome_labels)) {    
    outcome_labels[[x]]  } else {    
      x  }
  # ------------------ Prepare variables ------------------
  # Derive 'HRR_type' and 'overlap_type' from 'group'
  es_plot <- es_plot %>%
    mutate(
      HRR_type = if_else(grepl("Same HRR", group), "Same HRR", "Different HRR"),
      overlap_type = case_when(
        grepl("^Gain", group) ~ "Gain Overlap",
        grepl("^Lose", group) ~ "Lose Overlap",
        TRUE ~ "Other"
      )
    )
  
  # Ensure clean factor ordering for legend and facets
  es_plot <- es_plot %>%
    mutate(
      HRR_type     = fct_relevel(HRR_type, "Same HRR", "Different HRR"),
      overlap_type = fct_relevel(overlap_type, "Gain Overlap", "Lose Overlap")
    ) 
  
  # Color palette keyed to overlap_type (consistent across facets)
  pal <- c(
    "Gain Overlap" = "#2a9d8f",  # teal
    "Lose Overlap" = "#e76f51"   # red-orange
  )
  
  # Optional linetypes (solid for Gain, dashed for Lose)
  linetypes <- c(
    "Gain Overlap" = "solid",
    "Lose Overlap" = "dashed"
  )
  
  # ------------------ Build plot ------------------
  gg <- ggplot(es_plot, aes(x = event_time, y = att)) +
    # Reference lines
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
    
    # --- Layer A: shaded ribbons (alpha only affects fill) ---
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high, fill = overlap_type, group = interaction(HRR_type, overlap_type)),
      alpha = 0.20, colour = NA
    ) +
    
    # --- Layer B: ribbon borders (no fill, full opacity) ---
    geom_ribbon(
      aes(ymin = ci_low, ymax = ci_high, colour = overlap_type, group = interaction(HRR_type, overlap_type)),
      fill = NA, linewidth = 0.5
    ) +
    
    # Lines and points for the ATT curve
    geom_line(aes(color = overlap_type, linetype = overlap_type), linewidth = 0.5) +
    geom_point(aes(color = overlap_type, shape = overlap_type), size = 1) +
    
    # Axes and scales
    scale_x_continuous(breaks = -4:4) +
    scale_color_manual(values = pal) +   # lines, borders, points
    scale_fill_manual(values = pal) +    # ribbon interior
    scale_linetype_manual(values = linetypes) +
    scale_shape_manual(values = c("Gain Overlap" = 16, "Lose Overlap" = 17)) +
    
    # Facet: side-by-side panels for HRR_type
    facet_wrap(~ HRR_type, nrow = 1, scales = "fixed") +  # use scales = "free_y" if ranges differ a lot
    
    # Labels and theme
    labs(
      x = "\nEvent time",
      y = "ATT and 90% CI\n"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      text = element_text(family = "serif"),
      legend.title  = element_blank(),
      legend.position = "right",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.2)))
}

# Helper: single-panel graph for Same HRR groups only
create_same_hrr_graph <- function(es_plot,
                                  outcome_var,              # string: "closure", etc.
                                  palette = c("Gain Overlap" = "#2a9d8f",  # teal
                                              "Lose Overlap" = "#e76f51"), # red-orange
                                  linetypes = c("Gain Overlap" = "solid",
                                                "Lose Overlap" = "dashed"),
                                  shapes = c("Gain Overlap" = 16, "Lose Overlap" = 17),
                                  base_size = 13,
                                  use_serif = TRUE) {
  
  # Map outcome variable to display label
  outcome_label <- if (outcome_var %in% names(outcome_labels)) outcome_labels[[outcome_var]] else outcome_var
  
  # Keep only Same HRR groups
  es_same <- es_plot |>
    dplyr::filter(group %in% c("Gain Overlap in Same HRR", "Lose Overlap in Same HRR")) |>
    dplyr::mutate(
      overlap_type = ifelse(grepl("^Gain", group), "Gain Overlap", "Lose Overlap"),
      overlap_type = factor(overlap_type, levels = c("Gain Overlap", "Lose Overlap"))
    )
  
  # Build plot: single panel (no facets)
  g <- ggplot2::ggplot(es_same, ggplot2::aes(x = event_time, y = att)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
    
    # Shaded CI (fill only)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_low, ymax = ci_high, fill = overlap_type, group = overlap_type),
      alpha = 0.20, colour = NA
    ) +
    # CI borders (color only)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_low, ymax = ci_high, colour = overlap_type, group = overlap_type),
      fill = NA, linewidth = 0.5
    ) +
    
    # Lines and points
    ggplot2::geom_line(ggplot2::aes(color = overlap_type, linetype = overlap_type), linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = overlap_type, shape = overlap_type), size = 1) +
    
    ggplot2::scale_x_continuous(breaks = -4:4) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_linetype_manual(values = linetypes) +
    ggplot2::scale_shape_manual(values = shapes) +
    
    ggplot2::labs(
      x = "\nEvent time",
      y = "ATT and 90% CI"
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = if (use_serif) ggplot2::element_text(family = "serif") else ggplot2::element_text(),
      legend.title  = ggplot2::element_blank(),
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.2)))
  
  g
}



# Friendly labels for outcomes
outcome_labels <- c("closure"            = "Likelihood of Closure",  
                    "any_formal_sameHRR" = "Likelihood of Having Overlap in-System",  
                    "independent"        = "Likelihood of Being Independent",
                    "num_services" = "Number of Services Offered",
                    "FTRNTF" = "Number of Full-time Nurses",
                    "JNTMD" = "Has Joint Venture",
                    "tot_operating_exp" = "Total Operating Expenses",
                    "build_purch" = "Building Purchases",
                    "fixedequipment_purch" = "Fixed Equipment Purchases",
                    "movableequipment_purch" = "Movable Equipment Purchases",
                    "any_purch" = "Any Purchases",
                    "bed_conc" = "Bed Concentration",
                    "tot_discharges" = "Total Discharges",
                    "perc_mcaid" = "Percent Patients Medicaid",
                    "perc_mcare" = "Percent Patients Medicare",
                    "mcaid_discharges" = "Medicaid Discharges",
                    "mcare_discharges" = "Medicare Discharges")

# ----------------------------------------------------------------------------------
#                                     Create Data Sets
# ----------------------------------------------------------------------------------

# generic filtering
hospital_data <- readRDS(paste0(created_data_path, "hospital_data_boardandexec.rds"))

# Filter to either being present in 2015-2020 at least or dropping out of the data
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

# filter out hospitals who don't stay connected for more than 3 years
#hospital_data <- hospital_data %>%
  #filter((maxyr_sameHRR_part - minyr_sameHRR_part > 3) | minyr_sameHRR_part==0)

# create variables for always, become, and never connected in the same and different HRRs
hospital_data <- hospital_data %>%
  group_by(Filer.EIN) %>%
  mutate(always_same = ifelse(minyr_sameHRR_part==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_sameHRR_part==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_same = ifelse(minyr_sameHRR_part>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_same = ifelse(minyr_sameHRR_part==0,1,0)) %>%
  mutate(always_diff = ifelse(minyr_diffHRR_part==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_diffHRR_part==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_diff = ifelse(minyr_diffHRR_part>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_diff = ifelse(minyr_diffHRR_part==0,1,0)) %>%
  mutate(always_formal_same = ifelse(minyr_sameHRR_formal==min(TaxYr[!is.na(any_partnership_sameHRR)]) & maxyr_sameHRR_formal==max(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         become_formal_same = ifelse(minyr_sameHRR_formal>min(TaxYr[!is.na(any_partnership_sameHRR)]),1,0),
         never_formal_same = ifelse(minyr_sameHRR_formal==0,1,0)) %>%
  ungroup()
  

# create different data sets based on the control and treatment groups I want to use -------------------------------

# For the purpose of closures, drop the mechanical losses of connection
hospital_data_closures <- hospital_data %>%
  mutate(maxyr_sameHRR_part = ifelse(dropout_both_ind & maxyr_sameHRR_part==dropout_year-1,NA,maxyr_sameHRR_part),
         maxyr_diffHRR_part = ifelse(dropout_both_ind & maxyr_diffHRR_part==dropout_year-1,NA,maxyr_diffHRR_part))

hospital_data_behaviors <- hospital_data %>%
  filter(any_sameHRR_nonmiss_2014_2021) %>%
  filter((minyr_sameHRR_part>0 | maxyr_sameHRR_part>0)  & minyr_diffHRR_part>0) %>%   # filter control group to connected in different HRR
  filter(always_formal_same==1 | never_formal_same==1 | abs(minyr_sameHRR_formal - minyr_sameHRR_part)>2) %>% # filter out changes to in-system connectedness
  group_by(Filer.EIN) %>%
  filter(sum(independent, na.rm=T)==n() | sum(independent, na.rm=T)==0) %>% # filter out those who switch being affiliated with a system
  filter(minyr_sameHRR_part!=min(TaxYr)) %>% # filter out those treated in the first period
  ungroup() %>%
  filter(minyr_sameHRR_part==0 | maxyr_sameHRR_part - minyr_sameHRR_part>2)  # make sure they stay connected at least 2 years

# ----------------------------------------------------------------------------------
#                                 Overlap -> Consolidation
# ----------------------------------------------------------------------------------
varlist <- c("any_formal_sameHRR","any_formal_diffHRR", "independent")

# Use full hospital data for this estimation
consolidation_results <- lapply(varlist, function(x){
  es_gain_same <- run_event_study(filter(hospital_data, become_same==1 | always_diff==1 | never_diff==1),
                                  treat = "minyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Gain Overlap in Same HRR")
  es_gain_diff <- run_event_study(filter(hospital_data, become_diff==1 | always_same==1 | never_same==1),
                                  treat = "minyr_diffHRR_part", outcome_var = x) %>%
    mutate(group = "Gain Overlap in Different HRR")
  es_lose_same <- run_event_study(filter(hospital_data, become_same==1 | always_diff==1 | never_diff==1),
                                  treat = "maxyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Lose Overlap in Same HRR")
  es_lose_diff <- run_event_study(filter(hospital_data, become_diff==1 | always_same==1 | never_same==1),
                                  treat = "maxyr_diffHRR_part", outcome_var = x) %>%
    mutate(group = "Lose Overlap in Different HRR")
  
  es <- rbind(es_gain_same, es_gain_diff, es_lose_same, es_lose_diff)
  
  # (Optional) remove rows where att is NA (no usable data for that k)
  es_plot <- es %>% filter(!is.na(att))
  
  gg <- create_side_by_side_graph(es_plot, x)
  
  ggsave(gg, filename = paste0(objects_path, x, ".pdf"), width=8, height=5, units="in")
  
  list(gg, es_plot)
})

varlist <- c("closure")
closure_results <- lapply(varlist, function(x){
  es_gain_same <- run_event_study(filter(hospital_data, become_same==1 | always_diff==1 | never_diff==1),
                                  treat = "minyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Gain Overlap in Same HRR")
  es_gain_diff <- run_event_study(filter(hospital_data, become_diff==1 | always_same==1 | never_same==1),
                                  treat = "minyr_diffHRR_part", outcome_var = x) %>%
    mutate(group = "Gain Overlap in Different HRR")
  es_lose_same <- run_event_study(filter(hospital_data_closures, become_same==1 | always_diff==1 | never_diff==1),
                                  treat = "maxyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Lose Overlap in Same HRR")
  es_lose_diff <- run_event_study(filter(hospital_data_closures, become_diff==1 | always_same==1 | never_same==1),
                                  treat = "maxyr_diffHRR_part", outcome_var = x) %>%
    mutate(group = "Lose Overlap in Different HRR")
  
  es <- rbind(es_gain_same, es_gain_diff, es_lose_same, es_lose_diff)
  
  # (Optional) remove rows where att is NA (no usable data for that k)
  es_plot <- es %>% filter(!is.na(att))
  
  gg <- create_side_by_side_graph(es_plot, x)
  
  ggsave(gg, filename = paste0(objects_path, x, ".pdf"), width=8, height=5, units="in")
  
  list(gg, es_plot)
})

# ----------------------------------------------------------------------------------
#                                 Overlap -> Behaviors
# ----------------------------------------------------------------------------------
varlist <- list("perc_mcaid", "mcaid_discharges", "perc_mcare", "mcare_discharges", "tot_discharges",
                "bed_conc", "any_purch", "movableequipment_purch", "fixedequipment_purch", "build_purch",
                "tot_operating_exp", "JNTMD", "FTRNTF", "num_services") 

# Use full hospital data for this estimation
behavior_results <- lapply(varlist, function(x){
  es_gain_same <- run_event_study(hospital_data_behaviors,
                                  treat = "minyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Gain Overlap in Same HRR")
  es_lose_same <- run_event_study(hospital_data_behaviors,
                                  treat = "maxyr_sameHRR_part", outcome_var = x) %>%
    mutate(group = "Lose Overlap in Same HRR")

  
  es <- rbind(es_gain_same, es_lose_same)
  
  # (Optional) remove rows where att is NA (no usable data for that k)
  es_plot <- es %>% filter(!is.na(att))
  
  gg <- create_same_hrr_graph(es_plot, outcome_var=x)
  
  ggsave(gg, filename = paste0(objects_path, x, ".pdf"), width=6, height=5, units="in")
  
  list(gg)
})





# Let's try a triple differences --------------------------------------------

# -------- Build one stacked 2x2 across same-HRR cohorts with a diff-HRR status 
# Pre = g-1; Post = g + post_offset (event time k).
build_ddd_stack <- function(df,
                            pre_offset   = -1,
                            post_offset  = 0,
                            id_var       = "Filer.EIN",
                            time_var     = "TaxYr",
                            same_cohort  = "minyr_sameHRR_part",
                            diff_cohort  = "minyr_diffHRR_part") {
  needed <- c(id_var, time_var, same_cohort, diff_cohort)
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
  
  cohort_years <- df %>%
    filter(.data[[same_cohort]] > 0) %>%
    distinct(.data[[same_cohort]]) %>%
    pull() %>% sort()
  
  stacked <- bind_rows(lapply(cohort_years, function(g) {
    pre_year  <- g + pre_offset
    post_year <- g + post_offset
    
    df %>%
      filter(.data[[time_var]] %in% c(pre_year, post_year)) %>%
      mutate(
        TreatedSame   = as.integer(.data[[same_cohort]] == g),
        clean_control = (.data[[same_cohort]] == 0 | .data[[same_cohort]] > g),
        Post          = as.integer(.data[[time_var]] == post_year),
        # Time-varying third dimension: connected in a different HRR by year t
        DiffStatus_t  = as.integer(.data[[diff_cohort]] > 0 & .data[[time_var]] >= .data[[diff_cohort]]),
        stack_id      = g
      ) %>%
      # keep treated in cohort g and clean controls for the same-HRR dimension
      filter(TreatedSame == 1 | clean_control) %>%
      group_by(.data[[id_var]]) %>%
      # ensure each unit appears in both pre and post within this stack
      filter(n_distinct(Post) == 2) %>%
      ungroup() %>%
      # standardize id/time for FE & clustering
      mutate(id = .data[[id_var]], t = .data[[time_var]])
  }))
  
  # OPTIONAL: require both DiffStatus groups present in the stack (for DDD identification)
  stacks_with_both_groups <- stacked %>%
    group_by(stack_id) %>%
    summarise(ndiff_groups = n_distinct(DiffStatus_t), .groups = "drop") %>%
    filter(ndiff_groups == 2) %>%
    pull(stack_id)
  
  stacked <- stacked %>% filter(stack_id %in% stacks_with_both_groups)
  
  # stack-specific FE
  stacked %>%
    mutate(
      id_stack         = interaction(stack_id, id, drop = TRUE, lex.order = TRUE),
      time_stack       = interaction(stack_id, t,  drop = TRUE, lex.order = TRUE),
      # allow baseline differences by DiffStatus within each stack
      stack_diff_group = interaction(stack_id, DiffStatus_t, drop = TRUE, lex.order = TRUE),
      id_for_cluster   = id
    )
}

# -------- Estimate DDD on the stacked data 
# Outcome is dynamic; returns ATT(DDD), SE, and counts.
estimate_ddd_stacked <- function(stacked_df, outcome_var) {
  required <- c("TreatedSame","Post","DiffStatus_t","id_stack","time_stack","stack_diff_group","id_for_cluster")
  if (!all(required %in% names(stacked_df))) stop("Stacked df lacks required columns. Run build_ddd_stack().")
  
  if (nrow(stacked_df) == 0) {
    return(tibble(
      outcome    = outcome_var,
      ddd        = NA_real_,
      se         = NA_real_,
      n_units    = 0L,
      n_rows     = 0L,
      n_treated  = 0L,
      n_controls = 0L
    ))
  }
  
  # Formula: triple interaction + stack-specific FE (unit, time, and group)
  fml <- as.formula(paste0(outcome_var, " ~ TreatedSame:Post:DiffStatus_t | id_stack + time_stack + stack_diff_group"))
  
  m <- feols(fml, data = stacked_df, cluster = ~ id_for_cluster)
  
  tibble(
    outcome    = outcome_var,
    ddd        = unname(coef(m)[["TreatedSame:Post:DiffStatus_t"]]),
    se         = unname(sqrt(diag(vcov(m)))[["TreatedSame:Post:DiffStatus_t"]]),
    n_units    = dplyr::n_distinct(stacked_df$id_for_cluster),
    n_rows     = nrow(stacked_df),
    n_treated  = dplyr::n_distinct(stacked_df$id_for_cluster[stacked_df$TreatedSame == 1]),
    n_controls = dplyr::n_distinct(stacked_df$id_for_cluster[stacked_df$clean_control == TRUE])
  )
}

# -------- Event-time runner: k in {-4,...,-2,0,1,2,3,4} 
run_ddd_event_study <- function(df, outcome_var,
                                ks           = c(-4,-3,-2, 0, 1, 2, 3, 4),
                                pre_offset   = -1,
                                id_var       = "Filer.EIN",
                                time_var     = "TaxYr",
                                same_cohort  = "minyr_sameHRR_part",
                                diff_cohort  = "minyr_diffHRR_part") {
  purrr::map_df(ks, function(k) {
    stk <- build_ddd_stack(df,
                           pre_offset  = pre_offset,
                           post_offset = k,
                           id_var      = id_var,
                           time_var    = time_var,
                           same_cohort = same_cohort,
                           diff_cohort = diff_cohort)
    est <- estimate_ddd_stacked(stk, outcome_var = outcome_var)
    est %>%
      mutate(
        event_time = k,
        ci_low  = ddd - qnorm(0.975) * se,
        ci_high = ddd + qnorm(0.975) * se
      )
  }) %>%
    arrange(event_time)
}


ddd_es <- run_ddd_event_study(hospital_data, outcome_var = "perc_mcaid")

# Inspect results with counts
ddd_es %>% select(event_time, ddd, se, ci_low, ci_high, n_treated, n_controls, n_units, n_rows)



