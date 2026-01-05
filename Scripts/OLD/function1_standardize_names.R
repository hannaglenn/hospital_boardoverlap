standardize_names_optimized <- function(data, max_dist) {
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

standardize_names_by_hrr <- function(data, max_dist) {
  setDT(data)  # Convert to data.table for efficiency
  
  # Split data by HRRCODE (instead of Filer.EIN)
  hrr_list <- split(data, by = "HRRCODE", keep.by = TRUE)
  
  # Function to process each HRR group
  process_hrr <- function(hrr_data) {
    if (nrow(hrr_data) <= 1) return(hrr_data)  # Skip HRRs with only 1 name
    
    unique_names <- unique(hrr_data[, .(name_cleaned)])  # Reduce size
    
    # Compute approximate name matches only within this HRR
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
    hrr_data <- merge(hrr_data, matches, by.x = "name_cleaned", by.y = "name_cleaned.y", all.x = TRUE)
    hrr_data[, name_cleaned := fcoalesce(correct_name, name_cleaned)]
    hrr_data[, correct_name := NULL]
    
    return(hrr_data)
  }
  
  # Process HRR groups separately to reduce memory usage
  corrected_list <- lapply(hrr_list, process_hrr)
  
  # Recombine data
  corrected_data <- rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
  
  return(corrected_data)
}
