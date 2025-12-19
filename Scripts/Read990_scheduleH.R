library(xml2)
library(XML)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# write a script to loop through all xml files published by the IRS
# Here, I will check whether it's a hospital by checking if there is a Schedule H section

replace_null <- function(x) {
  x <- purrr::map(x, ~replace(.x, is.null(.x), NA_character_))
  purrr::map(x, ~if(is.list(.x)) replace_null(.x) else .x)
}


# make a list of all folders to search through
#folders <- list.files(path = paste0(drive_path, "IRS_XML_990s/"), full.names = FALSE)

# create empty data frames to store data
#all_ein_data <- data.frame()
#all_people_data <- data.frame()

# read in temp data sets
all_ein_data <- readRDS(paste0(created_data_path, "all_ein_data_scheduleH.rds"))
all_people_data <- readRDS(paste0(created_data_path, "all_people_data_scheduleH.rds"))

# loop through each folder
for (folder in folders) {
  # make a list of all files in the folder
  files <- list.files(path = paste0(drive_path, "IRS_XML_990s/",folder), pattern = ".xml", full.names = FALSE)
  
  # loop through files
  for (file in files){
    # read in the xml file
    # See if the file is non-empty
    if (file.size(paste0(drive_path, "IRS_XML_990s/",folder,"/",file)) > 0){
      xml <- xmlParse(paste0(drive_path, "IRS_XML_990s/",folder,"/",file))
      xml_list <- xmlToList(xml, addAttributes = TRUE)
      
      # Return TRUE if there is a Schedule H in the return
      hosp <- ifelse("IRS990ScheduleH" %in% names(xml_list[["ReturnData"]]),1,0)
    
      # check if the ein is in the list of eins
      if (hosp==1){
        ein <- xml_list[["ReturnHeader"]][["Filer"]][["EIN"]]
        # convert to list format
        xml_list <- xmlToList(xml, addAttributes = TRUE)
        # fill all missing values with NA
        xml_list <- replace_null(xml_list)
        
        header <- as.data.frame(xml_list[["ReturnHeader"]])
        
        data <- list_flatten(xml_list[["ReturnData"]], name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- keep(data, ~length(.x) == 1)
        data <- as.data.frame(data)
        
        # convert from wide to long
        data <- data %>%
          gather(key = "variable", value = "value") %>%
          distinct() %>%
          filter(!stringr::str_detect(variable, "attrs$"))
        
        simple_vars <- data %>%
          filter(stringr::str_detect(variable, "^[A-Za-z0-9]+\\_[A-Za-z0-9]+$")) %>%
          mutate(variable = stringr::str_remove(variable, "^[A-Za-z0-9]+\\_"))
        
        # long to wide in variable
        simple_vars <- simple_vars %>%
          distinct(variable, .keep_all = TRUE) %>%
          spread(key = variable, value = value) 
        
        # Create data set for board/employees/key executives in the 990s
        people_vars <- data %>%
          filter(stringr::str_detect(variable, regex("PersonNm|TitleTxt|AverageHoursPerWeek|BusinessName", ignore_case=T)))
        
        # get rid of beginning stuff
        people_vars <- people_vars %>%
          separate(variable, c("Form", "Section", "Variable"), sep = "\\_", extra="merge")
        
        # look for any variables that don't have a number and add it to the end
        people_vars <- people_vars %>%
          mutate(Variable = ifelse(stringr::str_detect(Variable, "[0-9]+$"), Variable, paste0(Variable, ".1000"))) 
        
        # long to wide using numbers at the end of variables
        people_vars <- people_vars %>%
          select(-Form) %>%
          separate(Variable, c("Variable", "id"), sep = "\\.", extra="merge") %>%
          pivot_wider(names_from="Variable", values_from="value") %>%
          select(-id) 
        
        # change all columns to character
        people_vars <- mutate_all(people_vars, as.character)
        
        
        # get data from the header section of the 990
        cols <- stringr::str_detect(names(header), regex("filer|ein|taxyr|taxperiodbegindt|taxperiodenddt|businessname|state|zip", ignore_case=TRUE))
        header_vars <- header[, cols]
        
        # combine simple variables and header variables into an EIN level data set
        ein_data <- cbind(header_vars, simple_vars)
        
        # save ein and tax year to add to the people data
        ein_tax_year <- ein_data %>%
          select(Filer.EIN, TaxYr)
        
        if (nrow(people_vars) > 0){
          # combine people data with ein and tax year
          people_vars <- cbind(ein_tax_year, people_vars)
          
          all_ein_data <- bind_rows(all_ein_data, ein_data)
          all_people_data <- bind_rows(all_people_data, people_vars)
        }
      }
    }
  }
}


additional_folders <- list.files(path = "./data/", full.names = FALSE)

# loop through each folder from the webscraping exercise to get older XMLs as well (see DownloadXMLs.R)
for (folder in additional_folders) {
  # make a list of all files in the folder
  files <- list.files(path = paste0("./data/",folder), full.names = FALSE)
  
  # loop through files
  for (file in files){
    # read in the xml file
    # See if the file is non-empty
    if (file.size(paste0("./data/",folder,"/",file)) > 0){
      xml <- xmlParse(paste0("./data/",folder,"/",file))
      xml_list <- xmlToList(xml, addAttributes = TRUE)
      
        ein <- xml_list[["ReturnHeader"]][["Filer"]][["EIN"]]
        # convert to list format
        xml_list <- xmlToList(xml, addAttributes = TRUE)
        # fill all missing values with NA
        xml_list <- replace_null(xml_list)
        
        header <- as.data.frame(xml_list[["ReturnHeader"]])
        
        data <- list_flatten(xml_list[["ReturnData"]], name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- list_flatten(data, name_repair = "minimal")
        data <- keep(data, ~length(.x) == 1)
        data <- as.data.frame(data)
        
        # convert from wide to long
        data <- data %>%
          gather(key = "variable", value = "value") %>%
          distinct() %>%
          filter(!stringr::str_detect(variable, "attrs$"))
        
        simple_vars <- data %>%
          filter(stringr::str_detect(variable, "^[A-Za-z0-9]+\\_[A-Za-z0-9]+$")) %>%
          mutate(variable = stringr::str_remove(variable, "^[A-Za-z0-9]+\\_"))
        
        # long to wide in variable
        simple_vars <- simple_vars %>%
          distinct(variable, .keep_all = TRUE) %>%
          spread(key = variable, value = value) 
        
        # Create data set for board/employees/key executives in the 990s
        people_vars <- data %>%
          filter(stringr::str_detect(variable, regex("PersonNm|TitleTxt|AverageHoursPerWeek|BusinessName", ignore_case=T)))
        
        # get rid of beginning stuff
        people_vars <- people_vars %>%
          separate(variable, c("Form", "Section", "Variable"), sep = "\\_", extra="merge")
        
        # look for any variables that don't have a number and add it to the end
        people_vars <- people_vars %>%
          mutate(Variable = ifelse(stringr::str_detect(Variable, "[0-9]+$"), Variable, paste0(Variable, ".1000"))) 
        
        # long to wide using numbers at the end of variables
        people_vars <- people_vars %>%
          select(-Form) %>%
          separate(Variable, c("Variable", "id"), sep = "\\.", extra="merge") %>%
          pivot_wider(names_from="Variable", values_from="value") %>%
          select(-id) 
        
        # change all columns to character
        people_vars <- mutate_all(people_vars, as.character)
        
        # get data from the header section of the 990
        cols <- stringr::str_detect(names(header), regex("filer|ein|taxyr|taxperiodbegindt|taxperiodenddt|businessname|state|zip", ignore_case=TRUE))
        header_vars <- header[, cols]
        
        # combine simple variables and header variables into an EIN level data set
        ein_data <- cbind(header_vars, simple_vars)
        
        # save ein and tax year to add to the people data
        ein_tax_year <- ein_data %>%
          select(Filer.EIN, TaxYr)
        
        if (nrow(people_vars) > 0){
          # combine people data with ein and tax year
          people_vars <- cbind(ein_tax_year, people_vars)
          
          all_ein_data <- bind_rows(all_ein_data, ein_data)
          all_people_data <- bind_rows(all_people_data, people_vars)
        }
    }
  }
}

# get rid of duplicate rows
all_ein_data <- all_ein_data %>%
  distinct()
all_people_data <- all_people_data %>%
  distinct()

# save temporary version of both data sets
saveRDS(all_ein_data, paste0(created_data_path, "all_ein_data_scheduleH.rds"))
saveRDS(all_people_data, paste0(created_data_path, "all_people_data_scheduleH.rds"))

# how many EINs are in the data
num_eins <- all_people_data %>%
  distinct(Filer.EIN) %>%
  nrow()

