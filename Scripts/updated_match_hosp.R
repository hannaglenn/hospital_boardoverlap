## Match AHA IDs to tax identifiers

library(dplyr)
library(stringr)
library(readr)
library(cdlTools)

# Read in EIN data from XMLs
all_ein_data <- readRDS("CreatedData/all_ein_data_scheduleH.rds")

# How many EINs are in this data?
length(unique(all_ein_data$Filer.EIN))

# Get rid of EINs that don't appear from at least 2018-2021
all_ein_data <- all_ein_data %>%
  mutate(present = ifelse(TaxYr>=2018 & TaxYr<=2021, 1, 0)) %>%
  group_by(Filer.EIN) %>%
  filter(sum(present)>3) %>%
  ungroup()

# Combine the variables that end with "BusinessNameLine1Txt"
all_ein_data <- all_ein_data %>%
  mutate(BusinessName1 = Filer.BusinessName.BusinessNameLine1Txt) %>%
  mutate(BusinessName1 = ifelse(is.na(BusinessName1), Filer.BusinessNameLine1Txt, BusinessName1))

# Combine Line2 of business name
all_ein_data <- all_ein_data %>%
  mutate(BusinessName2 = Filer.BusinessName.BusinessNameLine2Txt)

# Change businessname to all caps
all_ein_data <- all_ein_data %>%
  mutate(BusinessName1 = toupper(BusinessName1),
         BusinessName2 = toupper(BusinessName2))

# Select and rename relevant variables
# keep all variables with USAddress in the variable name
ein_data <- all_ein_data %>%
  select(TaxYr, Filer.EIN, BusinessName1, BusinessName2, Filer.USAddress.AddressLine1Txt, Filer.USAddress.StateAbbreviationCd,
         Filer.USAddress.ZIPCd, Filer.USAddress.CityNm)

# Work through any missing business names to determine most accurate name
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName2), BusinessName1, NA))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(grepl("^DBA|^D/B/A", BusinessName2), BusinessName2, BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "^DBA|^D/B/A"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "DBA$"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & (BusinessName2=="GROUP RETURN"|BusinessName2=="LETTER RULING"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & (str_detect(BusinessName1, "AND$|OF$|\\&$|-$")|str_detect(BusinessName2, "^AND|^OF|^\\&|^-")), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & !str_detect(BusinessName2, "\\s"), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "EASTERN MAINE HEALTHCARE SYSTEMS"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "^C/O"), BusinessName2, BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "^C/O"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "^F/K/A|^FKA|^\\(FKA|^\\(F/K/A|^FORMERLY KNOWN AS"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & BusinessName1==BusinessName2, BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "OZARK HEALTH"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & BusinessName2=="ADMINISTERING WILLS EYE HOSPITAL", "WILLS EYE HOSPITAL", BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "WINDBER HOSPITAL INC"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "BELLIN HEALTH OCONTO HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "POMERENE HOSPITAL"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "DBA\\s"), str_extract(BusinessName2, "DBA.+"), BusinessName)) %>%
  mutate(BusinessName = str_remove(BusinessName, "DBA\\s"))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "PERRY COUNTY MEMORIAL HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT INLAND HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT ACADIA HOSPITAL"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "MCKENZIE HEALTH SYSTEM"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "NORTHERN LIGHT"), BusinessName2, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "ALEGENT HEALTH-COMMUNITY MEMORIAL"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName2, "DISTRICT HEALTH FACILITIES CORP"), BusinessName1, BusinessName))
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName) & str_detect(BusinessName1, "\\(FKA|FKA"), str_remove(BusinessName1, "FKA.+"), BusinessName))

# For any still missing observations, paste the two business names together
ein_data <- ein_data %>%
  mutate(BusinessName = ifelse(is.na(BusinessName), paste(BusinessName1, BusinessName2, sep=" "), BusinessName))

# Keep relevant variables
ein_data <- ein_data %>%
  select(TaxYr, Filer.EIN, BusinessName, Filer.USAddress.AddressLine1Txt, Filer.USAddress.StateAbbreviationCd, Filer.USAddress.ZIPCd,
         Filer.USAddress.CityNm) %>%
  mutate(BusinessName = str_trim(BusinessName))


# Read in the AHA data
AHA <- read_csv("RawData/AHAdata_20052023.csv")

# Keep relevant variables
AHA <- AHA %>%
  select(YEAR, ID, MNAME, MLOCADDR, FSTCD, MLOCZIP, MLOCCITY) 

# Standardize the variables in both data sets
# Convert AHA hospital name to standard characters
AHA <- AHA %>%
  mutate(MNAME = iconv(MNAME, "latin1", "ASCII", sub=""))
# Fix common abbreviations in the AHA data
AHA <- AHA %>%
  mutate(MNAME = str_replace(MNAME, "Med\\s", "MEDICAL"),
         MNAME = str_replace(MNAME, "Ctr\\s", "CENTER"))
AHA <- AHA %>%
  mutate(MNAME = toupper(MNAME)) %>%
  mutate(MNAME = str_remove_all(MNAME, "-|\\s\\.")) %>%
  mutate(MLOCADDR = toupper(MLOCADDR),
         MLOCCITY = toupper(MLOCCITY))

# Convert zip codes into first five digits
AHA <- AHA %>%
  mutate(MLOCZIP = str_sub(MLOCZIP, 1, 5)) %>%
  mutate(MLOCZIP = ifelse(nchar(MLOCZIP)==4, paste("0", MLOCZIP, sep=""), MLOCZIP))

# do the same for the tax data
ein_data <- ein_data %>%
  mutate(BusinessName = str_remove_all(BusinessName, "-|\\s\\."),
         Filer.USAddress.AddressLine1Txt = toupper(Filer.USAddress.AddressLine1Txt),
         Filer.USAddress.CityNm = toupper(Filer.USAddress.CityNm))

ein_data <- ein_data %>%
  mutate(Filer.USAddress.ZIPCd = str_sub(Filer.USAddress.ZIPCd, 1, 5))

# Convert tax state abbreviation to FIPS code
ein_data <- ein_data %>%
  mutate(Filer.Stfips = fips(Filer.USAddress.StateAbbreviationCd))

# limit the AHA data to 2016 and later
AHA <- AHA %>%
  filter(YEAR>=2016)

# Remove "INC" and "Association" from EIN names
ein_data <- ein_data %>%
  mutate(BusinessName = str_remove_all(BusinessName, "INC|ASSOCIATION|ASSOC"))

# Join the data sets using stringdist_inner_join
joined_data <- ein_data %>%
  fuzzyjoin::stringdist_left_join(AHA, 
                                   by=c("BusinessName"="MNAME"), 
                                   method="jw", 
                                   max_dist=0.1,
                                   distance_col = "jw_dist")


# remove the YEAR element
joined_data <- joined_data %>%
  select(-YEAR) %>%
  distinct()

# Only keep matches in the same state
joined_data <- joined_data %>%
  filter(Filer.Stfips==FSTCD | is.na(FSTCD)) %>%
  distinct()

joined_data <- joined_data %>%
  distinct(TaxYr, Filer.EIN, ID, .keep_all = TRUE)

# Remove non-matches (missing ID)
joined_data <- joined_data %>%
  filter(!is.na(ID))

# create an index of things that indicate a good match
joined_data <- joined_data %>%
  mutate(address_dist = stringdist::stringdist(Filer.USAddress.AddressLine1Txt, MLOCADDR, method="jw")) %>%
  mutate(index = 0) %>%
  mutate(index = ifelse(Filer.USAddress.CityNm==MLOCCITY, index+1, index)) %>%
  mutate(index = ifelse(Filer.USAddress.ZIPCd==MLOCZIP, index+1, index)) %>%
  mutate(index = ifelse(address_dist<.07, index+2, index)) %>%
  mutate(index = ifelse(str_detect(BusinessName, MLOCCITY), index+1, index)) %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(index = ifelse(jw_dist==min(jw_dist), index+1, index)) %>%
  mutate(index = ifelse(jw_dist==0, index+2, index)) %>%
  ungroup()

# look at all observations with index less than 2
observe <- joined_data %>%
  filter(index<2) 

# Remove any matches with index less than 2 (not a good match)
joined_data <- joined_data %>%
  filter(index>=2 | is.na(index))

# look at multiples
observe <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(n()>1)

# Within a Filer.EIN, year: pick the match with the highest index
joined_data <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(index==max(index)) %>%
  ungroup()
  
# Fix any multiples that I can
joined_data <- joined_data %>%
  filter(!(Filer.EIN==311724085 & ID==6411370)) %>%
  filter(!(Filer.EIN==813747248 & ID==6731215)) %>%
  filter(!(Filer.EIN==470439599 & ID==6660418))

# Drop the rest of the observations with multiple matches
joined_data <- joined_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  filter(n()==1) %>%
  ungroup()

# Merge the joined data with the original EIN data
ein_data <- ein_data %>%
  left_join(joined_data)
ein_data <- ein_data %>%
  distinct()

# Are any EINs associated with multiple AHAs?
observe <- ein_data %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN, ID) %>%
  group_by(Filer.EIN) %>%
  filter(n()>1)
  # none

# fill in missing IDs
ein_data <- ein_data %>%
  group_by(Filer.EIN) %>%
  tidyr::fill(ID, .direction="downup") %>%
  ungroup()

distinct_matches <- ein_data %>%
  select(-TaxYr) %>%
  distinct(Filer.EIN, ID, BusinessName, MNAME)

# How many matches?
distinct_matches %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN) %>%
  nrow()
  #1071

# look at a random sample of 10 eins
distinct_matches %>%
  filter(!is.na(ID)) %>%
  sample_n(10)




# look at observations that did not have a match
no_match <- ein_data %>%
  filter(is.na(ID)) %>%
  select(Filer.EIN, BusinessName, Filer.USAddress.AddressLine1Txt, Filer.USAddress.CityNm, Filer.USAddress.StateAbbreviationCd, Filer.USAddress.ZIPCd,
         Filer.Stfips)

# merge to AHA based on similar addresses
no_match_join <- no_match %>%
  fuzzyjoin::stringdist_inner_join(AHA, 
                                   by=c("Filer.USAddress.AddressLine1Txt"="MLOCADDR"), 
                                   method="jw", 
                                   max_dist=0.07,
                                   distance_col = "jw_dist")

# get rid of year element
no_match_join <- no_match_join %>%
  select(-YEAR) %>%
  distinct()

# Only keep matches in the same state
no_match_join <- no_match_join %>%
  filter(Filer.Stfips==FSTCD | is.na(FSTCD)) %>%
  distinct(Filer.EIN, ID, .keep_all = TRUE)

# create variable for string distance between the names
no_match_join <- no_match_join %>%
  mutate(name_dist = stringdist::stringdist(BusinessName, MNAME, method="jw")) %>%
  filter(name_dist<0.4)

# select the match with the lowest name distance when there are multiple
no_match_join <- no_match_join %>%
  group_by(Filer.EIN) %>%
  filter(name_dist==min(name_dist)) %>%
  ungroup()

# remove duplicates
no_match_join <- no_match_join %>%
  distinct(Filer.EIN, ID, BusinessName)

distinct_matches <- bind_rows(distinct_matches, no_match_join)

# remove observations with NA if they have a non-NA value
distinct_matches <- distinct_matches %>%
  group_by(Filer.EIN) %>%
  filter(!(n()>1 & is.na(ID))) %>%
  ungroup()

# look at those with no matches
no_matches2 <- distinct_matches %>%
  filter(is.na(ID)) 




# manually fill in matches I can find ####
distinct_matches <- distinct_matches %>%
  mutate(ID = ifelse(Filer.EIN==990298651, 6950305, ID),
         ID = ifelse(Filer.EIN==390848401, 6451370, ID),
         ID = ifelse(Filer.EIN==391928401, 6451370, ID),
         ID = ifelse(Filer.EIN==390808480, 6451720, ID),
         ID = ifelse(Filer.EIN==411884597, 6611742, ID),
         ID = ifelse(Filer.EIN==410706143, 6610520, ID),
         ID = ifelse(Filer.EIN==231370484, 6230745, ID),
         ID = ifelse(Filer.EIN==221482276, 6221135, ID),
         ID = ifelse(Filer.EIN==310537486, 6410490, ID),
         ID = ifelse(Filer.EIN==630308739, 6530013, ID),
         ID = ifelse(Filer.EIN==160743037, 6214240, ID),
         ID = ifelse(Filer.EIN==250987222, 6230510, ID),
         ID = ifelse(Filer.EIN==841262971, 6840018, ID),
         ID = ifelse(Filer.EIN==452438973, 6611750, ID),
         ID = ifelse(Filer.EIN==520607971, 6320750, ID),
         ID = ifelse(Filer.EIN==410724034, 6610590, ID),
         ID = ifelse(Filer.EIN==610624096, 6519020, ID),
         ID = ifelse(Filer.EIN==810243720, 6810450, ID),
         ID = ifelse(Filer.EIN==561509260, 6361210, ID),
         ID = ifelse(Filer.EIN==810245848, 6810485, ID),
         ID = ifelse(Filer.EIN==043398280, 6160004, ID),
         ID = ifelse(Filer.EIN==751844139, 6740082, ID),
         ID = ifelse(Filer.EIN==751844139, 6740082, ID),
         ID = ifelse(Filer.EIN==203749695, 6740381, ID),
         ID = ifelse(Filer.EIN==582026750, 6380525, ID),
         ID = ifelse(Filer.EIN==205497506, 6380690, ID),
         ID = ifelse(Filer.EIN==431656689, 6630730, ID),
         ID = ifelse(Filer.EIN==201649466, 6440043, ID),
         ID = ifelse(Filer.EIN==560530233, 6360950, ID),
         ID = ifelse(Filer.EIN==810977948, 6450860, ID),
         ID = ifelse(Filer.EIN==420733472, 6620150, ID),
         ID = ifelse(Filer.EIN==421529472, 6620150, ID),
         ID = ifelse(Filer.EIN==751976930, 6740219, ID),
         ID = ifelse(Filer.EIN==731440267, 6730705, ID),
         ID = ifelse(Filer.EIN==731440267, 6730705, ID),
         ID = ifelse(Filer.EIN==850106941, 6850390, ID),
         ID = ifelse(Filer.EIN==231529076, 6231375, ID),
         ID = ifelse(Filer.EIN==231529076, 6231375, ID),
         ID = ifelse(Filer.EIN==470533373, 6660500, ID),
         ID = ifelse(Filer.EIN==592142859, 6390082, ID),
         ID = ifelse(Filer.EIN==331007002, 6640004, ID),
         ID = ifelse(Filer.EIN==730579295, 6730310, ID),
         ID = ifelse(Filer.EIN==591943502, 6390283, ID),
         ID = ifelse(Filer.EIN==361509000, 6430930, ID),
         ID = ifelse(Filer.EIN==362166000, 6430930, ID),
         ID = ifelse(Filer.EIN==464007700, 6740880, ID),
         ID = ifelse(Filer.EIN==582032904, 6380775, ID),
         ID = ifelse(Filer.EIN==590624414, 6390545, ID),
         ID = ifelse(Filer.EIN==611297707, 6510081, ID),
         ID = ifelse(Filer.EIN==311131099, 6510283, ID),
         ID = ifelse(Filer.EIN==590624371, 6390530, ID),
         ID = ifelse(Filer.EIN==640668465, 6540810, ID),
         ID = ifelse(Filer.EIN==480543789, 6671025, ID),
         ID = ifelse(Filer.EIN==810515463, 6810085, ID),
         ID = ifelse(Filer.EIN==411878730, 6610390, ID),
         ID = ifelse(Filer.EIN==261175213, 6640003, ID),
         ID = ifelse(Filer.EIN==731444504, 6739140, ID),
         ID = ifelse(Filer.EIN==310833936, 6410391, ID),
         ID = ifelse(Filer.EIN==420680370, 6620560, ID),
         ID = ifelse(Filer.EIN==522218584, 6330130, ID),
         ID = ifelse(Filer.EIN==231352156, 6231660, ID),
         ID = ifelse(Filer.EIN==810226578, 6810560, ID),
         ID = ifelse(Filer.EIN==751008430, 6741390, ID),
         ID = ifelse(Filer.EIN==741595711, 6743366, ID),
         ID = ifelse(Filer.EIN==470468078, 6660192, ID),
         ID = ifelse(Filer.EIN==450226729, 6640260, ID),
         ID = ifelse(Filer.EIN==390808526, 6450100, ID),
         ID = ifelse(Filer.EIN==470379834, 6660090, ID),
         ID = ifelse(Filer.EIN==470379755, 6660002, ID),
         ID = ifelse(Filer.EIN==470443636, 6660520, ID),
         ID = ifelse(Filer.EIN==520591639, 6320170, ID),
         ID = ifelse(Filer.EIN==620532345, 6520019, ID),
         ID = ifelse(Filer.EIN==250965237, 6231100, ID),
         ID = ifelse(Filer.EIN==910715805, 6910273, ID),
         ID = ifelse(Filer.EIN==626002604, 6520506, ID),
         ID = ifelse(Filer.EIN==250965274, 6230270, ID),
         ID = ifelse(Filer.EIN==240795463, 6232970, ID),
         ID = ifelse(Filer.EIN==813048423, 6370025, ID),
         ID = ifelse(Filer.EIN==362596381, 6431613, ID),
         ID = ifelse(Filer.EIN==850138775, 6850007, ID),
         ID = ifelse(Filer.EIN==621113169, 6520695, ID),
         ID = ifelse(Filer.EIN==560585243, 6360690, ID),
         ID = ifelse(Filer.EIN==042121377, 6391095, ID),
         ID = ifelse(Filer.EIN==390773970, 6451390, ID),
         ID = ifelse(Filer.EIN==430662495, 6630230, ID),
         ID = ifelse(Filer.EIN==560525657, 6361403, ID),
         ID = ifelse(Filer.EIN==813257997, 6540116, ID),
         ID = ifelse(Filer.EIN==440584290, 6630045, ID),
         ID = ifelse(Filer.EIN==381474929, 6440120, ID),
         ID = ifelse(Filer.EIN==592447554, 6390050, ID),
         ID = ifelse(Filer.EIN==941502014, 6931085, ID),
         ID = ifelse(Filer.EIN==410726173, 6610550, ID),
         ID = ifelse(Filer.EIN==450226553, 6640440, ID),
         ID = ifelse(Filer.EIN==710407683, 6710108, ID),
         ID = ifelse(Filer.EIN==392031968, 6660010, ID),
         ID = ifelse(Filer.EIN==752771569, 6749550, ID),
         ID = ifelse(Filer.EIN==460225483, 6650530, ID),
         ID = ifelse(Filer.EIN==251550350, 6230049, ID),
         ID = ifelse(Filer.EIN==741109665, 6743120, ID),
         ID = ifelse(Filer.EIN==540551711, 6341085, ID),
         ID = ifelse(Filer.EIN==251801532, 6233100, ID),
         ID = ifelse(Filer.EIN==742842747, 6670850, ID),
         ID = ifelse(Filer.EIN==420843389, 6620724, ID),
         ID = ifelse(Filer.EIN==481226977, 6670075, ID),
         ID = ifelse(Filer.EIN==043633263, 6370018, ID),
         ID = ifelse(Filer.EIN==592980620, 6390411, ID),
         ID = ifelse(Filer.EIN==453765471, 6380598, ID),
         ID = ifelse(Filer.EIN==440577118, 6631320, ID),
         ID = ifelse(Filer.EIN==610444716, 6510040, ID),
         ID = ifelse(Filer.EIN==060646917, 6160009, ID),
         ID = ifelse(Filer.EIN==611388556, 6510140, ID),
         ID = ifelse(Filer.EIN==390837206, 6451630, ID),
         ID = ifelse(Filer.EIN==900054984, 6141130, ID),
         ID = ifelse(Filer.EIN==540506332, 6341040, ID),
         ID = ifelse(Filer.EIN==941461843, 6933853, ID),
         ID = ifelse(Filer.EIN==390806367, 6450775, ID),
         ID = ifelse(Filer.EIN==930391573, 6920110, ID),
         ID = ifelse(Filer.EIN==352087092, 6420760, ID),
         ID = ifelse(Filer.EIN==465032999, 6519071, ID),
         ID = ifelse(Filer.EIN==450227311, 6640060, ID),
         ID = ifelse(Filer.EIN==344440884, 6410217, ID),
         ID = ifelse(Filer.EIN==580968382, 6380285, ID),
         ID = ifelse(Filer.EIN==141364513, 6211490, ID),
         ID = ifelse(Filer.EIN==850105601, 6850055, ID),
         ID = ifelse(Filer.EIN==593149293, 6390046, ID),
         ID = ifelse(Filer.EIN==202401676, 6420830, ID),
         ID = ifelse(Filer.EIN==382027689, 6441500, ID),
         ID = ifelse(Filer.EIN==363208390, 6431790, ID),
         ID = ifelse(Filer.EIN==381360584, 6441595, ID),
         ID = ifelse(Filer.EIN==486005089, 6671210, ID),
         ID = ifelse(Filer.EIN==420710268, 6620805, ID),
         ID = ifelse(Filer.EIN==470408242, 6660855, ID),
         ID = ifelse(Filer.EIN==486099245, 6670787, ID),
         ID = ifelse(Filer.EIN==570342027, 6370540, ID),
         ID = ifelse(Filer.EIN==410307617, 6610620, ID),
         ID = ifelse(Filer.EIN==810373589, 6810123, ID),
         ID = ifelse(Filer.EIN==810469886, 6810129, ID),
         ID = ifelse(Filer.EIN==816016152, 6810405, ID),
         ID = ifelse(Filer.EIN==450308379, 6640265, ID),
         ID = ifelse(Filer.EIN==383369438, 6440023, ID),
         ID = ifelse(Filer.EIN==823844150, 6710433, ID),
         ID = ifelse(Filer.EIN==610523304, 6510745, ID),
         ID = ifelse(Filer.EIN==370662580, 6430360, ID),
         ID = ifelse(Filer.EIN==363637465, 6431927, ID),
         ID = ifelse(Filer.EIN==930415219, 6920743, ID),
         ID = ifelse(Filer.EIN==391536207, 6450485, ID),
         ID = ifelse(Filer.EIN==550379108, 6350630, ID),
         ID = ifelse(Filer.EIN==646010402, 6540455, ID),
         ID = ifelse(Filer.EIN==371119538, 6433070, ID),
         ID = ifelse(Filer.EIN==133957095, 6210024, ID),
         ID = ifelse(Filer.EIN==362548549, 6432750, ID),
         ID = ifelse(Filer.EIN==460239781, 6650345, ID),
         ID = ifelse(Filer.EIN==440655986, 6630650, ID),
         ID = ifelse(Filer.EIN==582224545, 6380020, ID),
         ID = ifelse(Filer.EIN==440537826, 6630175, ID),
         ID = ifelse(Filer.EIN==562276994, 6360978, ID),
         ID = ifelse(Filer.EIN==610965365, 6511000, ID),
         ID = ifelse(Filer.EIN==030183721, 6130290, ID),
         ID = ifelse(Filer.EIN==821162805, 6820050, ID),
         ID = ifelse(Filer.EIN==453072990, 6381160, ID),
         ID = ifelse(Filer.EIN==362181997, 6431720, ID),
         ID = ifelse(Filer.EIN==420707096, 6620230, ID),
         ID = ifelse(Filer.EIN==840482695, 6840067, ID),
         ID = ifelse(Filer.EIN==261861676, 6521230, ID),
         ID = ifelse(Filer.EIN==941044474, 6930007, ID),
         ID = ifelse(Filer.EIN==141731786, 6210007, ID),
         ID = ifelse(Filer.EIN==910567263, 6911260, ID),
         ID = ifelse(Filer.EIN==364251846, 6431926, ID),
         ID = ifelse(Filer.EIN==450227752, 6640215, ID),
         ID = ifelse(Filer.EIN==150346515, 6212350, ID),
         ID = ifelse(Filer.EIN==150552726, 6212370, ID),
         ID = ifelse(Filer.EIN==630385130, 6530030, ID),
         ID = ifelse(Filer.EIN==370661218, 6431490, ID),
         ID = ifelse(Filer.EIN==810405434, 6810160, ID),
         ID = ifelse(Filer.EIN==250984595, 6230210, ID),
         ID = ifelse(Filer.EIN==455055149, 6730051, ID),
         ID = ifelse(Filer.EIN==741461220, 6742801, ID),
         ID = ifelse(Filer.EIN==370681540, 6432615, ID),
         ID = ifelse(Filer.EIN==814670687, 6911115, ID))
         
         





distinct_matches <- distinct_matches %>%
  distinct(Filer.EIN, ID, .keep_all=T)



# how many matches?
distinct_matches %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN) %>%
  nrow()
  # 1628!!!!

# duplicates in filer.ein?
distinct_matches %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN, ID) %>%
  group_by(Filer.EIN) %>%
  filter(n()>1)
  # 0

# duplicates in ID?
observe <- distinct_matches %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN, ID, .keep_all = T) %>%
  group_by(ID) %>%
  filter(n()>1)

# deal with duplicates that are wrong
distinct_matches <- distinct_matches %>%
  mutate(ID = ifelse(Filer.EIN==131624135, NA , ID),
         ID = ifelse(Filer.EIN==222807681, NA , ID),
         ID = ifelse(Filer.EIN==251550350, NA , ID),
         ID = ifelse(Filer.EIN==465143606, 6230552 , ID),
         ID = ifelse(Filer.EIN==231352203, 6232820 , ID),
         ID = ifelse(Filer.EIN==560585243, NA , ID),
         ID = ifelse(Filer.EIN==331216751, NA , ID),
         ID = ifelse(Filer.EIN==351869951, NA , ID),
         ID = ifelse(Filer.EIN==391264986, 6451580 , ID),
         ID = ifelse(Filer.EIN==611362001, NA, ID),
         ID = ifelse(Filer.EIN==610920842, NA, ID),
         ID = ifelse(Filer.EIN==454394739, 6230456, ID)) %>%
  filter(Filer.EIN!=390807063 & Filer.EIN!=390902199)

distinct_matches <- distinct_matches %>%
  distinct(Filer.EIN, ID)

# save distinct ein, ID crosswalk
saveRDS(distinct_matches, "CreatedData/updated_ein_aha_cw.rds")

# how many unique EINs?
cw %>%
  filter(!is.na(ID)) %>%
  distinct(Filer.EIN) %>%
  nrow()
