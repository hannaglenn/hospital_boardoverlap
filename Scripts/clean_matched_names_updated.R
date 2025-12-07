library(readr)
library(stringr)
library(dplyr)
library(gender)

source("./Scripts/paths.R")

# Script to clean up the name columns of people data scraped from 990s

all_people_data <- readRDS(paste0(created_data_path, "/all_people_data_scheduleH.rds"))
cw <- readRDS(paste0(created_data_path, "/updated_ein_aha_cw.rds"))

# only keep observations where EIN is found in the crosswalk
all_people_data <- all_people_data %>%
  filter(Filer.EIN %in% cw$Filer.EIN)

# look at unique values of section
unique(all_people_data$Section)

# only keep relevant sections
all_people_data <- all_people_data %>%
  filter(Section %in% c("Form990PartVIISectionAGrp", "RltdOrgOfficerTrstKeyEmplGrp"))

# fill personNm with other lines
all_people_data <- all_people_data %>%
  mutate(PersonNm = ifelse(is.na(PersonNm), BusinessName_BusinessNameLine1Txt, PersonNm)) %>%
  filter(!is.na(PersonNm) & PersonNm != "'")
# this fills all missing person names

# Look at missing titles
observe <- all_people_data %>%
  filter(is.na(TitleTxt))

# remove observations with no title text
all_people_data <- all_people_data %>%
  filter(!is.na(TitleTxt))

# tabulate the number of unique EINs for every value of TaxYr
all_people_data %>%
  group_by(TaxYr) %>%
  summarise(n = n_distinct(Filer.EIN))

# Remove 2015 and 2023
all_people_data <- all_people_data %>%
  filter(TaxYr!=2015 & TaxYr!=2023)

# create data set with only relevant variables
people_data <- all_people_data %>%
  select(TaxYr, Filer.EIN, PersonNm, TitleTxt)

# convert to all lowercase
people_data <- people_data %>%
  mutate(PersonNm = tolower(PersonNm),
         TitleTxt = tolower(TitleTxt))

# GOAL 1: GET ANY RELEVANT TITLES FROM THE NAME COLUMN #################
# I got these lists by going through all of the names with at least 3 words. Don't do this again unless you add more names!!
doctor_list <- c("md", "do", "urologist", "mdmba")
other_doctor_list <- c("mbbch", "mbbs", "dds", "dentist", "od", "pharmd", "pt", "radiologist", "physician", "surgeon", "otolaryngologist")
nurse_list <- c("rn", "dnp", "aprn", "crna", "np", "scn", "fnp", "cne", "fnp-c", "apnp", "fnp-", "cenp", "mphrn", "acnp", "msnrnfaan", "crnp", "msnrn",
                "eddrn", "dpn", "drnp", "anp", "edrn", "arnp", "pnp", "cnp")
ha_list <- c("mha", "fache", "mhadrph", "mhsa", "mhcm", "mhs")
remove_list <- c("mbbch", "mbbs", "until", "as of", "eff", "end", "beg", "thru", "through", "osb",
                 "ending", "term", "mph", "deceased", "to", "begin", "left", "see statement", "director", "chair", "hired", "termed",
                 "began", "retired", "from", "resigned", "off", "vp", "ended", "started", "trm", "dir of", "pd by", "provost",
                 "start", "oct", "treasurer", "jan", "st", "chief", "t0", "entered", "interim", "board", "fmr", "int", "trustee",
                 "vice president", "dir", "bh", "vppres", "msn", "mst", "assoc", "departed", "msw", "ceo", "cbe", "secretary", "rsmterm",
                 "effective", "effec", "former", "admininstrator", "consultant", "cnm", "fellow", "beginning", "president",
                 "dr", "bishop", "esq", "jr", "sr", "md", "phd", "dds", "ii", "iii", "iv", "ms", "mr", "do","most rev", "rev", "rn", "edd", "cpa", "od",
                 "the rt", "dnp", "bc", "faan", "mb", "chb", "mbbh", "mbbch", "chcio", "rsm", "part year", "family practice", "major general",
                 "sister", "mha", "magistrate", "- see sch o", "see sch o", "sch o", "dc","the very rev", "the most reverend", "the honorable", "the hon",
                 "general", "vice pres", "fr", "mba", "jd", "ma", "bsn", "cco", "chco", "mpas", "pac", "dsc", "cbs", "chs", "ed d",
                 "rd", "bsbn", "mshrm", "cpm", "ccim", "ecc", "home care", "st john", "ph d", "csj", "non voting", "ex officio",
                 "dmd", "ret maj gen", "sc", "pharm d", "int ceo", "msn", "osf", "mother", "rtscra", "dha", "dh", "evpcao", "evpcoo",
                 "dha", "family medicine", "mshr", "aprn", "cssf", "offcr", "cfo", "chcp", "ccep", "facp", "fhm", "cphq", "op", "cpe", "vd", "dvm",
                 "cma", "drph", "mhp", "pmhnp", "er", "fsgm", "eid", "dpm", "staff acct", "cfp", "cpfo", "pe", "father", "esquire", "phjc", "dnsc",
                 "csfn", "dpth", "br", "fsc", "emhs", "rpa", "facsm", "mn", "adm", "scd", "psyd", "jcdjv", "fp", "msc", "bs", "mfa", "csc", "snd",
                 "pa-c", "nhs", "facs", "vpgeneral couns", "preside", "gen counsel", "pa", "vpcfo", "lmt", "ccvi", "pe", "scc", "rrt", "wvote",
                 "nea-", "ph", "brother", "senior", "facc", "msf", "rph", "facep", "dml", "employed", "member", "dsw", "-april", "april-dec", "facs",
                 "inactive", "dmsc", "jul-", "dec-jun", "-jun", "lnha", "hon", "cc", "joined", "mdboard", "mddirector", "nlh", "presceo", "svp",
                 "lega", "presidentceo", "sp", "of", "fsa", "dmin", "dhsc", "sphr", "csa", "hm", "lcda", "lcdo", "mrs", "ms", "mr", "faap", "sessions",
                 "corporate compliance", "wm", "cso", "thm", "osp", "mpf", "commissioner", "honorable", "mpd", "ne-", "- care", "ambulatory",
                 "treasasst", "secr", "administrative", "pd", "mdiv", "msgr", "dba", "msed", "ba", "sectreas", "sectrint", "prsceo", "mdphd", "ihm",
                 "sen", "facog", "caqsh", "mmm", "fccp", "fc", "msm", "legal", "offi", "fann", "svpcfo", "executive", "dpt", "foo", "network dev", 
                 "cfoasst", "pc-a", "bds", "cme", "pp", "psy d", "vice", "key employe", "senior", "phcns", "fmm", "presidenceo", "mrcp", "lohr",
                 "lmhc", "mppm", "lcsw", "rnc-ob", "fabc", "bcps", "dabr", "csjp", "business", "cfe", "chfp", "counselman", "cpcu", "key", "pbvm",
                 "pres", "directorvp", "chairpers", "cnl", "non-voting", "jul", "vpcfocoo", "financial", "till", "mpa", "gen'l", "counse", "msf",
                 "exec", "dirintrm", "cgma", "fhfma", "frmr", "reverend", "ed", "acm", "fa", "mlt", "cfa", "umms rep", "chairperson", "voting", 
                 "annual life", "jrdo", "ssm", "mdpres", "med", "fabf", "ncpsya", "fac", "pr", "llm", "mshcm", "- mar", "crmc", "cno", "mhcds", 
                 "facr", "senator", "finance", "mso", "rhsj", "mot otrl", "ocn", "lsw", "mmhc", "cfre", "msa", "cws", "dph", "mscprp", "dec-june",
                 "mdms", "mdret", "dpa", "rdh", "faia", "jwc", "chairman", "msmphrncph", "fmp", "physd", "phdcne", "- dec", "then", "clinical",
                 "lpd", "-sept", "-may", "june", "incoming", "elect", "macp", "- reg", "- treas", "strategy", "dmv", "membervp", "jr", "feb-dec", "-aug",
                 "- dec", "cnsl", "empl", "hcomp", "and", "admin", "dec-mar", "rtt", "may-dec", "svpchief", "cic", "mbamsnrn", "msrncnscenp",
                 "dm", "medical", "evp", "patient", "philanthropy", "clinic", "ahmc", "division", "-care div med th", "-care div fin", "-care div ops",
                 "-cvn clinics", "-care div", "chse", "dns", "care division", "- nov", "south meadows", "cmo","pmh", "mpp", "ahdl", "-dec", "cpmsm",
                 "hacp", "coo", "dnpapn", "anp", "pastor", "dnpmsnrn", "mgmt", "secrtreas", "jrv", "vpceo", "scch", "frcp", "ssj", "ncmp", "mscr", "dsf",
                 "mbr", "mspt", "jrdo", "clin", "coo", "svpchief", "sectreasurer", "and", "msha", "mphd", "apa", "ahp", "ee", "contracted", "phr",
                 "iii", "facg", "mdterm", "macp", "lcpc", "ccp", "rnc", "msob", "ceosecretary", "wchd", "cfosr", "cfovp", "cphrm", "cpps", "cooasst",
                 "treasto", "-directorjul-apr", "apr", "fmp", "ops", "scrn", "msnrn", "rnc", "fccm", "mdoff", "informa", "family", "orthopedic", 
                 "ppcme", "august", "partial", "lmh", "evp ballad health", "past", "tmh", "- aug", "evp", "lmhtrh", "chairperson", "treas", "nov", "dec",
                 "rtrtc", "dmo", "truste", "presiden", "fach", "fhf", "year", "prn", "execut", "february", "frm", "- mar", "ncr", "fdtn", "ipd", 
                 "aug", "nov", "feb", "only", "part yr", "care div", "-cur", "- mar", " - care", "- reg", "-c", "- fin", "-directorjul", "-july",
                 "acns", "mpah", "achce", "judge", "rabbi", "rc")

# Create name_cleaned variable
people_data <- people_data %>%
  mutate(name_cleaned = PersonNm)

# remove "see schedule o"
people_data <- people_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, " - see sch o"))

# Remove punctuation and digits from names
people_data <- people_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "\\.|,|'")) %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "[0-9]+")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# create indicator columns
create_indicator <- function(names_vector, title_list) {
  pattern <- paste(title_list, collapse = "|")  # Combine into regex pattern
  as.integer(grepl(pattern, names_vector, ignore.case = TRUE))
}

people_data$doctor <- create_indicator(people_data$PersonNm, doctor_list)
people_data$other_doctor <- create_indicator(people_data$PersonNm, other_doctor_list)
people_data$nurse <- create_indicator(people_data$PersonNm, nurse_list)
people_data$ha <- create_indicator(people_data$PersonNm, ha_list)

# remove anything occuring after the words "paid by" or in parentheses
people_data <- people_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, "paid by .+$")) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "\\(.+\\)"))

# remove any titles or extra from name_cleaned column
all_remove_list <- c(doctor_list, other_doctor_list, nurse_list, ha_list, remove_list)

people_data <- people_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, paste(all_remove_list, collapse = "\\b|\\b")))

# Remove dashes that happen at the end
people_data <- people_data %>%
  mutate(name_cleaned = str_squish(name_cleaned)) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "-$")) %>%
  mutate(name_cleaned = str_squish(name_cleaned)) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "-$")) 

# Remove "esq" occuring at the end
people_data <- people_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, "esq$"))

# remove weird lines
people_data <- people_data %>%
  filter(PersonNm!="non compensated trustees")

# # look at observations with a lot of words
# observe <- people_data %>%
#   mutate(name_cleaned = str_remove_all(name_cleaned, "\\b[a-z]\\b")) %>%
#   mutate(name_cleaned = str_squish(name_cleaned)) %>%
#   filter(str_count(name_cleaned, "\\s")>1) %>%
#   distinct(name_cleaned)
# write.csv(observe, paste0(created_data_path, "observe.csv"))

# GOAL 2: ASSIGN POSITIONS ####
# change titles to lower case
people_data <- people_data %>%
  mutate(TitleTxt = tolower(TitleTxt))

people_data <- people_data %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "ex officio|ex-officio|exofficio|ex-off"))

# remove rows containing "former"
people_data <- people_data %>%
  filter(!(str_detect(TitleTxt, "former|\\bfmr\\b|past|\\bfr\\b|\\bfrmr\\b|fomer|retire")))

# remove punctuation and "and"
people_data <- people_data %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "[[:punct:]]")) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "and"))

ceo_list <- c("ceo", "chief executive", "hospital pres")
cfo_list <- c("cfo", "chief fin", "senior finance")
coo_list <- c("coo", "chief operat")
cmo_list <- c("cmo", "chief med")
cno_list <- c("cno", "chief nurs")
other_chief_list <- c("c[a-su-z]o", "administrator", "chief", "exec", "cheif", "officer", "ofcr")
pres_list <- c("pres")
vp_list <- c("vp", "svp", "vice pres")
board_list <- c("director$", "board", "member", "trustee", "chai", "secretary", "sec", "treasurer", "treas", "trustte", "brd")



people_data <- people_data %>% 
  mutate(TitleTxt = str_squish(TitleTxt)) %>%
  mutate(position = ifelse(str_detect(TitleTxt, paste(ceo_list, collapse="|")), "ceo", NA)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(cfo_list, collapse="|")), "cfo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(coo_list, collapse="|")), "coo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(cmo_list, collapse="|")), "cmo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(cno_list, collapse="|")), "cno", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(other_chief_list, collapse="|")), "other_chief", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(vp_list, collapse="|")), "vp", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(pres_list, collapse="|")), "president", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, paste(board_list, collapse="|")), "board", position))

# look for the important titles in the name column
people_data <- people_data %>% 
  mutate(position = ifelse(str_detect(PersonNm, paste(ceo_list, collapse="|")), "ceo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(cfo_list, collapse="|")), "cfo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(coo_list, collapse="|")), "coo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(cmo_list, collapse="|")), "cmo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(cno_list, collapse="|")), "cno", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(other_chief_list, collapse="|")), "other_chief", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(vp_list, collapse="|")), "vp", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(pres_list, collapse="|")), "president", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(PersonNm, paste(board_list, collapse="|")), "board", position))

# Remove anyone that doesn't have a position (these are nurses and physicians left)
people_data <- people_data %>%
  filter(!is.na(position))

# take random sample of 100 obs to check
sample <- people_data %>%
  sample_n(size=100, replace=FALSE)
  # looks good!

# create non-voting indicator
people_data <- people_data %>%
  mutate(nonvoting = ifelse(str_detect(TitleTxt, "nonvoting|non-voting|nonvting|non-vting|nonvtng|non-vtng"),1,0)) %>%
  mutate(nonvoting = ifelse(str_detect(PersonNm, "nonvoting|non-voting|nonvting|non-vting|nonvtng|non-vtng"),1,nonvoting))

people_data <- people_data %>%
  select(-PersonNm, -TitleTxt) %>%
  distinct()
# 216k observations

# GOAL 3: GET MALE/FEMALE OF EACH NAME #########
firstnames <- read_csv(paste0(raw_data_path, "/yob2000.txt"), col_names = FALSE) %>% 
  filter(X3>50) %>%
  select(X1) %>%
  rename(name=X1) %>%
  mutate(name=tolower(name))

firstnames_list <- as.list(firstnames)[["name"]]

# format names 
people_data <- people_data %>%
  mutate(name_cleaned = str_trim(name_cleaned)) %>%
  mutate(formatted_name = str_remove_all(name_cleaned, "\\b[a-z]\\b")) %>%
  mutate(formatted_name = str_squish(formatted_name))

# drop people with only one name
people_data <- people_data %>%
  filter(str_count(formatted_name, "\\s")>0)

# next, split phrase up into individual words
people_data <- people_data %>%
  tidyr::separate_wider_delim(formatted_name, delim=" ", names_sep = ".", too_few = "align_start")

# count up number of times first names occur in the first word of name_cleaned in a firm_year
people_data <- people_data %>%
  mutate(first_count = ifelse(formatted_name.1 %in% firstnames_list,1,0)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(total_names = n()) %>%
  mutate(first_first = sum(first_count)) %>%
  ungroup()


# If the majority of names have a first name located first, record that as the first name
people_data <- people_data %>%
  mutate(first_name = ifelse(first_first/total_names>.5, formatted_name.1, NA))

# if there are only two names and firstname is still missing, fill it with the second name
people_data <- people_data %>%
  mutate(first_name = ifelse(is.na(first_name) & is.na(formatted_name.3), formatted_name.2, first_name))

# Now do the same but looking at the third name
people_data <- people_data %>%
  mutate(third_count = ifelse(formatted_name.3 %in% firstnames_list,1,0)) %>%
  mutate(first_name = ifelse(is.na(first_name) & third_count>0, formatted_name.3, first_name))

# Now do the same but looking at the second name
people_data <- people_data %>%
  mutate(second_count = ifelse(formatted_name.2 %in% firstnames_list,1,0)) %>%
  mutate(first_name = ifelse(is.na(first_name) & second_count>0, formatted_name.2, first_name))

# Now do the same but looking at the fourth name
people_data <- people_data %>%
  mutate(fourth_count = ifelse(formatted_name.4 %in% firstnames_list,1,0)) %>%
  mutate(first_name = ifelse(is.na(first_name) & fourth_count>0, formatted_name.4, first_name))

# for the rest (less than 100), just set the first name as first name
people_data <- people_data %>%
  mutate(first_name = ifelse(is.na(first_name), formatted_name.1, first_name))

# get rid of unneccesary variables
people_data <- people_data %>%
  distinct(TaxYr, Filer.EIN, name_cleaned, first_name, position, doctor, nurse, other_doctor, ha, nonvoting)

# Use "gender" package to predict gender of each name
genders <- people_data %>%
  distinct(first_name) %>%
  rowwise() %>%
  do(results = gender(.$first_name, years = c(1960,2000), method="ssa")) %>%
  mutate(n = nrow(results)) %>%
  filter(n>0) %>%
  do(bind_rows(.$results)) %>%
  select(-year_min, -year_max)

# join back to original data
people_data <- people_data %>%
  left_join(genders, by=c("first_name"="name"))
  # only 2% of observations are missing gender

# save data
saveRDS(people_data, paste0(created_data_path, "cleaned_people_data.rds"))
write.csv(people_data, paste0(created_data_path, "cleaned_people_data.csv"))






