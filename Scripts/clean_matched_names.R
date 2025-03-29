library(readr)
library(stringr)
library(dplyr)

created_data_path <- "CreatedData/"

# Script to clean up the name columns of people data scraped from 990s

all_people_data <- readRDS(paste0(created_data_path, "/all_people_data_scheduleH.rds"))
cw <- readRDS(paste0(created_data_path, "/updated_ein_aha_cw.rds"))

cw <- cw %>%
  filter(!is.na(ID))

# only keep observations where EIN is found in either ein_hosp or ein_sys
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

# First goal is to clean up the names. ####

# create data set with only relevant variables
people_data <- all_people_data %>%
  select(TaxYr, Filer.EIN, PersonNm, TitleTxt)

# convert to all lowercase
people_data <- people_data %>%
  mutate(PersonNm = tolower(PersonNm),
         TitleTxt = tolower(TitleTxt))

# create a column for md
people_data <- people_data %>%
  mutate(md = ifelse(str_detect(PersonNm, "\\smd$|\\smd\\s"),1,0)) %>%
  mutate(md = ifelse(str_detect(TitleTxt, "\\smd$|\\smd\\s"),1,md))

# create column for DO
people_data <- people_data %>%
  mutate(do = ifelse(str_detect(PersonNm, "\\sdo$|\\sdo\\s"),1,0)) %>%
  mutate(do = ifelse(str_detect(TitleTxt, "\\sdo$|\\sdo\\s"),1,do))

# column for other doctor titles
people_data <- people_data %>%
  mutate(other_doctor = ifelse(str_detect(PersonNm, "\\smbbch\\s|\\smbbs$|\\bdc\\b|\\bdds\\b|dentist"),1,0)) %>%
  mutate(other_doctor = ifelse(str_detect(TitleTxt, "\\smbbch\\s|\\smbbs$|\\bdc\\b|\\bdds\\b|dentist"),1,other_doctor))

# create column for "RN"
people_data <- people_data %>%
  mutate(nurse = ifelse(str_detect(PersonNm, "\\srn$|\\sdnp$|\\aprn$|\\bcrna$"),1,0)) %>%
  mutate(nurse = ifelse(str_detect(TitleTxt, "rn|dnp|aprn|nurse practitioner|fnp|nurse|crna|anp|\\bnp\\b|\\ip\\b|registered n|\\bapn\\b|nursing|\\blpn\\b"),1,nurse))

# create column for "mha"
people_data <- people_data %>%
  mutate(mha = ifelse(str_detect(PersonNm, "\\smha$|\\smha\\s"),1,0)) %>%
  mutate(mha = ifelse(str_detect(TitleTxt, "\\smha$|\\smha\\s"),1,mha))

observe <- people_data %>%
  filter(str_detect(name_cleaned, "-"))

# remove "see schedule o"
people_data <- people_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, " - see sch o"))

# Remove punctuation and digits from names
people_data <- people_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "[[:punct:]]")) %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "[0-9]+"))

# remove common doctor titles and anything that comes after them in the string
titles <- c("md", "do", "od", "rn", "mbbch", "mbbs", "dnp", "until", "as of", "mha", "eff", "end", "beg", "thru", "through", "osb",
            "ending", "term", "mph", "deceased", "to", "begin", "left", "see statement", "director", "chair", "hired", "termed",
            "began", "retired", "from", "resigned", "off", "vp", "ended", "started", "trm", "dir of", "pd by", "pharmd", "provost",
            "start", "oct", "treasurer", "jan", "st", "chief", "t0", "entered", "interim", "board", "crna", "fmr", "int", "trustee",
            "vice president", "dir", "bh", "vppres", "msn", "mst", "assoc", "departed", "msw", "ceo", "cbe", "secretary", "rsmterm",
            "effective", "effec", "former", "admininstrator", "consultant", "cnm", "fellow", "beginning", "president")
# remove anything that comes after these titles
people_data <- people_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, paste0("\\b", titles, "\\b", ".*", collapse="|")))

# remove common titles
titles <- c("dr", "bishop", "esq", "jr", "sr", "md", "phd", "dds", "ii", "iii", "iv", "ms", "mr", "do","most rev", "rev", "rn", "edd", "cpa", "od",
            "the rt", "dnp", "bc", "faan", "mb", "chb", "mbbh", "mbbch", "chcio", "rsm", "part year", "family practice", "major general",
            "sister", "mha", "magistrate", "- see sch o", "see sch o", "sch o", "dc","the very rev", "the most reverend", "the honorable", "the hon",
            "general", "vice pres", "fr", "mba", "jd", "ma", "bsn", "cco", "chco", "mpas", "pac", "dsc", "cbs", "chs", "ed d",
            "rd", "bsbn", "mshrm", "cpm", "ccim", "ecc", "home care", "st john", "ph d", "csj", "non voting", "ex officio",
            "dmd", "ret maj gen", "sc", "pharm d", "int ceo", "msn", "osf", "mother", "rtscra", "dha", "dh", "evpcao", "evpcoo",
            "dha", "family medicine", "mshr", "aprn", "cssf", "offcr", "cfo", "chcp", "ccep")
people_data <- people_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, paste0("\\b", titles, "\\b", collapse = "|")))

# Trim extra spaces
people_data <- people_data %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# remove middle initials
#people_data <- people_data %>%
#mutate(name_cleaned = str_replace_all(name_cleaned, "\\s[a-z]\\s", "\\s"))

# get rid of initials at the beginning of the name
#people_data <- people_data %>%
#mutate(name_cleaned = str_remove(name_cleaned, "^[a-z]\\s"))

# goal 2: clean up TitleTxt column ####

# change titles to lower case
people_data <- people_data %>%
  select(-PersonNm) %>%
  mutate(TitleTxt = tolower(TitleTxt))

# create data set of unique titles
unique_titles <- people_data %>%
  distinct(TitleTxt)

# create column for ex-officio
people_data <- people_data %>%
  mutate(ex_officio = ifelse(str_detect(TitleTxt, "ex officio|ex-officio|exofficio|ex-off"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "ex officio|ex-officio|exofficio|ex-off"))

# remove rows containing "former"
people_data <- people_data %>%
  filter(!(str_detect(TitleTxt, "former|\\bfmr\\b|past|\\bfr\\b")))

# remove punctuation and "and"
people_data <- people_data %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "[[:punct:]]")) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "and"))


# Create column for CEO
people_data <- people_data %>%
  mutate(ceo = ifelse(str_detect(TitleTxt, "ceo|chief executive officer|hospital pre|hospital president"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "ceo|chief executive officer|hospital pre|hospital president")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for cfo
people_data <- people_data %>%
  mutate(cfo = ifelse(str_detect(TitleTxt, "cfo|chief financial officer|senior finance"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "cfo|chief financial officer|senior finance")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for cqo
people_data <- people_data %>%
  mutate(cqo = ifelse(str_detect(TitleTxt, "cqo|chief quality officer"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "cqo|chief quality officer")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for cmo
people_data <- people_data %>%
  mutate(cmo = ifelse(str_detect(TitleTxt, "cmo|chief medical officer|chief medical|chief medica"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "cmo|chief medical officer|chief medical|chief medica")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for board member
people_data <- people_data %>%
  mutate(board_member = ifelse(str_detect(TitleTxt, "board|trustee|director|member|secretary|diretor|treasurer|treas|bd mem|trustte|asst sec|Trtu|\\bmem\\b|\\btr\\b|asec|\\bsec\\b|at large|bd mbr"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "board|trustee|director|member|secretary|diretor|treasurer|treas|bd mem|trustte|asst sec|Trtu|\\bmem\\b|\\btr\\b|asec|\\bsec\\b|at large|bd mbr")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# repeat board member but in the name column
people_data <- people_data %>%
  mutate(board_member = ifelse(str_detect(name_cleaned, "board|trustee|director|member|secretary|diretor|treasurer|treas|bd mem|trustte|asst sec|Trtu|\\bmem\\b|\\btr\\b|asec|\\bsec\\b|at large|trea|turstee"),1,board_member)) %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "board|trustee|director|member|secretary|diretor|treasurer|treas|bd mem|trustte|asst sec|Trtu|\\bmem\\b|\\btr\\b|asec|\\bsec\\b|at large|trea|turstee"))

# create column for president
people_data <- people_data %>%
  mutate(president = ifelse(str_detect(TitleTxt, "president|pres"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "president|pres")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for vice chairman
people_data <- people_data %>%
  mutate(vice_chairman = ifelse(str_detect(TitleTxt, "vice c|vice chairman|vice chair|vchm|vice chairperson|vice chr|vice-chairpe|vicechairman|vicechairwoman|vice cha|vicechair|vicechairma|vice c|\\bvc\\b"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "vice c|vice chairman|vice chair|vchm|vice chairperson|vice chr|vice-chairpe|vicechairman|vicechairwoman|vice cha|vicechair|vicechairma|vice c|\\bvc\\b"), TitleTxt) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for chair
people_data <- people_data %>%
  mutate(chair = ifelse(str_detect(TitleTxt, "chair|chm|chairperson|cha|chairma|chairwoma|\\bch\\b"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chair|chm|chairperson|cha|chairma|chairwoma")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for physcian
# create list of physicians
physician_titles <- c("physician", "phys", "dr", "doctor", "surgeon", "obsgyn", "obgyn", "orthopedic", "neuro", "radiation onc", "md", "do", "psychiatrist", 
                      "anesthesiologist", "pathologist", "radiology", "radiologist", "\\bpa\\b", "urology", "urologist", "hospitalist", "emergency medicine", "\\bpt\\b",
                      "pulmonologist", "gastroenterologist", "podiatrist", "anesthesiaologist", "otolaryngologist", "neonatologist", "intensivist", "nephrology",
                      "provider", "orthopedist", "anesthetist", "gastroenterology", "radiation", "\\bpac\\b", "perfusionist",
                      "\\bent\\b", "paramedic", "family pract", "ortho", "surgery", "pharmac", "\\bgyn\\b", "attending", "gastrology",
                      "\\bapp\\b", "ophthalmolog", "ophthalmology", "pediatrics", "anesthesia", "hematologist", "pulmonary")

people_data <- people_data %>%
  mutate(physician = ifelse(str_detect(TitleTxt, paste0(physician_titles, collapse = "|")),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, paste0(physician_titles, collapse = "|"))) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for pharmacist
people_data <- people_data %>%
  mutate(pharmacist = ifelse(str_detect(TitleTxt, "pharmacist|pharmd|pharmacy"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "pharmacist|pharmd|pharmacy")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for chief operating officer
people_data <- people_data %>%
  mutate(coo = ifelse(str_detect(TitleTxt, "chief operating officer|coo|chief operat|chief operating fi"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chief operating officer|coo|chief operat|chief operating fi")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# chief of specific doctor things
people_data <- people_data %>%
  mutate(chief = ifelse(str_detect(TitleTxt, "chief of vascular surgery|chief vascular surgery"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chief of vascular surgery|chief vascular surgery")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for chief nursing officer
people_data <- people_data %>%
  mutate(cno = ifelse(str_detect(TitleTxt, "chief nursing officer|cno|chief nursing ficer|chief nursin"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chief nursing officer|cno|chief nursing ficer|chief nursin")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# creat column for fache
people_data <- people_data %>%
  mutate(fache = ifelse(str_detect(TitleTxt, "fache"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "fache")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create chief staff column
people_data <- people_data %>%
  mutate(chief_staff = ifelse(str_detect(TitleTxt, "chief of staff|chief staff"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chief of staff|chief staff")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# remove random words
people_data <- people_data %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "\\bman\\b|person|assistant|outgoing|sr|1st|2nd|\\bof\\b")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create "division chief" column
people_data <- people_data %>%
  mutate(division_chief = ifelse(str_detect(TitleTxt, "division chief|div chief"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "division chief|div chief")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for vp
people_data <- people_data %>%
  mutate(vp = ifelse(str_detect(TitleTxt, "vp|vice president|vppres|hosp ops|evc"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "vp.*|vice president.*|vppres.*|hosp ops|evc")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create committee community column
people_data <- people_data %>%
  mutate(committee_community = ifelse(str_detect(TitleTxt, "committee community|committee comm"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "committee community|committee comm")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for dean
people_data <- people_data %>%
  mutate(dean = ifelse(str_detect(TitleTxt, "dean|chief academic officer|provost"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "dean|chief academic officer|provost")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# remove single letters
people_data <- people_data %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "\\b[a-z]\\b")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for administrator
people_data <- people_data %>%
  mutate(administrator = ifelse(str_detect(TitleTxt, "administrator|admin|administration|adm|office manag"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "administrator|admin|administration|adm|office manag")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for other chief officer/executive director
people_data <- people_data %>%
  mutate(other_chief_officer = ifelse(str_detect(TitleTxt, "chief|officer|c[a-z]{2}|exec|\\bed\\b|senior leader"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "chief|officer|c[a-z]{2}|exec|\\bed\\b|senior leader")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# create column for program director
people_data <- people_data %>%
  mutate(program_director = ifelse(str_detect(TitleTxt, "program director|program dir|dir|mgr|supervisor|ager|head|\\bsupe\\b|supv|mgmt|manager|manag"),1,0)) %>%
  mutate(TitleTxt = str_remove_all(TitleTxt, "program director|program dir|dir|mgr|supervisor|ager|head|\\bsupe\\b|supv|mgmt|manager|manag")) %>%
  mutate(TitleTxt = str_squish(TitleTxt))

# look at observations where TitleTxt is not empty and position columns are all zero
# create list of all position columns created
positions <- c("ceo", "vp", "cfo", "cqo", "cmo", "board_member", "president", "vice_chairman", "chair", "physician", "pharmacist", "coo", "chief", "cno", 
               "fache", "chief_staff", "division_chief", "nurse", "committee_community", "md", "other_doctor", "do", "mha", "administrator", "other_chief_officer", 
               "program_director", "ex_officio")

observe <- people_data %>%
  filter(TitleTxt != "" & rowSums(select(., all_of(positions))) == 0)



# combine positions if there are several rows with the same name, ein, and year
people_data <- people_data %>%
  group_by(TaxYr, Filer.EIN, name_cleaned) %>%
  summarise(ceo = max(ceo),
            vp = max(vp),
            cfo = max(cfo),
            cqo = max(cqo),
            cmo = max(cmo),
            board_member = max(board_member),
            president = max(president),
            vice_chairman = max(vice_chairman),
            chair = max(chair),
            physician = max(physician),
            pharmacist = max(pharmacist),
            coo = max(coo),
            chief = max(chief),
            cno = max(cno),
            fache = max(fache),
            chief_staff = max(chief_staff),
            division_chief = max(division_chief),
            nurse = max(nurse),
            committee_community = max(committee_community),
            md = max(md),
            other_doctor = max(other_doctor),
            do = max(do),
            mha = max(mha),
            administrator = max(administrator),
            other_chief_officer = max(other_chief_officer),
            program_director = max(program_director),
            ex_officio = max(ex_officio)) %>%
  ungroup()


# save this version of the data
saveRDS(people_data, paste0(created_data_path, "/cleaned_people_data2.rds"))













