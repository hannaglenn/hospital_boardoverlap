
# ============================================================
#  IRS 990 XML -> Firm-level + People (name + title only)
#  Schema-agnostic XML + robust text fallback
#  Foreign address handling + Option A filtering
#  Parallel processing (furrr) + diagnostics
#  Recursively scans nested folders under nonprofit_xmls
# ============================================================

# ---------------------------
# CONFIG
# ---------------------------
INPUT_DIR      <- "./data/nonprofit_xml"       # top-level folder with nested subfolders
FIRMS_OUT_CSV  <- "firms_990.csv"
PEOPLE_OUT_CSV <- "people_990.csv"       # ONLY person_name + title (+ ein, year keys)
DIAG_OUT_CSV   <- "parse_diagnostics.csv"
ERRORS_OUT_CSV <- "parse_errors.csv"

WORKERS <- max(1L, parallel::detectCores() - 1L)

# ---------------------------
# Packages
# ---------------------------
# install.packages(c("xml2","dplyr","purrr","stringr","tidyr","janitor","readr","furrr","future"))
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(janitor)
library(readr)
library(furrr)
library(future)

# ---------------------------
# Helpers
# ---------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

xml_text_or_na <- function(doc_or_node, xpath) {
  node <- xml2::xml_find_first(doc_or_node, xpath)
  if (inherits(node, "xml_missing")) return(NA_character_)
  val <- xml2::xml_text(node)
  if (length(val) == 0) NA_character_ else stringr::str_squish(val)
}

find_all <- function(doc_or_node, xpath) xml2::xml_find_all(doc_or_node, xpath)
strip_ns <- function(doc) { xml2::xml_ns_strip(doc); doc }

# Normalize raw text before regex/line parsing
read_clean_text <- function(path) {
  txt <- tryCatch(readr::read_file(path), error = function(e) NA_character_)
  if (is.na(txt)) return(NA_character_)
  txt <- stringr::str_replace_all(txt, "\r", "")
  txt <- stringr::str_replace_all(txt, "[\u00A0\\t]+", " ")
  txt
}

# ---------- Text fallbacks (firm) ----------
guess_org_from_text <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(NA_character_)
  lines <- stringr::str_split(txt, "\n", simplify = FALSE)[[1]] |> stringr::str_squish()
  cand <- lines[stringr::str_detect(lines, "^[A-Z0-9&'\\.\\-/ ]+$")]
  cand <- cand[!stringr::str_detect(cand, "^WWW\\.|HTTPS?://|^PO BOX|^C/O|^SUITE$|^[A-Z]{2}$|^\\d{5}(-?\\d{4})?$")]
  cand <- cand[!stringr::str_detect(cand, "^\\d+$")]
  pref <- cand[stringr::str_detect(cand, "HOSPITAL|MEDICAL|CENTER|HEALTH|FOUNDATION|COLLEGE|INSTITUTE|UNIVERSITY|ASSOCIATION|CLINIC")]
  if (length(pref) > 0) return(pref[1])
  if (length(cand) > 0)  return(cand[1])
  NA_character_
}

guess_addr_from_text <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(list(city=NA_character_, state=NA_character_, zip=NA_character_))
  lines <- stringr::str_split(txt, "\n", simplify = FALSE)[[1]] |> stringr::str_squish()
  states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA",
              "MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD",
              "TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")
  for (i in seq_len(length(lines) - 2)) {
    if (stringr::str_detect(lines[i], "^[A-Z][A-Z]+(?:[ -][A-Z]+)*$") &&
        lines[i+1] %in% states &&
        stringr::str_detect(lines[i+2], "^\\d{5}(?:-?\\d{4})?$")) {
      return(list(city = lines[i], state = lines[i+1],
                  zip = stringr::str_extract(lines[i+2], "\\d{5}(?:-?\\d{4})?")))
    }
  }
  for (ln in lines) {
    m <- stringr::str_match(ln, "^([A-Z][A-Z]+(?:[ -][A-Z]+)*)\\s*,?\\s*([A-Z]{2})\\s+(\\d{5}(?:-?\\d{4})?)$")
    if (!is.na(m[1])) return(list(city = m[2], state = m[3], zip = m[4]))
  }
  list(city = NA_character_, state = NA_character_, zip = NA_character_)
}

guess_ein_from_text <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(NA_character_)
  lines <- stringr::str_split(txt, "\n", simplify = FALSE)[[1]] |> stringr::str_squish()
  org_line <- guess_org_from_text(txt)
  org_idx  <- if (!is.na(org_line)) {
    idx <- which(lines == org_line)
    if (length(idx) > 0) idx[1] else NA_integer_
  } else NA_integer_
  if (!is.na(org_idx) && org_idx > 1) {
    maybe_ein <- lines[org_idx - 1]
    if (stringr::str_detect(maybe_ein, "^[0-9]{9}$")) return(maybe_ein)
  }
  limit <- min(length(lines), 80L)
  for (i in seq_len(limit)) {
    ln <- lines[i]
    if (stringr::str_detect(ln, "^[0-9]{9}$")) {
      next_line <- if (i < length(lines)) lines[i + 1] else ""
      prev_line <- if (i > 1) lines[i - 1] else ""
      if (stringr::str_detect(next_line, "HOSPITAL|MEDICAL|CENTER|HEALTH|FOUNDATION|COLLEGE|INSTITUTE|UNIVERSITY|ASSOCIATION|CLINIC")) {
        return(ln)
      }
      if (!(stringr::str_detect(prev_line, "^[A-Z]{2}$") ||
            stringr::str_detect(next_line, "^\\d{5}(?:-?\\d{4})?$"))) {
        return(ln)
      }
    }
  }
  m <- stringr::str_match(txt, "(?s)(?<!\\d)(\\d{9})(?!\\d)")
  if (!is.null(m) && length(m) >= 2 && !is.na(m[2])) return(m[2])
  NA_character_
}

guess_year_from_text <- function(txt) {
  if (is.null(txt) || length(txt) == 0 || is.na(txt)) return(NA_integer_)
  m1 <- stringr::str_match(txt, "(?s)(\\d{4})-\\d{2}-\\d{2}")
  if (!is.null(m1) && length(m1) >= 2 && !is.na(m1[2])) {
    y <- suppressWarnings(as.integer(m1[2])); if (!is.na(y)) return(y)
  }
  m2 <- stringr::str_match(txt, "(?is)tax\\s*(yr|year|period).*?(19\\d{2}|20\\d{2})")
  if (!is.null(m2) && length(m2) >= 3 && !is.na(m2[3])) {
    y <- suppressWarnings(as.integer(m2[3])); if (!is.na(y)) return(y)
  }
  y3 <- stringr::str_extract(txt, "\\b(20\\d{2})\\b")
  if (!is.na(y3)) { y <- suppressWarnings(as.integer(y3)); if (!is.na(y)) return(y) }
  y4 <- stringr::str_extract(txt, "\\b(19\\d{2})\\b")
  if (!is.na(y4)) { y <- suppressWarnings(as.integer(y4)); if (!is.na(y)) return(y) }
  NA_integer_
}

# Strong header-block extractor (EIN -> ORG -> CITY/ST/ZIP)
extract_header_block_from_text <- function(txt) {
  if (is.null(txt) || is.na(txt)) return(list(ein=NA, org=NA, city=NA, state=NA, zip=NA))
  patA <- paste0(
    "(?m)^(\\d{9})\\s*\\n",                                     # EIN
    "([A-Z0-9&'\\.\\-/ ]{3,})\\s*\\n",                           # ORG (uppercase)
    "(?:.*\\n){0,3}?",                                           # up to 3 lines of street/address
    "([A-Z][A-Z]+(?:[ -][A-Z]+)*)\\s*\\n",                       # CITY
    "([A-Z]{2})\\s*\\n",                                         # STATE
    "(\\d{5}(?:-?\\d{4})?)"                                      # ZIP or ZIP+4
  )
  mA <- stringr::str_match(txt, patA)
  
  patB <- paste0(
    "(?m)^(\\d{9})\\s*\\n",                                     # EIN
    "([A-Z0-9&'\\.\\-/ ]{3,})\\s*\\n",                           # ORG
    "(?:.*\\n){0,6}?",                                           # up to 6 lines
    "([A-Z][A-Z]+(?:[ -][A-Z]+)*)\\s*,\\s*([A-Z]{2})\\s*(\\d{5}(?:-?\\d{4})?)" # CITY, ST ZIP
  )
  mB <- stringr::str_match(txt, patB)
  
  if (!is.null(mA) && length(mA) >= 6 && !is.na(mA[2])) {
    return(list(ein = mA[2], org = stringr::str_squish(mA[3]),
                city = mA[4], state = mA[5], zip = mA[6]))
  }
  if (!is.null(mB) && length(mB) >= 6 && !is.na(mB[2])) {
    return(list(ein = mB[2], org = stringr::str_squish(mB[3]),
                city = mB[4], state = mB[5], zip = mB[6]))
  }
  return(list(
    ein  = guess_ein_from_text(txt),
    org  = guess_org_from_text(txt),
    city = guess_addr_from_text(txt)$city,
    state= guess_addr_from_text(txt)$state,
    zip  = guess_addr_from_text(txt)$zip
  ))
}

# ---------- People fallback (text) ----------
people_titles_regex <- "(?i)^(CHAIR|VICE\\s*CHAIR|VICE\\s*CHAIRMA|SECRETARY|TREASURER|TRUSTEE|DIRECTOR|PRESIDENT(?:\\b|\\s*[-/]\\s*.*)?|CEO\\b|CFO\\b|COO\\b|PHYSICIAN(?:\\b|\\s*.*)|PHARMACIST|DIVISION\\s*CHIEF|SYSTEM\\s*EXECUTIVE|CEO\\s*LCH|PRESIDENT\\s*-\\s*CMMC)$"

looks_like_name <- function(line) {
  stringr::str_detect(line, "^[A-Z][A-Z'\\-\\. ]+[A-Z]$") &&
    !stringr::str_detect(line, "(?i)HOSPITAL|CENTER|HEALTH|MEDICAL|COLLEGE|INSTITUTE|UNIVERSITY|FOUNDATION|INC|CORP|DBA|GROUP|VILLAGE|SCHEDULE|FORM|PART|LINE")
}

extract_people_from_text <- function(txt, ein, year) {
  if (is.null(txt) || is.na(txt)) return(tibble())
  lines <- stringr::str_split(txt, "\n", simplify = FALSE)[[1]] |> stringr::str_squish()
  n <- length(lines)
  out <- list()
  i <- 1
  while (i <= n) {
    ln <- lines[i]
    if (looks_like_name(ln)) {
      jmax <- min(n, i + 3)
      found <- FALSE
      for (j in (i+1):jmax) {
        tl <- lines[j]
        if (stringr::str_detect(tl, people_titles_regex)) {
          out[[length(out)+1]] <- tibble(ein = ein, year = year, person_name = ln, title = tl)
          i <- j + 1; found <- TRUE; break
        }
      }
      if (!found) i <- i + 1
    } else {
      i <- i + 1
    }
  }
  if (length(out) == 0) return(tibble())
  dplyr::bind_rows(out) %>% dplyr::distinct() %>% janitor::clean_names()
}

# ---------------------------
# Firm-level parser (schema-agnostic XML + Foreign + TEXT fallback)
# ---------------------------

parse_firm <- function(doc, path = NULL) {
  doc <- strip_ns(doc)
  
  # EIN
  ein <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='Filer']//*[local-name()='EIN']")
  
  # Org name (Line 1 & Line 2) with older/newer tag names and <Name> wrapper
  name1 <- xml_text_or_na(doc,
                          paste0(".//ReturnHeader//*[local-name()='Filer']",
                                 "//*[local-name()='BusinessNameLine1' or local-name()='BusinessNameLine1Txt']"))
  if (is.na(name1) || name1 == "") {
    name1 <- xml_text_or_na(doc,
                            paste0(".//ReturnHeader//*[local-name()='Filer']//*[local-name()='Name']",
                                   "//*[local-name()='BusinessNameLine1' or local-name()='BusinessNameLine1Txt']"))
  }
  
  name2 <- xml_text_or_na(doc,
                          paste0(".//ReturnHeader//*[local-name()='Filer']",
                                 "//*[local-name()='BusinessNameLine2' or local-name()='BusinessNameLine2Txt']"))
  if (is.na(name2) || name2 == "") {
    name2 <- xml_text_or_na(doc,
                            paste0(".//ReturnHeader//*[local-name()='Filer']//*[local-name()='Name']",
                                   "//*[local-name()='BusinessNameLine2' or local-name()='BusinessNameLine2Txt']"))
  }
  
  # ----- Address: US and Foreign variants -----
  us_base      <- ".//ReturnHeader//*[local-name()='Filer']//*[local-name()='USAddress' or local-name()='AddressUS']"
  foreign_base <- ".//ReturnHeader//*[local-name()='Filer']//*[local-name()='ForeignAddress']"
  
  has_foreign <- length(find_all(doc, foreign_base)) > 0
  has_us      <- length(find_all(doc, us_base))      > 0
  
  if (has_foreign) {
    city     <- xml_text_or_na(doc, paste0(foreign_base, "/*[local-name()='CityNm' or local-name()='City']"))
    state    <- xml_text_or_na(doc, paste0(foreign_base, "/*[local-name()='ProvinceOrStateNm' or local-name()='State']"))
    zip      <- xml_text_or_na(doc, paste0(foreign_base, "/*[local-name()='ForeignPostalCd' or local-name()='PostalCode']"))
    country  <- xml_text_or_na(doc, paste0(foreign_base, "/*[local-name()='CountryCd' or local-name()='Country']"))
  } else if (has_us) {
    city     <- xml_text_or_na(doc, paste0(us_base, "/*[local-name()='City' or local-name()='CityNm']"))
    state    <- xml_text_or_na(doc, paste0(us_base, "/*[local-name()='State' or local-name()='StateAbbreviationCd']"))
    zip      <- xml_text_or_na(doc, paste0(us_base, "/*[local-name()='ZIPCode' or local-name()='ZIPCd']"))
    country  <- "US"
  } else {
    city <- state <- zip <- country <- NA_character_
  }
  
  # Year (TaxYr or derive from End/Begin)
  tax_yr <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='TaxYr']")
  end_dt <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='TaxPeriodEndDate' or local-name()='TaxPeriodEndDt']")
  beg_dt <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='TaxPeriodBeginDate' or local-name()='TaxPeriodBeginDt']")
  if (is.na(tax_yr) || nchar(tax_yr) < 4) {
    end_dt <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='TaxPeriodEndDate' or local-name()='TaxPeriodEndDt']")
    if (!is.na(end_dt) && grepl("^\\d{4}-\\d{2}-\\d{2}$", end_dt)) tax_yr <- substr(end_dt, 1, 4)
    if (is.na(tax_yr) || nchar(tax_yr) < 4) {
      beg_dt <- xml_text_or_na(doc, ".//ReturnHeader//*[local-name()='TaxPeriodBeginDate' or local-name()='TaxPeriodBeginDt']")
      if (!is.na(beg_dt) && grepl("^\\d{4}-\\d{2}-\\d{2}$", beg_dt)) tax_yr <- substr(beg_dt, 1, 4)
    }
  }
  
  # Normalize ZIP+4 with no dash (US only)
  if (!is.na(zip) && stringr::str_detect(zip, "^\\d{9}$") && (toupper(country) %in% c("US", "USA"))) {
    zip <- paste0(substr(zip, 1, 5), "-", substr(zip, 6, 9))
  }
  
  tibble::tibble(
    ein      = ein,
    org_name = stringr::str_squish(paste(name1 %||% "", name2 %||% "")),
    city     = city,
    state    = state,   # may be NA for ForeignAddress
    zip      = zip,     # may be ForeignPostalCd for foreign addresses
    country  = country, # "US" or IRS country code (e.g., "RQ" for Puerto Rico)
    year     = if (!is.na(tax_yr)) as.integer(tax_yr) else NA_integer_,
    beg_dt = beg_dt,
    end_dt=end_dt
  ) %>% janitor::clean_names()
}

validate_firm <- function(firm_row, file) {
  # Minimal location completeness rules:
  has_us_loc  <- (!is.na(firm_row$city) && !is.na(firm_row$state) && !is.na(firm_row$zip) &&
                    toupper(firm_row$country %||% "") %in% c("US","USA"))
  has_for_loc <- (!is.na(firm_row$city) && !is.na(firm_row$zip) &&
                    !is.na(firm_row$country) && !(toupper(firm_row$country) %in% c("US","USA")))
  
  tibble(
    file                 = file,
    ein_missing          = is.na(firm_row$ein) || firm_row$ein == "",
    org_name_missing     = is.na(firm_row$org_name) || firm_row$org_name == "",
    location_missing     = !(has_us_loc || has_for_loc),
    year_missing         = is.na(firm_row$year),
    firm_needs_review    = ein_missing | org_name_missing | location_missing | year_missing
  )
}

# ---------------------------
# People-level parser (schema-agnostic XML, then TEXT fallback)
# ---------------------------
extract_people_from_nodes_xml <- function(doc, nodes, ein, year) {
  purrr::map_dfr(nodes, function(n) {
    # Try explicit person fields first
    person <- xml_text_or_na(n, ".//*[local-name()='NamePerson' or local-name()='PersonName' or local-name()='PersonNm'][1]")
    # If missing, use BusinessName lines as the person's name
    if (is.na(person) || person == "") {
      person <- xml_text_or_na(n, ".//*[local-name()='BusinessName']/*[local-name()='BusinessNameLine1' or local-name()='BusinessNameLine1Txt'][1]")
      if (is.na(person) || person == "") {
        person <- xml_text_or_na(n, ".//*[local-name()='BusinessName']/*[local-name()='BusinessNameLine2' or local-name()='BusinessNameLine2Txt'][1]")
      }
    }
    
    # Title variants
    title  <- xml_text_or_na(n, ".//*[local-name()='Title' or local-name()='TitleTxt'][1]")
    
    tibble(ein = ein, year = year, person_name = person, title = title)
  }) %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(person_name), nchar(person_name) > 0)
}


parse_people <- function(doc, ein, year, path = NULL) {
  doc <- strip_ns(doc)
  
  # Canonical and older nodes:
  nodes_990_grp <- find_all(doc, ".//ReturnData//IRS990//*[local-name()='Form990PartVIISectionAGrp']")
  nodes_990_sec <- find_all(doc, ".//ReturnData//IRS990//*[local-name()='Form990PartVIISectionA']")  # older layout (no 'Grp')
  nodes_990ez   <- find_all(doc, ".//ReturnData//IRS990EZ//*[local-name()='OfficerDirectorTrusteeKeyEmplGrp']")
  nodes_990pf   <- find_all(doc, ".//ReturnData//IRS990PF//*[local-name()='OfcrDirTrstKeyEmplGrp']")
  
  # Generic: any element that contains one of the person tags + one of the title tags
  nodes_generic <- find_all(
    doc,
    ".//ReturnData//*[ (./*[local-name()='NamePerson'] or ./*[local-name()='PersonName'] or ./*[local-name()='PersonNm'] or ./*[local-name()='BusinessName']/*[local-name()='BusinessNameLine1' or local-name()='BusinessNameLine1Txt']) and (./*[local-name()='Title'] or ./*[local-name()='TitleTxt']) ]"
  )
  
  used  <- NA_character_
  nodes <- NULL
  
  if (length(nodes_990_grp) > 0) { nodes <- nodes_990_grp; used <- "irs990_sectionA_grp" }
  else if (length(nodes_990_sec) > 0) { nodes <- nodes_990_sec; used <- "irs990_sectionA" }
  else if (length(nodes_990ez)  > 0) { nodes <- nodes_990ez;  used <- "irs990ez" }
  else if (length(nodes_990pf)  > 0) { nodes <- nodes_990pf;  used <- "irs990pf" }
  else if (length(nodes_generic) > 0) { nodes <- nodes_generic; used <- "generic_xml" }
  
  people_xml <- tibble()
  if (!is.null(nodes) && length(nodes) > 0) {
    people_xml <- extract_people_from_nodes_xml(doc, nodes, ein, year)
  }
  
  people_txt <- tibble()
  if (nrow(people_xml) == 0 && !is.null(path)) {
    txt <- read_clean_text(path)
    if (!is.na(txt)) {
      people_txt <- extract_people_from_text(txt, ein, year)
      if (nrow(people_txt) > 0) used <- ifelse(is.na(used), "text_fallback", paste0(used, "+text_fallback"))
    }
  }
  
  people <- dplyr::bind_rows(people_xml, people_txt) %>% dplyr::distinct()
  
  diag <- tibble::tibble(
    schema_used          = used,
    people_nodes_found   = if (!is.null(nodes)) length(nodes) else 0L,
    people_rows          = nrow(people),
    n_missing_names      = sum(is.na(people$person_name) | people$person_name == "", na.rm = TRUE),
    n_missing_titles     = sum(is.na(people$title) | people$title == "", na.rm = TRUE)
  )
  list(people = people, diag = diag)
}

# ---------------------------
# End-to-end safe parser (per-file)
# ---------------------------

parse_990_xml_safe <- function(path) {
  res <- tryCatch({
    doc  <- xml2::read_xml(path)
    
    firm <- parse_firm(doc, path = path)
    firm_diag <- validate_firm(firm %>% slice(1), file = path)
    
    ein  <- firm$ein %>% dplyr::first()
    year <- firm$year %>% dplyr::first()
    end_year = as.numeric(substr(firm$end_dt,1,4)) %>% dplyr::first()
    
    
    ppl     <- parse_people(doc, ein = ein, year = end_year, path = path)
    people  <- ppl$people
    diag_p  <- ppl$diag
    
    diag <- tibble(
      file                  = path,
      schema_used           = diag_p$schema_used,
      people_nodes_found    = diag_p$people_nodes_found,
      people_rows           = diag_p$people_rows,
      n_missing_names       = diag_p$n_missing_names,
      n_missing_titles      = diag_p$n_missing_titles,
      ein                   = firm$ein %>% dplyr::first(),
      year                  = firm$year %>% dplyr::first(),
      country               = firm$country %>% dplyr::first(),
      firm_ein_missing      = firm_diag$ein_missing,
      firm_name_missing     = firm_diag$org_name_missing,
      firm_location_missing = firm_diag$location_missing,
      firm_year_missing     = firm_diag$year_missing,
      firm_needs_review     = firm_diag$firm_needs_review |
        (diag_p$people_rows == 0) |
        (is.na(diag_p$schema_used) | grepl("text_fallback", diag_p$schema_used %||% ""))
    )
    
    list(
      firm   = firm,
      people = people,
      diag   = diag,
      error  = NA_character_
    )
  }, error = function(e) {
    list(
      firm   = tibble(ein = NA_character_, org_name = NA_character_, city = NA_character_, state = NA_character_, zip = NA_character_, country = NA_character_, year = NA_integer_),
      people = tibble(),
      diag   = tibble(
        file                  = path,
        schema_used           = NA_character_,
        people_nodes_found    = NA_integer_,
        people_rows           = 0L,
        n_missing_names       = NA_integer_,
        n_missing_titles      = NA_integer_,
        ein                   = NA_character_,
        year                  = NA_integer_,
        country               = NA_character_,
        firm_ein_missing      = TRUE,
        firm_name_missing     = TRUE,
        firm_location_missing = TRUE,
        firm_year_missing     = TRUE,
        firm_needs_review     = TRUE
      ),
      error  = as.character(e$message)
    )
  })
  res
}

# ---------------------------
# Parallel batch run (furrr)
# ---------------------------

if (.Platform$OS.type == "windows") {
  future::plan(multisession, workers = WORKERS)
} else {
  future::plan(multisession, workers = WORKERS)
  # future::plan(multicore, workers = WORKERS)  # enable on Linux servers if preferred
}

xml_files <- list.files(
  INPUT_DIR,
  pattern     = "\\.(xml)$",
  full.names  = TRUE,
  recursive   = TRUE,
  ignore.case = TRUE
)

xml_files <- xml_files[
  file.exists(xml_files) &
    !dir.exists(xml_files) &
    file.access(xml_files, 4) == 0
]
xml_files <- unique(xml_files)

if (length(xml_files) == 0L) {
  stop(sprintf("No XML files found under (recursively): %s", INPUT_DIR))
}

results <- furrr::future_map(xml_files, parse_990_xml_safe, .progress = TRUE)

firms_df   <- bind_rows(map(results, "firm"))  %>% distinct()
people_df  <- bind_rows(map(results, "people")) %>%
  arrange(ein, year, person_name) # only person_name + title (+ keys)
diag_df    <- bind_rows(map(results, "diag"))  %>% arrange(firm_needs_review, desc(people_rows))
errors_tbl <- tibble(file = xml_files, error = map_chr(results, ~ .x$error %||% NA_character_)) %>%
  filter(!is.na(error))

# ---------------------------
# Drop foreign-address observations
# ---------------------------
# Keep US (US/USA) or missing country; drop explicit foreign country codes (e.g., RQ, CA, MX)
foreign_firms <- firms_df %>%
  filter(!is.na(country), !toupper(country) %in% c("US", "USA"))

keep_firms <- firms_df %>%
  filter(is.na(country) | toupper(country) %in% c("US", "USA"))

# Remove people rows for EIN/year combinations flagged as foreign
drop_keys <- foreign_firms %>% select(ein, year)

people_df <- people_df %>%
  anti_join(drop_keys, by = c("ein", "year"))

firms_df  <- keep_firms

# Keep diagnostics only for retained filings; also mark which were excluded
diag_df <- diag_df %>%
  mutate(excluded_foreign = if_else(!is.na(country) & !toupper(country) %in% c("US","USA"), TRUE, FALSE)) %>%
  semi_join(keep_firms %>% select(ein, year), by = c("ein","year"))

# ---------------------------
# Write outputs + summary
# ---------------------------
observe <- diag_df %>%
    filter(firm_needs_review | people_rows == 0 | grepl("text_fallback", schema_used %||% "")) %>%
    select(file, schema_used, people_nodes_found, people_rows,
           n_missing_names, n_missing_titles,
           firm_ein_missing, firm_name_missing, firm_location_missing, firm_year_missing,
           excluded_foreign, firm_needs_review) %>%
    arrange(desc(firm_needs_review), desc(people_rows))

# define a year column based solely on the end date
firms_df <- firms_df %>%
  mutate(end_year = as.numeric(substr(end_dt,1,4)))
people_df <- people_df %>%
  rename(end_year=year)

saveRDS(firms_df, file = "./CreatedData/new_xmls_ein_data.rds")
saveRDS(people_df, file = "./CreatedData/new_xmls_people_data.rds")

# ============================================================
# End of script

