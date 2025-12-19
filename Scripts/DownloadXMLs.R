
# NONPROFIT EXPLORER: SEARCH → SCRAPE ORG PAGE → DOWNLOAD ALL XMLs
# Parallel across EINs with future.apply (polite: 2–4 workers)

suppressPackageStartupMessages({
  library(httr)         # HTTP requests
  library(jsonlite)     # JSON parsing
  library(stringr)      # String helpers
  library(rvest)        # HTML scraping
  library(xml2)         # url_absolute for absolute URLs
  library(fs)           # Files/dirs
  library(glue)         # String interpolation
  library(curl)         # Streaming downloads
  library(future.apply) # Parallel across EINs
})

# ------------ Configuration ------------
BASE_SEARCH <- "https://projects.propublica.org/nonprofits/api/v2/search.json"
ORG_PAGE    <- "https://projects.propublica.org/nonprofits/organizations"
UA          <- "Hanna-Glenn-Research/1.0 (+R httr; Nonprofit XML downloader)"

# Output root directory
out_root <- "data/nonprofit_xml"
fs::dir_create(out_root)

# Concurrency (adjust politely)
workers <- 6  # start with 2–4; raise only if stable

# ------------ Helpers ------------

# Normalize EIN to digits-only (expect 9 digits)
normalize_ein <- function(x) {
  d <- stringr::str_replace_all(x, "[^0-9]", "")
  if (nchar(d) != 9) {
    warning(glue::glue("EIN '{x}' normalizes to '{d}' (not 9 digits)."))
  }
  d
}

# Format EIN with hyphen for search q=XX-XXXXXXX
format_ein_hyphen <- function(d) {
  if (nchar(d) == 9) paste0(substr(d, 1, 2), "-", substr(d, 3, 9)) else d
}

# GET with UA and simple retry/backoff
api_get <- function(url, query = list(), retries = 3, sleep_sec = 1) {
  for (i in seq_len(retries)) {
    resp <- httr::GET(url, query = query, httr::user_agent(UA), httr::timeout(30))
    if (!httr::http_error(resp)) return(resp)
    message(glue::glue("HTTP {httr::status_code(resp)} for {url}; retry {i}/{retries}"))
    Sys.sleep(sleep_sec * i)
  }
  stop(glue::glue("Failed after {retries} attempts: {url}"))
}

# 1) Search EIN and enforce "exactly one result"
search_exactly_one <- function(ein_digits) {
  q <- format_ein_hyphen(ein_digits)
  resp <- api_get(BASE_SEARCH, query = list(q = q))
  dat  <- httr::content(resp, as = "parsed", type = "application/json")
  orgs <- dat$organizations
  n    <- length(orgs)
  
  if (n == 1) {
    return(orgs[[1]])
  } else {
    message(glue::glue("Skip {ein_digits}: search returned {n} results (need exactly 1)."))
    return(NULL)
  }
}

# --- Debug helper: show what the scraper found for a single EIN (optional) ---
debug_show_links <- function(ein_digits) {
  url  <- paste0(ORG_PAGE, "/", ein_digits)
  resp <- httr::GET(url, httr::user_agent(UA), httr::timeout(30))
  pg   <- rvest::read_html(resp)
  a_nodes <- rvest::html_elements(pg, "a")
  texts   <- tryCatch(rvest::html_text2(a_nodes),
                      error = function(e) rvest::html_text(a_nodes, trim = TRUE))
  hrefs   <- rvest::html_attr(a_nodes, "href"); hrefs[is.na(hrefs)] <- ""
  
  idx <- (grepl("(?i)^\\s*XML\\s*$", texts) |
            grepl("(?i)(download-xml|\\.xml($|\\?))", hrefs))
  candidates <- xml2::url_absolute(hrefs[idx], url)
  list(n = sum(idx), first_10 = head(candidates, 10))
}

# 2) Scrape org page, collect all XML links (accept 'download-xml' and '.xml')
get_xml_links_from_org_page <- function(ein_digits) {
  url  <- paste0(ORG_PAGE, "/", ein_digits)  # org pages use digits-only EIN in path
  resp <- httr::GET(url, httr::user_agent(UA), httr::timeout(30))
  if (httr::http_error(resp)) {
    message(glue::glue("Failed to load org page for {ein_digits}: HTTP {httr::status_code(resp)}"))
    return(character(0))
  }
  pg <- rvest::read_html(resp)
  
  # Anchor tags on the page
  a_nodes <- rvest::html_elements(pg, "a")
  
  # rvest::html_text2 trims by default; fall back if older rvest
  texts <- tryCatch(rvest::html_text2(a_nodes),
                    error = function(e) rvest::html_text(a_nodes, trim = TRUE))
  hrefs <- rvest::html_attr(a_nodes, "href"); hrefs[is.na(hrefs)] <- ""
  
  # ACCEPT:
  #  - anchor text equals "XML" (case-insensitive, trimmed)
  #  - href contains "download-xml"
  #  - href ends with ".xml"
  idx <- (grepl("(?i)^\\s*XML\\s*$", texts) |
            grepl("(?i)(download-xml|\\.xml($|\\?))", hrefs))
  
  urls <- xml2::url_absolute(hrefs[idx], url)
  # DO NOT filter out non-.xml endings; 'download-xml' returns XML content
  unique(urls)
}

# 3) Download ALL XMLs; avoid collisions by appending an index (_1.xml, _2.xml, ...)
download_xmls <- function(urls, dest_dir) {
  fs::dir_create(dest_dir)
  if (length(urls) == 0) {
    message(glue::glue("No XML links for {basename(dest_dir)}"))
    return(invisible(NULL))
  }
  
  for (i in seq_along(urls)) {
    u <- urls[i]
    # Base name from URL path (ignore query)
    base <- basename(strsplit(u, "\\?")[[1]][1])
    if (!grepl("\\.xml$", base, ignore.case = TRUE)) base <- paste0(base, ".xml")
    fname <- paste0(sub("\\.xml$", "", base), "_", i, ".xml")
    fpath <- file.path(dest_dir, fname)
    
    tryCatch(
      {
        curl::curl_download(u, fpath, quiet = TRUE)
        message(glue::glue("Saved: {fpath}"))
        Sys.sleep(runif(1, 0.3, 0.8))  # polite per-file pause
      },
      error = function(e) {
        message(glue::glue("Download failed for {u}: {conditionMessage(e)}"))
      }
    )
  }
}

# Per-EIN pipeline: search → scrape → download

process_one_ein <- function(ein) {
  dest <- file.path(out_root, ein)
  
  # If folder already exists and has XML files, skip
  if (fs::dir_exists(dest) && length(fs::dir_ls(dest, glob = "*.xml")) > 0) {
    message(glue::glue("Skip EIN {ein}: folder already exists with XML files"))
    return(list(ein = ein, n_xml = length(fs::dir_ls(dest, glob = "*.xml")), status = "already_downloaded"))
  }
  
  message(glue::glue("Start EIN {ein}"))
  hit <- search_exactly_one(ein)
  if (is.null(hit)) {
    return(list(ein = ein, n_xml = 0, status = "search_skipped"))
  }
  
  # Scrape org page and collect XML links
  urls <- get_xml_links_from_org_page(ein)
  
  # Download all XMLs
  download_xmls(urls, dest)
  
  list(ein = ein, n_xml = length(urls), status = if (length(urls) > 0) "ok" else "no_xml_found")
}

# ------------ Run (parallel across EINs) ------------
# 1) Provide your EIN list (hyphens optional). Replace the example with your vector.
source("./Scripts/paths.R")
cw <- readRDS(paste0(created_data_path, "updated_ein_aha_cw.rds"))

eins <- list(unique(cw$Filer.EIN))[[1]]

# 2) Normalize EINs to digits-only
eins_norm <- vapply(eins, normalize_ein, FUN.VALUE = character(1))

# 3) Launch polite parallel processing across EINs
plan(multisession, workers = workers)

results <- future_lapply(eins_norm,  FUN = function(e) {    
  Sys.sleep(runif(1, 0.2, 0.8))  
  # stagger starts per worker    
  process_one_ein(e)  },  future.seed = TRUE)
  
plan(sequential)  # reset when done
  
# 4) Summarize result and write a log CSV
summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(ein = x$ein, n_xml = x$n_xml, status = x$status, stringsAsFactors = FALSE)
}))
print(summary_df)
  






