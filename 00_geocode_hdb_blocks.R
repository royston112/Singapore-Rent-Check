# 00_geocode_hdb_blocks.R
# Looks up the location (lat/lon + SVY21 x/y + postal code) of every residential HDB block
# using OneMap Search (Singapore Land Authority). RUN ONCE; the result is saved to an .rds file.
#
# INPUT   HDBPropertyInformation.csv            (data.gov.sg, in this folder)
# OUTPUT  hdb_block_coords.rds                  one row per residential block, with coordinates
#         hdb_geocode_review.csv                blocks that did not match cleanly (check by hand)
#         hdb_geocode_cache.rds                 progress file, so the script can resume if stopped
#
# HOW TO RUN
#   1. Open this file in RStudio (working directory = this folder) and click "Source".
#   2. When asked, type your OneMap account email and password. They are only used to get a
#      token and are NOT saved anywhere. Press Enter on a blank email to try without a token.
#      (Tip: to avoid typing them, put ONEMAP_EMAIL=... and ONEMAP_PASSWORD=... in ~/.Renviron)
#   3. Leave it running (roughly 45-90 min for ~10,800 blocks). If it stops (laptop sleeps,
#      Wi-Fi drops), just Source it again: it skips blocks already done.
#
# CREDIT (put on the app's Data & limits page): "Block coordinates: OneMap, Singapore Land Authority"

for (pkg in c("tidyverse", "httr2", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)
library(httr2)

BLOCK_FILE  <- "HDBPropertyInformation.csv"
CACHE_FILE  <- "hdb_geocode_cache.rds"
OUT_FILE    <- "hdb_block_coords.rds"
REVIEW_FILE <- "hdb_geocode_review.csv"
PAUSE_SEC   <- 0.25     # ~4 searches per second, about 240 a minute, to stay under OneMap's limit
SAVE_EVERY  <- 200      # write progress to the cache file every N blocks

if (!file.exists(BLOCK_FILE)) stop("Can't find ", BLOCK_FILE, " in ", getwd())


# ============================================================
# 1. Token (optional today, but OneMap warns it may become required)
# ============================================================

get_token <- function() {
  email <- Sys.getenv("ONEMAP_EMAIL")
  pw    <- Sys.getenv("ONEMAP_PASSWORD")
  if (email == "" && interactive()) email <- readline("OneMap email (blank = no token): ")
  if (email == "") return(NULL)
  if (pw == "") {
    pw <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::askForPassword("OneMap password")
    } else readline("OneMap password: ")
  }
  resp <- request("https://www.onemap.gov.sg/api/auth/post/getToken") |>
    req_body_json(list(email = email, password = pw)) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  tok <- tryCatch(resp_body_json(resp)$access_token, error = \(e) NULL)
  if (is.null(tok)) {
    warning("Could not get a OneMap token (status ", resp_status(resp), "). Continuing without one.")
    return(NULL)
  }
  message("Got a OneMap token.")
  tok
}
TOKEN <- get_token()


# ============================================================
# 2. Blocks to look up
# ============================================================

squish_up <- function(x) str_squish(str_to_upper(x))

blocks <- read_csv(BLOCK_FILE, col_types = cols(.default = "c")) |>
  filter(residential == "Y") |>
  transmute(blk_no = squish_up(blk_no), street = squish_up(street)) |>
  distinct()
message(nrow(blocks), " residential blocks to locate.")

# HDB abbreviates street words; OneMap spells them out. Used only to CHECK a match.
ABBREV <- c(
  "AVE" = "AVENUE", "ST" = "STREET", "RD" = "ROAD", "DR" = "DRIVE", "CRES" = "CRESCENT",
  "CTRL" = "CENTRAL", "CTR" = "CENTRE", "STH" = "SOUTH", "NTH" = "NORTH", "UPP" = "UPPER",
  "LOR" = "LORONG", "BT" = "BUKIT", "JLN" = "JALAN", "TG" = "TANJONG", "KG" = "KAMPONG",
  "PK" = "PARK", "PL" = "PLACE", "CL" = "CLOSE", "TER" = "TERRACE", "HTS" = "HEIGHTS",
  "MKT" = "MARKET", "C'WEALTH" = "COMMONWEALTH", "GDNS" = "GARDENS", "GDN" = "GARDEN", "SQ" = "SQUARE",
  "BLVD" = "BOULEVARD", "LK" = "LINK", "GRN" = "GREEN"
)
road_key <- function(x) {
  x <- squish_up(x)
  x <- str_replace_all(x, "^ST\\.? (?=[A-Z]+'S)", "SAINT ")       # ST. GEORGE'S -> SAINT GEORGE'S
  words <- str_split(x, " ")
  map_chr(words, \(w) {
    w <- ifelse(w %in% names(ABBREV), ABBREV[w], w)
    str_remove_all(paste(w, collapse = ""), "[^A-Z0-9]")          # compare letters/digits only
  })
}


# ============================================================
# 3. One OneMap search, returning the best matching result
# ============================================================

search_onemap <- function(blk, street) {
  req <- request("https://www.onemap.gov.sg/api/common/elastic/search") |>
    req_url_query(searchVal = paste(blk, street), returnGeom = "Y",
                  getAddrDetails = "Y", pageNum = 1) |>
    req_retry(max_tries = 5, is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504),
              backoff = \(i) 2^i) |>
    req_timeout(30) |>
    req_error(is_error = \(r) FALSE)
  if (!is.null(TOKEN)) req <- req_headers(req, Authorization = TOKEN)

  resp <- tryCatch(req_perform(req), error = \(e) NULL)
  empty <- tibble(lat = NA_real_, lon = NA_real_, x = NA_real_, y = NA_real_,
                  postal = NA_character_, onemap_address = NA_character_,
                  match = "no_result", status = NA_integer_)
  if (is.null(resp)) return(mutate(empty, match = "request_failed"))
  if (resp_status(resp) != 200) return(mutate(empty, match = "http_error", status = resp_status(resp)))

  res <- resp_body_json(resp, simplifyVector = TRUE)$results
  if (length(res) == 0 || NROW(res) == 0) return(mutate(empty, status = 200L))
  res <- as_tibble(res) |>
    mutate(blk_ok  = squish_up(BLK_NO) == blk,
           road_ok = road_key(ROAD_NAME) == road_key(street),
           has_postal = POSTAL != "NIL",
           score = 4 * blk_ok + 2 * road_ok + has_postal) |>
    arrange(desc(score))
  best <- res[1, ]
  match <- case_when(best$blk_ok & best$road_ok ~ "exact",
                     best$blk_ok                ~ "block_only",   # road spelt differently: check
                     TRUE                       ~ "weak")         # different block: check
  tibble(lat = as.numeric(best$LATITUDE), lon = as.numeric(best$LONGITUDE),
         x = as.numeric(best$X), y = as.numeric(best$Y),
         postal = best$POSTAL, onemap_address = best$ADDRESS,
         match = match, status = 200L)
}


# ============================================================
# 4. Loop with a resumable cache
# ============================================================

cache <- if (file.exists(CACHE_FILE)) readRDS(CACHE_FILE) else tibble()
done  <- if (nrow(cache)) paste(cache$blk_no, cache$street) else character(0)
# Retry anything that failed for a technical reason last time
if (nrow(cache)) {
  cache <- filter(cache, !match %in% c("request_failed", "http_error"))
  done  <- paste(cache$blk_no, cache$street)
}
todo <- blocks |> filter(!paste(blk_no, street) %in% done)
message(nrow(cache), " already done, ", nrow(todo), " to go.")

if (nrow(todo) > 0) {
  new_rows <- vector("list", nrow(todo))
  pb <- txtProgressBar(min = 0, max = nrow(todo), style = 3)
  for (i in seq_len(nrow(todo))) {
    r <- search_onemap(todo$blk_no[i], todo$street[i])
    # Token expired mid-run? Get a new one and retry once.
    if (identical(r$status, 401L) && !is.null(TOKEN)) {
      TOKEN <- get_token()
      r <- search_onemap(todo$blk_no[i], todo$street[i])
    }
    new_rows[[i]] <- bind_cols(todo[i, ], r)
    setTxtProgressBar(pb, i)
    if (i %% SAVE_EVERY == 0 || i == nrow(todo)) {
      cache <- bind_rows(cache, bind_rows(new_rows[seq_len(i)][!map_lgl(new_rows[seq_len(i)], is.null)]))
      cache <- distinct(cache, blk_no, street, .keep_all = TRUE)
      saveRDS(cache, CACHE_FILE)
    }
    Sys.sleep(PAUSE_SEC)
  }
  close(pb)
}


# ============================================================
# 5. Checks and save
# ============================================================

coords <- readRDS(CACHE_FILE) |>
  right_join(blocks, by = c("blk_no", "street")) |>
  mutate(
    # Anything outside Singapore's rough bounding box is treated as a failed match
    in_sg = between(lat, 1.15, 1.48) & between(lon, 103.6, 104.1),
    across(c(lat, lon, x, y), \(v) if_else(in_sg %in% TRUE, v, NA_real_)),
    match = if_else(!(in_sg %in% TRUE) & match %in% c("exact", "block_only", "weak"), "outside_sg", match),
    match = replace_na(match, "not_run")
  ) |>
  select(blk_no, street, lat, lon, x, y, postal, onemap_address, match)

message("\nMatch summary:")
print(count(coords, match, sort = TRUE))

# Different blocks landing on the exact same point usually means a wrong match
same_point <- coords |> filter(!is.na(lat)) |> add_count(lat, lon, name = "n_same") |> filter(n_same > 1)
message(nrow(same_point), " blocks share a point with another block (included in the review file).")

review <- coords |>
  filter(match != "exact" | paste(blk_no, street) %in% paste(same_point$blk_no, same_point$street)) |>
  arrange(match, street, blk_no)
write_csv(review, REVIEW_FILE)
message(nrow(review), " blocks written to ", REVIEW_FILE, " for a manual check.")

# Only keep coordinates we trust; review-file blocks stay in the table with their match flag
saveRDS(coords, OUT_FILE)
message("Saved ", OUT_FILE, ": ", sum(!is.na(coords$lat)), " of ", nrow(coords), " blocks have coordinates.")

# How many rental contracts will get a location? (needs hdb_rentals.rds in this folder)
if (file.exists("hdb_rentals.rds")) {
  hdb <- readRDS("hdb_rentals.rds") |>
    mutate(blk_no = squish_up(block), street = squish_up(street_name))
  j <- left_join(hdb, coords, by = c("blk_no", "street"))
  message(sprintf("HDB rental contracts with coordinates: %.1f%%  (CENTRAL town: %.1f%%)",
                  100 * mean(!is.na(j$lat)),
                  100 * mean(!is.na(j$lat[j$town == "CENTRAL"]))))
  unmatched_streets <- j |> filter(is.na(lat)) |> count(street, sort = TRUE)
  if (nrow(unmatched_streets)) {
    message("Rental streets with no coordinates (top 15):")
    print(head(unmatched_streets, 15))
  }
}
