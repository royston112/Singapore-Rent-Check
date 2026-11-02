# 01c_amenities.R
# Builds amenities.rds: one table of places the app can show near a home.
# Run AFTER 01b_clean_new_data.R. Run once; lookups are cached, so re-running is quick.
#   source("01c_amenities.R")
#
# INPUTS (all in this folder)
#   mrt_exits.rds            <- from 01b (MRT/LRT exits with opening dates)
#   Hawker_centres.geojson   <- NEA, data.gov.sg
#   School_information.csv   <- MOE school directory, data.gov.sg
#   extra_places.csv         <- OPTIONAL. Your own list: columns kind, name, and either lon + lat
#                               or postal (looked up in OneMap). kind is one of:
#                               mall, foodcourt, petrol, or anything else you want to add.
#   amenities.R              <- the helper functions (same folder)
#
# OUTPUTS
#   amenities.rds            <- the table the app reads
#   amenities_review.csv     <- counts per kind + schools that could not be located (check these)
#   data_cache/              <- saved OneMap answers, so nothing is looked up twice
#
# OPTIONAL: malls, petrol stations and food courts from OpenStreetMap (set FETCH_OSM <- TRUE below).
#   No government dataset for these was found. OpenStreetMap is volunteer-mapped, so coverage and names
#   vary; check amenities_review.csv. Credit "OpenStreetMap contributors" (ODbL) on Data & limits.
#   This query has NOT been tested by the person who wrote it. If it returns 0 rows, try the same query
#   at overpass-turbo.eu and tell me what it says.
#
# SCHOOLS: only schools in the MOE directory (primary, secondary, junior college, mixed level, centralised
#   institute). Kindergartens, international schools, polytechnics and universities are NOT in it.
#   A school is placed on the map only if OneMap finds its postal code at one spot (within 100 m).
#   Check the addresses in amenities_review.csv: a postal match does not prove the school has not moved.
#
# All places are TODAY's locations. They are not the locations at each historical rental date.

for (pkg in c("tidyverse", "sf", "httr2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)
library(sf)
library(httr2)
source("amenities.R")

FETCH_OSM <- TRUE       # TRUE = fetch places from OpenStreetMap (needs internet; cached after the first success)
OSM_KINDS <- c("mall")  # which ones: "mall", "petrol", "foodcourt". Malls only is the recommended choice.

need <- c("mrt_exits.rds", "Hawker_centres.geojson", "School_information.csv", "amenities.R")
if (any(!file.exists(need))) stop("Missing in ", getwd(), ":\n  ", paste(need[!file.exists(need)], collapse = "\n  "))

`%||%` <- function(a, b) if (is.null(a)) b else a
BEFORE_DATA <- as.Date("1900-01-01")   # "opened" for everything that is not a new MRT/LRT station

# Longitude/latitude -> SVY21 metres (what the HDB and URA data use)
to_xy <- function(lon, lat) {
  pts <- st_as_sf(tibble(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326) |> st_transform(3414)
  m <- st_coordinates(pts)
  tibble(x = m[, 1], y = m[, 2])
}


# ============================================================
# 1. MRT / LRT exits (already cleaned in 01b, with opening dates)
# ============================================================

mrt <- readRDS("mrt_exits.rds") |>
  transmute(kind = "mrt",
            name = str_to_title(str_remove(station, " (MRT|LRT) STATION$")),
            detail = if_else(is.na(line), rail_type, paste0(rail_type, ", ", line)),
            group_id = station, lon, lat, opened)


# ============================================================
# 2. Hawker centres (NEA). Only open ones; "Under Construction" is dropped.
# ============================================================

hk <- st_read("Hawker_centres.geojson", quiet = TRUE) |> st_zm(drop = TRUE)
if (is.na(st_crs(hk))) st_crs(hk) <- 4326
hk <- st_transform(hk, 4326)
hk_xy <- st_coordinates(hk)
hawker <- hk |>
  st_drop_geometry() |>
  mutate(lon = hk_xy[, 1], lat = hk_xy[, 2],
         stalls = suppressWarnings(as.integer(NUMBER_OF_COOKED_FOOD_STALLS))) |>
  filter(STATUS %in% c("Existing", "Existing (new)", "Existing (replacement)", "Interim Centre")) |>
  transmute(kind = "hawker", name = str_squish(NAME),
            detail = if_else(is.na(stalls), "", paste(stalls, "cooked-food stalls")),
            group_id = paste(NAME, ADDRESSPOSTALCODE), lon, lat, opened = BEFORE_DATA)


# ============================================================
# 3. Schools (MOE directory) located through OneMap by postal code
# ============================================================

TOKEN <- onemap_token()     # "" = try without a token (worked for 00_geocode_hdb_blocks.R)
dir.create("data_cache/school_search", recursive = TRUE, showWarnings = FALSE)
if (!dir.exists("data_cache/school_search")) {
  warning("Could not create data_cache/school_search in ", getwd(),
          ". Answers will not be cached (the script still works, just slower on a re-run). ",
          "If this folder is read-only (for example inside a synced or protected folder), use setwd() to a normal folder.")
}

.fails <- 0      # consecutive failed requests (the script stops after 5 instead of hanging)
school_lookup <- function(postal) {
  out <- function(status, lon = NA_real_, lat = NA_real_, matched = NA_character_) {
    tibble(postal_code = postal, lon = lon, lat = lat, geocode_status = status, matched_address = matched)
  }
  if (is.na(postal) || !grepl("^[0-9]{6}$", postal)) return(out("bad_postal_code"))

  cache <- file.path("data_cache/school_search", paste0(postal, ".rds"))
  if (file.exists(cache)) {
    body <- readRDS(cache)
  } else {
    resp <- request("https://www.onemap.gov.sg/api/common/elastic/search") |>
      req_url_query(searchVal = postal, returnGeom = "Y", getAddrDetails = "Y", pageNum = 1) |>
      onemap_auth(TOKEN) |>
      req_timeout(30) |>
      req_retry(max_tries = 2, is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp_status(resp) %in% c(401, 403)) stop("OneMap refused the request (", resp_status(resp), "). Check your token.")
    if (resp_status(resp) != 200) {                                  # not cached: retried next run
      .fails <<- .fails + 1
      if (.fails >= 5) stop("OneMap has refused 5 requests in a row (last status ", resp_status(resp),
                            "). It may be limiting tokenless requests. Use a token, wait and re-run (cached schools are skipped), ",
                            "or copy Roy's data_cache/school_search folder into this folder.")
      return(out("request_failed"))
    }
    .fails <<- 0
    body <- resp_body_json(resp, simplifyVector = TRUE)
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    try(suppressWarnings(saveRDS(body, cache)), silent = TRUE)     # caching is a convenience, never fatal
    Sys.sleep(0.3)
  }

  r <- body$results
  if (is.null(r) || !is.data.frame(r) || !nrow(r)) return(out("no_result"))
  if (!all(c("POSTAL", "LONGITUDE", "LATITUDE", "ADDRESS") %in% names(r))) return(out("no_result"))
  r <- as_tibble(r) |>
    filter(as.character(POSTAL) == postal) |>
    transmute(lon = as.numeric(LONGITUDE), lat = as.numeric(LATITUDE), matched = ADDRESS) |>
    filter(is.finite(lon), is.finite(lat), between(lon, 103.5, 104.2), between(lat, 1.1, 1.6)) |>
    distinct(lon, lat, .keep_all = TRUE)
  if (!nrow(r)) return(out("no_exact_postal_match"))

  # Several results for one postal code are fine if they are all on the same site (within 100 m)
  spread_m <- sqrt((diff(range(r$lon)) * 111320 * cos(1.35 * pi / 180))^2 + (diff(range(r$lat)) * 110574)^2)
  if (spread_m > 100) return(out("ambiguous"))
  out("postal_match", r$lon[1], r$lat[1], r$matched[1])
}

schools_raw <- read_csv("School_information.csv", col_types = cols(.default = "c")) |>
  transmute(school_name = str_squish(school_name), address = str_squish(address),
            postal_code = str_pad(str_squish(postal_code), 6, pad = "0"),
            school_level = str_squish(mainlevel_code))

message("Locating ", n_distinct(schools_raw$postal_code), " school postal codes (about 2 minutes the first time)...")
school_geo <- map_dfr(unique(schools_raw$postal_code), school_lookup)
schools <- left_join(schools_raw, school_geo, by = "postal_code")

# Explicit user-selected candidates. Selection is not independent verification.
if (file.exists("school_geocode_overrides.csv")) {
  ov <- read_csv("school_geocode_overrides.csv", col_types = cols(.default = "c"))
  required <- c("school_name", "postal_code", "lon", "lat", "matched_address", "verification_note")
  if (!all(required %in% names(ov))) stop("School override file has missing columns.")
  ov <- ov |> mutate(school_name = str_to_upper(str_squish(school_name)),
                     postal_code = str_pad(str_trim(postal_code), 6, pad = "0"),
                     lon = as.numeric(lon), lat = as.numeric(lat))
  if (anyDuplicated(ov[c("school_name", "postal_code")])) stop("Duplicate school overrides.")
  if (any(!is.finite(ov$lon) | !is.finite(ov$lat)) ||
      any(!between(ov$lon, 103.5, 104.2) | !between(ov$lat, 1.1, 1.6))) {
    stop("Invalid override coordinates.")
  }
  if (any(is.na(ov$verification_note) | !nzchar(str_trim(ov$verification_note)))) {
    stop("Each override needs a selection or verification note.")
  }
  keys <- paste(str_to_upper(str_squish(schools$school_name)), schools$postal_code, sep = "|")
  ov_keys <- paste(ov$school_name, ov$postal_code, sep = "|")
  if (!all(ov_keys %in% keys)) stop("Override does not match a source school.")
  idx <- match(keys, ov_keys)
  hit <- which(!is.na(idx))
  schools$lon[hit] <- ov$lon[idx[hit]]
  schools$lat[hit] <- ov$lat[idx[hit]]
  schools$matched_address[hit] <- ov$matched_address[idx[hit]]
  schools$geocode_status[hit] <- "manual_selected_unverified"
  write_csv(ov, "school_overrides_applied.csv")
  message("Applied ", length(hit), " user-selected school locations (unverified).")
}

message("Schools located: ", sum(is.finite(schools$lon)), " of ", nrow(schools))

school <- schools |>
  filter(is.finite(lon), is.finite(lat)) |>
  transmute(kind = "school", name = str_to_title(school_name), detail = str_to_title(school_level),
            group_id = paste(school_name, postal_code), lon, lat, opened = BEFORE_DATA)


# ============================================================
# 4. Optional extras: your own list, and/or OpenStreetMap
# ============================================================

extra <- tibble(kind = character(), name = character(), detail = character(), group_id = character(),
                lon = numeric(), lat = numeric(), opened = as.Date(character()))

if (file.exists("extra_places.csv")) {
  ex <- read_csv("extra_places.csv", col_types = cols(.default = "c"))
  for (col in c("detail", "lon", "lat", "postal")) if (!col %in% names(ex)) ex[[col]] <- NA_character_
  ex <- ex |> mutate(lon = as.numeric(lon), lat = as.numeric(lat), postal = str_pad(str_squish(postal), 6, pad = "0"))
  need_geo <- is.na(ex$lon) | is.na(ex$lat)
  if (any(need_geo)) {
    g <- map_dfr(ex$postal[need_geo], school_lookup)        # same postal-code lookup as the schools
    ex$lon[need_geo] <- g$lon
    ex$lat[need_geo] <- g$lat
  }
  extra <- bind_rows(extra, ex |> filter(is.finite(lon), is.finite(lat)) |>
                       transmute(kind = str_squish(kind), name = str_squish(name), detail = coalesce(detail, ""),
                                 group_id = paste(kind, name, round(lon, 4)), lon, lat, opened = BEFORE_DATA))
  message("extra_places.csv: ", nrow(extra), " places used of ", nrow(ex))
}

fetch_osm_places <- function(kinds = OSM_KINDS) {
  cache <- file.path("data_cache", paste0("osm_places_", paste(sort(kinds), collapse = "-"), ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  clauses <- c(mall      = '  nwr["shop"="mall"](area.sg);',
               petrol    = '  nwr["amenity"="fuel"](area.sg);',
               foodcourt = '  nwr["amenity"="food_court"](area.sg);')
  query <- paste0('[out:json][timeout:90];\narea["ISO3166-1"="SG"][admin_level=2]->.sg;\n(\n',
                  paste(clauses[kinds], collapse = "\n"), '\n);\nout center tags;')
  resp <- request("https://overpass-api.de/api/interpreter") |>
    req_user_agent("DBA3702 student project (rental price tool)") |>
    req_body_form(data = query) |>
    req_timeout(120) |>
    req_retry(max_tries = 2) |>
    req_perform()
  els <- resp_body_json(resp)$elements
  if (!length(els)) stop("Overpass returned no places. Try the query at overpass-turbo.eu.")
  raw <- map_dfr(els, \(e) {
    tg <- e$tags
    tibble(shop = as.character(tg$shop %||% NA), amenity = as.character(tg$amenity %||% NA),
           name = as.character(tg$name %||% NA), brand = as.character(tg$brand %||% NA),
           operator = as.character(tg$operator %||% NA),
           lon = as.numeric(e$lon %||% e$center$lon %||% NA), lat = as.numeric(e$lat %||% e$center$lat %||% NA))
  })
  dir.create("data_cache", showWarnings = FALSE)
  saveRDS(raw, cache)
  raw
}

osm_raw <- NULL
if (FETCH_OSM) {
  osm_raw <- tryCatch(fetch_osm_places(OSM_KINDS), error = function(e) {
    message("OpenStreetMap fetch failed (", conditionMessage(e), "). Carrying on without it. ",
            "Add malls by hand in extra_places.csv instead (see extra_places_TEMPLATE.csv).")
    NULL
  })
}
if (!is.null(osm_raw) && nrow(osm_raw) > 0) {
  osm <- osm_raw |>
    filter(is.finite(lon), is.finite(lat)) |>
    mutate(kind = case_when(coalesce(shop, "") == "mall"            ~ "mall",
                            coalesce(amenity, "") == "fuel"         ~ "petrol",
                            coalesce(amenity, "") == "food_court"   ~ "foodcourt"),
           name = case_when(kind == "mall"      ~ name,
                            kind == "petrol"    ~ coalesce(name, brand, operator, "Petrol station"),
                            kind == "foodcourt" ~ coalesce(name, "Food court")),
           detail = coalesce(brand, operator, ""),
           lon3 = round(lon, 3), lat3 = round(lat, 3)) |>
    filter(!is.na(kind), kind %in% OSM_KINDS, !is.na(name))
  # The same place can be mapped twice (a point and a building outline): keep one
  osm <- bind_rows(osm |> filter(kind == "mall") |> distinct(name, .keep_all = TRUE),
                   osm |> filter(kind != "mall") |> distinct(kind, name, lon3, lat3, .keep_all = TRUE)) |>
    transmute(kind, name, detail, group_id = paste(kind, name, lon3, lat3), lon, lat, opened = BEFORE_DATA)
  extra <- bind_rows(extra, osm)
  message("OpenStreetMap places: ", paste(names(table(osm$kind)), table(osm$kind), collapse = ", "))
}


# ============================================================
# 5. Combine, add SVY21 x/y, save
# ============================================================

amen <- bind_rows(mrt, hawker, school, extra) |>
  filter(is.finite(lon), is.finite(lat), between(lon, 103.5, 104.2), between(lat, 1.1, 1.6))
amen <- bind_cols(amen, to_xy(amen$lon, amen$lat)) |>
  select(kind, name, detail, group_id, lon, lat, x, y, opened)
saveRDS(amen, "amenities.rds")

review <- bind_rows(
  amen |> count(kind, name = "places") |> mutate(note = "count in amenities.rds"),
  schools |> filter(geocode_status == "manual_selected_unverified") |>
    transmute(kind = "school_OVERRIDE_UNVERIFIED", places = NA_integer_,
              note = paste(school_name, "|", matched_address)),
  schools |> filter(!is.finite(lon)) |>
    transmute(kind = "school_NOT_LOCATED", places = NA_integer_,
              note = paste(school_name, "|", address, "|", postal_code, "|", geocode_status))
)
write_csv(review, "amenities_review.csv")
# The matched address for every located school, so you can compare it with the directory address
write_csv(schools |> filter(is.finite(lon)) |> select(school_name, address, postal_code, matched_address),
          "schools_address_check.csv")

message("\nSaved amenities.rds")
print(count(amen, kind))
message("Check amenities_review.csv (unlocated schools) and schools_address_check.csv (matched addresses).")
