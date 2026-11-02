# 01b_clean_new_data.R
# Adds location, planning area, building age and MRT distance to the rental data.
# Run AFTER 00_geocode_hdb_blocks.R. Does NOT change the original .rds files.
# Run:  source("01b_clean_new_data.R")
#
# INPUTS (all in this folder)
#   hdb_rentals.rds, private_rentals.rds                 <- from 01_clean_data.R
#   hdb_block_coords.rds                                 <- from 00_geocode_hdb_blocks.R (OneMap, SLA)
#   HDBPropertyInformation.csv                           <- data.gov.sg (HDB)
#   MasterPlan2025PlanningAreaBoundaryNoSea.geojson      <- data.gov.sg (URA)
#   LTAMRTStationExitGEOJSON.geojson                     <- data.gov.sg (LTA)
#   mrt_station_openings.csv                             <- hand-compiled from LTA announcements
#
# OUTPUTS
#   hdb_rentals_plus.rds       HDB contracts + block location, planning area, building age, MRT distance
#   private_rentals_plus.rds   private contracts + planning area, MRT distance
#   planning_areas.rds         simplified planning-area polygons (lon/lat) for the map
#   mrt_exits.rds              cleaned MRT/LRT exits with opening dates, for the map
#
# DECISIONS BAKED IN
#   - HDB contracts whose block is not in HDB Property Information are KEPT (about 0.1%;
#     mostly demolished/redeveloped blocks on West Coast Rd, Tanglin Halt Rd, MacPherson Lane,
#     C'wealth Dr). Their rent, town and flat type are valid, so they still count in town-level
#     benchmarks and trends. Their location, planning area, building age and MRT fields are NA,
#     so they drop out ONLY of features that need those fields (map, age/MRT filters).
#     has_location = FALSE marks them.
#   - Planning area comes from each block's / project's actual location, not the HDB "town"
#     label. This also splits the HDB "CENTRAL" town into its real planning areas.
#   - Building age = contract year - year the block was completed. block_height is the TALLEST
#     floor of the block, NOT the floor of the flat (unit floor is not in any public data).
#   - approx_lease_left = 99 - building age. Approximate: the lease starts slightly before completion.
#   - MRT distance is straight-line metres to the nearest exit, counting ONLY stations that had
#     opened by the contract month (see mrt_station_openings.csv). Not walking distance.

for (pkg in c("tidyverse", "sf")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tidyverse)
library(sf)

f <- list(
  hdb_rent  = "hdb_rentals.rds",
  priv_rent = "private_rentals.rds",
  coords    = "hdb_block_coords.rds",
  blocks    = "HDBPropertyInformation.csv",
  plan_area = "MasterPlan2025PlanningAreaBoundaryNoSea.geojson",
  mrt       = "LTAMRTStationExitGEOJSON.geojson",
  openings  = "mrt_station_openings.csv"
)
missing <- unlist(f)[!file.exists(unlist(f))]
if (length(missing)) stop("Missing in ", getwd(), ":\n  ", paste(missing, collapse = "\n  "))

squish_up <- function(x) str_squish(str_to_upper(x))
SVY21 <- 3414   # Singapore's metre grid; distances are straight-line metres on it


# ============================================================
# 1. PLANNING AREAS (URA Master Plan 2025)
# ============================================================

plan_areas <- st_read(f$plan_area, quiet = TRUE) |>
  st_zm(drop = TRUE) |>
  st_make_valid() |>
  transmute(planning_area = str_to_title(PLN_AREA_N),
            region        = str_to_title(str_remove(REGION_N, " REGION$"))) |>
  st_transform(SVY21)

# Give each point (x, y in SVY21) a planning area. Points on a boundary go to the nearest area.
add_planning_area <- function(locs) {
  pts <- st_as_sf(locs, coords = c("x", "y"), crs = SVY21, remove = FALSE)
  hit <- st_within(pts, plan_areas)
  idx <- map_int(hit, \(i) if (length(i)) i[1] else NA_integer_)
  if (anyNA(idx)) idx[is.na(idx)] <- st_nearest_feature(pts[is.na(idx), ], plan_areas)
  locs$planning_area <- plan_areas$planning_area[idx]
  locs$region        <- plan_areas$region[idx]
  locs
}

# Lighter version for the leaflet map (simplified to ~20 m, back to lon/lat)
planning_areas_map <- plan_areas |>
  st_simplify(dTolerance = 20, preserveTopology = TRUE) |>
  st_transform(4326)


# ============================================================
# 2. MRT / LRT EXITS (LTA) + OPENING DATES
# ============================================================

# A few exits are labelled with a station code instead of a name
CODE_NAMES <- c(
  "CC9"  = "PAYA LEBAR MRT STATION",          # new exits at an existing station
  "DT18" = "TELOK AYER MRT STATION",          # new exits at an existing station
  "DT4"  = "HUME MRT STATION",
  "NE18" = "PUNGGOL COAST MRT STATION",
  "CC30" = "KEPPEL MRT STATION",
  "CC31" = "CANTONMENT MRT STATION",
  "CC32" = "PRINCE EDWARD ROAD MRT STATION"
)

openings <- read_csv(f$openings, col_types = cols(opened = col_date(), .default = "c"))

exits_raw <- st_read(f$mrt, quiet = TRUE) |> st_zm(drop = TRUE)
ll <- st_coordinates(exits_raw)
exits <- exits_raw |>
  mutate(lon = ll[, 1], lat = ll[, 2]) |>
  st_transform(SVY21)
xy <- st_coordinates(exits)
exits <- exits |>
  st_drop_geometry() |>
  mutate(
    ex = xy[, 1], ey = xy[, 2],
    station_raw = squish_up(STATION_NA),
    station     = coalesce(unname(CODE_NAMES[station_raw]), station_raw),
    exit_code   = EXIT_CODE,
    rail_type   = str_extract(station, "MRT|LRT")
  ) |>
  # Remove exact duplicates (same station, exit and point, to the nearest metre)
  mutate(rx = round(ex), ry = round(ey)) |>
  distinct(station, exit_code, rx, ry, .keep_all = TRUE) |>
  left_join(select(openings, station, line, stage, opened), by = "station") |>
  mutate(opened_known = !is.na(opened),
         opened = coalesce(opened, as.Date("1900-01-01"))) |>   # not listed = open before our data
  select(station, exit_code, rail_type, line, stage, opened, opened_known, lon, lat, ex, ey)

not_found <- setdiff(openings$station, exits$station)
if (length(not_found)) warning("Stations in mrt_station_openings.csv not found in the exits file: ",
                               paste(not_found, collapse = ", "))

# Each date a new station opened starts a new "epoch"; a contract uses the epoch of its month.
epochs <- sort(unique(exits$opened))
epoch_of <- function(month) epochs[findInterval(month, epochs)]

# Nearest open exit for each location in each epoch. locs needs loc_id, x, y.
nearest_mrt <- function(locs) {
  D <- sqrt(outer(locs$x, exits$ex, "-")^2 + outer(locs$y, exits$ey, "-")^2)
  map_dfr(epochs, \(e) {
    open <- which(exits$opened <= e)
    k    <- open[max.col(-D[, open, drop = FALSE], ties.method = "first")]
    tibble(loc_id = locs$loc_id, epoch = e,
           dist_mrt_m      = round(D[cbind(seq_len(nrow(locs)), k)]),
           nearest_station = str_to_title(str_remove(exits$station[k], " (MRT|LRT) STATION$")))
  })
}

mrt_band <- function(d) cut(d, c(0, 300, 600, 1000, Inf), right = FALSE,
                            labels = c("Under 300 m", "300-600 m", "600 m-1 km", "Over 1 km"))


# ============================================================
# 3. HDB: block location + building age + planning area + MRT
# ============================================================

block_info <- read_csv(f$blocks, col_types = cols(.default = "c")) |>
  filter(residential == "Y") |>
  transmute(blk_no = squish_up(blk_no), street = squish_up(street),
            year_completed = as.integer(year_completed),
            block_height   = as.integer(max_floor_lvl))

blocks <- readRDS(f$coords) |>
  filter(!is.na(x), !is.na(y)) |>
  select(blk_no, street, lat, lon, x, y, postal) |>
  inner_join(block_info, by = c("blk_no", "street")) |>
  mutate(loc_id = row_number()) |>
  add_planning_area()

hdb <- readRDS(f$hdb_rent) |>
  mutate(blk_no = squish_up(block), street = squish_up(street_name)) |>
  left_join(blocks, by = c("blk_no", "street"))

no_loc <- filter(hdb, is.na(loc_id))
message(sprintf("HDB: %d of %d contracts (%.2f%%) have no block location. KEPT, with location fields left blank:",
                nrow(no_loc), nrow(hdb), 100 * nrow(no_loc) / nrow(hdb)))
print(count(no_loc, street, sort = TRUE))

hdb_nearest <- nearest_mrt(blocks)

hdb_plus <- hdb |>
  mutate(
    has_location = !is.na(loc_id),
    building_age = year(month) - year_completed,
    building_age = if_else(building_age < 0, NA_integer_, as.integer(building_age)),
    age_band = cut(building_age, c(-Inf, 10, 20, 30, 40, Inf), right = FALSE,
                   labels = c("Under 10 years", "10-19 years", "20-29 years", "30-39 years", "40+ years")),
    approx_lease_left = pmax(99L - building_age, 0L),
    epoch = epoch_of(month)
  ) |>
  left_join(hdb_nearest, by = c("loc_id", "epoch")) |>
  mutate(mrt_band = mrt_band(dist_mrt_m)) |>
  select(month, town, town_label, block, street_name, flat_type, monthly_rent,
         has_location, planning_area, region, lat, lon, x, y, postal,
         year_completed, building_age, age_band, approx_lease_left, block_height,
         dist_mrt_m, nearest_station, mrt_band)


# ============================================================
# 4. PRIVATE: planning area + MRT (URA already gives x, y in SVY21)
# ============================================================

private <- readRDS(f$priv_rent)

priv_locs <- private |>
  filter(!is.na(x), !is.na(y)) |>
  distinct(x, y) |>
  mutate(loc_id = row_number()) |>
  add_planning_area()

# Fill in lon/lat for the map if 01_clean_data.R was run without sf
if (all(is.na(private$lon))) {
  ll_p <- st_as_sf(priv_locs, coords = c("x", "y"), crs = SVY21) |> st_transform(4326) |> st_coordinates()
  priv_locs$lon_fill <- ll_p[, 1]
  priv_locs$lat_fill <- ll_p[, 2]
}

priv_nearest <- nearest_mrt(priv_locs)

private_plus <- private |>
  left_join(priv_locs, by = c("x", "y")) |>
  mutate(epoch = epoch_of(month)) |>
  left_join(priv_nearest, by = c("loc_id", "epoch")) |>
  mutate(mrt_band = mrt_band(dist_mrt_m))
if ("lon_fill" %in% names(private_plus)) {
  private_plus <- private_plus |> mutate(lon = lon_fill, lat = lat_fill) |> select(-lon_fill, -lat_fill)
}
private_plus <- select(private_plus, -loc_id, -epoch)


# ============================================================
# 5. CHECKS
# ============================================================

message("\n---- CHECKS ----")
message(sprintf("HDB: %s contracts (all kept) | location known: %.1f%% | building age known: %.1f%% | MRT distance: %.1f%%",
                format(nrow(hdb_plus), big.mark = ","),
                100 * mean(hdb_plus$has_location),
                100 * mean(!is.na(hdb_plus$building_age)), 100 * mean(!is.na(hdb_plus$dist_mrt_m))))
message(sprintf("Private: %s contracts | planning area known: %.1f%% | MRT distance: %.1f%%",
                format(nrow(private_plus), big.mark = ","),
                100 * mean(!is.na(private_plus$planning_area)), 100 * mean(!is.na(private_plus$dist_mrt_m))))

message("\nHDB 'CENTRAL' town now split into these planning areas:")
print(hdb_plus |> filter(town == "CENTRAL") |> count(planning_area, sort = TRUE))

message("\nHDB towns whose blocks fall in more than one planning area (normal near borders):")
print(hdb_plus |> distinct(town, planning_area) |> count(town, name = "n_areas") |> filter(n_areas > 1))

message("\nBuilding age (HDB contracts):")
print(count(hdb_plus, age_band))

message("\nMRT distance bands:")
print(bind_rows(HDB = count(hdb_plus, mrt_band), Private = count(private_plus, mrt_band), .id = "market"))

far <- sum(hdb_plus$dist_mrt_m > 5000, na.rm = TRUE) + sum(private_plus$dist_mrt_m > 5000, na.rm = TRUE)
if (far > 0) warning(far, " contracts are more than 5 km from any MRT/LRT exit. Check their coordinates.")


# ============================================================
# 6. SAVE
# ============================================================

saveRDS(hdb_plus,           "hdb_rentals_plus.rds")
saveRDS(private_plus,       "private_rentals_plus.rds")
saveRDS(planning_areas_map, "planning_areas.rds")
saveRDS(select(exits, -ex, -ey), "mrt_exits.rds")
message("\nSaved hdb_rentals_plus.rds, private_rentals_plus.rds, planning_areas.rds, mrt_exits.rds")
