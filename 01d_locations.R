# 01d_locations.R
# Gives every private rental contract a location_id: the real place the contract belongs to.
# Run AFTER 01b_clean_new_data.R (it reads private_rentals_plus.rds) and BEFORE 03_backtest.R and the app.
#   source("01d_locations.R")
#
# WHY: URA's "project" column is not a reliable place. Some contracts carry a generic name
# ("NON-LANDED HOUSING DEVELOPMENT", "LANDED HOUSING DEVELOPMENT"), so one name covers unrelated
# sites kilometres apart. Grouping by name alone mixes them on the map and in the benchmark.
#
# RULE (same as Roy's version):
#   - Condo / EC contracts are grouped by project name + street + district + property type.
#   - If a group's contracts are more than 500 m apart, it is NOT one development: split it by coordinate.
#     (500 m is a review heuristic, not proof. Check location_review.csv.)
#   - Landed homes are always separate by coordinate.
#   - Contracts without coordinates stay grouped with their name/street so they still count.
#   - No contract is dropped. The map position of a location is its most frequently reported
#     coordinate pair (not an average).
#
# INPUT   private_rentals_plus.rds
# OUTPUT  private_rentals_loc.rds    all contracts, plus location_id (the app and 03_backtest.R read this)
#         private_locations.rds      one row per location: id, label, coordinates, contracts, spread
#         location_review.csv        condo/EC groups that were split, for a manual check

library(tidyverse)

SPLIT_M <- 500
if (!file.exists("private_rentals_plus.rds")) stop("Can't find private_rentals_plus.rds in ", getwd())
private <- readRDS("private_rentals_plus.rds")
stopifnot(all(c("project", "street", "district", "property_group", "x", "y", "lon", "lat") %in% names(private)))
n_rows <- nrow(private)

txt <- function(x) str_to_upper(str_squish(x))
private <- private |> mutate(.proj = txt(project), .street = txt(street))

keys       <- c(".proj", ".street", "district", "property_group")
group_keys <- c(keys, "point_key")

spread <- private |>
  filter(is.finite(x), is.finite(y)) |>
  group_by(across(all_of(keys))) |>
  summarise(spread_m = sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2), .groups = "drop")

private <- private |>
  left_join(spread, by = keys) |>
  mutate(split_point = property_group == "Landed" | coalesce(spread_m > SPLIT_M, FALSE),
         point_key   = if_else(split_point, paste(x, y, sep = ":"), "development"))

# Most frequently reported coordinate pair of each location
reps <- private |>
  filter(is.finite(x), is.finite(y)) |>
  count(across(all_of(c(group_keys, "x", "y", "lon", "lat"))), name = "coordinate_records") |>
  group_by(across(all_of(group_keys))) |>
  arrange(desc(coordinate_records), x, y, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(-coordinate_records)

locations <- private |>
  distinct(across(all_of(group_keys))) |>
  arrange(across(all_of(group_keys))) |>
  mutate(location_id = sprintf("P%05d", row_number())) |>
  left_join(reps, by = group_keys) |>
  left_join(spread, by = keys)

private <- private |>
  left_join(select(locations, all_of(group_keys), location_id), by = group_keys)

contracts <- count(private, location_id, name = "contracts")

locations <- locations |>
  left_join(contracts, by = "location_id") |>
  transmute(location_id, project = .proj, street = .street, district, property_group,
            x, y, lon, lat, spread_m, contracts,
            generic = str_detect(.proj, "HOUSING DEVELOPMENT$"),   # URA's placeholder names
            label   = paste(str_to_title(.proj), str_to_title(.street), sep = " | "))
# Same label twice in one district + property type: add the id so the two can be told apart
dup <- duplicated(paste(locations$district, locations$property_group, locations$label)) |
  duplicated(paste(locations$district, locations$property_group, locations$label), fromLast = TRUE)
locations$label[dup] <- paste(locations$label[dup], locations$location_id[dup], sep = " | ")
locations <- locations |>
  mutate(dropdown_label = paste0(label, " (", format(contracts, big.mark = ","), " contracts)"))

private <- private |> select(-.proj, -.street, -spread_m, -split_point, -point_key)

stopifnot(nrow(private) == n_rows, !anyNA(private$location_id), !anyDuplicated(locations$location_id))

saveRDS(private,   "private_rentals_loc.rds")
saveRDS(locations, "private_locations.rds")

# Condo / EC groups that were split, so a person can check them
review <- locations |>
  filter(property_group != "Landed", is.finite(spread_m), spread_m > SPLIT_M) |>
  arrange(desc(contracts)) |>
  select(location_id, district, property_group, project, street, contracts, spread_m, generic, lon, lat)
write_csv(review, "location_review.csv")

message("Private contracts: ", format(n_rows, big.mark = ","), " (all kept)")
message("Locations: ", format(nrow(locations), big.mark = ","), " (",
        paste(names(table(locations$property_group)), table(locations$property_group), collapse = ", "), ")")
message("Contracts without coordinates: ", format(sum(!is.finite(private$x)), big.mark = ","),
        " (kept; they count in district-level benchmarks but have no map point)")
message("Condo/EC groups split because contracts are over ", SPLIT_M, " m apart: ", nrow(review),
        " (see location_review.csv)")
message("Locations with 10+ contracts overall: ", sum(locations$contracts >= 10), " of ", nrow(locations),
        ". Landed locations are small: most fall back to the 1 km or district level.")
message("Saved private_rentals_loc.rds, private_locations.rds, location_review.csv")
