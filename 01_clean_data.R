# 01_clean_data.R
# Cleans the three raw files and saves fast-loading .rds files for the Shiny app.
# Everything lives in ONE folder (your working directory): no sub-folders needed.
# Run:  source("01_clean_data.R")   (re-run whenever the raw data changes)
#
# INPUTS  (in your working directory, next to this script):
#   ura_rental_all.csv                      <- made by ura_pull_rentals.R
#   RentingOutofFlatsfromJan2021.csv        <- data.gov.sg (HDB)
#   PrivateResidentialPropertyRentalIndexBaseQuarter2009Q1100Quarterly.csv  <- data.gov.sg
#   (change the three file names below if yours are called something else)
#
# OUTPUTS (saved in the same working directory):
#   private_rentals.rds, hdb_rentals.rds, rental_index.rds
#
# DECISIONS BAKED IN (change on purpose, not by accident):
#   - Exact duplicate rows are KEPT. URA gives no unit ID, so two identical rows are usually
#     two real contracts (same project, size band, rent and month). Dropping them would
#     delete ~8% of private contracts.
#   - Outliers are KEPT. The benchmarks use percentiles (p25 / median / p75), which are robust.
#   - Price per sqft (psf) is only computed for condos and ECs, whose size bands are 100 sqft
#     wide. Landed bands are 500 sqft wide, so psf would be too crude. Open-ended bands
#     (">3000", "<=1000") also get no psf.
#   - Floor level is not in any public rental data, so it does not appear here.

library(tidyverse)   # dplyr, readr, stringr, lubridate, ...

ura_file   <- "ura_rental_all.csv"
hdb_file   <- "RentingOutofFlatsfromJan2021.csv"
index_file <- "PrivateResidentialPropertyRentalIndexBaseQuarter2009Q1100Quarterly.csv"

for (f in c(ura_file, hdb_file, index_file)) {
  if (!file.exists(f)) {
    stop("Can't find '", f, "' in your working directory:\n  ", getwd(),
         "\nMove the file there, or run setwd() to the folder that has it.")
  }
}


# ============================================================
# 1. PRIVATE RENTAL CONTRACTS (URA)
# ============================================================

private <- read_csv(ura_file, col_types = cols(.default = "c"), na = c("", "NA")) |>
  mutate(
    rent  = as.numeric(rent),
    
    # leaseDate is "mmyy", e.g. "0922" = Sep 2022
    month = as.Date(paste0("20", str_sub(leaseDate, 3, 4), "-", str_sub(leaseDate, 1, 2), "-01")),
    
    # Bedrooms: "00" is a data error -> NA; "03"/"05" become 3/5; 5 or more is grouped as "5+"
    bedrooms     = suppressWarnings(as.integer(noOfBedRoom)),
    bedrooms     = if_else(bedrooms == 0L, NA_integer_, bedrooms),
    bedrooms_grp = case_when(
      is.na(bedrooms) ~ NA_character_,
      bedrooms >= 5L  ~ "5+",
      TRUE            ~ as.character(bedrooms)
    ),
    
    property_group = case_when(
      propertyType == "Non-landed Properties" ~ "Condo / apartment",
      propertyType == "Executive Condominium" ~ "Executive condominium",
      TRUE                                    ~ "Landed"
    ),
    
    # Size band, e.g. "700-800". Open-ended bands (">3000", "<=1000") have no midpoint.
    area_band = areaSqft,
    band_lo   = suppressWarnings(as.numeric(str_extract(areaSqft, "^\\d+(?=-)"))),
    band_hi   = suppressWarnings(as.numeric(str_extract(areaSqft, "(?<=-)\\d+$"))),
    sqft_mid  = (band_lo + band_hi) / 2,
    area_band_order = case_when(          # for sorting the size-band dropdown
      str_starts(areaSqft, "<=") ~ 0,
      str_starts(areaSqft, ">")  ~ as.numeric(str_extract(areaSqft, "\\d+")) + 0.5,
      TRUE                       ~ band_lo
    ),
    
    # Approximate rent per sqft per month, using the middle of the size band
    psf = if_else(property_group != "Landed", rent / sqft_mid, NA_real_),
    
    x = as.numeric(x),
    y = as.numeric(y)
  )

# Convert URA's SVY21 coordinates (EPSG:3414) to longitude/latitude for the map.
# Needs the sf package: install.packages("sf")   (the script still works without it)
if (requireNamespace("sf", quietly = TRUE)) {
  xy  <- private |> filter(!is.na(x), !is.na(y)) |> distinct(x, y)
  pts <- sf::st_as_sf(xy, coords = c("x", "y"), crs = 3414, remove = FALSE) |>
    sf::st_transform(4326)
  ll  <- sf::st_coordinates(pts)
  xy$lon <- ll[, 1]
  xy$lat <- ll[, 2]
  private <- left_join(private, xy, by = c("x", "y"))
} else {
  message("sf package not installed: map coordinates (lon/lat) skipped. install.packages('sf') to add them.")
  private$lon <- NA_real_
  private$lat <- NA_real_
}

private <- private |>
  select(
    month, quarter = refPeriod, project, street, district,
    property_type = propertyType, property_group,
    bedrooms, bedrooms_grp, area_band, area_band_order, sqft_mid,
    rent, psf, x, y, lon, lat
  )

message("PRIVATE: ", format(nrow(private), big.mark = ","), " contracts, ",
        format(min(private$month)), " to ", format(max(private$month)))
message("  psf available for ", round(100 * mean(!is.na(private$psf)), 1), "% of contracts; ",
        "bedrooms for ", round(100 * mean(!is.na(private$bedrooms_grp)), 1), "%; ",
        "map coordinates for ", round(100 * mean(!is.na(private$lon)), 1), "%")


# ============================================================
# 2. HDB RENTAL CONTRACTS (data.gov.sg)
#    Fields: month, town, block, street, flat type, rent. No floor area, floor level or lease term.
# ============================================================

hdb <- read_csv(hdb_file, col_types = cols(.default = "c")) |>
  mutate(
    month        = ym(rent_approval_date),        # "2026-09" -> 2026-09-01
    monthly_rent = as.numeric(monthly_rent),
    flat_type    = factor(flat_type,
                          levels = c("1-ROOM", "2-ROOM", "3-ROOM", "4-ROOM", "5-ROOM", "EXECUTIVE")),
    town_label   = str_to_title(town)
  ) |>
  select(month, town, town_label, block, street_name, flat_type, monthly_rent)

stopifnot(!anyNA(hdb$month), !anyNA(hdb$monthly_rent))
if (anyNA(hdb$flat_type)) warning("Some HDB flat types did not match the expected list; check unique(hdb$flat_type).")

message("HDB: ", format(nrow(hdb), big.mark = ","), " contracts, ",
        format(min(hdb$month)), " to ", format(max(hdb$month)), "; ",
        n_distinct(hdb$town), " towns")


# ============================================================
# 3. PRIVATE RENTAL INDEX (data.gov.sg), trend lines only
#    Index (2009-Q1 = 100), NOT dollar rents. "All Residential" stops after 2021-Q2,
#    so for trends use the "Landed" and "Non-Landed" series.
# ============================================================

rental_index <- read_csv(index_file, col_types = cols(.default = "c")) |>
  mutate(
    index         = as.numeric(index),
    quarter_start = make_date(as.integer(str_sub(quarter, 1, 4)),
                              (as.integer(str_sub(quarter, 7, 7)) - 1L) * 3L + 1L,
                              1L)
  ) |>
  select(quarter, quarter_start, property_type, locality, index)

message("INDEX: ", nrow(rental_index), " rows, ", format(min(rental_index$quarter_start)),
        " to ", format(max(rental_index$quarter_start)))


# ============================================================
# 4. SAVE
# ============================================================

saveRDS(private,      "private_rentals.rds")
saveRDS(hdb,          "hdb_rentals.rds")
saveRDS(rental_index, "rental_index.rds")
message("Saved private_rentals.rds, hdb_rentals.rds, rental_index.rds to ", getwd())