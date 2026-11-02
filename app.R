# app.R: Singapore Rent Check (DBA3702 team project)
#
# Flow:
#   1. Home: the user says whether they are a tenant or a landlord.
#   2. Rent check: ONE page with one search bar on top, filters under it, the answer
#      (big median + verdict / listing range) first, then a map of the surrounding blocks.
#      A Tenant / Landlord toggle at the top switches the result cards without losing the search.
#   3. Data & limits: unchanged.
# The tenant and landlord views call the SAME benchmark functions from 02_benchmark.R.
#
# FILES (all in this one folder):
#   app.R        <- this file
#   02_benchmark.R
#   hdb_rentals_plus.rds, private_rentals_plus.rds, planning_areas.rds, mrt_exits.rds  <- 01b_clean_new_data.R
#   private_rentals_loc.rds, private_locations.rds  <- 01d_locations.R (run after 01b)
#   rental_index.rds        <- 01_clean_data.R
#   backtest_results.rds    <- 03_backtest.R (optional; shown on Data & limits)
#   amenities.rds, amenities.R  <- 01c_amenities.R (optional; MRT / hawker / school markers + nearby places)
#
# RUN: open this file in RStudio and click "Run App", or run shiny::runApp("app.R")

# Install any missing packages (only runs the first time)
for (pkg in c("shiny", "bslib", "tidyverse", "sf", "leaflet", "plotly", "shinyWidgets", "shinycssloaders", "httr2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
# Coloured value boxes (theme =) and tooltip() need bslib 0.6 or newer
if (packageVersion("bslib") < "0.6.0") install.packages("bslib")

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)

source("02_benchmark.R")                      # gives find_comparables(), tenant_check(), etc.
hdb            <- readRDS("hdb_rentals_plus.rds")
private        <- readRDS("private_rentals_loc.rds")      # from 01d_locations.R (adds location_id)

# Distance-to-MRT bands. 01b_clean_new_data.R stops at "Over 1 km"; split that into 1-2 km and over 2 km.
MRT_BREAKS <- c(0, 300, 600, 1000, 2000, Inf)
MRT_LABELS <- c("Under 300 m", "300-600 m", "600 m-1 km", "1-2 km", "Over 2 km")
recut_mrt  <- function(d) cut(d, MRT_BREAKS, right = FALSE, labels = MRT_LABELS)
hdb$mrt_band <- recut_mrt(hdb$dist_mrt_m)
if ("dist_mrt_m" %in% names(private)) private$mrt_band <- recut_mrt(private$dist_mrt_m)
locations      <- readRDS("private_locations.rds")        # one row per private location
planning_areas <- readRDS("planning_areas.rds")
mrt_exits      <- readRDS("mrt_exits.rds")
rental_index   <- readRDS("rental_index.rds")
backtest_results   <- if (file.exists("backtest_results.rds"))   readRDS("backtest_results.rds")   else NULL
backtest_breakdown <- if (file.exists("backtest_breakdown.rds")) readRDS("backtest_breakdown.rds") else NULL

HDB_AS_OF  <- max(hdb$month)
PRIV_AS_OF <- max(private$month)

# Nearby places (built by 01c_amenities.R). The app still runs without them.
source("amenities.R")
# Vector-safe labels: one label per amenity, including when routes are off.
describe_distance <- function(dist_m, walk_m = NA_real_, walk_min = NA_real_) {
  n <- length(dist_m)
  if (!n) return(character(0))
  walk_m <- rep_len(walk_m, n)
  walk_min <- rep_len(walk_min, n)
  est_min <- pmax(1, round(dist_m * WALK_FACTOR / WALK_M_PER_MIN))
  result <- sprintf("%s \u00b7 ~%d min walk", fmt_dist(dist_m), as.integer(est_min))
  routed <- is.finite(walk_m) & is.finite(walk_min) & walk_m >= 0 & walk_min >= 0
  result[routed] <- sprintf("%s on foot \u00b7 %d min",
                            fmt_dist(walk_m[routed]), as.integer(round(walk_min[routed])))
  result[!is.finite(dist_m) & !routed] <- "Distance unavailable"
  result
}
amen         <- if (file.exists("amenities.rds")) readRDS("amenities.rds") else NULL
HAS_AMEN     <- !is.null(amen)
AMEN_AS_OF   <- max(HDB_AS_OF, PRIV_AS_OF)
amen_kinds   <- if (HAS_AMEN) intersect(names(AMEN_LABEL), unique(amen$kind)) else character(0)
amen_choices <- setNames(amen_kinds, unname(AMEN_LABEL[amen_kinds]))

# Font Awesome names for each kind of place (see the note below on the two Font Awesome versions)
AMEN_STYLE$hawker$icon    <- "utensils"
AMEN_STYLE$foodcourt$icon <- "utensils"
AMEN_STYLE$petrol$icon    <- "gas-pump"
# The app ends up with two copies of Font Awesome: Shiny's (v6) and leaflet's (v4.7). The fork-and-knife glyph is
# called "utensils" in v6 and "cutlery" in v4.7, so either name alone showed a blank pin in one of them.
# Giving the icon BOTH class names works whichever copy wins: each copy draws the one name it knows.
AMEN_STYLE$hawker$alt    <- "cutlery"
AMEN_STYLE$foodcourt$alt <- "cutlery"
AMEN_STYLE$petrol$icon   <- "tint"          # "tint" exists in both versions
# Leaflet marker for a kind of place: white glyph on a coloured pin, like the other places
amen_marker_icon <- function(st) {
  makeAwesomeIcon(icon = st$icon, library = "fa", markerColor = st$colour, iconColor = "white",
                  extraClasses = if (!is.null(st$alt)) paste0("fa-", st$alt) else NULL)
}

# Card heading: a short bold title with an optional one-line grey subtitle
ch_title <- function(title, sub = NULL) {
  div(class = "ch", div(class = "ch-title", title), if (!is.null(sub)) div(class = "ch-sub", sub))
}
# Budget colours, used on the map and in the "fits your budget" table
BUDGET_COL <- c(likely = "#0f7a63", possible = "#f39c12", above = "#bdc3c7")
budget_fill <- function(share) unname(ifelse(share >= 0.5, BUDGET_COL["likely"],
                                      ifelse(share >= 0.25, BUDGET_COL["possible"], BUDGET_COL["above"])))
add_budget_legend <- function(proxy, budget) {
  addLegend(proxy, layerId = "property_legend", position = "bottomleft", group = "points",
            colors = unname(BUDGET_COL),
            labels = c("Likely: half+ of rentals at or under", "Possible: a quarter to half", "Mostly above"),
            title = sprintf("Your budget: %s", money(budget)))
}

stat <- function(label, value) div(div(class = "stat-label", label), div(class = "stat-value", value))

# Same colour + icon for each kind of place everywhere: map pins, map legend, checkboxes, nearby list.
# Hex values match the leaflet awesome-marker colours used in AMEN_STYLE (amenities.R).
PIN_HEX    <- c(purple = "#D252B9", orange = "#F69730", green = "#72B026", blue = "#38AADD",
                pink = "#FF91EA", cadetblue = "#436978", red = "#D63E2A")
AMEN_SHORT <- c(mrt = "MRT", hawker = "Hawker centre", school = "School", mall = "Mall",
                foodcourt = "Food court", petrol = "Petrol station")
amen_pin <- function(colour, icon, alt = NULL) {
  sprintf('<span class="amen-pin" style="background:%s"><i class="fa fa-%s%s"></i></span>',
          PIN_HEX[[colour]], icon, if (!is.null(alt)) paste0(" fa-", alt) else "")
}
amen_kind_pin <- function(k) amen_pin(AMEN_STYLE[[k]]$colour, AMEN_STYLE[[k]]$icon, AMEN_STYLE[[k]]$alt)
amen_legend_html <- function(kinds) {
  rows <- vapply(kinds, function(k) sprintf("<div>%s%s</div>", amen_kind_pin(k), AMEN_LABEL[[k]]), "")
  paste0('<div class="amen-legend"><strong>Map pins</strong>',
         sprintf("<div>%sSelected home</div>", amen_pin("red", "home")),
         paste(rows, collapse = ""), "</div>")
}
amen_choice_names <- lapply(amen_kinds, function(k) HTML(paste0(amen_kind_pin(k), AMEN_LABEL[[k]])))
HAS_OSM      <- HAS_AMEN && any(amen$kind %in% c("mall", "petrol", "foodcourt"))
WALK_LIVE    <- nzchar(Sys.getenv("ONEMAP_TOKEN"))    # live walking routes need a OneMap token (see amenities.R)
WALK_READY   <- WALK_LIVE || length(ls(.walk_cache)) > 0   # ... or routes saved earlier in data_cache/walk_cache.rds

# Split once at startup, so each input change only filters one town / district (~10k rows)
hdb_by_town         <- split(hdb, hdb$town)
private_by_district <- split(private, private$district)

money <- function(x) paste0("$", format(round(x), big.mark = ",", trim = TRUE))
pct   <- function(x, digits = 0) sprintf(paste0("%.", digits, "f%%"), 100 * x)
signed_pct <- function(x) if (is.na(x)) "Not enough data" else sprintf("%+.1f%%", 100 * x)
or_any <- function(x) if (is.null(x)) "any" else x


# ============================================================
# Choices for the dropdowns
# ============================================================

hdb_towns    <- hdb |> distinct(town, town_label) |> arrange(town_label)
town_choices <- setNames(hdb_towns$town, hdb_towns$town_label)
flat_choices <- levels(hdb$flat_type)

districts        <- sort(unique(private$district))
# Postal districts are numbers in the data; give each its general location (URA list of postal districts)
DISTRICT_AREAS <- c(
  "01" = "Raffles Place, Marina, Cecil",          "02" = "Tanjong Pagar, Chinatown",
  "03" = "Queenstown, Tiong Bahru, Alexandra",     "04" = "Harbourfront, Telok Blangah, Sentosa",
  "05" = "Pasir Panjang, Clementi, West Coast",    "06" = "City Hall, Clarke Quay",
  "07" = "Bugis, Beach Road, Golden Mile",         "08" = "Little India, Farrer Park",
  "09" = "Orchard, River Valley",                  "10" = "Bukit Timah, Holland, Tanglin",
  "11" = "Newton, Novena, Thomson",                "12" = "Balestier, Toa Payoh, Serangoon",
  "13" = "Macpherson, Braddell, Potong Pasir",     "14" = "Geylang, Eunos, Paya Lebar",
  "15" = "Katong, Joo Chiat, Marine Parade",       "16" = "Bedok, Upper East Coast, Siglap",
  "17" = "Loyang, Changi",                         "18" = "Tampines, Pasir Ris",
  "19" = "Serangoon Garden, Hougang, Punggol, Sengkang", "20" = "Bishan, Ang Mo Kio, Thomson",
  "21" = "Upper Bukit Timah, Clementi Park, Ulu Pandan", "22" = "Jurong, Boon Lay, Tuas",
  "23" = "Bukit Panjang, Choa Chu Kang, Bukit Batok, Hillview", "24" = "Lim Chu Kang, Tengah",
  "25" = "Kranji, Woodlands",                      "26" = "Upper Thomson, Springleaf, Mandai",
  "27" = "Yishun, Sembawang",                      "28" = "Seletar, Yio Chu Kang"
)
# "D16 Bedok, Upper East Coast, Siglap" (falls back to "District 16" if a code is not in the list)
district_label <- function(d) {
  key  <- suppressWarnings(sprintf("%02d", as.integer(as.character(d))))
  area <- unname(DISTRICT_AREAS[key])
  ifelse(is.na(area), paste("District", d), paste0("D", key, " ", area))
}
district_choices <- setNames(districts, district_label(districts))

group_choices <- c("Condo / apartment", "Executive condominium", "Landed")
bed_choices   <- c("Any" = "any", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5+" = "5+")
age_choices   <- c("Any age" = "any", setNames(levels(hdb$age_band), levels(hdb$age_band)))
mrt_choices   <- c("Any distance" = "any", setNames(levels(hdb$mrt_band), levels(hdb$mrt_band)))
pa_choices    <- sort(planning_areas$planning_area)

# Floor-area bands that actually exist for each property type, sorted small to large
band_choices <- lapply(setNames(group_choices, group_choices), function(g) {
  d <- private[private$property_group == g, c("area_band", "area_band_order")]
  d <- d[!is.na(d$area_band) & !duplicated(d$area_band), ]
  d$area_band[order(d$area_band_order)]
})
with_any <- function(bands) c("Any size" = "any", setNames(bands, paste(bands, "sqft")))

# Floor area as a range slider. URA bands look like "700-800", "<=1000" or ">3000".
band_bounds <- function(b) {
  num <- function(x) suppressWarnings(as.numeric(x))
  tibble(band = b,
         lo = case_when(str_starts(b, "<=") ~ 0,
                        str_starts(b, ">")  ~ num(str_extract(b, "\\d+")),
                        TRUE                ~ num(str_extract(b, "^\\d+"))),
         hi = case_when(str_starts(b, "<=") ~ num(str_extract(b, "\\d+")),
                        str_starts(b, ">")  ~ Inf,
                        TRUE                ~ num(str_extract(b, "\\d+$"))))
}
NO_LIMIT <- "No limit"
range_ends <- function(x) c(x[1], x[length(x)])
size_ticks <- function(group) {
  bb    <- band_bounds(band_choices[[group]])
  edges <- sort(unique(c(bb$lo, bb$hi[is.finite(bb$hi)])))
  ticks <- format(edges, big.mark = ",", trim = TRUE)
  if (any(!is.finite(bb$hi))) ticks <- c(ticks, NO_LIMIT)
  ticks
}
tick_value <- function(t) if (identical(t, NO_LIMIT)) Inf else as.numeric(gsub(",", "", t))
# Slider range -> the URA bands that sit fully inside it ("any" when the whole range is picked)
band_from_range <- function(group, rng) {
  if (is.null(group) || length(rng) != 2) return("any")
  ticks <- size_ticks(group)
  if (identical(rng[1], ticks[1]) && identical(rng[2], ticks[length(ticks)])) return("any")
  lo <- tick_value(rng[1]); hi <- tick_value(rng[2])
  bb <- band_bounds(band_choices[[group]])
  out <- bb$band[bb$lo >= lo & bb$hi <= hi]
  if (!length(out)) out <- "none"                     # nothing fits: shows "not enough data"
  attr(out, "label") <- paste0(rng[1], if (is.finite(hi)) paste0(" to ", rng[2]) else "+", " sqft")
  out
}

# HDB blocks, with the facts we show as context (latest contract for each block)
block_info <- hdb |>
  arrange(desc(month)) |>
  distinct(town, block, street_name, .keep_all = TRUE) |>
  transmute(town, block, street_name, has_location, lon, lat, x, y, year_completed, block_height,
            dist_mrt_m, nearest_station,
            lease_left_now = pmax(99 - (year(HDB_AS_OF) - year_completed), 0),
            key   = paste(block, street_name, sep = "|"),
            label = paste("Blk", block, str_to_title(street_name))) |>
  arrange(street_name, block)
block_choices_by_town <- lapply(split(block_info, block_info$town), function(d) {
  town_lab <- names(town_choices)[match(d$town[1], town_choices)]
  c(setNames("any", paste("All blocks in", town_lab)), setNames(d$key, d$label))
})

# Private locations by district and property type (see 01d_locations.R). The dropdown value is the location_id.
project_choices <- function(district, group) {
  d <- locations[locations$district == district & locations$property_group == group, ]
  d <- d[order(d$label), ]
  c(setNames("any", paste("All projects in", district_label(district))), setNames(d$location_id, d$dropdown_label))
}
location_label <- function(id) {
  l <- locations$label[match(id, locations$location_id)]
  ifelse(is.na(l), id, l)
}

# Centres of towns / districts for "cheaper alternatives nearby"
hdb_centroids  <- area_centroids(hdb, "town")
priv_centroids <- area_centroids(private, "district")


# ============================================================
# Precomputed data for the Explore page (last 12 months)
# ============================================================

hdb12  <- hdb     |> filter(month > HDB_AS_OF  %m-% months(12))
priv12 <- private |> filter(month > PRIV_AS_OF %m-% months(12))
pa_with_hdb     <- unique(na.omit(hdb$planning_area))
pa_with_private <- unique(na.omit(private$planning_area))

mrt_stations <- mrt_exits |>
  filter(opened <= max(HDB_AS_OF, PRIV_AS_OF)) |>
  group_by(station) |>
  summarise(lon = mean(lon), lat = mean(lat), rail_type = first(rail_type),
            line = first(na.omit(line)), opened = first(opened),
            opened_known = first(opened_known), .groups = "drop") |>
  mutate(popup = paste0("<strong>", str_to_title(station), "</strong>",
                        if_else(is.na(line), "", paste0("<br/>", line)),
                        if_else(opened_known, paste0("<br/>Opened ", format(opened, "%d %b %Y")), "")))

area_stats <- function(d, value_col) {
  d |>
    filter(!is.na(planning_area), !is.na(.data[[value_col]])) |>
    group_by(planning_area) |>
    summarise(n = n(),
              p25    = unname(quantile(.data[[value_col]], 0.25)),
              median = median(.data[[value_col]]),
              p75    = unname(quantile(.data[[value_col]], 0.75)),
              .groups = "drop")
}


# ============================================================
# Helpers used by the server
# ============================================================

# Read the selected inputs into the `sel` list that find_comparables() expects
get_sel <- function(input, p) {
  g <- function(x) input[[paste0(p, x)]]
  if (identical(g("market"), "hdb")) {
    block <- street <- NULL
    if (is_set(g("block"))) {
      parts  <- str_split_fixed(g("block"), "\\|", 2)
      block  <- parts[1]
      street <- parts[2]
    }
    list(market = "hdb", town = g("town"), flat = g("flat"), block = block, street = street,
         age = g("age"), mrt = if (is_set(g("block"))) "any" else g("mrt"))
  } else {
    beds <- if (identical(g("group"), "Landed")) NULL else g("beds")
    list(market = "private", district = g("district"), group = g("group"), beds = beds,
         band = band_from_range(g("group"), g("band_rng")), project = g("project"), mrt = if (is_set(g("project"))) "any" else g("pmrt"))
  }
}

# Turn the selected inputs into a benchmark (the same function for both pages)
get_bench <- function(input, p) {
  sel <- get_sel(input, p)
  if (sel$market == "hdb") {
    req(sel$town, sel$flat)
    find_comparables(hdb, "hdb", sel, as_of = HDB_AS_OF, local = hdb_by_town[[sel$town]])
  } else {
    req(sel$district, sel$group)
    find_comparables(private, "private", sel, as_of = PRIV_AS_OF, local = private_by_district[[sel$district]])
  }
}

# Warn when a private search mixes all unit sizes (the back-test showed these ranges are much wider)
broad_warning <- function(sel) {
  if (sel$market != "private") return(NULL)
  if (identical(sel$group, "Landed")) {
    if (is_set(sel$band)) return(NULL)
    msg <- "Tip: choose a floor area. Without it, this range mixes small and very large landed homes, so it is wide."
  } else {
    if (is_set(sel$beds) || is_set(sel$band)) return(NULL)
    msg <- "Tip: choose the number of bedrooms or a floor area. Without them, this range mixes studios and penthouses, so it is wide and less reliable."
  }
  div(class = "alert alert-warning py-2", msg)
}

# Where an asking rent sits, counting half of the contracts at exactly that rent
position_text <- function(chk, amount) {
  if (is.na(chk$percentile)) return("")
  if (chk$equal > 0) {
    sprintf("%s sits at about the %s percentile: %s of comparable contracts were lower and %s were exactly %s.",
            money(amount), ordinal(round(100 * chk$percentile)), pct(chk$below), pct(chk$equal), money(amount))
  } else {
    sprintf("%s sits at about the %s percentile: %s of comparable contracts were lower.",
            money(amount), ordinal(round(100 * chk$percentile)), pct(chk$below))
  }
}
ordinal <- function(n) {
  suffix <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suffix)
}

# Size-adjusted estimate card (private, non-landed, unit size entered)
size_card <- function(est) {
  if (is.null(est)) return(NULL)
  if (!isTRUE(est$ok)) {
    return(card(card_body(sprintf("Only %d comparables have a rent per sqft, which is too few for a size-adjusted estimate.", est$n))))
  }
  card(card_body(
    strong(sprintf("Size-adjusted estimate for %s sqft: %s", format(est$sqft, big.mark = ","), money(est$estimate))), br(),
    sprintf("Typical %s to %s. Comparables' rent per sqft: median $%.2f (typical $%.2f to $%.2f), n = %d.",
            money(est$low), money(est$high), est$median_psf, est$p25_psf, est$p75_psf, est$n),
    if (!is.null(est$asking_psf)) tagList(br(), sprintf(
      "Your asking rent works out to $%.2f per sqft, about the %s percentile of comparables.",
      est$asking_psf, ordinal(round(100 * est$percentile)))),
    br(),
    span(class = "text-muted small", "Approximate: URA gives sizes as bands, so each contract's rent per sqft uses the middle of its band.")
  ))
}

# Plain-English names
cap1      <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
flat_nice <- function(f) str_to_sentence(tolower(f))                     # "4-ROOM" -> "4-room"
type_nice <- function(g, plural = FALSE) {
  base <- switch(g, "Condo / apartment" = "condo", "Executive condominium" = "EC", "Landed" = "landed home", tolower(g))
  if (plural) paste0(base, "s") else base
}
beds_nice <- function(b) if (identical(b, "5+")) "5+ bedroom " else paste0(b, "-bedroom ")
district_code <- function(d) sprintf("D%02d", suppressWarnings(as.integer(as.character(d))))

# Result title, e.g. "4-room HDB at Blk 650 Jln Tenaga, Bedok" or "3-bedroom condo in D05 Pasir Panjang, ..."
describe_segment <- function(sel) {
  if (sel$market == "hdb") {
    out <- if (is_set(sel$block)) {
      paste0(flat_nice(sel$flat), " HDB at Blk ", sel$block, " ", str_to_title(sel$street), ", ", town_name(sel$town))
    } else {
      paste0(flat_nice(sel$flat), " HDB in ", town_name(sel$town))
    }
    extra <- c(if (is_set(sel$age)) age_label(sel$age), if (is_set(sel$mrt)) mrt_label(sel$mrt))
  } else {
    beds  <- if (is_set(sel$beds)) beds_nice(sel$beds) else ""
    where <- if (is_set(sel$project)) paste0(" at ", location_label(sel$project)) else paste0(" in ", district_label(sel$district))
    out   <- cap1(paste0(beds, type_nice(sel$group), where))
    if (is_set(sel$band)) out <- paste0(out, ", ", attr(sel$band, "label") %||% paste(sel$band, "sqft"))
    extra <- if (is_set(sel$mrt)) mrt_label(sel$mrt)
  }
  if (length(extra)) out <- paste0(out, " (", paste(extra, collapse = ", "), ")")
  out
}

# "4-room flats", "3-bedroom condos"
home_phrase <- function(sel) {
  if (sel$market == "hdb") return(paste0(tolower(sel$flat), " flats"))
  paste0(if (is_set(sel$beds)) beds_nice(sel$beds) else "", type_nice(sel$group, plural = TRUE))
}
# Comparable level in plain words, e.g. "across Bedok", "on Jln Tenaga", "within 1 km of this block"
level_phrase <- function(b, sel) {
  lv    <- b$level
  extra <- sub("^Same (town|district)", "", lv)
  if (lv == "Same block") return("in this block")
  if (lv == "Same street") return(paste("on", str_to_title(sel$street)))
  if (lv == "Same project") return("in this project")
  if (startsWith(lv, "Within 1 km")) return(if (sel$market == "hdb") "within 1 km of this block" else "within 1 km of this project")
  if (startsWith(lv, "Same town")) return(paste0("across ", town_name(sel$town), extra))
  if (startsWith(lv, "Same district")) return(paste0("across ", district_code(sel$district), extra))
  tolower(lv)
}

# One plain line under the title, replacing the old "Comparables: Same town" badge
data_line <- function(b, sel) {
  s <- b$summary
  tagList(
    p(class = "data-line", icon("chart-simple"),
      sprintf(" Based on %s rentals of %s %s, last %d months.",
              format(s$n, big.mark = ","), home_phrase(sel), level_phrase(b, sel), s$window_months)),
    if (!is.null(b$note)) p(class = "text-warning small", b$note)
  )
}

recency_note <- function(b) {
  s   <- b$summary
  rec <- recency_info(b, rental_index)
  if (!isTRUE(s$widened) || is.null(rec)) return(NULL)
  tagList(
    p(class = "text-muted",
      sprintf("Fewer than %d contracts in the last 12 months, so this uses the last %d months. %s of these comparables are more than 12 months old.",
              MIN_N, s$window_months, pct(rec$old_share))),
    if (!is.na(rec$adj_median)) p(class = "text-muted",
      sprintf("Trend-adjusted median (an estimate): %s, scaling older rents by %s.", money(rec$adj_median), rec$method))
  )
}

context_note <- function(b) {
  txt <- context_line(b)
  if (is.null(txt)) return(NULL)
  p(class = "text-muted", txt)
}

# Background detail, folded away under a small "Why this range?" link
why_box <- function(b) {
  tags$details(
    class = "why",
    tags$summary("Why this range?"),
    div(class = "why-body",
        p(class = "text-muted", "The typical range is the middle half of these signed rents (25th to 75th percentile). We start with the most similar homes and widen the search until there are enough rentals. See Data & limits for details."),
        if (length(b$relaxed)) p(class = "text-muted",
                                 paste0("There weren't enough recent rentals with all your choices, so these were relaxed: ",
                                        paste(b$relaxed, collapse = ", "), ".")),
        context_note(b),
        recency_note(b))
  )
}

# Facts about the chosen home (block_height is the TALLEST floor in the block, not the flat's floor)
# Shortlist star: small star in tables, labelled button on cards
star_icon <- function(id, on) {
  tags$a(href = "#", class = paste("star", if (on) "on"), `data-id` = id, onclick = "return toggleShortlist(this);",
         title = if (on) "Remove from favourites" else "Add to favourites", if (on) "\u2605" else "\u2606")
}
star_button <- function(id, on) {
  tags$a(href = "#", class = paste("star-btn", if (on) "on"), `data-id` = id, onclick = "return toggleShortlist(this);",
         title = if (on) "Click to remove from favourites" else "Click to add to favourites",
         span(class = "star-glyph", if (on) "\u2605" else "\u2606"),
         if (on) " Remove from favourites" else " Add to favourites")
}
block_card <- function(sel, saved = character(0)) {
  if (sel$market == "hdb" && is_set(sel$block)) {
    bi <- block_info[block_info$town == sel$town & block_info$block == sel$block &
                       block_info$street_name == sel$street, ]
    if (nrow(bi) == 0 || !isTRUE(bi$has_location[1])) return(NULL)
    bi <- bi[1, ]
    return(card(
      card_header(class = "d-flex align-items-start gap-2",
                  ch_title("About this block", paste("Blk", bi$block, str_to_title(bi$street_name))),
                  div(class = "ms-auto", star_button(paste("blk", bi$town, bi$block, bi$street_name, sep = "|"),
                                                     paste("blk", bi$town, bi$block, bi$street_name, sep = "|") %in% saved))),
      card_body(div(class = "stat-grid",
        stat("Built", bi$year_completed),
        stat("Lease left", sprintf("~%d years", round(bi$lease_left_now))),
        stat("Tallest floor", bi$block_height),
        stat("Nearest MRT", paste0(str_to_title(bi$nearest_station), ", ", fmt_dist(bi$dist_mrt_m)))
      ), p(class = "text-muted small mb-0", "Lease is approximate (99 years minus block age). MRT distance is a straight line."))
    ))
  }
  if (sel$market == "private" && is_set(sel$project)) {
    pr <- private[private$location_id == sel$project, ]
    if (!nrow(pr)) return(NULL)
    return(card(
      card_header(class = "d-flex align-items-start gap-2",
                  ch_title("About this project", location_label(sel$project)),
                  div(class = "ms-auto", star_button(paste("prj", sel$project, sep = "|"),
                                                     paste("prj", sel$project, sep = "|") %in% saved))),
      card_body(div(class = "stat-grid",
        stat("District", district_label(pr$district[1])),
        stat("Type", pr$property_group[1]),
        stat("Nearest MRT", paste0(str_to_title(first(na.omit(pr$nearest_station))), ", ",
                                   fmt_dist(median(pr$dist_mrt_m, na.rm = TRUE)))),
        stat("Contracts on record", format(nrow(pr), big.mark = ","))
      ))
    ))
  }
  card(card_body(class = "text-muted",
                 "Pick a block or project (search above or click a dot on the map) to see its age, lease and nearest MRT."))
}

# The point to measure "places near this home" from. `p` is the Tenant/Landlord selection or the map click:
# an HDB block, or a private location (p$project is a location_id). A landed location is a coordinate
# URA reported, which can be an address point or an area, so treat its nearby places as approximate.
point_for <- function(p) {
  if (is.null(p)) return(NULL)   # also used to place the selected-home pin, so it works without amenities
  if (identical(p$market, "hdb")) {
    if (!is_set(p$block)) return(NULL)
    b <- block_info[block_info$town == p$town & block_info$block == p$block &
                      block_info$street_name == p$street, ]
    if (!nrow(b) || !isTRUE(b$has_location[1]) || !is.finite(b$x[1])) return(NULL)
    list(label = paste("Blk", b$block[1], str_to_title(b$street_name[1])),
         lon = b$lon[1], lat = b$lat[1], x = b$x[1], y = b$y[1])
  } else {
    if (!is_set(p$project)) return(NULL)
    b <- locations[locations$location_id == p$project, ]
    if (!nrow(b) || !is.finite(b$x[1])) return(NULL)
    list(label = b$label[1], lon = b$lon[1], lat = b$lat[1], x = b$x[1], y = b$y[1])
  }
}

# Card listing the nearest places of each kind. `near` comes from nearest_amenities() / add_walking().
nearby_card <- function(pt, near) {
  if (is.null(pt) || is.null(near) || !nrow(near)) return(NULL)
  kinds <- intersect(names(AMEN_LABEL), unique(near$kind))
  blocks <- lapply(kinds, function(k) {
    d <- near[near$kind == k, ]
    d <- d[order(coalesce(d$walk_m, d$dist_m * WALK_FACTOR)), ]
    div(div(class = "nearby-kind", HTML(amen_kind_pin(k)), AMEN_LABEL[[k]]),
        tags$ul(class = "nearby-list", lapply(seq_len(nrow(d)), function(i)
          tags$li(d$name[i], br(), span(class = "text-muted small", d$dist_text[i])))))
  })
  card(card_header(ch_title("Nearby places", pt$label)),
       card_body(div(class = "nearby-grid", blocks),
                 p(class = "text-muted small mb-0",
                   "Today's locations. Walk times are rough estimates unless walking routes are on. A nearby school doesn't guarantee a place.")))
}

# Same card for the Tenant / Landlord pages (straight-line only, so the page stays fast)
nearby_card_for <- function(sel) {
  if (!HAS_AMEN || !length(amen_kinds)) return(NULL)
  pt <- point_for(sel)
  if (is.null(pt)) return(NULL)
  nearby_card(pt, nearest_amenities(amen, pt$x, pt$y, kinds = amen_kinds, n = 3, as_of = AMEN_AS_OF))
}

# Range strip: where a rent sits among comparable signed rents.
# Green = below the typical range, grey = typical range (25th-75th percentile), red = above it.
# Headline: "$380 below median" + "cheaper than 77% of similar rentals".
range_strip <- function(bench, value, who = "You", role = c("tenant", "landlord")) {
  role <- match.arg(role)
  s   <- bench$summary
  v   <- bench$values
  chk <- tenant_check(bench, value)
  pct <- chk$percentile
  gap <- value - s$median

  # Scale: 5th-95th percentile, widened to include the pin, padded a little
  lo <- min(unname(quantile(v, 0.05)), value, s$p25)
  hi <- max(unname(quantile(v, 0.95)), value, s$p75)
  pad <- 0.08 * (hi - lo)
  if (pad <= 0) pad <- 50
  lo <- lo - pad; hi <- hi + pad
  pos <- function(x) 100 * (x - lo) / (hi - lo)
  p25 <- pos(s$p25); med <- pos(s$median); p75 <- pos(s$p75); you <- pos(value)

  # Range-end labels are also in the legend, so drop them if they would crowd the median label
  # The arrow and dot always sit exactly at the rent; only the text label shifts so it stays inside the strip
  pin_shift <- if (you < 12) "translateX(-14px)" else if (you > 88) "translateX(calc(-100% + 14px))" else "translateX(-50%)"

  p25_lab <- if (med - p25 < 12) "" else money(s$p25)
  p75_lab <- if (p75 - med < 12) "" else money(s$p75)

  zone <- if (pct < 0.25) "below" else if (pct <= 0.75) "typical" else "above"
  pin_col <- c(below = "#3B6D11", typical = "#2c3e50", above = "#A32D2D")[[zone]]

  gap_txt <- if (gap > 0) paste(money(gap), "above median") else if (gap < 0) paste(money(-gap), "below median") else "At the median"
  share_txt <- if (role == "tenant") {
    if (gap <= 0) sprintf("cheaper than %d%% of similar rentals", round(100 * (1 - pct))) else sprintf("pricier than %d%% of similar rentals", round(100 * pct))
  } else {
    if (gap >= 0) sprintf("higher than %d%% of similar rentals", round(100 * pct)) else sprintf("lower than %d%% of similar rentals", round(100 * (1 - pct)))
  }

  strip <- sprintf(
    '<div class="rs">
       <div class="rs-pin" style="left:%.1f%%;transform:%s;color:%s">%s: %s</div>
       <div class="rs-arrow" style="left:%.1f%%;border-top-color:%s"></div>
       <div class="rs-bar">
         <div style="width:%.1f%%;background:#C0DD97"></div>
         <div style="width:%.1f%%;background:#E3E1D9"></div>
         <div style="flex:1;background:#F7C1C1"></div>
       </div>
       <div class="rs-med" style="left:%.1f%%"></div>
       <div class="rs-dot" style="left:%.1f%%;border-color:%s"></div>
     </div>
     <div class="rs-labels">
       <span style="left:%.1f%%;transform:translateX(-100%%)">%s</span>
       <span class="rs-medlab" style="left:%.1f%%">Median %s</span>
       <span style="left:%.1f%%">%s</span>
     </div>',
    you, pin_shift, pin_col, who, money(value),
    you, pin_col,
    p25, p75 - p25,
    med,
    you, pin_col,
    p25 - 1, p25_lab,
    med, money(s$median),
    p75 + 1, p75_lab)

  tagList(
    div(class = "rs-head",
        span(class = "rs-gap", style = sprintf("color:%s", pin_col), gap_txt),
        span(class = "rs-share", HTML("&middot;"), share_txt)),
    HTML(strip),
    div(class = "rs-legend",
        span(span(class = "rs-key", style = "background:#C0DD97"), "Cheaper than most"),
        span(span(class = "rs-key", style = "background:#E3E1D9"), sprintf("Typical range (%s to %s)", money(s$p25), money(s$p75))),
        span(span(class = "rs-key", style = "background:#F7C1C1"), "Pricier than most"))
  )
}

# Histogram of comparable rents with the typical range marked (and a rent line, if given)
plot_comparables <- function(bench, line_at = NULL, line_label = "Orange line: the asking rent you entered") {
  v <- bench$values
  s <- bench$summary
  rng <- unname(quantile(v, c(0.01, 0.99)))       # trim extreme rents from the view
  if (!is.null(line_at) && isTRUE(is.finite(line_at))) rng <- range(c(rng, line_at))
  p <- ggplot(tibble(rent = v), aes(x = rent)) +
    geom_histogram(bins = 30, fill = "#9ecae1", colour = "white") +
    geom_vline(xintercept = c(s$p25, s$median, s$p75),
               linetype = c("dashed", "solid", "dashed"), colour = "#08519c") +
    coord_cartesian(xlim = rng) +
    labs(x = "Monthly rent ($)", y = "Number of contracts",
         subtitle = "Solid line: median  \u00b7  dashed lines: typical range") +
    theme_minimal(base_size = 13)
  if (!is.null(line_at) && isTRUE(is.finite(line_at))) {
    p <- p + geom_vline(xintercept = line_at, colour = "#d94801", linewidth = 1.2) +
      labs(caption = line_label)
  }
  p
}

# Not enough data: say so plainly and offer one-click ways to widen the search
too_few_card <- function(seg, b, sel = NULL) {
  s <- b$summary
  btns <- if (!is.null(sel)) Filter(Negate(is.null), list(
    if (sel$market == "private" && is_set(sel$band))
      actionButton("broaden_band", "Try any floor area", class = "btn-primary btn-sm"),
    if (is_set(sel$mrt))
      actionButton("broaden_mrt", "Try any MRT distance", class = "btn-primary btn-sm"),
    if (sel$market == "hdb" && is_set(sel$age))
      actionButton("broaden_age", "Try any block age", class = "btn-primary btn-sm")
  )) else list()
  card(card_body(
    div(class = "result-title mt-0", seg),
    p(sprintf("Only %d similar rentals in the last %d months, even after widening the search. That's too few for a reliable range.",
              s$n, s$window_months)),
    if (length(btns)) div(class = "d-flex flex-wrap gap-2", btns)
    else p(class = "text-muted mb-0", "Try a nearby area. Bedroom counts are never mixed, because that gives ranges almost no real rental falls into."),
    if (!is.null(b$note)) p(class = "text-warning small mt-2", b$note)
  ))
}

alternatives_table <- function(alt) {
  if (is.null(alt) || nrow(alt) == 0) return(NULL)
  alt |>
    transmute(Area = area,
              `Distance (km)` = sprintf("%.1f", distance_km),
              `Median rent` = money(median),
              `vs your selection` = if_else(is.na(diff), "-",
                                            paste0(if_else(diff < 0, "-", "+"), money(abs(diff)),
                                                   " (", sprintf("%+.0f%%", 100 * diff_pct), ")")),
              Contracts = n)
}

# Simple HTML table (avoids nesting render functions inside renderUI)
html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tags$table(
    class = "table table-sm",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(as.list(df[i, ]), function(v) tags$td(as.character(v))))
    }))
  )
}

premium_table <- function(tab, label) {
  if (is.null(tab) || nrow(tab) == 0) return(NULL)
  tab |>
    transmute(!!label := as.character(group),
              Contracts = n,
              `Median rent` = money(median),
              `vs all comparables` = paste0(if_else(diff < 0, "-", "+"), money(abs(diff))))
}



# ============================================================
# Search index for the one search bar (built once at startup)
# id formats match the map marker ids: "town|T", "blk|T|BLOCK|STREET", "dist|D", "prj|LOCATION_ID"
# ============================================================

town_name <- function(t) {
  nm <- names(town_choices)[match(t, town_choices)]
  ifelse(is.na(nm), str_to_title(t), nm)
}

hdb_search_index <- bind_rows(
  tibble(id = paste("town", unname(town_choices), sep = "|"),
         text = paste0(names(town_choices), " (HDB town)")),
  block_info |>
    transmute(id   = paste("blk", town, block, street_name, sep = "|"),
              text = paste0(label, ", ", town_name(town), " (HDB)"))
) |> distinct(id, .keep_all = TRUE)

private_search_index <- bind_rows(
  tibble(id = paste("dist", districts, sep = "|"),
         text = paste0(district_label(districts), " (private, whole district)")),
  locations |>
    filter(is.finite(lon), is.finite(lat)) |>
    transmute(id   = paste("prj", location_id, sep = "|"),
              text = paste0(label, ", ", district_label(district), " (", property_group, ")"))
) |> distinct(id, .keep_all = TRUE)

SEARCH_PLACEHOLDER <- 'Any HDB town, block, street or condo name, e.g. "Bedok" or "650 Jln Tenaga"'
# One index for both markets: picking a result switches HDB / private automatically
all_search_index <- bind_rows(hdb_search_index, private_search_index)
search_choices <- function(idx) setNames(idx$id, idx$text)


# ============================================================
# UI
# ============================================================

app_css <- "
/* ---------- Type scale: every text size in the app uses one of these seven steps ---------- */
:root {
  --fs-display: 2.5rem;    /* 40px  headline numbers (median), landing title */
  --fs-h1:      1.625rem;  /* 26px  result title, range-strip headline, value-box numbers */
  --fs-h2:      1.25rem;   /* 20px  card titles, map bar */
  --fs-h3:      1.0625rem; /* 17px  small headings, key figures, search box */
  --fs-body:    1rem;      /* 16px  normal text */
  --fs-small:   .875rem;   /* 14px  secondary text, captions, buttons, notes */
  --fs-tiny:    .75rem;    /* 12px  labels, badges, axis and legend text */
}
body { font-size: var(--fs-body); }
.landing h1 { font-size: var(--fs-display); }
h3, .h3 { font-size: var(--fs-h1); }
h4, .h4 { font-size: var(--fs-h2); }
h5, .h5 { font-size: var(--fs-h3); }
small, .small, .text-muted.small, .form-text, .help-block { font-size: var(--fs-small); }
.control-label, .form-label { font-size: var(--fs-small); font-weight: 600; }
.badge { font-size: var(--fs-tiny); }
.bslib-value-box .value-box-title { font-size: var(--fs-small) !important; }
.bslib-value-box .value-box-value { font-size: var(--fs-h1) !important; }
.bslib-value-box p { font-size: var(--fs-tiny); }
.value-unit { font-size: var(--fs-small); font-weight: 400; }
.table { font-size: var(--fs-small); }
.navbar .nav-link { font-size: var(--fs-body); }
.leaflet .legend, .leaflet .info { font-size: var(--fs-tiny); }
.landing { max-width: 820px; margin: 56px auto 32px; text-align: center; }
.landing .lead { color: #6c757d; }
.role-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 16px; margin-top: 28px; }
.role-card { width: 100%; padding: 28px 20px; border: 2px solid #dee2e6; border-radius: 12px;
             background: #fff; color: #2c3e50; text-align: center; white-space: normal; }
.role-card:hover, .role-card:focus { border-color: #0f7a63; background: #f4fbf9; color: #2c3e50; }
.role-card .role-icon { font-size: var(--fs-display); margin-bottom: 8px; }
.role-card h4 { margin-bottom: 6px; }
.role-card p { margin: 0; color: #6c757d; }

.seg-toggle .form-group, .seg-toggle .shiny-input-container { margin-bottom: 0; width: auto; }
.seg-toggle .control-label { display: block; font-size: var(--fs-small); color: #6c757d; margin-bottom: 4px; }
.seg-toggle .shiny-options-group { display: inline-flex; border: 1px solid #2c3e50; border-radius: 8px; overflow: hidden; }
.seg-toggle label.radio-inline { margin: 0 !important; padding: 0 !important; }
.seg-toggle input[type=radio] { position: absolute; opacity: 0; pointer-events: none; }
.seg-toggle label.radio-inline span { display: block; padding: 7px 16px; cursor: pointer; }
.seg-toggle label.radio-inline + label.radio-inline span { border-left: 1px solid #2c3e50; }
.seg-toggle input[type=radio]:checked + span { background: #2c3e50; color: #fff; }

.big-search .control-label { font-weight: 600; }
.big-search .selectize-input { min-height: 50px; font-size: var(--fs-h3); padding: 13px 14px; border: 2px solid #2c3e50; }
.asking-box { width: 230px; }
.asking-box input { min-height: 50px; font-size: var(--fs-h3); }
.search-card .form-group { margin-bottom: .5rem; }
.map-options .form-group { margin-bottom: 0; }
.amen-pin { display: inline-flex; align-items: center; justify-content: center; width: 22px; height: 22px;
            border-radius: 50%; color: #fff; font-size: 11px; margin-right: 6px; vertical-align: middle; }
.amen-pin i { width: auto !important; height: auto !important; margin: 0 !important; float: none !important;
              opacity: 1 !important; line-height: 1; font-size: 11px; }
.amen-emoji { font-size: 12px; line-height: 1; }
.awesome-marker { font-size: 14px; }
.amen-legend { background: #fff; padding: 8px 12px; border-radius: 6px; box-shadow: 0 1px 4px rgba(0,0,0,.25);
               font-size: var(--fs-tiny); line-height: 2; }

/* Card outlines: a clear border and soft shadow so each section reads as its own box */
/* !important because bslib's own card styles load after this sheet and would win otherwise */
.card, .bslib-card { --bs-card-border-color: #9aa5b1; --bs-card-border-width: 2px;
                     border: 2px solid #9aa5b1 !important; border-radius: 14px !important;
                     box-shadow: 0 3px 10px rgba(44, 62, 80, .12) !important; margin-bottom: 18px; }
.card.bslib-value-box { border: none !important; box-shadow: 0 3px 10px rgba(44, 62, 80, .16) !important; }
.map-acc .accordion-item { border: 2px solid #0f7a63 !important; border-radius: 14px !important; }
.search-card { border-color: #2c3e50 !important; }
.cmp-card { border-width: 1.5px !important; border-color: #c9d1d9 !important; box-shadow: 0 1px 4px rgba(44, 62, 80, .08); }

/* Headings: short bold title + small grey subtitle */
.card-header { background: #f8f9fb; border-bottom: 1.5px solid #e3e7eb; padding: 14px 16px 10px;
               border-top-left-radius: 12px !important; border-top-right-radius: 12px !important; }
.ch-title { font-family: var(--bs-headings-font-family); font-size: var(--fs-h2); font-weight: 700; color: #2c3e50; line-height: 1.25; }
.ch-sub { font-size: var(--fs-small); color: #6c757d; margin-top: 2px; }
.result-title { font-family: var(--bs-headings-font-family); font-size: var(--fs-h1); font-weight: 700; color: #2c3e50; margin: 18px 0 6px; }
.value-box-title { font-weight: 600; }

/* About this block / project */
.stat-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 12px 18px; }
.stat-label { font-size: var(--fs-tiny); text-transform: uppercase; letter-spacing: .04em; color: #6c757d; }
.stat-value { font-weight: 700; font-size: var(--fs-h3); }

/* Nearby places */
.nearby-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 14px 24px; }
.nearby-kind { display: flex; align-items: center; font-weight: 700; margin-bottom: 4px; }
.nearby-list { padding-left: 1.1rem; margin-bottom: 0; }
.nearby-list li { margin-bottom: 4px; }

/* Plain data line, Why-this-range box, empty state */
.data-line { color: #6c757d; font-size: var(--fs-small); margin-bottom: 12px; }
details.why { margin: 4px 0 16px; }
details.why summary { cursor: pointer; color: #1f6fa8; font-weight: 600; font-size: var(--fs-small); }
details.why .why-body { margin-top: 8px; padding: 12px 14px; background: #f8f9fa; border-radius: 8px; font-size: var(--fs-small); }
details.why .why-body p:last-child { margin-bottom: 0; }
.empty-state { text-align: center; padding: 48px 16px; color: #6c757d; }
.empty-state .fa, .empty-state svg { font-size: var(--fs-display); color: #2c3e50; margin-bottom: 10px; }
.empty-state h3 { color: #2c3e50; font-weight: 700; }

/* Section shortcuts (sticky) */
.jump-bar { position: sticky; top: 0; z-index: 1020; display: flex; flex-wrap: wrap; gap: 6px; align-items: center;
            background: rgba(255,255,255,.96); border-bottom: 1px solid #e9ecef; padding: 8px 4px; margin: 4px 0 14px; }
.jump-bar .jb-label { font-size: var(--fs-tiny); color: #6c757d; margin-right: 4px; }
.jump-bar a { font-size: var(--fs-small); font-weight: 600; color: #2c3e50; text-decoration: none; padding: 4px 12px;
              border: 1px solid #dee2e6; border-radius: 999px; background: #fff; }
.jump-bar a:hover { border-color: #2c3e50; background: #f1f3f5; }
[id^='sec-'] { scroll-margin-top: 70px; }

/* Shortlist stars */
.star { font-size: var(--fs-h3); text-decoration: none; color: #adb5bd; }
.star.on, .star:hover { color: #f39c12; }
/* Add to favourites: solid yellow pill; once added it turns pale with a filled star */
.star-btn { display: inline-block; font-size: var(--fs-small); font-weight: 700; text-decoration: none; color: #3d2c00;
            padding: 6px 14px; background: #ffc83d; border: 2px solid #e0a800; border-radius: 999px;
            white-space: nowrap; box-shadow: 0 2px 4px rgba(224, 168, 0, .35); transition: background .15s, transform .1s; }
.star-btn:hover { background: #ffb800; color: #3d2c00; transform: translateY(-1px); }
.star-btn:active { transform: translateY(0); }
.star-btn .star-glyph { font-size: var(--fs-h3); line-height: 1; vertical-align: -1px; }
/* Saved: white pill, gold filled star, text offers to remove */
.star-btn.on { color: #5c4300; background: #fff; border-color: #f0b400; box-shadow: none; }
.star-btn.on .star-glyph { color: #f0a500; }
.star-btn.on:hover { background: #fff4d1; }
.l-summary { font-size: var(--fs-body); }
.l-summary .fa, .l-summary svg { color: #f39c12; margin-right: 6px; }

/* Favourites: side-by-side comparison */
.cmp-strip { margin-bottom: 18px; }
.cmp-row { display: grid; grid-template-columns: 180px 1fr; align-items: center; gap: 12px; height: 34px; }
.cmp-name { font-weight: 600; font-size: var(--fs-small); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.cmp-track { position: relative; height: 34px; border-bottom: 1px dashed #e9ecef; }
.cmp-bar { position: absolute; top: 12px; height: 10px; border-radius: 5px; background: #9ecae1; }
.cmp-med { position: absolute; top: 9px; width: 16px; height: 16px; margin-left: -8px; border-radius: 50%; background: #08519c; border: 2px solid #fff; }
.cmp-medlab { position: absolute; top: -4px; transform: translateX(-50%); font-size: var(--fs-tiny); color: #08519c; font-weight: 700; white-space: nowrap; }
.cmp-budget { position: absolute; top: 0; bottom: 0; width: 2px; margin-left: -1px; background: #d94801; }
.cmp-axis { display: flex; justify-content: space-between; margin-left: 192px; font-size: var(--fs-tiny); color: #6c757d; }
.cmp-key { display: flex; flex-wrap: wrap; gap: 14px; margin: 6px 0 0 192px; font-size: var(--fs-tiny); color: #6c757d; }
.cmp-key-bar, .cmp-key-med, .cmp-key-budget { display: inline-block; margin-right: 5px; vertical-align: middle; }
.cmp-key-bar { width: 18px; height: 8px; border-radius: 4px; background: #9ecae1; }
.cmp-key-med { width: 10px; height: 10px; border-radius: 50%; background: #08519c; }
.cmp-key-budget { width: 2px; height: 12px; background: #d94801; }
.cmp-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(230px, 1fr)); gap: 14px; margin-bottom: 12px; }
.cmp-card { border: 1px solid #dee2e6; border-radius: 12px; padding: 14px; background: #fff; }
.cmp-title { font-weight: 700; color: #2c3e50; }
.cmp-badges { display: flex; flex-wrap: wrap; gap: 4px; min-height: 22px; margin: 8px 0 4px; }
.cmp-median { font-family: var(--bs-headings-font-family); font-size: var(--fs-h1); font-weight: 700; color: #2c3e50; margin-bottom: 6px; }
.cmp-per { font-size: var(--fs-tiny); font-weight: 400; color: #6c757d; }
.cmp-stat { display: flex; justify-content: space-between; gap: 8px; padding: 5px 0; border-top: 1px solid #f1f3f5; font-size: var(--fs-small); }
.cmp-stat-label { color: #6c757d; }
.cmp-stat-value { font-weight: 600; text-align: right; }
.cmp-stat.win .cmp-stat-value { color: #0f7a63; }

/* Favourites: count in the navbar, slim bar on Rent check, empty state */
.fav-count { background: #ffc83d; color: #3d2c00; font-size: var(--fs-tiny); vertical-align: 1px; }
.fav-bar { display: flex; flex-wrap: wrap; align-items: center; gap: 10px; padding: 10px 16px; margin-bottom: 18px;
           border: 1.5px solid #f0b400; border-radius: 12px; background: #fffaf0; font-size: var(--fs-small); }
.fav-bar.empty { border-style: dashed; border-color: #c9d1d9; background: #fafbfc; color: #6c757d; }
.fav-bar .fav-star { color: #f0a500; font-size: var(--fs-h3); }
.fav-bar a { font-weight: 700; }
.fav-page { max-width: 1200px; margin: 8px auto 0; }
.fav-empty { text-align: center; padding: 40px 16px; }
.fav-empty .fav-big-star { font-size: var(--fs-display); color: #f0a500; line-height: 1; }
.fav-empty ul { display: inline-block; text-align: left; margin: 12px 0 18px; }

/* Map popups: a clear button to select the dot */
.pop-select { display: inline-block; margin: 4px 0 2px; padding: 4px 12px; border-radius: 999px; background: #0f7a63;
              color: #fff !important; font-weight: 700; text-decoration: none; }
.pop-select:hover { background: #0b5e4c; }

/* Map key under each map */
.map-key { font-size: var(--fs-small); color: #495057; margin-top: 6px; }
.map-key-title { font-weight: 700; color: #2c3e50; margin-bottom: 4px; }
.map-key-row { display: grid; grid-template-columns: 64px 1fr; gap: 8px; padding: 3px 0; border-top: 1px solid #f1f3f5; }
.map-key-label { font-weight: 700; color: #2c3e50; }
.key-swatch { display: inline-block; width: 11px; height: 11px; border-radius: 50%; margin: 0 4px 0 2px; vertical-align: -1px; }
.key-grad { display: inline-block; width: 44px; height: 10px; border-radius: 5px; margin-right: 6px; vertical-align: -1px; }

/* Explore: areas ranked */
.rank-scroll { max-height: 600px; overflow-y: auto; }
.rank-scroll thead th { position: sticky; top: 0; background: #fff; z-index: 2; }
.rank-med { position: relative; min-width: 110px; }
.rank-bar { position: absolute; left: 0; top: 50%; height: 20px; margin-top: -10px; background: #dbe9f6; border-radius: 4px; z-index: 0; }
.rank-med span { position: relative; z-index: 1; font-weight: 700; padding-left: 6px; }

/* Range strip */
.rs-head { display: flex; flex-wrap: wrap; align-items: baseline; gap: 8px; margin-bottom: 6px; }
.rs-gap { font-family: var(--bs-headings-font-family); font-size: var(--fs-h1); font-weight: 700; }
.rs-share { font-size: var(--fs-h3); color: #495057; }
.rs { position: relative; height: 64px; margin: 0 8px; }
.rs-bar { position: absolute; left: 0; right: 0; top: 34px; height: 18px; border-radius: 9px; overflow: hidden; display: flex; }
.rs-med { position: absolute; top: 29px; width: 3px; height: 28px; margin-left: -1.5px; background: #2c3e50; border-radius: 2px; }
.rs-dot { position: absolute; top: 33px; width: 20px; height: 20px; margin-left: -10px; border-radius: 50%;
          background: #fff; border: 4px solid; box-shadow: 0 1px 3px rgba(0,0,0,.3); }
.rs-pin { position: absolute; top: 0; font-weight: 700; font-size: var(--fs-small); white-space: nowrap; line-height: 1.2; }
.rs-arrow { position: absolute; top: 21px; width: 0; height: 0; margin-left: -6px; border-left: 6px solid transparent;
            border-right: 6px solid transparent; border-top: 8px solid; }
.rs-pin i { display: block; width: 0; height: 0; margin: 1px auto 0; border-left: 6px solid transparent;
            border-right: 6px solid transparent; border-top: 8px solid; }
.rs-labels { position: relative; height: 22px; margin: 2px 8px 0; font-size: var(--fs-tiny); color: #6c757d; }
.rs-labels span { position: absolute; white-space: nowrap; }
.rs-labels .rs-medlab { transform: translateX(-50%); color: #2c3e50; font-weight: 700; top: 0; }
.rs-legend { display: flex; flex-wrap: wrap; gap: 16px; margin-top: 10px; font-size: var(--fs-tiny); color: #6c757d; }
.rs-key { display: inline-block; width: 12px; height: 12px; border-radius: 3px; margin-right: 6px; vertical-align: -1px; }

/* Fits-your-budget table */
.budget-scroll { max-height: 380px; overflow-y: auto; }
.budget-scroll thead th { position: sticky; top: 0; background: #fff; }

/* Map: collapsed bar, full-screen size */
.map-acc .accordion-item { border: none; border-radius: 12px; overflow: hidden; }
.map-acc .accordion-button { font-family: var(--bs-headings-font-family); font-weight: 700; font-size: var(--fs-h2);
                             background: #0f7a63; color: #fff; padding: 18px 22px; box-shadow: none; }
.map-acc .accordion-button:hover { background: #0b5e4c; }
.map-acc .accordion-button:not(.collapsed) { background: #2c3e50; color: #fff; }
.map-acc .accordion-button::after { filter: brightness(0) invert(1); width: 1.4rem; height: 1.4rem; background-size: 1.4rem; }
.map-acc .accordion-icon { font-size: var(--fs-h2); }
.map-acc .accordion-title { flex: 1; display: flex; align-items: center; }
.map-cta { margin-left: auto; margin-right: 12px; font-size: var(--fs-small); font-weight: 600; padding: 4px 12px;
           border: 1.5px solid #fff; border-radius: 999px; }
.map-acc .accordion-button.collapsed .map-cta-close, .map-acc .accordion-button:not(.collapsed) .map-cta-open { display: none; }
.bslib-card[data-full-screen='true'] #map, .bslib-card[data-full-screen='true'] #e_map { height: calc(100vh - 160px) !important; }

/* Wide tables scroll sideways instead of squashing */
.budget-scroll, .rank-scroll, .table-responsive, #backtest_table, #backtest_worst { overflow-x: auto; }

/* ---------- Phones (up to 576px wide) ---------- */
@media (max-width: 576px) {
  /* Smaller steps on phones; every element follows automatically */
  :root { --fs-display: 2rem; --fs-h1: 1.375rem; --fs-h2: 1.125rem; --fs-h3: 1rem; }
  .landing { margin-top: 28px; }
  .role-card { padding: 20px 14px; }

  .search-card .shiny-input-container { width: 100% !important; }
  .asking-box { width: 100%; }
  .big-search .selectize-input { min-height: 46px; padding: 11px 12px; }
  .seg-toggle label.radio-inline span { padding: 6px 11px; font-size: var(--fs-small); }

  .rs-labels span:not(.rs-medlab) { display: none; }   /* the legend already lists the range */

  /* Jump bar: one swipeable row instead of three wrapped rows */
  .jump-bar { flex-wrap: nowrap; overflow-x: auto; -webkit-overflow-scrolling: touch; }
  .jump-bar a { flex: 0 0 auto; }

  #map, #e_map { height: 380px !important; }
  .leaflet .legend, .leaflet .info { font-size: 10.5px; padding: 4px 6px; line-height: 1.3; }
  .leaflet .legend i { width: 10px; height: 10px; }
  .amen-legend { font-size: 11px; line-height: 1.6; padding: 6px 8px; }
  .map-acc .accordion-button { padding: 14px 14px; }
  .map-cta { display: none; }

  .cmp-row { grid-template-columns: 96px 1fr; }
  .cmp-axis, .cmp-key { margin-left: 108px; }
  .card-header .map-options .form-check, .card-header .map-options .checkbox { font-size: var(--fs-small); }
}
"

# Leaflet draws grey tiles if its box changes size while hidden; nudge it to redraw after
# the map panel opens or the card enters / leaves full screen.
app_js <- "

$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('rentSearchSelection', function(m) {
    var el = document.getElementById('s_search');
    if (!el || !el.selectize) return;
    var c = el.selectize;
    c.syncingSelection = true;
    if (m.id) c.addOption({value: m.id, label: m.label});
    c.setValue(m.id || '', true);
    c.syncingSelection = false;
  });
  Shiny.addCustomMessageHandler('rentLoadFavourites', function(m) {
    var ids = [];
    try { ids = JSON.parse(localStorage.getItem('sg-rent-check:favourites:v1') || '[]'); }
    catch (e) {}
    Shiny.setInputValue('stored_favourites', {ids: Array.isArray(ids) ? ids : []}, {priority:'event'});
  });
  Shiny.addCustomMessageHandler('rentSaveFavourites', function(m) {
    try { localStorage.setItem('sg-rent-check:favourites:v1', JSON.stringify(m.ids || [])); }
    catch (e) {}
  });
});
function copyResultLink(button) {
  var field = document.getElementById('result-share-url');
  var status = document.getElementById('result-share-status');
  if (!field || !status) return false;
  function done(ok) {
    button.disabled = false;
    button.textContent = ok ? 'Copied!' : 'Copy link';
    status.textContent = ok ? 'Link copied. You can now paste it into a message.' :
      'Your browser blocked copying. Select the link and copy it manually.';
  }
  function fallback() {
    field.focus();
    field.select();
    field.setSelectionRange(0, field.value.length);
    var ok = false;
    try { ok = document.execCommand('copy'); } catch (err) {}
    done(ok);
  }
  button.disabled = true;
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(field.value).then(function () { done(true); }, fallback);
  } else {
    fallback();
  }
  return false;
}
function pickPlace(el) {
  Shiny.setInputValue('pick_place', el.getAttribute('data-id'), {priority: 'event'});
  var r = document.getElementById('result');
  if (r) r.scrollIntoView({behavior: 'smooth', block: 'start'});
}
function jumpTo(id) {
  if (id === 'sec-map') {
    var b = document.querySelector('.map-acc .accordion-button.collapsed');
    if (b) b.click();
  }
  var el = document.getElementById(id);
  if (el) setTimeout(function () { el.scrollIntoView({behavior: 'smooth', block: 'start'}); }, id === 'sec-map' ? 350 : 0);
  return false;
}
function checkHome(el) {
  Shiny.setInputValue('explore_check', el.getAttribute('data-id'), {priority: 'event'});
  window.scrollTo({top: 0, behavior: 'smooth'});
  return false;
}
document.addEventListener('shown.bs.tab', function () { nudgeMap(); });
function goFav() {
  Shiny.setInputValue('go_fav', Date.now(), {priority: 'event'});
  window.scrollTo({top: 0, behavior: 'smooth'});
  return false;
}
function toggleShortlist(el) {
  Shiny.setInputValue('shortlist_toggle', el.getAttribute('data-id'), {priority: 'event'});
  return false;
}
function nudgeMap() { setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 250); }
document.addEventListener('shown.bs.collapse', function (e) {
  nudgeMap();
  if (e.target.closest && e.target.closest('.map-acc') && window.Shiny) {
    setTimeout(function () { Shiny.setInputValue('map_opened', Date.now()); }, 300);
  }
});
document.addEventListener('click', function (e) {
  if (e.target.closest && e.target.closest('.bslib-full-screen-enter, .bslib-full-screen-exit')) nudgeMap();
});
document.addEventListener('keyup', function (e) { if (e.key === 'Escape') nudgeMap(); });
"

# Spinner while an output recalculates (old content stays visible underneath)
spin <- function(x, height = "150px") {
  shinycssloaders::withSpinner(x, type = 8, color = "#0f7a63", size = 0.7, proxy.height = height, hide.ui = FALSE)
}

# Shown (in JavaScript) once an area has been chosen
AREA_SET <- "(input.s_market == 'hdb' && input.s_town) || (input.s_market == 'private' && input.s_district)"

seg_toggle <- function(id, label, choices, selected = NULL) {
  div(class = "seg-toggle", radioButtons(id, label, choices = choices, selected = selected, inline = TRUE))
}

is_hdb     <- "input.s_market == 'hdb'"
is_private <- "input.s_market == 'private'"
is_tenant  <- "input.s_role == 'tenant'"
is_lord    <- "input.s_role == 'landlord'"

# Build inputs inside the UI request so bookmarked values can be restored.
search_card <- function() card(
  class = "search-card",
  card_body(
    # Who you are + type of home
    div(class = "d-flex flex-wrap gap-4 align-items-end mb-3",
        seg_toggle("s_role", "I am a", c("Tenant" = "tenant", "Landlord" = "landlord")),
        seg_toggle("s_market", "Type of home", c("HDB flat" = "hdb", "Private (condo, EC, landed)" = "private")),
        div(class = "ms-auto text-end",
            conditionalPanel(is_tenant, h5(class = "mb-1", "Is this rent fair?")),
            conditionalPanel(is_lord,   h5(class = "mb-1", "What should I list at?")),
            conditionalPanel(AREA_SET,
                             bookmarkButton(label = "Share this result", icon = icon("link"),
                                            class = "btn-outline-primary btn-sm mt-1",
                                            title = "Get a link that opens this exact search")))),

    # The one search bar (+ asking rent for tenants)
    div(class = "d-flex flex-wrap gap-3 align-items-end",
        div(class = "big-search flex-grow-1",
            selectizeInput("s_search", "Search", choices = NULL, width = "100%",
                           options = list(placeholder = SEARCH_PLACEHOLDER, maxOptions = 100, closeAfterSelect = TRUE,
                                          hideSelected = FALSE,
                                          onInitialize = I("function() {
  var control = this;
  var batch = 100;
  function more() {
    var box = control.$dropdown_content[0];
    if (!box || !control.isOpen) return;
    var total = control.currentResults ? control.currentResults.total : 0;
    if (control.settings.maxOptions >= total) return;
    var top = box.scrollTop;
    control.settings.maxOptions += batch;
    control.refreshOptions(false);
    box.scrollTop = top;
  }
  control.$dropdown_content.on('scroll.browseAll', function() {
    if (this.scrollTop + this.clientHeight >= this.scrollHeight - 60) more();
  });
  control.$control_input.on('keydown.browseAll', function(e) {
    if (e.key === 'ArrowDown' && control.$activeOption &&
        control.$activeOption.nextAll('[data-selectable]').length === 0) more();
  });
}"),
                                          onType = I("function() { this.settings.maxOptions = 100; }"),
                                          onDropdownOpen = I("function() {
  this.settings.maxOptions = 100;
  this.setTextboxValue('');
  this.lastQuery = null;
  this.refreshOptions(false);
  this.$dropdown_content.scrollTop(0);
}"),
                                          onItemAdd = I("function(value, item) { if (this.syncingSelection) return; Shiny.setInputValue('s_search_pick', value, {priority: 'event'}); this.setTextboxValue(''); this.close(); this.blur(); }")))),
        conditionalPanel(is_tenant, class = "asking-box",
                         numericInput("t_asking", "Asking rent per month ($)", value = NA_real_, min = 0, step = 50))),

    # Filters
    conditionalPanel(
      is_hdb,
      layout_columns(
        col_widths = c(4, 5, 3),
        selectInput("s_town", "Area (HDB town)", choices = c("Choose a town" = "", town_choices), selected = ""),
        selectizeInput("s_block", "Block", choices = NULL),
        selectInput("s_flat", "Flat type", choices = flat_choices, selected = "4-ROOM")
      )
    ),
    conditionalPanel(
      is_private,
      layout_columns(
        col_widths = c(3, 3, 4, 2),
        selectInput("s_district", "Area (postal district)", choices = c("Choose a district" = "", district_choices), selected = ""),
        selectInput("s_group", "Property type", choices = group_choices),
        selectizeInput("s_project", "Project", choices = NULL),
        conditionalPanel("input.s_group != 'Landed'",
                         selectInput("s_beds", "Bedrooms", choices = bed_choices))
      )
    ),
    accordion(
      id = "more_filters", open = FALSE, class = "accordion-flush",
      accordion_panel(
        "More filters",
        conditionalPanel(
          is_hdb,
          layout_columns(
            col_widths = c(4, 4),
            selectInput("s_age", "Block age", choices = age_choices),
            conditionalPanel("!input.s_block || input.s_block == 'any'",
                             selectInput("s_mrt", "Distance to MRT (when no block is picked)", choices = mrt_choices))
          )
        ),
        conditionalPanel(
          is_private,
          layout_columns(
            col_widths = c(4, 4),
            shinyWidgets::sliderTextInput("s_band_rng", "Floor area (sqft)", width = "100%",
                                          choices = size_ticks("Condo / apartment"),
                                          selected = range_ends(size_ticks("Condo / apartment"))),
            conditionalPanel("!input.s_project || input.s_project == 'any'",
                             selectInput("s_pmrt", "Distance to MRT (when no project is picked)", choices = mrt_choices))
          )
        )
      )
    )
  )
)

map_card <- function() card(
  full_screen = TRUE,
  card_header(
    class = "map-options d-flex flex-wrap gap-3 align-items-center",
    div(class = "d-flex flex-wrap gap-3 align-items-center",
        tooltip(
          checkboxInput("s_enough", "Only blocks with 3+ rentals", TRUE),
          "A median worked out from only 1 or 2 rentals isn't reliable, so those markers are hidden. Untick to see every block or project with at least one rental."
        ),
        checkboxInput("s_show_mrt", "All MRT stations", FALSE),
        conditionalPanel("input.s_role == 'tenant' && input.t_asking > 0",
                         checkboxInput("s_budget_map", "Colour by my budget", TRUE)),
        tooltip(
          checkboxInput("s_areas", "Shade by neighbourhood", FALSE),
          "Shades each URA planning area (e.g. Tampines, Bedok) by its median rent for the selected home type, in place of the individual dots. Grey areas have too few rentals."
        ))
  ),
  spin(leafletOutput("map", height = 500), "500px"),
  card_footer(
    if (HAS_AMEN) div(
      class = "map-options d-flex flex-wrap gap-3 align-items-center",
      checkboxGroupInput("s_amen", "Show nearby:", inline = TRUE,
                         choiceNames = amen_choice_names, choiceValues = unname(amen_kinds),
                         selected = intersect(c("mrt", "hawker", "school", "mall"), amen_kinds)),
      if (WALK_READY) checkboxInput("s_walk", if (WALK_LIVE) "Use walking routes (slower)" else "Use saved walking routes", FALSE)
    ),
    uiOutput("map_note"),
    actionLink("go_explore", "Browse the whole island in Explore \u2192", class = "small fw-semibold")
  )
)

ui <- function(request) page_navbar(
  id = "nav",
  title = span("Singapore Rent Check ",
               tags$small(class = "text-white-50 ms-2",
                          sprintf("Data to %s", format(max(HDB_AS_OF, PRIV_AS_OF), "%b %Y")))),
  window_title = "Singapore Rent Check",
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   # Flatly's teal / blue / red are too light for white text; these pass WCAG AA (4.5:1)
                   success = "#0f7a63", info = "#1f6fa8", danger = "#c0392b",
                   base_font    = font_google("DM Sans", wght = c(400, 500, 700), local = FALSE),
                   heading_font = font_google("DM Sans", wght = c(400, 500, 700), local = FALSE)),
  fillable = FALSE,
  header = tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_js))),

  # ---- 1. Home: pick tenant or landlord ----
  nav_panel(
    "Home", value = "home",
    div(class = "landing",
        h1("Singapore Rent Check"),
        p(class = "lead", "Compare any HDB or private rent with what people actually signed in the last 12 months."),
        h5(class = "mt-4", "First, which one are you?"),
        div(class = "role-grid",
            actionButton("go_tenant", class = "role-card",
                         label = div(div(class = "role-icon", icon("key")),
                                     h4("I'm renting"),
                                     p("Check if an asking rent is fair and find cheaper areas nearby."))),
            actionButton("go_landlord", class = "role-card",
                         label = div(div(class = "role-icon", icon("building")),
                                     h4("I'm a landlord"),
                                     p("See what similar homes rented for and pick a listing price.")))),
        p(class = "text-muted small mt-4", "You can switch between tenant and landlord at any time on the next page."))
  ),

  # ---- 2. Rent check: search, answer, map ----
  nav_panel(
    "Rent check", value = "main",
    search_card(),
    spin(uiOutput("result")),
    conditionalPanel(AREA_SET, uiOutput("jump_bar")),
    # Tenant with an asking rent / budget: which blocks or projects can work at that price
    conditionalPanel(
      paste0("input.s_role == 'tenant' && input.t_asking > 0 && (", AREA_SET, ")"),
      card(
        id = "sec-budget",
        card_header(
          class = "d-flex flex-wrap gap-3 align-items-center",
          uiOutput("budget_title"),
          div(class = "ms-auto map-options",
              radioButtons("budget_sort", NULL, inline = TRUE,
                           choices = c("Best chance" = "chance", "Lowest rent" = "rent",
                                       "Nearest MRT" = "mrt", "Newest" = "new")))
        ),
        spin(uiOutput("budget_body"))
      )
    ),
    conditionalPanel(
      AREA_SET,
      layout_columns(
        id = "sec-spread",
        col_widths = c(8, 4),
        card(card_header(ch_title("Rent spread", "What similar homes rented for, last 12 months")),
             spin(plotOutput("plot", height = 280), "280px")),
        uiOutput("block_info")
      )
    ),
    # Favourites sit below Rent spread, so adding one never pushes the content you're reading down
    # One slim line; the full comparison lives on the Favourites tab
    conditionalPanel(AREA_SET, uiOutput("fav_bar")),
    # Map is collapsed until asked for; the card's expand icon opens it full screen
    div(id = "sec-map"),
    accordion(
      id = "map_acc", open = FALSE, class = "map-acc mb-3",
      accordion_panel(
        title = tagList(span("Show on map: nearby blocks, MRT, schools and more"),
                        span(class = "map-cta map-cta-open", "Open map"),
                        span(class = "map-cta map-cta-close", "Hide map")),
        value = "map", icon = icon("map-location-dot"),
        map_card()
      )
    ),
    conditionalPanel(
      AREA_SET,
    layout_columns(
      id = "sec-nearby",
      col_widths = c(6, 6),
      uiOutput("nearby"),
      div(id = "sec-alt",
          conditionalPanel(is_tenant, uiOutput("t_alt")),
          conditionalPanel(is_lord, uiOutput("l_premium")))
    ),
    conditionalPanel(
      is_lord,
      div(id = "sec-demand", uiOutput("l_summary")),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header(ch_title("Demand", "Contracts signed per month, last 24 months")),
             plotOutput("l_demand", height = 260)),
        card(card_header(ch_title("Best months to list", "Rent vs trend, and contracts, by calendar month")),
             plotOutput("l_season", height = 260),
             p(class = "text-muted small", "Top: how far each month's rent sat above or below the trend. Bottom: average contracts. Only a few years of data, so treat it as a rough guide."))
      )
    ),
    card(
      id = "sec-trend",
      card_header(
        class = "d-flex flex-wrap gap-3 align-items-center",
        ch_title("Rent trend", "Monthly median for the area, not your unit"),
        div(class = "ms-auto d-flex flex-wrap gap-3 align-items-center map-options",
            radioButtons("s_trend_level", NULL, inline = TRUE,
                         choices = c("Town / district" = "area", "Planning area" = "pa")),
            conditionalPanel("input.s_trend_level == 'pa'",
                             selectInput("s_pa", NULL, choices = pa_choices, selected = "Tampines", width = "220px")))
      ),
      spin(plotly::plotlyOutput("trend", height = "340px"), "340px"),
      p(class = "text-muted small",
        "Hover over the chart to see each line. Months with fewer than 3 contracts are blank. Changes can reflect the mix of homes rented, not just the market.")
    )
    )   # end AREA_SET
  ),

  # ---- 3. Explore: browse the whole island ----
  nav_panel(
    "Explore", value = "explore",
    card(
      class = "search-card",
      card_body(
        div(class = "d-flex flex-wrap gap-4 align-items-end",
            seg_toggle("e_market", "Type of home", c("HDB flat" = "hdb", "Private (condo, EC, landed)" = "private")),
            conditionalPanel("input.e_market == 'hdb'",
                             selectInput("e_flat", "Flat type", choices = flat_choices, selected = "4-ROOM", width = "180px")),
            conditionalPanel("input.e_market == 'private'",
                             div(class = "d-flex gap-3",
                                 selectInput("e_group", "Property type", choices = group_choices, width = "220px"),
                                 conditionalPanel("input.e_group != 'Landed'",
                                                  selectInput("e_beds", "Bedrooms", choices = bed_choices, width = "130px")))),
            div(class = "asking-box",
                numericInput("e_budget", "Your budget ($ per month)", value = NA_real_, min = 0, step = 50)),
            div(class = "ms-auto text-end",
                h5(class = "mb-1", "Where should I look?"),
                p(class = "text-muted small mb-0", "Browse the whole island, then open any block or area in Rent check.")))
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header(
          class = "map-options d-flex flex-wrap gap-3 align-items-center",
          tooltip(checkboxInput("e_enough", "Only blocks with 3+ rentals", TRUE),
                  "A median from only 1 or 2 rentals isn't reliable, so those dots are hidden. Untick to see every block or project with at least one rental."),
          checkboxInput("e_show_mrt", "All MRT stations", FALSE),
          conditionalPanel("input.e_budget > 0", checkboxInput("e_budget_map", "Colour by my budget", TRUE)),
          tooltip(checkboxInput("e_areas", "Shade by neighbourhood", FALSE),
                  "Shades each URA planning area by its median rent for the selected home type, in place of the dots. Grey areas have too few rentals.")
        ),
        spin(leafletOutput("e_map", height = 640), "640px"),
        card_footer(uiOutput("e_map_note"))
      ),
      card(
        card_header(class = "d-flex flex-wrap gap-2 align-items-center",
                    uiOutput("e_rank_title"),
                    div(class = "ms-auto map-options",
                        radioButtons("e_rank_sort", NULL, inline = TRUE,
                                     choices = c("Cheapest" = "cheap", "Most rentals" = "busy", "Best chance" = "odds")))),
        spin(uiOutput("e_rank"))
      )
    )
  ),

  # ---- 4. Favourites: compare saved homes side by side ----
  nav_panel(
    title = span("\u2605 Favourites ", span(class = "badge rounded-pill fav-count", textOutput("fav_count", inline = TRUE))),
    value = "favourites",
    div(class = "fav-page", uiOutput("shortlist_card"))
  ),

  # ---- 5. Data & limits ----
  nav_panel(
    "Data & limits",
    card(card_body(
      h4("What this tool does"),
      p("It compares a rent with rents that were actually signed for similar homes, so tenants can sanity-check an asking rent and landlords can see a sensible range to list at."),
      h4("Where the data comes from"),
      tags$ul(
        tags$li(sprintf("Private homes: URA rental contracts filed for stamp duty. %s contracts, %s to %s.",
                        format(nrow(private), big.mark = ","),
                        format(min(private$month), "%B %Y"), format(max(private$month), "%B %Y"))),
        tags$li(sprintf("HDB flats: HDB renting-out records from data.gov.sg. %s contracts, %s to %s. Rents are declared by the owner and are not verified by HDB.",
                        format(nrow(hdb), big.mark = ","),
                        format(min(hdb$month), "%B %Y"), format(max(hdb$month), "%B %Y"))),
        tags$li("Block coordinates: OneMap, Singapore Land Authority."),
        tags$li(
          "Planning area boundaries: URA Master Plan 2025 (data.gov.sg). All rental records, including earlier years, are classified using these boundaries."
        ),
        tags$li("MRT exits: LTA (data.gov.sg)."),
        tags$li("Block age: HDB Property Information (data.gov.sg)."),
        tags$li("Station opening dates: compiled from LTA announcements."),
        tags$li("Private rental index: URA (data.gov.sg)."),
        tags$li("Hawker centres: NEA (data.gov.sg). Schools: MOE school directory (data.gov.sg), located with OneMap, Singapore Land Authority."),
        if (HAS_OSM) tags$li("Malls, petrol stations and food courts: OpenStreetMap contributors (ODbL).")
      ),
      h4("How the benchmark works"),
      p(sprintf("We look for comparable contracts, starting with the most similar homes and widening step by step until there are at least %d. For HDB: the same block, then the same street, then the same flat type within 1 km of the block (even across a town boundary), then the same town (with your optional block-age and MRT filters, which are dropped one at a time if needed). For private homes: the same project, then the same type within 1 km of the project, then the same district, then any floor area. We never mix bedroom counts: in testing, that gave ranges almost no real contract fell into. The page always says which level was used.",
                MIN_N)),
      p("Many rents are round $100 amounts, so several contracts often equal the rent you enter. When we say where a rent sits (its percentile), we count half of those exact matches, and we show how many there were."),
      p(sprintf("At each level we keep contracts from the last 12 months. If that leaves fewer than %d, we look back 24 months. If it is still fewer than %d at the broadest level, we say there is not enough data rather than show a shaky number. The typical range is the middle 50%% of contracts (25th to 75th percentile).",
                MIN_N, MIN_N)),
      p("Price direction and year-on-year change compare the average of the middle 80% of contracts in each period (the top and bottom 10% are left out). We do not use the median for these changes because rents cluster on round $100 amounts, so the median often stays on the same figure and shows a false 0.0% change."),
      p("When we have to use 24 months, we also show a trend-adjusted median. It scales the older rents by how much the same group's median changed between the two years (or, for thin private groups, by the URA rental index). It is an estimate."),
      h4("How accurate is it? (back-test)"),
      p("We hid the last 3 months of contracts, benchmarked each one using only earlier contracts, and checked how often it landed inside the typical range (about 50% is what a well-calibrated range should give) and how far it was from the median."),
      tableOutput("backtest_table"),
      h5("Where it is least reliable"),
      p(class = "text-muted small", "Towns, districts and types with at least 30 held-out contracts, ranked by median % error (worst first)."),
      tableOutput("backtest_worst"),
      h4("What the data cannot tell us"),
      tags$ul(
        tags$li("These are signed rents, not asking prices. Listings are often higher than what is finally agreed."),
        tags$li("The flat's own floor level, view, furnishing and condition are not in any public rental data. 'Tallest floor in block' is the height of the whole block, not the flat's floor."),
        tags$li("Lease left is approximate: 99 years minus the block's age. The lease usually starts slightly before the block is completed."),
        tags$li("Distance to MRT is a straight line to the nearest exit, not walking distance. It counts only stations that had opened by the month of each contract."),
        tags$li("Places near a home (stations, hawker centres, schools and so on) are today's locations, not those at each rental date. Only the MRT/LRT distances used in the rent filters respect opening dates. Walking routes come from OneMap when switched on; otherwise the page shows a straight-line distance and a rough walk time. Only schools in the MOE directory are included (no kindergartens, international schools or universities)."),
        tags$li("Private homes are grouped into locations. Condos and ECs are grouped by project name, street and district. URA gives some contracts a generic name (such as 'Non-landed housing development') that covers unrelated sites, so any group whose contracts are more than 500 m apart is split by coordinate, and landed homes are always separate by coordinate. The 500 m limit is a rule of thumb. Most landed locations have few contracts, so their benchmark usually comes from a wider level."),
        tags$li("Differences by MRT distance or block age are associations, not causes. Other things (estate, flat size, condition) differ too."),
        tags$li("A block or project missing from the maps or search had no rental contract of the selected type in the data (or, on the maps, none in the last 12 months). That does not mean it has no flats or units for rent; it means no signed rent was recorded for it. Its result falls back to nearby or town/district comparables."),
        tags$li("197 HDB contracts and 1,935 private contracts have no location. They count in town and district benchmarks, but not on the maps or in the block-age and MRT filters."),
        tags$li("HDB towns and URA planning areas do not line up exactly. The calculator uses HDB towns; the map uses planning areas, based on each block's real location."),
        tags$li("HDB records only give town and flat type, with no floor area. Private records give floor area in bands (for example 700-800 sqft), so rent per sqft is approximate."),
        tags$li("Bedroom count is missing for some private contracts and for landed homes, so those drop out when you filter by bedrooms.")
      )
    ))
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ---- Home: pick a role, then go to the rent check page ----
  observeEvent(input$go_tenant, {
    updateRadioButtons(session, "s_role", selected = "tenant")
    nav_select("nav", "main")
  })
  observeEvent(input$go_landlord, {
    updateRadioButtons(session, "s_role", selected = "landlord")
    nav_select("nav", "main")
  })

  # ---- keep the block / project / floor-area dropdowns in step with the other inputs ----
  pending <- reactiveValues(block = NULL, project = NULL, band_rng = NULL)   # waiting for their dropdowns
  shortlist <- reactiveVal(character(0))                                     # saved "blk|..." / "prj|..." ids

  # ---- Share link (URL bookmarking): restore the block / project / size, which load after the area ----
  setBookmarkExclude(c("map_bounds", "map_center", "map_zoom", "map_marker_click", "map_marker_mouseover",
                       "map_marker_mouseout", "map_shape_click", "map_shape_mouseover", "map_shape_mouseout",
                       "map_click", "map_groups", "map_opened", "map_acc", "more_filters", "s_search",
                       "pick_place", "shortlist_toggle", "go_tenant", "go_landlord", "l_list",
                       "broaden_band", "broaden_mrt", "broaden_age", "jump",
                       "e_map_bounds", "e_map_center", "e_map_zoom", "e_map_marker_click", "e_map_marker_mouseover",
                       "e_map_marker_mouseout", "e_map_shape_click", "e_map_shape_mouseover", "e_map_shape_mouseout",
                       "e_map_click", "e_map_groups", "explore_check", "go_explore", "go_fav", "fav_to_main", "fav_to_explore", "s_search_pick", "stored_favourites"))
  onBookmark(function(state) state$values$shortlist <- shortlist())
  favourites_loaded <- reactiveVal(FALSE)
  bookmark_favourites <- FALSE
  session$onFlushed(function() {
    session$sendCustomMessage("rentLoadFavourites", list())
  }, once = TRUE)
  observeEvent(input$stored_favourites, {
    ids <- as.character(unlist(input$stored_favourites$ids, use.names = FALSE))
    valid <- all_search_index$id[grepl("^(blk|prj)\\|", all_search_index$id)]
    if (!bookmark_favourites) shortlist(head(unique(ids[ids %in% valid]), 6))
    favourites_loaded(TRUE)
  }, once = TRUE)
  observe({
    req(favourites_loaded())
    session$sendCustomMessage("rentSaveFavourites", list(ids = unname(as.list(shortlist()))))
  })

  # Replace the default bookmark dialog with a compact URL and copy button.
  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Share this result",
      p("Copy this link to share your current selections and saved shortlist."),
      tags$label(
        `for` = "result-share-url", "Result link"
      ),
      tags$textarea(
        id = "result-share-url", class = "form-control",
        readonly = "readonly", rows = 4,
        style = "width:100%; overflow-wrap:anywhere; resize:vertical;",
        onclick = "this.select();", url
      ),
      tags$p(
        id = "result-share-status", role = "status",
        `aria-live` = "polite", class = "text-muted",
        style = "margin-top:12px;"
      ),
      footer = tagList(
        modalButton("Close"),
        tags$button(
          type = "button", class = "btn btn-primary",
          onclick = "return copyResultLink(this);", "Copy link"
        )
      ),
      easyClose = TRUE
    ))
  })

  # onRestore runs before any observer, so the dropdown observers below pick these up
  onRestore(function(state) {
    bookmark_favourites <<- TRUE
    pending$block    <- state$input$s_block
    pending$project  <- state$input$s_project
    pending$band_rng <- state$input$s_band_rng
    if (length(state$values$shortlist)) shortlist(unlist(state$values$shortlist))
  })
  onRestored(function(state) nav_select("nav", "main", session = session))

  set_block_choices <- function(town, selected = "any") {
    if (!isTruthy(town)) {
      updateSelectizeInput(session, "s_block", choices = c("Choose a town first" = "any"), selected = "any", server = TRUE)
      return(invisible(NULL))
    }
    ch <- block_choices_by_town[[town]]
    if (is.null(ch)) ch <- setNames("any", paste("All blocks in", town_name(town)))
    updateSelectizeInput(session, "s_block", choices = ch, selected = selected, server = TRUE)
  }
  set_project_choices <- function(district, group, selected = "any") {
    updateSelectizeInput(session, "s_project", choices = project_choices(district, group),
                         selected = selected, server = TRUE)
  }

  observeEvent(input$s_town, {
    keep <- or_any(pending$block)
    pending$block <- NULL
    set_block_choices(input$s_town, keep)
  })
  observeEvent(list(input$s_district, input$s_group), {
    req(input$s_district, input$s_group)
    keep <- or_any(pending$project)
    pending$project <- NULL
    set_project_choices(input$s_district, input$s_group, keep)
  })
  # When the property type changes, show only the floor-area bands that exist for it
  observeEvent(input$s_group, {
    ticks <- size_ticks(input$s_group)
    keep  <- pending$band_rng
    pending$band_rng <- NULL
    if (length(keep) != 2 || !all(keep %in% ticks)) keep <- range_ends(ticks)
    shinyWidgets::updateSliderTextInput(session, "s_band_rng", choices = ticks, selected = keep)
  })

  # ---- "Not enough data" buttons ----
  observeEvent(input$broaden_band, {
    shinyWidgets::updateSliderTextInput(session, "s_band_rng", selected = range_ends(size_ticks(input$s_group)))
  })
  observeEvent(input$broaden_mrt, {
    updateSelectInput(session, "s_mrt", selected = "any")
    updateSelectInput(session, "s_pmrt", selected = "any")
  })
  observeEvent(input$broaden_age, updateSelectInput(session, "s_age", selected = "any"))

  # Switching between HDB and private clears the asking rent. Changing block / town keeps it,
  # so a tenant can use it as a budget and click through the blocks that fit.
  observeEvent(input$s_market, {
    if (identical(input$nav, "main")) updateNumericInput(session, "t_asking", value = NA_real_)
  }, ignoreInit = TRUE)

  sel   <- reactive(get_sel(input, "s_"))
  bench <- reactive(get_bench(input, "s_"))

  # ---- The one search bar (and map clicks) set the Area and Block / Project dropdowns ----
  # Keep the complete index in the browser for instant searching.
  # Render only 100 matches initially; scrolling progressively reveals the rest.
  updateSelectizeInput(session, "s_search", choices = search_choices(all_search_index),
                       selected = character(0), server = FALSE)

  apply_pick <- function(id) {
    parts <- str_split(id, fixed("|"))[[1]]
    kind  <- parts[1]
    if (kind %in% c("town", "blk")) {
      town <- parts[2]
      key  <- if (kind == "blk" && length(parts) >= 4) paste(parts[3], parts[4], sep = "|") else "any"
      if (!identical(input$s_market, "hdb")) updateRadioButtons(session, "s_market", selected = "hdb")
      if (identical(as.character(input$s_town), town)) {
        set_block_choices(town, key)
      } else {
        pending$block <- key
        updateSelectInput(session, "s_town", selected = town)
      }
    } else if (kind == "dist") {
      d <- parts[2]
      if (!identical(input$s_market, "private")) updateRadioButtons(session, "s_market", selected = "private")
      if (identical(as.character(input$s_district), d)) {
        set_project_choices(d, input$s_group %||% group_choices[1], "any")
      } else {
        pending$project <- "any"
        updateSelectInput(session, "s_district", selected = d)
      }
    } else if (kind == "prj") {
      loc <- locations[locations$location_id == parts[2], ]
      if (!nrow(loc)) return(invisible(NULL))
      d <- as.character(loc$district[1])
      g <- as.character(loc$property_group[1])
      if (!identical(input$s_market, "private")) updateRadioButtons(session, "s_market", selected = "private")
      if (identical(as.character(input$s_district), d) && identical(input$s_group, g)) {
        set_project_choices(d, g, loc$location_id[1])
      } else {
        pending$project <- loc$location_id[1]
        updateSelectInput(session, "s_district", selected = d)
        updateSelectInput(session, "s_group", selected = g)
      }
    }
    invisible(NULL)
  }


  # Reflect selections from maps, filters and bookmarked URLs in the search label.
  observe({
    current <- sel()
    id <- ""
    if (identical(current$market, "hdb") && isTruthy(current$town)) {
      id <- if (is_set(current$block)) {
        paste("blk", current$town, current$block, current$street, sep = "|")
      } else paste("town", current$town, sep = "|")
    } else if (identical(current$market, "private") && isTruthy(current$district)) {
      id <- if (is_set(current$project)) paste("prj", current$project, sep = "|") else
        paste("dist", current$district, sep = "|")
    }
    row <- match(id, all_search_index$id)
    message <- if (!is.na(row)) {
      list(id = id, label = all_search_index$text[row])
    } else {
      list(id = "", label = "")
    }
    # Send after input updates, which otherwise clear the label during restoration.
    session$onFlushed(function() {
      session$sendCustomMessage("rentSearchSelection", message)
    }, once = TRUE)
  })

  # Keep the selected label visible; reopening the dropdown clears only the typed query.
  observeEvent(input$s_search_pick, {
    id <- input$s_search_pick
    req(length(id) == 1, nzchar(id))
    apply_pick(id)
  })

  # A click on a dot only opens its popup (a preview); the popup's "Select this block" button selects it,
  # so browsing the map never changes the results by accident.

  observeEvent(input$map_shape_click, {
    pa <- input$map_shape_click$id
    if (is.null(pa) || !pa %in% planning_areas$planning_area) return()
    updateSelectInput(session, "s_pa", selected = pa)
    updateRadioButtons(session, "s_trend_level", selected = "pa")
  })

  # ---- Result cards (tenant or landlord) ----
  asking_ok <- function(a) length(a) == 1L && isTRUE(is.finite(a)) && a > 0

  tenant_result <- function(b, sel, head) {
    s      <- b$summary
    dir    <- price_direction(b$seg, b$value_col, s$as_of)
    asking <- input$t_asking
    has_ask <- asking_ok(asking)
    chk <- if (has_ask) tenant_check(b, asking) else NULL

    verdict_box <- if (!has_ask) {
      value_box("Your asking rent", "Enter it above",
                p(class = "small", "We'll show where it sits among real signed rents."), theme = "light")
    } else {
      theme <- if (chk$percentile < 0.25) "success" else if (chk$percentile <= 0.75) "info" else "danger"
      gap   <- chk$gap_month
      verdict <- if (chk$percentile < 0.25) "Below typical" else if (chk$percentile <= 0.75) "Typical" else if (chk$percentile <= 0.90) "Above typical" else "Well above typical"
      value_box(paste("Your asking rent:", money(asking)), verdict,
                p(class = "small", chk$label), theme = theme)
    }

    detail <- if (has_ask) {
      gap_text <- if (chk$gap_month > 0) {
        tagList(sprintf("%s per month above the median (%s more over a 12-month lease).",
                        money(chk$gap_month), money(chk$gap_year)), br(),
                strong(sprintf("Suggested counter-offer: %s", money(chk$counter))),
                " (the median, rounded to the nearest $50).")
      } else if (chk$gap_month < 0) {
        sprintf("%s per month below the median (%s less over a 12-month lease). No counter-offer needed on price alone.",
                money(-chk$gap_month), money(-chk$gap_year))
      } else {
        "Exactly at the median."
      }
      card(card_body(
        range_strip(b, asking, who = "You", role = "tenant"),
        p(class = "small text-muted mt-3 mb-0", position_text(chk, asking), " ", gap_text)
      ))
    }

    tagList(
      head,
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Median rent", tagList(money(s$median), span(class = "value-unit", " / month")),
                  p(class = "small", sprintf("Price direction: %s (last 6 months vs the 6 before)", signed_pct(dir))),
                  theme = "primary", class = "median-box"),
        value_box("Typical range", paste(money(s$p25), "to", money(s$p75)),
                  p(class = "small", "The middle half of comparable signed rents"), theme = "light"),
        verdict_box
      ),
      detail,
      why_box(b)
    )
  }

  landlord_result <- function(b, sel, head) {
    s   <- b$summary
    r   <- landlord_range(b)
    yoy <- yoy_change(b$seg, b$value_col, s$as_of)
    rng <- listing_slider_range(b)
    tagList(
      head,
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Median rent (your anchor)", tagList(money(r$mid), span(class = "value-unit", " / month")),
                  p(class = "small", sprintf("%s comparable contracts", format(s$n, big.mark = ","))),
                  theme = "primary", class = "median-box"),
        value_box("Suggested listing range", paste(money(r$low), "to", money(r$high)),
                  p(class = "small", "Half of comparable homes rented within this range"), theme = "light"),
        value_box("Year on year", signed_pct(yoy),
                  p(class = "small", "Last 12 months vs the 12 before (middle 80% of contracts)"), theme = "light")
      ),
      card(card_body(
        sliderInput("l_list", "If I list at ($ per month)", min = rng[1], max = rng[2],
                    value = min(max(round50(r$mid), rng[1]), rng[2]), step = 50, pre = "$", width = "100%"),
        uiOutput("l_list_share"),
        p(class = "text-muted small mb-0",
          "Where you sit in the range depends on things this data cannot see: the flat's floor, view, furnishing, condition and lease length. These are signed rents, so listing near the median or slightly above is a judgement call.")
      )),
      why_box(b)
    )
  }

  area_set <- reactive({
    if (identical(input$s_market, "private")) isTruthy(input$s_district) else isTruthy(input$s_town)
  })

  output$result <- renderUI({
    if (!area_set()) {
      return(div(class = "empty-state",
                 icon("magnifying-glass-location"),
                 h3("Search a town, block or condo to start"),
                 p("Use the search bar above, pick an area from the filters, or open the map below and click a dot.")))
    }
    b   <- bench()
    s   <- b$summary
    sl  <- sel()
    seg <- describe_segment(sl)
    if (s$status != "ok") return(too_few_card(seg, b, sl))
    head <- tagList(div(class = "result-title", seg), data_line(b, sl), broad_warning(sl))
    if (identical(input$s_role, "landlord")) landlord_result(b, sl, head) else tenant_result(b, sl, head)
  })

  output$plot <- renderPlot({
    b <- bench()
    req(b$summary$status == "ok")
    if (identical(input$s_role, "landlord")) {
      plot_comparables(b, line_at = input$l_list, line_label = "Orange line: your listing price")
    } else {
      plot_comparables(b, line_at = if (asking_ok(input$t_asking)) input$t_asking else NULL)
    }
  })

  output$block_info <- renderUI(block_card(sel(), shortlist()))

  # ---- Tenant: which blocks / projects fit the asking rent (budget) ----
  # Uses the selected town / district and flat / property type, last 12 months, places with 3+ rentals.
  # share = fraction of a place's rentals at or under the budget.
  budget_places <- reactive({
    req(identical(input$s_role, "tenant"), asking_ok(input$t_asking))
    a  <- input$t_asking
    sl <- sel()
    if (sl$market == "hdb") {
      req(sl$town, sl$flat)
      d <- hdb12 |>
        filter(town == sl$town, flat_type == sl$flat) |>
        group_by(town, block, street_name) |>
        summarise(n = n(), k = sum(monthly_rent <= a), median = median(monthly_rent), .groups = "drop") |>
        left_join(select(block_info, town, block, street_name, year_completed, nearest_station, dist_mrt_m),
                  by = c("town", "block", "street_name")) |>
        mutate(id = paste("blk", town, block, street_name, sep = "|"),
               name = paste("Blk", block, str_to_title(street_name)))
      where <- paste0(town_name(sl$town), " \u00b7 ", sl$flat)
      unit  <- "blocks"
    } else {
      req(sl$district, sl$group)
      d <- priv12 |>
        filter(as.character(district) == as.character(sl$district), property_group == sl$group, !is.na(rent))
      if (is_set(sl$beds)) d <- filter(d, bedrooms_grp == sl$beds)
      d <- d |>
        group_by(location_id) |>
        summarise(n = n(), k = sum(rent <= a), median = median(rent),
                  nearest_station = first(na.omit(nearest_station)),
                  dist_mrt_m = median(dist_mrt_m, na.rm = TRUE), .groups = "drop") |>
        mutate(year_completed = NA_integer_,
               id = paste("prj", location_id, sep = "|"),
               name = location_label(location_id))
      where <- paste0(district_label(sl$district), " \u00b7 ", sl$group,
                      if (is_set(sl$beds)) paste0(", ", sl$beds, "-bedroom") else "")
      unit  <- "projects"
    }
    d <- d |> filter(n >= 3) |> mutate(share = k / n)
    list(all = d, fit = filter(d, share >= 0.25), asking = a, where = where, unit = unit)
  })

  output$budget_title <- renderUI({
    bp <- budget_places()
    ch_title(sprintf("%s that fit %s", str_to_sentence(bp$unit), money(bp$asking)),
             paste0(bp$where, ", last 12 months"))
  })

  output$budget_body <- renderUI({
    bp  <- budget_places()
    fit <- bp$fit
    if (!nrow(bp$all)) {
      return(card_body(p(class = "text-muted mb-0",
        sprintf("No %s here have 3+ rentals in the last 12 months, so there isn't enough data to check. Try a nearby area.", bp$unit))))
    }
    if (!nrow(fit)) {
      cheapest <- bp$all |> arrange(median) |> slice_head(n = 1)
      return(card_body(p(class = "mb-0", sprintf(
        "None of the %d %s here had even a quarter of their rentals at or under %s. The cheapest is %s, with a median of %s. See Cheaper areas nearby further down for other towns.",
        nrow(bp$all), bp$unit, money(bp$asking), cheapest$name, money(cheapest$median)))))
    }
    fit <- switch(input$budget_sort %||% "chance",
                  rent = arrange(fit, median),
                  mrt  = arrange(fit, dist_mrt_m),
                  new  = arrange(fit, desc(year_completed), median),
                  arrange(fit, desc(share), median))
    show_built <- bp$unit == "blocks"
    saved <- shortlist()
    rows <- lapply(seq_len(nrow(fit)), function(i) {
      r <- fit[i, ]
      likely <- r$share >= 0.5
      tags$tr(
        tags$td(star_icon(r$id, r$id %in% saved)),
        tags$td(tags$a(href = "#", `data-id` = r$id, onclick = "pickPlace(this); return false;", r$name)),
        tags$td(span(class = "badge", style = sprintf("background:%s;%s", budget_fill(r$share), if (likely) "" else "color:#212529;"),
                     if (likely) "Likely" else "Possible")),
        tags$td(sprintf("%d of %d (%.0f%%)", r$k, r$n, 100 * r$share)),
        tags$td(money(r$median)),
        tags$td(if (is.na(r$nearest_station) || !is.finite(r$dist_mrt_m)) "-"
                else paste0(str_to_title(r$nearest_station), ", ", fmt_dist(r$dist_mrt_m))),
        if (show_built) tags$td(if (is.na(r$year_completed)) "-" else r$year_completed)
      )
    })
    card_body(
      p(class = "mb-2",
        strong(sprintf("%d of %d %s", nrow(fit), nrow(bp$all), bp$unit)),
        sprintf(" had at least a quarter of their rentals at or under %s. Click a name to check it.", money(bp$asking))),
      div(class = "budget-scroll",
          tags$table(class = "table table-sm table-hover align-middle mb-0",
                     tags$thead(tags$tr(tags$th(""), tags$th(if (show_built) "Block" else "Project"), tags$th("Chance"),
                                        tags$th(sprintf("Rentals at or under %s", money(bp$asking))),
                                        tags$th("Median"), tags$th("Nearest MRT"),
                                        if (show_built) tags$th("Built"))),
                     tags$tbody(rows))),
      p(class = "text-muted small mb-0",
        sprintf("Likely: half or more of its rentals were at or under your budget. Possible: a quarter to half. Only %s with 3+ rentals in the last 12 months are checked. Tap the star to add one to your favourites. Open the map to see them coloured the same way.", bp$unit))
    )
  })

  observeEvent(input$pick_place, {
    id <- input$pick_place
    if (length(id) == 1 && grepl("^(blk|prj)\\|", id)) {
      apply_pick(id)
      if (!identical(input$nav, "main")) nav_select("nav", "main", session = session)
    }
  })

  # ---- Favourites tab: navbar count, slim bar, navigation ----
  output$fav_count <- renderText(length(shortlist()))
  observeEvent(input$go_fav, nav_select("nav", "favourites", session = session))
  observeEvent(input$fav_to_main, nav_select("nav", "main", session = session))
  observeEvent(input$fav_to_explore, nav_select("nav", "explore", session = session))

  output$fav_bar <- renderUI({
    n <- length(shortlist())
    if (n == 0) {
      return(div(class = "fav-bar empty",
                 span(class = "fav-star", "\u2606"),
                 "No favourites yet. Tap Add to favourites on a block, in the budget table or on the map to compare homes later."))
    }
    div(class = "fav-bar",
        span(class = "fav-star", "\u2605"),
        strong(sprintf("%d favourite%s saved", n, if (n == 1) "" else "s")),
        span("\u00b7"),
        tags$a(href = "#", onclick = "return goFav();", if (n == 1) "View it \u2192" else "Compare them \u2192"))
  })

  # ---- Shortlist: save homes and compare them side by side ----
  observeEvent(input$shortlist_toggle, {
    id  <- input$shortlist_toggle
    req(length(id) == 1, grepl("^(blk|prj)\\|", id))
    cur <- shortlist()
    if (id %in% cur) {
      shortlist(setdiff(cur, id))
    } else if (length(cur) >= 6) {
      showNotification("You can have up to 6 favourites. Remove one first.", type = "warning")
    } else {
      shortlist(c(cur, id))
      showNotification(tagList("\u2605 Added to favourites. ",
                               tags$a(href = "#", onclick = "return goFav();", style = "font-weight:700;", "View favourites")),
                       duration = 4)
    }
  })

  # One row per saved home, using the current flat type (HDB) or bedrooms (private), last 12 months
  shortlist_rows <- reactive({
    ids <- shortlist()
    req(length(ids) > 0)
    budget <- if (identical(input$s_role, "tenant") && asking_ok(input$t_asking)) input$t_asking else NULL
    bind_rows(lapply(ids, function(id) {
      parts <- str_split(id, fixed("|"))[[1]]
      if (parts[1] == "blk" && length(parts) >= 4) {
        flat <- input$s_flat %||% "4-ROOM"
        v  <- hdb12$monthly_rent[hdb12$town == parts[2] & hdb12$block == parts[3] &
                                 hdb12$street_name == parts[4] & hdb12$flat_type == flat]
        bi <- block_info[block_info$town == parts[2] & block_info$block == parts[3] & block_info$street_name == parts[4], ][1, ]
        tibble(id = id, name = paste("Blk", parts[3], str_to_title(parts[4])), area = town_name(parts[2]),
               what = flat_nice(flat), v = list(v), station = bi$nearest_station, dist = bi$dist_mrt_m,
               built = if (is.na(bi$year_completed)) "-" else as.character(bi$year_completed))
      } else if (parts[1] == "prj") {
        d <- priv12[priv12$location_id == parts[2] & !is.na(priv12$rent), ]
        g <- d$property_group[1] %||% NA_character_
        if (is_set(input$s_beds) && !identical(g, "Landed")) d <- d[d$bedrooms_grp %in% input$s_beds, ]
        loc <- locations[locations$location_id == parts[2], ][1, ]
        tibble(id = id, name = location_label(parts[2]), area = district_code(loc$district),
               what = paste0(if (is_set(input$s_beds)) beds_nice(input$s_beds) else "", type_nice(loc$property_group)),
               v = list(d$rent), station = first(na.omit(d$nearest_station)) %||% NA_character_,
               dist = suppressWarnings(median(d$dist_mrt_m, na.rm = TRUE)), built = "-")
      } else NULL
    })) |>
      mutate(n      = lengths(v),
             median = vapply(v, function(x) if (length(x)) median(x) else NA_real_, 0),
             p25    = vapply(v, function(x) if (length(x)) unname(quantile(x, .25)) else NA_real_, 0),
             p75    = vapply(v, function(x) if (length(x)) unname(quantile(x, .75)) else NA_real_, 0),
             share  = if (is.null(budget)) NA_real_ else vapply(v, function(x) if (length(x)) mean(x <= budget) else NA_real_, 0))
  })

  output$shortlist_card <- renderUI({
    ids <- shortlist()
    if (!length(ids)) {
      return(card(card_body(class = "fav-empty",
        div(class = "fav-big-star", "\u2606"),
        h3("No favourites yet"),
        p(class = "text-muted mb-0", "Save homes while you search, then compare up to 6 of them side by side here. You can add a favourite from:"),
        tags$ul(
          tags$li(strong("About this block / project"), " on Rent check: the yellow Add to favourites button"),
          tags$li(strong("Blocks that fit your budget"), " on Rent check: the star beside each block"),
          tags$li(strong("The map"), " on Rent check or Explore: Add to favourites in a dot's popup")),
        div(class = "d-flex justify-content-center gap-2",
            actionButton("fav_to_main", "Go to Rent check", class = "btn-primary"),
            actionButton("fav_to_explore", "Go to Explore", class = "btn-outline-primary")))))
    }
    d <- shortlist_rows()
    budget <- if (identical(input$s_role, "tenant") && asking_ok(input$t_asking)) input$t_asking else NULL
    multi <- nrow(d) >= 2

    # Which home "wins" each comparison (only when there are 2+ to compare)
    best <- function(x, fun) {
      ok <- is.finite(x)
      if (!multi || sum(ok) < 2) return(rep(FALSE, length(x)))
      ok & x == fun(x[ok])
    }
    built_num <- suppressWarnings(as.numeric(d$built))
    win_cheap <- best(ifelse(d$n > 0, d$median, NA), min)
    win_mrt   <- best(d$dist, min)
    win_new   <- best(built_num, max)
    win_odds  <- if (is.null(budget)) rep(FALSE, nrow(d)) else best(d$share, max)

    # 1) Comparison strip: every home's typical range on one shared scale
    vals <- c(d$p25[d$n >= 3], d$p75[d$n >= 3], d$median[d$n > 0], budget)
    strip <- NULL
    if (multi && length(vals)) {
      lo <- min(vals, na.rm = TRUE); hi <- max(vals, na.rm = TRUE)
      pad <- max(0.06 * (hi - lo), 100); lo <- lo - pad; hi <- hi + pad
      pos <- function(x) 100 * (x - lo) / (hi - lo)
      bars <- lapply(seq_len(nrow(d)), function(i) {
        r <- d[i, ]
        div(class = "cmp-row",
            div(class = "cmp-name", r$name),
            div(class = "cmp-track",
                if (r$n >= 3) div(class = "cmp-bar", style = sprintf("left:%.1f%%;width:%.1f%%", pos(r$p25), pos(r$p75) - pos(r$p25))),
                if (r$n > 0) div(class = "cmp-med", style = sprintf("left:%.1f%%", pos(r$median)), title = money(r$median)),
                if (r$n > 0) span(class = "cmp-medlab", style = sprintf("left:%.1f%%", pos(r$median)), money(r$median)),
                if (!is.null(budget)) div(class = "cmp-budget", style = sprintf("left:%.1f%%", pos(budget)))))
      })
      strip <- div(class = "cmp-strip",
                   bars,
                   div(class = "cmp-axis", span(money(lo)), span(money(hi))),
                   div(class = "cmp-key",
                       span(span(class = "cmp-key-bar"), "Typical range"),
                       span(span(class = "cmp-key-med"), "Median"),
                       if (!is.null(budget)) span(span(class = "cmp-key-budget"), paste("Your budget", money(budget)))))
    }

    # 2) One card per home, side by side
    stat_row <- function(label, value, win = FALSE) {
      div(class = paste("cmp-stat", if (win) "win"), span(class = "cmp-stat-label", label), span(class = "cmp-stat-value", value))
    }
    cards <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      badges <- Filter(Negate(is.null), list(
        if (win_cheap[i]) span(class = "badge bg-success", "Cheapest"),
        if (win_odds[i])  span(class = "badge bg-success", "Best chance"),
        if (win_mrt[i])   span(class = "badge bg-info", "Closest to MRT"),
        if (win_new[i])   span(class = "badge bg-secondary", "Newest")))
      div(class = "cmp-card",
          div(class = "d-flex align-items-start gap-2",
              div(tags$a(href = "#", class = "cmp-title", `data-id` = r$id, onclick = "pickPlace(this); return false;", r$name),
                  div(class = "small text-muted", paste(r$what, "·", r$area))),
              tags$a(href = "#", class = "ms-auto text-muted", `data-id` = r$id, title = "Remove from favourites",
                     onclick = "return toggleShortlist(this);", "✕")),
          div(class = "cmp-badges", badges),
          div(class = "cmp-median", if (r$n) money(r$median) else "-", span(class = "cmp-per", " / month median")),
          stat_row("Typical range", if (r$n >= 3) paste(money(r$p25), "to", money(r$p75)) else if (r$n) "Too few rentals" else "No rentals"),
          stat_row("Rentals, last 12 months", r$n),
          if (!is.null(budget)) stat_row(sprintf("At or under %s", money(budget)),
                                         if (is.na(r$share)) "-" else sprintf("%.0f%% of rentals", 100 * r$share), win_odds[i]),
          stat_row("Nearest MRT", if (is.na(r$station) || !is.finite(r$dist)) "-" else paste0(str_to_title(r$station), ", ", fmt_dist(r$dist)), win_mrt[i]),
          stat_row("Built", r$built, win_new[i]),
          tags$a(href = "#", class = "btn btn-outline-primary btn-sm w-100 mt-2", `data-id` = r$id,
                 onclick = "pickPlace(this); return false;", "Check this home"))
    })

    card(
      card_header(ch_title("★ Favourites",
                           if (multi) sprintf("Comparing %d homes side by side · last 12 months", nrow(d))
                           else "Star another home to compare side by side")),
      card_body(
        strip,
        div(class = "cmp-grid", cards),
        p(class = "text-muted small mb-0",
          "HDB homes use the flat type picked above; private homes use the bedrooms picked above. Add up to 6 with the ☆ star on a block, in the budget table or on the map.")
      )
    )
  })

  # ---- Section shortcuts ----
  output$jump_bar <- renderUI({
    lord <- identical(input$s_role, "landlord")
    link <- function(id, label) tags$a(href = "#", onclick = sprintf("return jumpTo('%s');", id), label)
    div(class = "jump-bar",
        span(class = "jb-label", "Jump to"),
        link("result", "Summary"),
        if (!lord && asking_ok(input$t_asking)) link("sec-budget", "Homes that fit"),
        link("sec-spread", "Rent spread"),
        if (length(shortlist())) tags$a(href = "#", onclick = "return goFav();", sprintf("\u2605 Favourites (%d)", length(shortlist()))),
        link("sec-map", "Map"),
        link("sec-nearby", "Nearby"),
        link("sec-alt", if (lord) "What moves rent" else "Cheaper areas"),
        if (lord) link("sec-demand", "Demand and seasons"),
        link("sec-trend", "Trend"))
  })

  # ---- Landlord: one-line takeaway above the demand / seasonality charts ----
  output$l_summary <- renderUI({
    req(area_set())
    b <- bench()
    req(b$summary$status == "ok")
    vol <- monthly_volume(b$seg, b$summary$as_of)
    ss  <- seasonality(b$seg, b$value_col)
    parts <- c(
      if (nrow(vol)) sprintf("%s similar homes rented in the last 12 months", format(sum(tail(vol$n, 12)), big.mark = ",")),
      if (nrow(ss)) sprintf("busiest month: %s (about %d a month)",
                            as.character(ss$cal_month[which.max(ss$avg_contracts)]), round(max(ss$avg_contracts))),
      if (nrow(ss)) sprintf("rents run highest in %s (%+.0f%% vs trend) and lowest in %s (%+.0f%%)",
                            as.character(ss$cal_month[which.max(ss$effect)]), 100 * max(ss$effect),
                            as.character(ss$cal_month[which.min(ss$effect)]), 100 * min(ss$effect))
    )
    if (!length(parts)) parts <- "Not enough history to spot seasonal patterns for this selection"
    card(card_body(class = "py-3",
                   div(class = "l-summary", icon("lightbulb"), cap1(paste(parts, collapse = " \u00b7 ")), ".")))
  })

  # ---- Tenant extras ----
  output$t_alt <- renderUI({
    sl  <- sel()
    alt <- if (sl$market == "hdb") {
      nearby_alternatives(hdb, "hdb", sl, hdb_centroids)
    } else {
      nearby_alternatives(private, "private", sl, priv_centroids)
    }
    tab <- alternatives_table(alt)
    if (is.null(tab)) return(NULL)
    if (sl$market == "hdb") {
      tab$Area <- town_name(tab$Area)
      names(tab)[names(tab) == "vs your selection"] <- paste("vs", town_name(sl$town), "median")
    } else {
      tab$Area <- district_label(tab$Area)
      names(tab)[names(tab) == "vs your selection"] <- sprintf("vs D%02d median", as.integer(as.character(sl$district)))
    }
    card(card_header(ch_title("Cheaper areas nearby", "Same flat / property type, last 12 months")),
         html_table(tab),
         p(class = "text-muted small",
           "Areas within 5 km (or the 6 nearest), sorted by median rent. The comparison is with the whole town or district median, not just your block or project."))
  })

  # ---- Landlord extras ----
  output$l_list_share <- renderUI({
    b <- bench()
    req(b$summary$status == "ok", input$l_list)
    chk <- tenant_check(b, input$l_list)
    tagList(range_strip(b, input$l_list, who = "Your listing", role = "landlord"),
            p(class = "small text-muted mt-3 mb-0", position_text(chk, input$l_list)))
  })

  output$l_demand <- renderPlot({
    b   <- bench()
    vol <- monthly_volume(b$seg, b$summary$as_of)
    ggplot(vol, aes(month, n)) +
      geom_col(fill = "#6baed6") +
      labs(x = NULL, y = "Contracts signed", subtitle = paste("Comparable level:", b$level)) +
      theme_minimal(base_size = 12)
  })

  output$l_season <- renderPlot({
    b  <- bench()
    ss <- seasonality(b$seg, b$value_col)
    validate(need(nrow(ss) > 0, "Not enough months of data for this selection to separate seasonality from the overall trend."))
    ss |>
      transmute(cal_month,
                `Rent vs the 12-month trend (%)` = 100 * effect,
                `Average contracts per month`    = avg_contracts) |>
      pivot_longer(-cal_month, names_to = "metric", values_to = "value") |>
      ggplot(aes(cal_month, value, fill = value >= 0)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 0, colour = "grey40") +
      scale_fill_manual(values = c(`TRUE` = "#6baed6", `FALSE` = "#fc9272")) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12)
  })

  output$l_premium <- renderUI({
    req(area_set())
    sl <- sel()
    lp <- if (sl$market == "hdb") location_premium(hdb, "hdb", sl) else location_premium(private, "private", sl)
    if (is.null(lp)) return(NULL)
    mrt_tab <- premium_table(lp$mrt, "Distance to MRT")
    age_tab <- premium_table(lp$age, "Block age")
    where <- if (sl$market == "hdb") paste(sl$flat, "flats in", town_name(sl$town)) else paste0(sl$group, ", ", district_label(sl$district))
    card(
      card_header(ch_title("What moves rent here", paste0(where, ", last 12 months"))),
      p(sprintf("All %s contracts: median %s. Groups with fewer than %d contracts are hidden.",
                format(lp$n, big.mark = ","), money(lp$overall), MIN_N)),
      html_table(mrt_tab),
      html_table(age_tab),
      p(class = "text-muted small",
        "These are associations, not causes: flats near MRT or in newer blocks may also differ in size, estate and condition. Distance is a straight line to the nearest exit.")
    )
  })

  # ---- Maps: shared background + layer drawing (used by Rent check "map" and Explore "e_map") ----
  base_map <- function() {
    # Background maps that need no API key. OneMap (SLA) requires its logo and credit to be shown.
    onemap_credit <- paste0(
      '<img src="https://www.onemap.gov.sg/web-assets/images/logo/om_logo.png" style="height:20px;width:20px;"/>',
      '&nbsp;<a href="https://www.onemap.gov.sg/" target="_blank">OneMap</a>&nbsp;&copy;&nbsp;contributors',
      '&nbsp;&#124;&nbsp;<a href="https://www.sla.gov.sg/" target="_blank">Singapore Land Authority</a>')
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles(urlTemplate = "https://www.onemap.gov.sg/maps/tiles/Grey/{z}/{x}/{y}.png",
               attribution = onemap_credit, group = "OneMap (grey)",
               options = tileOptions(minZoom = 11, maxZoom = 19)) |>
      addTiles(group = "OpenStreetMap") |>
      addLayersControl(baseGroups = c("OneMap (grey)", "OpenStreetMap"),
                       position = "topright", options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = 103.82, lat = 1.352, zoom = 11) |>
      htmlwidgets::onRender("
        function (el, x) {
          var map = this;
          // Redraw whenever the map box changes size (tab switch, accordion, full screen, phone layout).
          // Without this a map first drawn at the wrong size stays grey.
          if (window.ResizeObserver) new ResizeObserver(function () { map.invalidateSize(); }).observe(el);
          setTimeout(function () { map.invalidateSize(); }, 400);
          // If OneMap tiles fail to load, switch to OpenStreetMap
          var switched = false;
          map.eachLayer(function (l) {
            if (l instanceof L.TileLayer && l._url && l._url.indexOf('onemap') > -1) {
              l.on('tileerror', function () {
                if (switched) return;
                switched = true;
                el.querySelectorAll('.leaflet-control-layers-base label').forEach(function (lab) {
                  if (lab.textContent.indexOf('OpenStreetMap') > -1) lab.querySelector('input').click();
                });
              });
            }
          });
        }")
  }
  output$map   <- renderLeaflet(base_map())
  output$e_map <- renderLeaflet(base_map())

  # A map only exists once its tab / panel has been opened; leaflet then reports its bounds.
  map_ready   <- reactiveVal(FALSE)
  e_map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_bounds,   map_ready(TRUE),   once = TRUE)
  observeEvent(input$e_map_bounds, e_map_ready(TRUE), once = TRUE)

  # Popup buttons: on Rent check "Select this block" loads it; on Explore "Check this home" opens it in Rent check
  popup_links <- function(ids, mode, noun) {
    id_attr <- htmltools::htmlEscape(ids, attribute = TRUE)
    action  <- if (mode == "main") {
      sprintf('<a href="#" class="pop-select" data-id="%s" onclick="pickPlace(this); return false;">Select this %s &rarr;</a>', id_attr, noun)
    } else {
      sprintf('<a href="#" class="pop-select" data-id="%s" onclick="return checkHome(this);">Check this home &rarr;</a>', id_attr)
    }
    paste0("<br/>", action, "<br/>",
           sprintf('<a href="#" data-id="%s" onclick="return toggleShortlist(this);">☆ Add to favourites</a>', id_attr))
  }

  # Planning-area shading OR block / project dots (one colour scale at a time), plus optional MRT pins
  draw_layers <- function(map_id, mk, flat, group, beds, min_n, show_areas, show_mrt, budget, mode) {
    b_val <- if (is.null(budget)) Inf else budget
    proxy <- leafletProxy(map_id) |>
      clearGroup("areas") |> clearGroup("points") |> clearGroup("mrt") |>
      removeControl("area_legend") |> removeControl("property_legend")

    if (show_areas) {
      if (mk == "hdb") {
        req(flat)
        d  <- hdb12 |> filter(has_location, flat_type == flat)
        st <- area_stats(d, "monthly_rent")
        has_any <- pa_with_hdb
        none_label <- "No HDB flats"
      } else {
        req(group)
        d <- priv12 |> filter(property_group == group)
        if (group != "Landed" && is_set(beds)) d <- filter(d, bedrooms_grp == beds)
        st <- area_stats(d, "rent")
        has_any <- pa_with_private
        none_label <- "No private contracts"
      }
      shp <- planning_areas |>
        left_join(st, by = "planning_area") |>
        mutate(status = case_when(!planning_area %in% has_any ~ none_label,
                                  is.na(n) | n < MIN_N       ~ "Too few contracts",
                                  TRUE                       ~ "ok"),
               fill_value = if_else(status == "ok", median, NA_real_))
      dom <- shp$fill_value[!is.na(shp$fill_value)]
      if (length(dom) == 0) dom <- c(0, 1)
      pal <- colorNumeric("YlOrRd", domain = dom, na.color = "#d9d9d9")
      labels <- ifelse(shp$status == "ok",
                       sprintf("<strong>%s</strong><br/>Median %s<br/>Typical %s to %s<br/>%s contracts",
                               shp$planning_area, money(shp$median), money(shp$p25), money(shp$p75),
                               format(shp$n, big.mark = ",")),
                       sprintf("<strong>%s</strong><br/>%s", shp$planning_area, shp$status)) |>
        lapply(htmltools::HTML)
      proxy |>
        addPolygons(data = shp, layerId = ~planning_area, group = "areas",
                    fillColor = ~pal(fill_value), fillOpacity = 0.55,
                    color = "white", weight = 1, label = labels,
                    highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = FALSE)) |>
        addLegend(layerId = "area_legend", position = "bottomleft", pal = pal, values = dom,
                  labFormat = labelFormat(prefix = "$", big.mark = ","),
                  title = "Neighbourhood median rent<br/>($ per month, last 12 months)<br/><small>Grey: too few rentals</small>",
                  group = "areas")
    }

    if (mk == "hdb" && !show_areas) {
      req(flat)
      bl <- hdb12 |>
        filter(has_location, flat_type == flat) |>
        group_by(town, block, street_name) |>
        summarise(lat = first(lat), lon = first(lon), n = n(),
                  p25 = unname(quantile(monthly_rent, 0.25)), median = median(monthly_rent),
                  p75 = unname(quantile(monthly_rent, 0.75)),
                  year_completed = first(year_completed), nearest_station = first(nearest_station),
                  dist_mrt_m = first(dist_mrt_m), share = mean(monthly_rent <= b_val), .groups = "drop") |>
        filter(n >= min_n)
      if (nrow(bl) > 0) {
        pal2 <- colorNumeric("viridis", domain = bl$median)
        bl$fill  <- if (is.null(budget)) pal2(bl$median) else budget_fill(bl$share)
        bl$popup <- paste0(
          sprintf("<strong>Blk %s %s</strong><br/>%s &middot; completed %d<br/>Median %s (typical %s to %s)<br/>%d contracts, last 12 months<br/>Nearest MRT: %s, %s m",
                  bl$block, str_to_title(bl$street_name), town_name(bl$town), bl$year_completed, money(bl$median),
                  money(bl$p25), money(bl$p75), bl$n, bl$nearest_station, format(round(bl$dist_mrt_m))),
          popup_links(paste("blk", bl$town, bl$block, bl$street_name, sep = "|"), mode, "block"))
        proxy <- proxy |>
          addCircleMarkers(data = bl, lng = ~lon, lat = ~lat, group = "points",
                           layerId = ~paste("blk", town, block, street_name, sep = "|"),
                           radius = 7, stroke = TRUE, color = "#FFFFFF", weight = 1.5,
                           label = ~sprintf("Blk %s %s: median %s (%d rentals)", block, str_to_title(street_name), money(median), n),
                           fillColor = ~fill, fillOpacity = 0.85, popup = ~popup)
        if (is.null(budget)) {
          proxy |> addLegend(layerId = "property_legend", position = "bottomleft", pal = pal2, values = bl$median,
                             title = "Block median rent<br/>(last 12 months)", group = "points")
        } else {
          add_budget_legend(proxy, budget)
        }
      }
    }

    if (mk == "private" && !show_areas) {
      req(group)
      d <- priv12 |> filter(!is.na(lon), property_group == group)
      if (group != "Landed" && is_set(beds)) d <- filter(d, bedrooms_grp == beds)
      pr <- d |>
        filter(!is.na(rent)) |>
        group_by(location_id, district, property_group) |>
        summarise(planning_area = first(planning_area),
                  nearest_station = first(nearest_station), dist_mrt_m = median(dist_mrt_m),
                  n = n(), p25 = unname(quantile(rent, 0.25)), median = median(rent),
                  p75 = unname(quantile(rent, 0.75)), share = mean(rent <= b_val), .groups = "drop") |>
        filter(n >= min_n) |>
        left_join(select(locations, location_id, label, lon, lat), by = "location_id") |>
        filter(is.finite(lon), is.finite(lat))
      if (nrow(pr) > 0) {
        pal3 <- colorNumeric("viridis", domain = pr$median)
        pr$fill  <- if (is.null(budget)) pal3(pr$median) else budget_fill(pr$share)
        pr$popup <- paste0(
          sprintf("<strong>%s</strong><br/>%s, %s<br/>Nearest MRT: %s, about %s m<br/>Median %s (typical %s to %s)<br/>%d contracts, last 12 months",
                  pr$label, district_code(pr$district), pr$planning_area, pr$nearest_station,
                  format(round(pr$dist_mrt_m)), money(pr$median), money(pr$p25), money(pr$p75), pr$n),
          popup_links(paste("prj", pr$location_id, sep = "|"), mode, "project"))
        proxy <- proxy |>
          addCircleMarkers(data = pr, lng = ~lon, lat = ~lat, group = "points",
                           layerId = ~paste("prj", location_id, sep = "|"),
                           radius = ~pmin(3 + 1.2 * sqrt(n), 14), stroke = FALSE,
                           label = ~sprintf("%s: median %s (%d rentals)", label, money(median), n),
                           fillColor = ~fill, fillOpacity = 0.8, popup = ~popup)
        if (is.null(budget)) {
          proxy |> addLegend(layerId = "property_legend", position = "bottomleft", pal = pal3, values = pr$median,
                             title = "Project median rent<br/>(last 12 months)", group = "points")
        } else {
          add_budget_legend(proxy, budget)
        }
      }
    }

    if (show_mrt) {
      stations <- mrt_stations |> filter(is.finite(lon), is.finite(lat))
      proxy |>
        addAwesomeMarkers(
          data = stations, lng = ~lon, lat = ~lat,
          group = "mrt", layerId = ~paste0("station|", station),
          # Same purple train pin as the nearby-MRT pins, so blue always means a mall
          icon = makeAwesomeIcon(icon = AMEN_STYLE$mrt$icon, library = "fa",
                                 markerColor = AMEN_STYLE$mrt$colour, iconColor = "white"),
          label = ~str_to_title(station), popup = ~popup
        )
    }
    invisible(NULL)
  }

  # Rent check map: surrounding blocks / projects (the selected home's pin and nearby places are added below)
  observe({
    req(map_ready(), input$nav == "main", input$s_market)
    budget <- if (identical(input$s_role, "tenant") && isTRUE(input$s_budget_map) &&
                  asking_ok(input$t_asking)) input$t_asking else NULL
    draw_layers("map", input$s_market, input$s_flat, input$s_group, input$s_beds,
                min_n = if (isTRUE(input$s_enough)) 3 else 1, show_areas = isTRUE(input$s_areas),
                show_mrt = isTRUE(input$s_show_mrt), budget = budget, mode = "main")
  })

  # Explore map: the whole island
  observe({
    req(e_map_ready(), input$nav == "explore", input$e_market)
    budget <- if (isTRUE(input$e_budget_map) && asking_ok(input$e_budget)) input$e_budget else NULL
    draw_layers("e_map", input$e_market, input$e_flat, input$e_group, input$e_beds,
                min_n = if (isTRUE(input$e_enough)) 3 else 1, show_areas = isTRUE(input$e_areas),
                show_mrt = isTRUE(input$e_show_mrt), budget = budget, mode = "explore")
  })

  # ---- Explore: filters mirror the Rent check filters (only the visible tab pushes, so no ping-pong) ----
  sync <- function(from, to, tab, update) {
    observeEvent(input[[from]], {
      if (identical(input$nav, tab) && !identical(input[[from]], input[[to]])) update(to, input[[from]])
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  }
  upd_radio  <- function(id, v) updateRadioButtons(session, id, selected = v)
  upd_select <- function(id, v) updateSelectInput(session, id, selected = v)
  upd_num    <- function(id, v) updateNumericInput(session, id, value = if (is.null(v)) NA_real_ else v)
  sync("e_market", "s_market", "explore", upd_radio);  sync("s_market", "e_market", "main", upd_radio)
  sync("e_flat",   "s_flat",   "explore", upd_select); sync("s_flat",   "e_flat",   "main", upd_select)
  sync("e_group",  "s_group",  "explore", upd_select); sync("s_group",  "e_group",  "main", upd_select)
  sync("e_beds",   "s_beds",   "explore", upd_select); sync("s_beds",   "e_beds",   "main", upd_select)
  sync("e_budget", "t_asking", "explore", upd_num);    sync("t_asking", "e_budget", "main", upd_num)
  # Switching tabs: bring Explore up to date with whatever Rent check shows
  observeEvent(input$nav, {
    if (!identical(input$nav, "explore")) return()
    if (!identical(input$e_market, input$s_market)) upd_radio("e_market", input$s_market)
    if (!identical(input$e_flat, input$s_flat))     upd_select("e_flat", input$s_flat)
    if (!identical(input$e_group, input$s_group))   upd_select("e_group", input$s_group)
    if (!identical(input$e_beds, input$s_beds))     upd_select("e_beds", input$s_beds)
    if (!identical(input$e_budget, input$t_asking)) upd_num("e_budget", input$t_asking)
  }, ignoreInit = TRUE)

  # "Check this home" / "Check this area" on Explore: fill in Rent check and go there
  observeEvent(input$explore_check, {
    id <- input$explore_check
    req(length(id) == 1, grepl("^(blk|prj|town|dist)\\|", id))
    apply_pick(id)
    nav_select("nav", "main", session = session)
  })
  observeEvent(input$go_explore, nav_select("nav", "explore", session = session))
  # Focus Explore on the selected property or area when entering the tab.
  # Market/filter changes within Explore retain the user's current viewport.
  observeEvent(list(input$nav, e_map_ready()), {
    req(identical(input$nav, "explore"), e_map_ready())
    current <- isolate(sel())
    pt <- point_for(current)
    proxy <- leafletProxy("e_map", session = session) |>
      clearGroup("explore_focus")
    if (!is.null(pt) && is.finite(pt$lon) && is.finite(pt$lat)) {
      proxy |>
        addCircleMarkers(lng = pt$lon, lat = pt$lat, radius = 11,
                         color = "#c0392b", weight = 3, fill = FALSE,
                         group = "explore_focus",
                         label = "Selected home / location",
                         options = pathOptions(interactive = FALSE)) |>
        setView(lng = pt$lon, lat = pt$lat, zoom = 15)
    } else {
      # Use all located properties in the area, independent of rental filters.
      pts <- NULL
      if (identical(current$market, "hdb") && isTruthy(current$town)) {
        pts <- block_info |>
          filter(town == current$town, is.finite(lon), is.finite(lat))
      } else if (identical(current$market, "private") && isTruthy(current$district)) {
        pts <- locations |>
          filter(as.character(district) == as.character(current$district),
                 is.finite(lon), is.finite(lat))
      }
      if (!is.null(pts) && nrow(pts) > 0L) {
        # Small padding also handles areas containing a single located property.
        proxy |> fitBounds(min(pts$lon) - 0.002, min(pts$lat) - 0.002,
                           max(pts$lon) + 0.002, max(pts$lat) + 0.002)
      } else {
        proxy |> setView(lng = 103.82, lat = 1.352, zoom = 11)
      }
    }
  }, ignoreInit = FALSE)

  # ---- Explore: areas ranked ----
  explore_rank <- reactive({
    req(input$e_market)
    budget <- if (asking_ok(input$e_budget)) input$e_budget else NULL
    b_val  <- if (is.null(budget)) Inf else budget
    if (input$e_market == "hdb") {
      req(input$e_flat)
      d <- hdb12 |>
        filter(flat_type == input$e_flat) |>
        group_by(area = town) |>
        summarise(n = n(), median = median(monthly_rent), p25 = unname(quantile(monthly_rent, .25)),
                  p75 = unname(quantile(monthly_rent, .75)), share = mean(monthly_rent <= b_val), .groups = "drop") |>
        mutate(label = town_name(area), id = paste("town", area, sep = "|"))
      what <- paste(flat_nice(input$e_flat), "HDB flats")
    } else {
      req(input$e_group)
      d <- priv12 |> filter(property_group == input$e_group, !is.na(rent))
      if (input$e_group != "Landed" && is_set(input$e_beds)) d <- filter(d, bedrooms_grp == input$e_beds)
      d <- d |>
        group_by(area = as.character(district)) |>
        summarise(n = n(), median = median(rent), p25 = unname(quantile(rent, .25)),
                  p75 = unname(quantile(rent, .75)), share = mean(rent <= b_val), .groups = "drop") |>
        mutate(label = district_label(area), id = paste("dist", area, sep = "|"))
      what <- cap1(paste0(if (input$e_group != "Landed" && is_set(input$e_beds)) beds_nice(input$e_beds) else "",
                          type_nice(input$e_group, plural = TRUE)))
    }
    list(d = filter(d, n >= MIN_N), budget = budget, what = what,
         unit = if (input$e_market == "hdb") "towns" else "districts")
  })

  output$e_rank_title <- renderUI({
    er <- explore_rank()
    ch_title(sprintf("Areas ranked: %s", er$what), sprintf("%d %s with %d+ rentals, last 12 months", nrow(er$d), er$unit, MIN_N))
  })

  output$e_rank <- renderUI({
    er <- explore_rank()
    d  <- er$d
    if (!nrow(d)) return(p(class = "text-muted", "Not enough rentals for this home type in any area. Try another type or bedroom count."))
    d <- switch(input$e_rank_sort %||% "cheap",
                busy = arrange(d, desc(n)),
                odds = if (is.null(er$budget)) arrange(d, median) else arrange(d, desc(share), median),
                arrange(d, median))
    top <- max(d$median)
    rows <- lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags$tr(
        tags$td(class = "text-muted", i),
        tags$td(tags$a(href = "#", `data-id` = r$id, onclick = "return checkHome(this);", r$label),
                div(class = "small text-muted", sprintf("Typical %s to %s", money(r$p25), money(r$p75)))),
        tags$td(class = "rank-med",
                div(class = "rank-bar", style = sprintf("width:%.0f%%", 100 * r$median / top)),
                span(money(r$median))),
        tags$td(format(r$n, big.mark = ",")),
        if (!is.null(er$budget)) tags$td(
          span(class = "badge", style = sprintf("background:%s;%s", budget_fill(r$share), if (r$share >= 0.5) "" else "color:#212529;"),
               sprintf("%.0f%%", 100 * r$share)))
      )
    })
    tagList(
      div(class = "rank-scroll",
          tags$table(class = "table table-sm table-hover align-middle mb-0",
                     tags$thead(tags$tr(tags$th("#"), tags$th(if (er$unit == "towns") "Town" else "District"),
                                        tags$th("Median"), tags$th("Rentals"),
                                        if (!is.null(er$budget)) tags$th(sprintf("≤ %s", money(er$budget))))),
                     tags$tbody(rows))),
      p(class = "text-muted small mt-2 mb-0",
        "Click an area to open it in Rent check. Areas with fewer than ", MIN_N, " rentals are left out.",
        if (!is.null(er$budget)) " The last column is the share of rentals at or under your budget.")
    )
  })

  # "How to read this map": one line per feature, matching what is actually drawn right now
  map_key <- function(market, areas, budget, enough, page) {
    hdb   <- identical(market, "hdb")
    thing <- if (hdb) "block" else "project"
    swatch <- function(col) span(class = "key-swatch", style = sprintf("background:%s", col))
    grad   <- function(cols) span(class = "key-grad", style = sprintf("background:linear-gradient(90deg,%s)", paste(cols, collapse = ",")))
    rows <- if (areas) list(
      list("Colour", tagList(grad(c("#ffffb2", "#fd8d3c", "#bd0026")),
                             "each neighbourhood (URA planning area) is shaded by its median monthly rent: lighter is cheaper, darker red is pricier. Grey means too few rentals.")),
      list("Hover", "a neighbourhood to see its median, typical range and number of contracts."),
      list("Click", if (page == "main") "a neighbourhood to show its rent trend further down the page." else "the dots view (untick Shade by neighbourhood) to pick a single home."),
      list("Note", "planning areas do not match HDB towns exactly.")
    ) else list(
      if (is.null(budget)) {
        list("Colour", tagList(grad(c("#440154", "#21908c", "#fde725")),
                               sprintf("median monthly rent of the %s over the last 12 months: purple is cheaper, yellow is pricier.", thing)))
      } else {
        list("Colour", tagList(sprintf("how well each %s fits your budget of %s: ", thing, money(budget)),
                               swatch(BUDGET_COL[["likely"]]), "Likely, half or more of its rentals were at or under it; ",
                               swatch(BUDGET_COL[["possible"]]), "Possible, a quarter to half; ",
                               swatch(BUDGET_COL[["above"]]), "Mostly above your budget."))
      },
      list("Size", if (hdb) "every block dot is the same size." else
             "bigger dots had more rental contracts in the last 12 months, so their median is more reliable."),
      list("Click", if (page == "main")
             sprintf("a dot to see its rents, then Select this %s in the popup to load it into the results above. Use the star in the popup to add it to favourites.", thing)
           else sprintf("a dot to see its rents, then Check this home to open it in Rent check, or the star to add it to favourites.")),
      if (page == "main") list("Pins", "the red house is the selected home, the circle around it is 1 km, and the coloured pins are the nearest MRT, hawker centres, schools and malls."),
      list("No dot", sprintf("no matching rentals in the last 12 months%s, or the address could not be located. It does not mean the %s does not exist.",
                             if (isTRUE(enough)) ", or fewer than 3 (untick Only blocks with 3+ rentals to show them)" else "", thing)),
      if (!hdb) list("Note", "unit sizes differ between projects, which affects their medians.")
    )
    rows <- Filter(Negate(is.null), rows)
    div(class = "map-key",
        div(class = "map-key-title", "How to read this map"),
        lapply(rows, function(r) div(class = "map-key-row", span(class = "map-key-label", r[[1]]), span(r[[2]]))))
  }

  output$map_note <- renderUI({
    budget <- if (identical(input$s_role, "tenant") && isTRUE(input$s_budget_map) && asking_ok(input$t_asking)) input$t_asking else NULL
    map_key(input$s_market, isTRUE(input$s_areas), budget, input$s_enough, "main")
  })
  output$e_map_note <- renderUI({
    budget <- if (isTRUE(input$e_budget_map) && asking_ok(input$e_budget)) input$e_budget else NULL
    map_key(input$e_market, isTRUE(input$e_areas), budget, input$e_enough, "explore")
  })

  # ---- Map: the selected home, places near it, and zoom ----
  e_near <- reactive({
    pt <- point_for(sel())
    if (is.null(pt) || !HAS_AMEN || !length(input$s_amen)) return(list(pt = pt, near = NULL))
    walk <- isTRUE(input$s_walk)
    near <- nearest_amenities(amen, pt$x, pt$y, kinds = input$s_amen, n = if (walk) 5 else 3, as_of = AMEN_AS_OF)
    if (walk) {
      # Route the 5 nearest by straight line, then keep the 3 nearest on foot
      near <- withProgress(message = "Looking up walking routes", value = 0.5,
                           add_walking(near, pt, Sys.getenv("ONEMAP_TOKEN")))
      near <- near |>
        group_by(kind) |>
        arrange(coalesce(walk_m, dist_m * WALK_FACTOR), .by_group = TRUE) |>
        slice_head(n = 3) |>
        ungroup()
    }
    list(pt = pt, near = near)
  })

  # Re-zoom only when the place changes (not when, say, the flat type changes)
  observeEvent(list(map_ready(), input$map_opened, input$nav, input$s_market, input$s_town, input$s_block,
                    input$s_district, input$s_group, input$s_project, input$s_amen, input$s_walk), {
    if (!map_ready() || !identical(input$nav, "main")) return()
    proxy <- leafletProxy("map") |> clearGroup("amen") |> clearGroup("amen_home") |> removeControl("amen_legend")
    en <- e_near()
    pt <- en$pt
    sl <- sel()

    if (is.null(pt)) {
      # No single block / project picked: zoom to the whole town / district
      pts <- if (identical(sl$market, "hdb")) {
        block_info[block_info$town == sl$town & is.finite(block_info$lon) & is.finite(block_info$lat), c("lon", "lat")]
      } else {
        locations[as.character(locations$district) == as.character(sl$district) &
                    is.finite(locations$lon) & is.finite(locations$lat), c("lon", "lat")]
      }
      if (nrow(pts)) {
        proxy |> fitBounds(min(pts$lon), min(pts$lat), max(pts$lon), max(pts$lat))
      }
      return()
    }

    near  <- en$near
    shown <- character(0)
    if (!is.null(near) && nrow(near)) {
      shown <- intersect(names(AMEN_STYLE), unique(near$kind))
      for (k in shown) {
        d   <- near[near$kind == k, ]
        st  <- AMEN_STYLE[[k]]
        lab <- paste0(AMEN_SHORT[[k]], ": ", d$name, " (", d$dist_text, ")")   # e.g. "MRT: Tampines (540 m ...)"
        proxy <- proxy |> addAwesomeMarkers(
          lng = d$lon, lat = d$lat, group = "amen",
          icon = amen_marker_icon(st),
          label = lab, popup = htmltools::htmlEscape(lab))
      }
    }
    xs <- c(pt$lon, if (!is.null(near)) near$lon)
    ys <- c(pt$lat, if (!is.null(near)) near$lat)
    proxy |>
      addAwesomeMarkers(lng = pt$lon, lat = pt$lat, group = "amen_home",
                        icon = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "red"),
                        label = pt$label, labelOptions = labelOptions(noHide = TRUE)) |>
      addCircles(lng = pt$lon, lat = pt$lat, radius = 1000, group = "amen_home",
                 color = "#2166ac", weight = 1, fillOpacity = 0.03,
                 options = pathOptions(interactive = FALSE)) |>
      # className avoids leaflet's "legend" CSS, which squashes <i> icons into 18px colour swatches
      addControl(html = amen_legend_html(shown), position = "topright", layerId = "amen_legend",
                 className = "amen-legend-ctl") |>
      fitBounds(min(xs) - 0.004, min(ys) - 0.004, max(xs) + 0.004, max(ys) + 0.004)
  })

  output$nearby <- renderUI({
    en <- e_near()
    if (is.null(en$pt)) {
      what <- if (identical(input$s_market, "hdb")) "a block" else "a project"
      return(card(card_body(class = "text-muted",
                            sprintf("Pick %s (search above, use the Block / Project box, or click a dot on the map) to see the MRT stations, hawker centres, schools and malls nearby.", what))))
    }
    nearby_card(en$pt, en$near)
  })

  # ---- Trend chart ----
  trend_data <- reactive({
    if (input$s_market == "hdb") {
      req(input$s_flat)
      d <- hdb |> filter(flat_type == input$s_flat)
      if (identical(input$s_trend_level, "pa")) {
        req(input$s_pa)
        d <- d |> filter(has_location, planning_area == input$s_pa)
        where <- paste(input$s_pa, "(planning area)")
      } else {
        req(input$s_town)
        d <- d |> filter(town == input$s_town)
        where <- town_name(input$s_town)
      }
      list(d = d, vc = "monthly_rent", label = paste0("HDB ", input$s_flat, " flats, ", where))
    } else {
      req(input$s_group)
      d <- private |> filter(property_group == input$s_group)
      if (input$s_group != "Landed" && is_set(input$s_beds)) d <- filter(d, bedrooms_grp == input$s_beds)
      if (identical(input$s_trend_level, "pa")) {
        req(input$s_pa)
        d <- d |> filter(planning_area == input$s_pa)
        where <- paste(input$s_pa, "(planning area)")
      } else {
        req(input$s_district)
        d <- d |> filter(district == input$s_district)
        where <- district_label(input$s_district)
      }
      beds <- if (input$s_group != "Landed" && is_set(input$s_beds)) paste0(", ", input$s_beds, "-bedroom") else ""
      list(d = d, vc = "rent", label = paste0(input$s_group, beds, ", ", where))
    }
  })

  # Hover shows each line's name and value for that month
  output$trend <- plotly::renderPlotly({
    td <- trend_data()
    ts <- trend_series(td$d, td$vc)
    validate(need(nrow(ts) > 0 && any(!is.na(ts$median)), "Not enough contracts for this selection."))
    ts <- ts |> mutate(range_txt = if_else(is.na(p25), NA_character_, paste(money(p25), "to", money(p75))))
    plotly::plot_ly(ts, x = ~month) |>
      plotly::add_ribbons(ymin = ~p25, ymax = ~p75, name = "Middle 50% of rents",
                          fillcolor = "rgba(158,202,225,0.5)", line = list(width = 0),
                          customdata = ~range_txt,
                          hovertemplate = "<b>Middle 50% of rents</b>: %{customdata}<extra></extra>") |>
      plotly::add_lines(y = ~median, name = "Monthly median", line = list(color = "#08519c", width = 1.5),
                        hovertemplate = "<b>Monthly median</b>: %{y:$,.0f}<extra></extra>") |>
      plotly::add_lines(y = ~roll3, name = "3-month rolling median", line = list(color = "#d94801", width = 3),
                        hovertemplate = "<b>3-month rolling median</b>: %{y:$,.0f}<extra></extra>") |>
      plotly::layout(hovermode = "x unified",
                     title = list(text = td$label, x = 0.01, font = list(size = 13)),
                     xaxis = list(title = "", hoverformat = "%b %Y"),
                     yaxis = list(title = "Monthly rent", tickprefix = "$", tickformat = ",.0f"),
                     legend = list(orientation = "h", x = 0, y = -0.12),
                     margin = list(t = 40)) |>
      plotly::config(displayModeBar = FALSE)
  })

  # ---- Data & limits ----
  output$backtest_table <- renderTable({
    if (is.null(backtest_results)) {
      return(tibble(Note = "Run 03_backtest.R to fill in this table."))
    }
    backtest_results |>
      transmute(Market = market, `Comparable level` = level, `Held-out contracts` = n_test,
                `Benchmarked` = pct(benchmarked),
                `Inside typical range` = pct(inside_p25_p75),
                `Median error ($)` = money(median_abs_error),
                `Median error (%)` = pct(median_abs_pct_error, 1))
  })

  output$backtest_worst <- renderTable({
    if (is.null(backtest_breakdown)) {
      return(tibble(Note = "Run 03_backtest.R to fill in this table."))
    }
    backtest_breakdown |>
      head(10) |>
      transmute(Market = market, `Grouped by` = by,
                Group = if_else(by == "District", district_label(group), str_to_title(group)),
                `Held-out contracts` = n_test,
                `Inside typical range` = pct(inside_p25_p75),
                `Median error ($)` = money(median_abs_error),
                `Median error (%)` = pct(median_abs_pct_error, 1))
  })
}

shinyApp(ui, server, enableBookmarking = "url")
