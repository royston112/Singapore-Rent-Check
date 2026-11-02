# 02_benchmark.R
# The core of the app. The Tenant page and the Landlord page BOTH call the same functions,
# so they can never disagree. Every function takes a data frame as its first argument.
#
# Load in the app (or console) with:
#   source("02_benchmark.R")
#   hdb     <- readRDS("hdb_rentals_plus.rds")
#   private <- readRDS("private_rentals_plus.rds")
#
# HOW IT WORKS
#   1. find_comparables() tries the most specific comparable group first (same block / project),
#      then widens step by step (same street, same town / district, optional filters dropped).
#   2. At each level: keep the last 12 months. If fewer than MIN_N contracts, look back 24 months.
#      The first level that reaches MIN_N is used. If none does, we say "not enough data".
#   3. Return p25 / median / p75, the raw values (for a histogram) and the rows behind them.
#
# WHAT THE NUMBERS ARE: rents that were actually signed (private: stamp duty filings; HDB:
# owner-declared and not verified by HDB). They are NOT asking prices. Unit floor, view,
# furnishing and lease length are not in the data, which is why the app shows a range.

library(tidyverse)

MIN_N     <- 10                  # minimum contracts before we show a benchmark (team decision: 10? 30?)
WINDOWS   <- c(12, 24)           # months to look back; the first is preferred, the second is the fallback
VALUE_COL <- c(hdb = "monthly_rent", private = "rent")
NEAR_RADIUS_M <- 1000            # "nearby" comparables: within 1 km (straight line) of the chosen block / project


# ---- small helpers ---------------------------------------------------------------------------

# Is an input actually set? ("any", "", NA and NULL all mean "not set")
is_set <- function(x) {
  !is.null(x) && length(x) > 0 && !all(is.na(x)) && !all(as.character(x) %in% c("", "any"))
}

# Keep rows where `col` is one of `val`. Does nothing if `val` is not set.
.keep <- function(df, col, val) {
  if (!is_set(val)) return(df)
  v <- as.character(df[[col]])
  df[!is.na(v) & v %in% as.character(val), ]
}

# Plain-English labels for the optional filters
age_label <- function(band) paste0("blocks ", sub("^Under", "under", band), " old")
mrt_label <- function(band) paste0(sub("^Under", "under", band), " from MRT")

round50 <- function(x) round(x / 50) * 50


# ---- core engine (used by everything) --------------------------------------------------------

.make_result <- function(vals, window_months, widened, status, as_of, rows) {
  ok <- identical(status, "ok")
  list(
    summary = tibble(
      status        = status,
      n             = length(vals),
      p25           = if (ok) unname(quantile(vals, 0.25)) else NA_real_,
      median        = if (ok) median(vals) else NA_real_,
      p75           = if (ok) unname(quantile(vals, 0.75)) else NA_real_,
      window_months = window_months,
      widened       = widened,
      as_of         = as_of
    ),
    values = vals,
    rows   = rows        # the contracts behind the numbers (used for context, psf, recency)
  )
}

.benchmark_core <- function(df, value_col, windows = WINDOWS, min_n = MIN_N, as_of) {
  df   <- df[!is.na(df[[value_col]]), ]
  sub  <- df[0, ]
  vals <- numeric(0)
  w    <- windows[length(windows)]
  for (w in windows) {
    cutoff <- as_of %m-% months(w)
    sub    <- df[df$month > cutoff & df$month <= as_of, ]
    vals   <- sub[[value_col]]
    if (length(vals) >= min_n) {
      return(.make_result(vals, w, widened = (w != windows[1]), status = "ok", as_of = as_of, rows = sub))
    }
  }
  .make_result(vals, w, widened = TRUE, status = "too_few", as_of = as_of, rows = sub)
}


# ---- A1: comparable levels -------------------------------------------------------------------
# sel (HDB)     : list(town, flat, block, street, age, mrt)          block/street/age/mrt optional
# sel (private) : list(district, group, beds, band, project, mrt)    beds/band/project/mrt optional
#                 sel$project holds a location_id (from 01d_locations.R), not a project name
# sel$near      : c(x, y) of the chosen block / project (set inside find_comparables)
# Each level is list(label, filters = list(column = value), relaxed = what was dropped,
#                    near = c(x, y) or NULL, all_areas = TRUE if it may cross town / district lines).

.hdb_levels <- function(sel) {
  base <- list(town = sel$town, flat_type = sel$flat)
  out  <- list()
  dropped <- character(0)
  if (is_set(sel$block) && is_set(sel$street)) {
    out <- c(out, list(list(label = "Same block",
                            filters = c(base, list(block = sel$block, street_name = sel$street)),
                            relaxed = dropped)))
    dropped <- c(dropped, "same block")
  }
  if (is_set(sel$street)) {
    out <- c(out, list(list(label = "Same street",
                            filters = c(base, list(street_name = sel$street)),
                            relaxed = dropped)))
    dropped <- c(dropped, "same street")
  }
  # Same flat type within 1 km of the block, even across a town boundary
  if (!is.null(sel$near)) {
    out <- c(out, list(list(label = "Within 1 km of this block",
                            filters = list(flat_type = sel$flat),
                            near = sel$near, all_areas = TRUE, relaxed = dropped)))
    dropped <- c(dropped, "within 1 km")
  }
  # Town level with the optional filters, then drop them one at a time (MRT first, then age)
  opt <- list(age_band = if (is_set(sel$age)) sel$age, mrt_band = if (is_set(sel$mrt)) sel$mrt)
  sets <- list(list(f = opt, rel = character(0)))
  if (is_set(sel$mrt)) sets <- c(sets, list(list(f = list(age_band = opt$age_band, mrt_band = NULL),
                                                 rel = "MRT distance")))
  if (is_set(sel$age)) sets <- c(sets, list(list(f = list(age_band = NULL, mrt_band = NULL),
                                                 rel = c(if (is_set(sel$mrt)) "MRT distance", "block age"))))
  for (s in sets) {
    desc <- c(if (is_set(s$f$age_band)) age_label(s$f$age_band),
              if (is_set(s$f$mrt_band)) mrt_label(s$f$mrt_band))
    label <- paste(c("Same town", desc), collapse = ", ")
    if (length(s$rel)) label <- paste0(label, " (", paste(s$rel, collapse = " and "), " filter relaxed)")
    out <- c(out, list(list(label = label, filters = c(base, compact(s$f)), relaxed = c(dropped, s$rel))))
  }
  out
}

.private_levels <- function(sel) {
  base <- list(district = sel$district, property_group = sel$group)
  bb   <- list(bedrooms_grp = if (is_set(sel$beds)) sel$beds, area_band = if (is_set(sel$band)) sel$band)
  out  <- list()
  dropped <- character(0)
  if (is_set(sel$project)) {
    out <- c(out, list(list(label = "Same project",
                            filters = c(base, compact(bb), list(location_id = sel$project)),
                            relaxed = dropped)))
    dropped <- c(dropped, "same project")
  }
  # Same property type (+ beds / size) within 1 km of the project, even across a district boundary
  if (!is.null(sel$near)) {
    out <- c(out, list(list(label = "Within 1 km of this project",
                            filters = c(list(property_group = sel$group), compact(bb)),
                            near = sel$near, all_areas = TRUE, relaxed = dropped)))
    dropped <- c(dropped, "within 1 km")
  }
  if (is_set(sel$mrt)) {
    out <- c(out, list(list(label = paste0("Same district, ", mrt_label(sel$mrt)),
                            filters = c(base, compact(bb), list(mrt_band = sel$mrt)),
                            relaxed = dropped)))
    dropped <- c(dropped, "MRT distance")
  }
  out <- c(out, list(list(label = "Same district", filters = c(base, compact(bb)), relaxed = dropped)))
  if (is_set(sel$band)) {
    dropped <- c(dropped, "floor area")
    out <- c(out, list(list(label = "Same district, any floor area",
                            filters = c(base, compact(list(bedrooms_grp = bb$bedrooms_grp))),
                            relaxed = dropped)))
  }
  # No "any bedrooms" step: the back-test showed mixing 1- and 5-bedroom homes gives a useless
  # range (3% of held-out contracts inside it). We say "not enough data" instead.
  out
}

# Returns the benchmark (summary, values, rows) plus:
#   seg      all contracts (any date) at the chosen level  -> trends, demand, seasonality, momentum
#   level    e.g. "Same block", "Same town, blocks 20-29 years old"
#   relaxed  filters that had to be dropped to reach MIN_N
#   note     a friendly message when a block / project does not match the town / district
#
# df     : all contracts (used for the 1 km level, which can cross town / district lines)
# local  : contracts in the chosen town / district. Pass a pre-split subset for speed; if NULL it
#          is filtered from df here.
find_comparables <- function(df, market = c("hdb", "private"), sel,
                             windows = WINDOWS, min_n = MIN_N, as_of = max(df$month),
                             local = NULL, radius_m = NEAR_RADIUS_M) {
  market    <- match.arg(market)
  value_col <- VALUE_COL[[market]]
  note      <- NULL
  if (is.null(local)) {
    local <- if (market == "hdb") .keep(df, "town", sel$town) else .keep(df, "district", sel$district)
  }

  # A10: block / project that does not belong to the chosen town / district
  if (market == "hdb" && is_set(sel$street)) {
    if (!any(local$street_name == sel$street, na.rm = TRUE)) {
      note <- "That block is not in the selected town, so town-level comparables are shown."
      sel$block <- NULL; sel$street <- NULL
    } else if (is_set(sel$block) &&
               !any(local$street_name == sel$street & local$block == sel$block, na.rm = TRUE)) {
      note <- "That block has no rental records in the selected town, so wider comparables are shown."
      sel$block <- NULL
    }
  }
  if (market == "private" && is_set(sel$project) && !any(local$location_id == sel$project, na.rm = TRUE)) {
    note <- "That project is not in the selected district, so district-level comparables are shown."
    sel$project <- NULL
  }

  # Location of the chosen block / project, for the 1 km level
  sel$near <- NULL
  pts <- NULL
  if (market == "hdb" && is_set(sel$block)) {
    pts <- local[local$block == sel$block & local$street_name == sel$street & !is.na(local$x), ]
  }
  if (market == "private" && is_set(sel$project)) {
    pts <- local[local$location_id == sel$project & !is.na(local$x), ]
  }
  if (!is.null(pts) && nrow(pts) > 0) sel$near <- c(median(pts$x), median(pts$y))

  levels <- if (market == "hdb") .hdb_levels(sel) else .private_levels(sel)
  res <- NULL
  for (lv in levels) {
    seg <- if (isTRUE(lv$all_areas)) df else local
    for (col in names(lv$filters)) seg <- .keep(seg, col, lv$filters[[col]])
    if (!is.null(lv$near)) {
      seg <- seg[!is.na(seg$x) & (seg$x - lv$near[1])^2 + (seg$y - lv$near[2])^2 <= radius_m^2, ]
    }
    b   <- .benchmark_core(seg, value_col, windows, min_n, as_of)
    res <- c(b, list(seg = seg, level = lv$label, relaxed = lv$relaxed))
    if (b$summary$status == "ok") break
  }
  res$note      <- note
  res$market    <- market
  res$value_col <- value_col
  res
}


# ---- simple wrappers kept from the first version ---------------------------------------------

benchmark_private <- function(private, districts = NULL, groups = NULL, beds = NULL,
                              bands = NULL, metric = c("rent", "psf"),
                              windows = WINDOWS, min_n = MIN_N) {
  metric <- match.arg(metric)
  df <- private
  if (!is.null(districts)) df <- df[df$district %in% districts, ]
  if (!is.null(groups))    df <- df[df$property_group %in% groups, ]
  if (!is.null(beds))      df <- df[!is.na(df$bedrooms_grp) & df$bedrooms_grp %in% beds, ]
  if (!is.null(bands))     df <- df[df$area_band %in% bands, ]
  .benchmark_core(df, value_col = metric, windows = windows, min_n = min_n, as_of = max(private$month))
}

benchmark_hdb <- function(hdb, towns = NULL, flat_types = NULL, windows = WINDOWS, min_n = MIN_N) {
  df <- hdb
  if (!is.null(towns))      df <- df[df$town %in% towns, ]
  if (!is.null(flat_types)) df <- df[as.character(df$flat_type) %in% flat_types, ]
  .benchmark_core(df, value_col = "monthly_rent", windows = windows, min_n = min_n, as_of = max(hdb$month))
}


# ---- A3: recency -----------------------------------------------------------------------------
# Share of comparables older than 12 months. If the window widened to 24 months, also a
# trend-adjusted median (an ESTIMATE): older rents are scaled by the segment's own change in
# median between the two 12-month halves; for thin private segments, by the URA rental index.

recency_info <- function(bench, index = NULL, min_half = 5) {
  s <- bench$summary
  r <- bench$rows
  if (s$status != "ok" || is.null(r) || nrow(r) == 0) return(NULL)
  cutoff <- s$as_of %m-% months(12)
  old    <- r$month <= cutoff
  out    <- list(old_share = mean(old), adj_median = NA_real_, method = NA_character_)
  if (!isTRUE(s$widened) || !any(old)) return(out)

  v <- r[[bench$value_col]]
  ratio <- NA_real_
  if (sum(!old) >= min_half && sum(old) >= min_half) {
    ratio      <- median(v[!old]) / median(v[old])
    out$method <- "this segment's own change in median between the two 12-month halves"
  } else if (bench$market == "private" && !is.null(index)) {
    ptype <- if (all(r$property_group == "Landed")) "Landed" else "Non-Landed"
    idx   <- index[index$property_type == ptype & index$locality == "Whole Island", ]
    new_i <- mean(idx$index[idx$quarter_start > cutoff], na.rm = TRUE)
    old_i <- mean(idx$index[idx$quarter_start > (s$as_of %m-% months(24)) & idx$quarter_start <= cutoff], na.rm = TRUE)
    if (is.finite(new_i) && is.finite(old_i) && old_i > 0) {
      ratio      <- new_i / old_i
      out$method <- paste0("the URA rental index (", ptype, ", whole island)")
    }
  }
  if (is.finite(ratio)) out$adj_median <- median(ifelse(old, v * ratio, v))
  out
}


# ---- A4: tenant and landlord modes -----------------------------------------------------------

# TENANT MODE: where does the asking rent sit among comparable signed contracts?
tenant_check <- function(bench, asking) {
  s <- bench$summary
  empty <- list(percentile = NA_real_, below = NA_real_, equal = NA_real_,
                gap_month = NA_real_, gap_year = NA_real_, counter = NA_real_)
  if (s$status != "ok") {
    return(c(empty, list(label = "Not enough comparable contracts to judge this rent")))
  }
  if (is.null(asking) || length(asking) == 0 || is.na(asking) || asking <= 0) {
    return(c(empty, list(label = "Enter an asking rent above $0")))
  }
  # Mid-rank percentile: rents cluster on round $100 amounts, so many contracts often equal the
  # asking rent exactly. Counting all of them as "at or below" overstates the position; counting
  # none understates it. We count half of the ties.
  below <- mean(bench$values <  asking)
  equal <- mean(bench$values == asking)
  pct   <- below + equal / 2
  label <- case_when(
    pct <  0.25 ~ "Lower than most comparable contracts",
    pct <= 0.75 ~ "Within the typical range",
    pct <= 0.90 ~ "Higher than most comparable contracts",
    TRUE        ~ "Well above comparable contracts"
  )
  gap <- asking - s$median
  list(percentile = pct, below = below, equal = equal, label = label,
       gap_month = gap, gap_year = 12 * gap,                 # 12-month lease
       counter   = min(round50(s$median), asking))           # never above the asking rent
}

# LANDLORD MODE: a sensible range to list at (typical range, anchored on the median).
landlord_range <- function(bench) {
  s <- bench$summary
  if (s$status != "ok") return(tibble(status = s$status, low = NA_real_, mid = NA_real_, high = NA_real_))
  tibble(status = "ok", low = s$p25, mid = s$median, high = s$p75)
}

# Range for the "If I list at $X" slider: 10th to 90th percentile, rounded to $50
listing_slider_range <- function(bench) {
  q <- round50(unname(quantile(bench$values, c(0.10, 0.90))))
  if (q[1] == q[2]) q <- q + c(-100, 100)
  q
}


# ---- A5: rent per sqft and a size-adjusted estimate (private, non-landed) ---------------------

# Comparables' rent per sqft x the unit's size. More precise than a range over all sizes.
size_estimate <- function(bench, sqft, min_n = MIN_N) {
  if (bench$market != "private" || bench$summary$status != "ok") return(NULL)
  if (is.null(sqft) || length(sqft) == 0 || is.na(sqft) || sqft <= 0) return(NULL)
  p <- bench$rows$psf
  p <- p[!is.na(p)]
  if (length(p) < min_n) return(list(ok = FALSE, n = length(p)))
  q <- unname(quantile(p, c(0.25, 0.5, 0.75)))
  list(ok = TRUE, n = length(p), sqft = sqft,
       median_psf = q[2], p25_psf = q[1], p75_psf = q[3],
       estimate = q[2] * sqft, low = q[1] * sqft, high = q[3] * sqft)
}

psf_check <- function(bench, asking, sqft, min_n = MIN_N) {
  est <- size_estimate(bench, sqft, min_n)
  if (is.null(est) || is.null(asking) || is.na(asking) || asking <= 0) return(NULL)
  if (!isTRUE(est$ok)) return(est)
  p <- bench$rows$psf
  p <- p[!is.na(p)]
  a <- asking / sqft
  c(est, list(asking_psf = a, percentile = mean(p < a) + mean(p == a) / 2))
}


# ---- A6: context line from comparables with location data ------------------------------------
context_line <- function(bench) {
  r <- bench$rows
  if (is.null(r) || nrow(r) == 0) return(NULL)
  loc <- r[!is.na(r$mrt_band), ]
  if (nrow(loc) == 0) return(NULL)
  top <- function(x) {
    t <- sort(table(as.character(x[!is.na(x)])), decreasing = TRUE)
    list(name = names(t)[1], share = t[[1]] / sum(t))
  }
  parts <- character(0)
  if (bench$market == "hdb" && any(!is.na(loc$age_band))) {
    a <- top(loc$age_band)
    parts <- c(parts, sprintf("the most common block age is %s (%.0f%%)", sub("^Under", "under", a$name), 100 * a$share))
  }
  m <- top(loc$mrt_band)
  parts <- c(parts, sprintf("the most common distance to MRT is %s (%.0f%%)", sub("^Under", "under", m$name), 100 * m$share))
  sprintf("Of the %s comparables with location data, %s.",
          format(nrow(loc), big.mark = ","), paste(parts, collapse = " and "))
}


# ---- A7: cheaper alternatives nearby ---------------------------------------------------------

# Centre of each town / district (median x, y in SVY21 metres of located contracts)
area_centroids <- function(df, area_col) {
  df |>
    filter(!is.na(x), !is.na(y)) |>
    group_by(area = .data[[area_col]]) |>
    summarise(cx = median(x), cy = median(y), .groups = "drop")
}

nearby_alternatives <- function(df, market = c("hdb", "private"), sel, centroids,
                                radius_km = 5, k = 6, top = 5, min_n = MIN_N, as_of = max(df$month)) {
  market   <- match.arg(market)
  vc       <- VALUE_COL[[market]]
  area_col <- if (market == "hdb") "town" else "district"
  here     <- if (market == "hdb") sel$town else sel$district
  c0 <- centroids[centroids$area == here, ]
  if (nrow(c0) == 0) return(NULL)

  cand <- centroids |>
    filter(area != here) |>
    mutate(distance_km = sqrt((cx - c0$cx)^2 + (cy - c0$cy)^2) / 1000) |>
    arrange(distance_km)
  pick <- filter(cand, distance_km <= radius_km)
  if (nrow(pick) < k) pick <- head(cand, k)          # too few within 5 km: use the k nearest

  seg <- df[df$month > (as_of %m-% months(12)) & df$month <= as_of, ]
  if (market == "hdb") {
    seg <- .keep(seg, "flat_type", sel$flat)
  } else {
    seg <- .keep(seg, "property_group", sel$group)
    seg <- .keep(seg, "bedrooms_grp", sel$beds)
  }
  stats <- seg |>
    group_by(area = .data[[area_col]]) |>
    summarise(n = n(), median = median(.data[[vc]], na.rm = TRUE), .groups = "drop")
  base <- stats$median[stats$area == here & stats$n >= min_n]
  base <- if (length(base)) base else NA_real_

  out <- pick |>
    inner_join(stats, by = "area") |>
    filter(n >= min_n) |>
    mutate(diff = median - base, diff_pct = diff / base) |>
    arrange(median) |>
    head(top) |>
    select(area, distance_km, n, median, diff, diff_pct)
  attr(out, "base_median") <- base
  out
}


# ---- A8: location premium (association only, not cause) --------------------------------------
location_premium <- function(df, market = c("hdb", "private"), sel, min_n = MIN_N, as_of = max(df$month)) {
  market <- match.arg(market)
  vc  <- VALUE_COL[[market]]
  seg <- df[df$month > (as_of %m-% months(12)) & df$month <= as_of, ]
  if (market == "hdb") {
    seg <- .keep(.keep(seg, "town", sel$town), "flat_type", sel$flat)
  } else {
    seg <- .keep(.keep(.keep(seg, "district", sel$district), "property_group", sel$group), "bedrooms_grp", sel$beds)
  }
  seg <- seg[!is.na(seg[[vc]]), ]
  if (nrow(seg) < min_n) return(NULL)
  overall <- median(seg[[vc]])
  by_band <- function(col) {
    seg |>
      filter(!is.na(.data[[col]])) |>
      group_by(group = .data[[col]]) |>
      summarise(n = n(), median = median(.data[[vc]]), .groups = "drop") |>
      filter(n >= min_n) |>
      mutate(diff = median - overall)
  }
  list(overall = overall, n = nrow(seg),
       mrt = by_band("mrt_band"),
       age = if (market == "hdb") by_band("age_band") else NULL)
}


# ---- segment summaries over time (Tenant, Landlord and Explore pages) ------------------------

# % CHANGES use the average of the middle 80% of contracts (trim the top and bottom 10%), not the
# median. Rents cluster on round $100 amounts, so medians often land on the same figure in both
# periods and show a false 0.0% change. The trimmed average still ignores outliers. Rent LEVELS
# shown to users are still medians and percentiles.
typical_level <- function(v) mean(v, trim = 0.1, na.rm = TRUE)

period_change <- function(seg, value_col, as_of, months_back, min_n = MIN_N) {
  v <- seg[[value_col]]
  recent <- seg$month >  (as_of %m-% months(months_back))     & seg$month <= as_of
  prev   <- seg$month >  (as_of %m-% months(2 * months_back)) & seg$month <= (as_of %m-% months(months_back))
  if (sum(recent & !is.na(v)) < min_n || sum(prev & !is.na(v)) < min_n) return(NA_real_)
  typical_level(v[recent]) / typical_level(v[prev]) - 1
}

# Last 6 months vs the previous 6 months
price_direction <- function(seg, value_col, as_of, min_n = MIN_N) {
  period_change(seg, value_col, as_of, 6, min_n)
}

# Year-on-year: last 12 months vs the 12 months before
yoy_change <- function(seg, value_col, as_of, min_n = MIN_N) {
  period_change(seg, value_col, as_of, 12, min_n)
}

# Contracts per month over the last `n_months` months (0 for months with none)
monthly_volume <- function(seg, as_of, n_months = 24) {
  all_months <- tibble(month = seq(as_of %m-% months(n_months - 1), as_of, by = "month"))
  counts <- seg |> filter(month %in% all_months$month) |> count(month, name = "n")
  all_months |> left_join(counts, by = "month") |> mutate(n = replace_na(n, 0L))
}

# Seasonality, adjusted for the overall trend.
# Each month's typical rent is compared with the typical rent over the 12 months centred on it
# (6 before, 5 after), so a general rise (e.g. 2022-2024) is not mistaken for a seasonal effect.
# Only months with a full 12-month window and at least `min_month_n` contracts are used.
# Returns one row per calendar month: effect (% above / below trend), months used, and the
# average number of contracts signed in that calendar month.
seasonality <- function(seg, value_col, min_month_n = 3, min_months = 24) {
  seg <- seg[!is.na(seg[[value_col]]), ]
  if (nrow(seg) == 0) return(tibble())
  v       <- seg[[value_col]]
  all_m   <- sort(unique(seg$month))
  first_m <- min(all_m); last_m <- max(all_m)
  monthly <- tibble(month = all_m) |>
    mutate(
      n     = map_int(month, \(m) sum(seg$month == m)),
      level = map_dbl(month, \(m) typical_level(v[seg$month == m])),
      full  = (month %m-% months(6)) >= first_m & (month %m+% months(5)) <= last_m,
      trend = map_dbl(month, \(m) typical_level(v[seg$month >= (m %m-% months(6)) & seg$month <= (m %m+% months(5))])),
      cal_month = lubridate::month(month, label = TRUE)
    )
  usable <- monthly |> filter(full, n >= min_month_n)
  if (nrow(usable) < min_months) return(tibble())
  volume <- monthly |> group_by(cal_month) |> summarise(avg_contracts = mean(n), .groups = "drop")
  usable |>
    mutate(effect = level / trend - 1) |>
    group_by(cal_month) |>
    summarise(effect = mean(effect), months_used = n(), .groups = "drop") |>
    left_join(volume, by = "cal_month")
}

# Monthly p25 / median / p75 plus a 3-month rolling median (pooled contracts, not an average of medians)
trend_series <- function(seg, value_col, min_month_n = 3) {
  seg <- seg[!is.na(seg[[value_col]]), ]
  if (nrow(seg) == 0) return(tibble())
  monthly <- seg |>
    group_by(month) |>
    summarise(n = n(),
              p25    = unname(quantile(.data[[value_col]], 0.25)),
              median = median(.data[[value_col]]),
              p75    = unname(quantile(.data[[value_col]], 0.75)),
              .groups = "drop") |>
    arrange(month) |>
    mutate(p25    = if_else(n >= min_month_n, p25, NA_real_),     # hide very thin months
           median = if_else(n >= min_month_n, median, NA_real_),
           p75    = if_else(n >= min_month_n, p75, NA_real_))
  vals <- seg[[value_col]]
  monthly$roll3 <- map_dbl(monthly$month, \(m) {
    v <- vals[seg$month > (m %m-% months(3)) & seg$month <= m]
    if (length(v) >= min_month_n) median(v) else NA_real_
  })
  monthly
}


# ---- helper for the MIN_N decision -----------------------------------------------------------
# How many HDB town x flat-type segments have enough contracts in the last `months_back` months?
hdb_segment_counts <- function(hdb, months_back = 12) {
  as_of <- max(hdb$month)
  hdb |>
    filter(month > as_of %m-% months(months_back)) |>
    count(town, flat_type, name = "n") |>
    arrange(n)
}

private_segment_counts <- function(private, months_back = 12) {
  as_of <- max(private$month)
  private |>
    filter(month > as_of %m-% months(months_back)) |>
    count(district, property_group, bedrooms_grp, area_band, name = "n") |>
    arrange(n)
}


# ---- A9: back-test ---------------------------------------------------------------------------
# Hold out the last `holdout_months` months. Benchmark each held-out contract using ONLY earlier
# contracts (no optional filters), then check how often it lands inside p25-p75 and how far it
# is from the median. Run from 03_backtest.R, not inside the app (takes a few minutes).

backtest <- function(df, market = c("hdb", "private"), holdout_months = 3,
                     windows = WINDOWS, min_n = MIN_N) {
  market   <- match.arg(market)
  vc       <- VALUE_COL[[market]]
  train_as_of <- max(df$month) %m-% months(holdout_months)
  train <- df[df$month <= train_as_of, ]
  test  <- df[df$month > train_as_of & !is.na(df[[vc]]), ]

  if (market == "hdb") {
    test$key  <- paste(test$town, test$flat_type, test$block, test$street_name, sep = "|")
    area_col  <- "town"
    type_col  <- "flat_type"
  } else {
    test$key  <- paste(test$district, test$property_group, test$bedrooms_grp, test$area_band, test$location_id, sep = "|")
    area_col  <- "district"
    type_col  <- "property_group"
  }
  # Pre-split once: each lookup needs its own town / district (local) and, for the 1 km level,
  # all contracts of the same flat / property type (which can cross boundaries).
  train_by_area <- split(train, train[[area_col]])
  train_by_type <- split(train, as.character(train[[type_col]]))
  keys <- test[!duplicated(test$key), ]
  message(market, ": ", format(nrow(test), big.mark = ","), " held-out contracts, ",
          format(nrow(keys), big.mark = ","), " distinct segments")

  res <- vector("list", nrow(keys))
  pb  <- txtProgressBar(min = 0, max = nrow(keys), style = 3)
  for (i in seq_len(nrow(keys))) {
    k     <- keys[i, ]
    local <- train_by_area[[as.character(k[[area_col]])]]
    pool  <- train_by_type[[as.character(k[[type_col]])]]
    if (is.null(local)) local <- train[0, ]
    if (is.null(pool))  pool  <- train[0, ]
    sel <- if (market == "hdb") {
      list(town = k$town, flat = as.character(k$flat_type), block = k$block, street = k$street_name)
    } else {
      list(district = k$district, group = k$property_group, beds = k$bedrooms_grp,
           band = k$area_band, project = k$location_id)
    }
    b <- find_comparables(pool, market, sel, windows, min_n, as_of = train_as_of, local = local)
    res[[i]] <- tibble(key = k$key, level = b$level, status = b$summary$status,
                       p25 = b$summary$p25, median = b$summary$median, p75 = b$summary$p75)
    setTxtProgressBar(pb, i)
  }
  close(pb)

  test |>
    transmute(key, actual = .data[[vc]],
              area = as.character(.data[[area_col]]), type = as.character(.data[[type_col]])) |>
    left_join(bind_rows(res), by = "key") |>
    mutate(market = market,
           level  = str_remove(level, " \\(.*\\)$"))   # drop "(... relaxed)" for grouping
}

# Where is the tool least reliable? Accuracy by town / district and by flat / property type.
# Only groups with at least `min_test` held-out contracts; sorted by median % error (worst first).
backtest_breakdown <- function(bt, min_test = 30) {
  f <- function(d) {
    ok <- d[d$status == "ok", ]
    tibble(n_test = nrow(d), benchmarked = nrow(ok) / nrow(d),
           inside_p25_p75 = mean(ok$actual >= ok$p25 & ok$actual <= ok$p75),
           median_abs_error = median(abs(ok$actual - ok$median)),
           median_abs_pct_error = median(abs(ok$actual - ok$median) / ok$actual))
  }
  bind_rows(
    bt |> group_by(market, group = area) |> group_modify(\(d, k) f(d)) |> ungroup() |>
      mutate(by = if_else(market == "hdb", "Town", "District")),
    bt |> group_by(market, group = type) |> group_modify(\(d, k) f(d)) |> ungroup() |>
      mutate(by = if_else(market == "hdb", "Flat type", "Property type"))
  ) |>
    filter(n_test >= min_test) |>
    arrange(desc(median_abs_pct_error)) |>
    select(market, by, group, everything())
}

summarise_backtest <- function(bt) {
  f <- function(d) {
    ok <- d[d$status == "ok", ]
    tibble(n_test            = nrow(d),
           benchmarked       = nrow(ok) / nrow(d),
           inside_p25_p75    = mean(ok$actual >= ok$p25 & ok$actual <= ok$p75),
           median_abs_error  = median(abs(ok$actual - ok$median)),
           median_abs_pct_error = median(abs(ok$actual - ok$median) / ok$actual))
  }
  bind_rows(
    f(bt) |> mutate(market = "All", level = "All"),
    bt |> group_by(market) |> group_modify(\(d, k) f(d)) |> ungroup() |> mutate(level = "All"),
    bt |> filter(status == "ok") |> group_by(market, level) |> group_modify(\(d, k) f(d)) |> ungroup()
  ) |>
    select(market, level, everything())
}


# ---- TRY IT (paste into the console after sourcing the file and loading the .rds files) ------
# Wrapped in if (FALSE) so it does NOT run when the Shiny app sources this file.
if (FALSE) {
  b <- find_comparables(hdb, "hdb", list(town = "TAMPINES", flat = "4-ROOM"))
  b$summary; b$level
  tenant_check(b, asking = 3200)
  landlord_range(b)

  p <- find_comparables(private, "private",
                        list(district = "15", group = "Condo / apartment", beds = "2", band = "700-800"))
  p$summary; p$level

  hdb_segment_counts(hdb) |> summarise(under_10 = sum(n < 10), under_30 = sum(n < 30), total = n())
}
