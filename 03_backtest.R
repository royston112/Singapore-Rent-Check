# 03_backtest.R
# (1) Step 0 data checks, and (2) the back-test for the fair price calculator.
# Run once (takes a few minutes):  source("03_backtest.R")
# Saves backtest_results.rds and backtest_breakdown.rds, which the app shows on Data & limits.

library(tidyverse)
source("02_benchmark.R")
hdb     <- readRDS("hdb_rentals_plus.rds")
private <- readRDS("private_rentals_loc.rds")   # from 01d_locations.R


# ============================================================
# STEP 0: DATA CHECKS
# ============================================================

message("---- STEP 0 ----")
message("HDB contracts:     ", format(min(hdb$month)), " to ", format(max(hdb$month)))
message("Private contracts: ", format(min(private$month)), " to ", format(max(private$month)))
if (min(private$month) < as.Date("2019-11-01")) {
  warning("Private data starts before Nov 2019; mrt_station_openings.csv only lists stations opened from Nov 2019.")
}

h <- hdb_segment_counts(hdb)
message(sprintf("HDB town x flat type (last 12 months): %d segments, %d with n < 10, %d with n < 30",
                nrow(h), sum(h$n < 10), sum(h$n < 30)))
p <- private_segment_counts(private)
message(sprintf("Private district x group x beds x band (last 12 months): %d segments, %d with n < 10, %d with n < 30",
                nrow(p), sum(p$n < 10), sum(p$n < 30)))

# How often will the optional filters still leave MIN_N contracts?
hdb12 <- hdb |> filter(month > max(month) %m-% months(12))
ok_segments <- hdb12 |> count(town, flat_type) |> filter(n >= MIN_N)
for (col in c("age_band", "mrt_band")) {
  cells <- hdb12 |>
    filter(!is.na(.data[[col]])) |>
    semi_join(ok_segments, by = c("town", "flat_type")) |>
    count(town, flat_type, .data[[col]])
  message(sprintf("HDB town x flat type (n >= %d) split by %s: %d cells, %.0f%% still have n >= %d",
                  MIN_N, col, nrow(cells), 100 * mean(cells$n >= MIN_N), MIN_N))
}


# ============================================================
# BACK-TEST (hold out the last 3 months)
# ============================================================

message("\n---- BACK-TEST ----")
bt <- bind_rows(backtest(hdb, "hdb"), backtest(private, "private"))
results <- summarise_backtest(bt)

print(results |>
        mutate(benchmarked          = sprintf("%.0f%%", 100 * benchmarked),
               inside_p25_p75       = sprintf("%.0f%%", 100 * inside_p25_p75),
               median_abs_error     = sprintf("$%.0f", median_abs_error),
               median_abs_pct_error = sprintf("%.1f%%", 100 * median_abs_pct_error)),
      n = Inf, width = Inf)

saveRDS(results, "backtest_results.rds")

# Where is the tool least reliable? (groups with 30+ held-out contracts, worst first)
breakdown <- backtest_breakdown(bt)
message("\nLeast reliable towns / districts / types (highest median % error):")
print(breakdown |>
        head(15) |>
        mutate(inside_p25_p75       = sprintf("%.0f%%", 100 * inside_p25_p75),
               median_abs_error     = sprintf("$%.0f", median_abs_error),
               median_abs_pct_error = sprintf("%.1f%%", 100 * median_abs_pct_error),
               benchmarked          = sprintf("%.0f%%", 100 * benchmarked)),
      n = Inf, width = Inf)
saveRDS(breakdown, "backtest_breakdown.rds")
message("\nSaved backtest_results.rds and backtest_breakdown.rds")
