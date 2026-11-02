# amenities.R
# Functions for "what is near this home": nearest MRT/LRT, hawker centres, schools, and any extra places.
# Functions only: safe to source before loading data. Used by 01c_amenities.R and the app.
#
#   nearest_amenities()  straight-line nearest places (fast, no internet)
#   walk_route()         one walking route from OneMap (needs a token; falls back to NA)
#   add_walking()        adds walking distance / time to the rows from nearest_amenities()
#   onemap_token()       gets a OneMap token (email + password asked in the console, never saved)
#
# WALKING DISTANCE: OneMap's routing service is called ONLY for the few places near the home the user
# picked, and each answer is cached for the session. It is never run for every property.
# Tokens expire (about 3 days), so refresh before a demo:
#   source("amenities.R"); Sys.setenv(ONEMAP_TOKEN = onemap_token()); shiny::runApp(...)
# If the token is missing or expired the app falls back to straight-line distance and says so.

library(dplyr)
library(stringr)
library(httr2)

# ---- labels and map icons, in display order ---------------------------------------------------

AMEN_LABEL <- c(mrt = "MRT / LRT stations", hawker = "Hawker centres", school = "Schools",
                mall = "Malls", foodcourt = "Food courts", petrol = "Petrol stations")
AMEN_STYLE <- list(
  mrt       = list(icon = "train",          colour = "purple"),
  hawker    = list(icon = "cutlery",        colour = "orange"),
  school    = list(icon = "graduation-cap", colour = "green"),
  mall      = list(icon = "shopping-cart",  colour = "blue"),
  foodcourt = list(icon = "cutlery",        colour = "pink"),
  petrol    = list(icon = "tint",           colour = "cadetblue")
)

# Rough walk time when no routed answer is available: path about 30% longer than the straight line,
# walked at about 80 m per minute. This is an estimate, and the page says so.
WALK_FACTOR     <- 1.3
WALK_M_PER_MIN  <- 80


# ---- OneMap token -------------------------------------------------------------------------------

# Returns a token string, or "" if no account details were given. Never stores the password.
# (Same sign-in call as 00_geocode_hdb_blocks.R.)
onemap_token <- function() {
  email <- Sys.getenv("ONEMAP_EMAIL")
  pw    <- Sys.getenv("ONEMAP_PASSWORD")
  if (email == "" && interactive()) email <- readline("OneMap email (blank = no token): ")
  if (email == "") return("")
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
    warning("Could not get a OneMap token (status ", resp_status(resp), ").")
    return("")
  }
  tok
}

# The token goes in the Authorization header exactly as 00_geocode_hdb_blocks.R sends it.
# If OneMap answers 401, try  paste("Bearer", token)  here instead.
onemap_auth <- function(req, token) {
  if (is.null(token) || !nzchar(token)) req else req_headers(req, Authorization = token)
}


# ---- distances ----------------------------------------------------------------------------------

fmt_dist <- function(m) ifelse(m >= 1000, sprintf("%.1f km", m / 1000), paste0(round(m), " m"))

# Text shown next to each place. Routed answer if we have one, otherwise straight line + estimate.
describe_distance <- function(
    dist_m,
    walk_m = NA_real_,
    walk_min = NA_real_
) {
  n <- length(dist_m)
  if (n == 0L) return(character(0))
  
  # Give every amenity its own walking-distance/time values.
  walk_m   <- rep_len(walk_m, n)
  walk_min <- rep_len(walk_min, n)
  
  est_min <- pmax(
    1,
    round(dist_m * WALK_FACTOR / WALK_M_PER_MIN)
  )
  
  # Start with one straight-line label per amenity.
  result <- sprintf(
    "%s in a straight line, roughly %d min walk",
    fmt_dist(dist_m),
    as.integer(est_min)
  )
  
  # Use routed distances only when both distance and time are valid.
  routed <- is.finite(walk_m) & is.finite(walk_min) &
    walk_m >= 0 & walk_min >= 0
  
  result[routed] <- sprintf(
    "%s on foot, about %d min",
    fmt_dist(walk_m[routed]),
    as.integer(round(walk_min[routed]))
  )
  
  result[!is.finite(dist_m) & !routed] <- "Distance unavailable"
  
  result
}

# amen : the table saved by 01c_amenities.R (kind, name, detail, group_id, lon, lat, x, y, opened)
# x, y : the home's SVY21 coordinates (metres), which both the HDB and URA data already have
# Returns the n nearest places of each kind, with straight-line distance in metres.
# MRT/LRT: only stations open by `as_of`, and only the nearest exit of each station.
nearest_amenities <- function(amen, x, y, kinds, n = 3, as_of = Sys.Date()) {
  d <- amen[amen$kind %in% kinds & is.finite(amen$x) & is.finite(amen$y) & ((amen$opened <= as_of) %in% TRUE), ]
  d$dist_m    <- sqrt((d$x - x)^2 + (d$y - y)^2)
  d           <- d[order(d$dist_m), ]
  d           <- d[!duplicated(paste(d$kind, d$group_id)), ]
  d           <- d |> group_by(kind) |> slice_head(n = n) |> ungroup()
  d$walk_m    <- rep(NA_real_, nrow(d))
  d$walk_min  <- rep(NA_real_, nrow(d))
  d$dist_text <- describe_distance(d$dist_m)
  d
}


# ---- walking routes (OneMap) ----------------------------------------------------------------------

.walk_cache <- new.env()
# Routes are also saved to disk, so a demo still works after the token has expired:
# any route looked up while a token worked is reused later with no internet or token.
WALK_CACHE_FILE <- "data_cache/walk_cache.rds"
if (file.exists(WALK_CACHE_FILE)) {
  saved <- tryCatch(readRDS(WALK_CACHE_FILE), error = function(e) NULL)
  if (is.list(saved)) for (k in names(saved)) assign(k, saved[[k]], envir = .walk_cache)
  rm(saved)
}

# One walking route. Returns list(dist_m, mins); both NA if anything goes wrong.
# CHECK ONCE BY HAND before relying on it: the response layout used here (route_summary with
# total_distance in metres and total_time in seconds) comes from OneMap's older routing docs.
# As a guard, an answer is thrown away if the implied speed is not a plausible walking speed (2-8 km/h),
# so a wrong unit can never put silly numbers on the page.
walk_route <- function(from_lon, from_lat, to_lon, to_lat, token, verbose = FALSE) {
  none <- list(dist_m = NA_real_, mins = NA_real_)
  key <- sprintf("%.5f,%.5f>%.5f,%.5f", from_lat, from_lon, to_lat, to_lon)
  hit <- .walk_cache[[key]]
  if (!is.null(hit)) return(hit)                       # saved route: no token needed
  if (is.null(token) || !nzchar(token)) return(none)

  resp <- tryCatch(
    request("https://www.onemap.gov.sg/api/public/routingsvc/route") |>
      req_url_query(start = sprintf("%.6f,%.6f", from_lat, from_lon),
                    end   = sprintf("%.6f,%.6f", to_lat, to_lon),
                    routeType = "walk") |>
      onemap_auth(token) |>
      req_timeout(20) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(),
    error = \(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) {
    if (verbose) message("Walking route failed: ", if (is.null(resp)) "no response (offline?)" else
      paste0("HTTP ", resp_status(resp), " ", substr(resp_body_string(resp), 1, 300)))
    return(none)
  }

  body <- tryCatch(resp_body_json(resp), error = \(e) NULL)
  rs   <- body$route_summary
  d <- suppressWarnings(as.numeric(rs$total_distance))
  s <- suppressWarnings(as.numeric(rs$total_time))
  if (length(d) != 1 || length(s) != 1 || !is.finite(d) || !is.finite(s) || d <= 0 || s <= 0) {
    if (verbose) message("Walking route answered, but not in the expected layout: ", substr(resp_body_string(resp), 1, 400))
    return(none)
  }
  kmh <- (d / 1000) / (s / 3600)
  if (kmh < 2 || kmh > 8) {
    if (verbose) message(sprintf("Walking route ignored: %.0f m in %.0f s is %.1f km/h, not a walking speed (units may differ).", d, s, kmh))
    return(none)
  }

  out <- list(dist_m = d, mins = s / 60)
  .walk_cache[[key]] <- out            # only successful answers are cached
  try({
    dir.create(dirname(WALK_CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
    saveRDS(as.list(.walk_cache), WALK_CACHE_FILE)
  }, silent = TRUE)
  out
}

# near : rows from nearest_amenities(); pt : list(lon, lat). Adds walk_m / walk_min and new text.
add_walking <- function(near, pt, token) {
  for (i in seq_len(nrow(near))) {
    r <- walk_route(pt$lon, pt$lat, near$lon[i], near$lat[i], token)
    near$walk_m[i]   <- r$dist_m
    near$walk_min[i] <- r$mins
  }
  near$dist_text <- describe_distance(near$dist_m, near$walk_m, near$walk_min)
  near
}


# ---- one-off check that walking routes work (run by hand, once, with a token) -----------------------
# Routes from the first hawker centre to its nearest MRT/LRT exit and prints both distances.
# A routed distance a bit LONGER than the straight line means it works.
#   source("amenities.R"); amen <- readRDS("amenities.rds")
#   walking_selftest(amen, onemap_token())
walking_selftest <- function(amen, token) {
  h <- amen[amen$kind == "hawker", ][1, ]
  n <- nearest_amenities(amen, h$x, h$y, kinds = "mrt", n = 1)
  r <- walk_route(h$lon, h$lat, n$lon[1], n$lat[1], token, verbose = TRUE)
  if (is.finite(r$dist_m)) {
    cat(sprintf("%s to %s: straight line %.0f m, walking route %.0f m, %.0f min. It works.\n",
                h$name, n$name[1], n$dist_m[1], r$dist_m, r$mins))
  } else {
    cat("No usable walking route came back. Read the message above. A missing or expired token is the usual cause.\n")
  }
  invisible(r)
}
