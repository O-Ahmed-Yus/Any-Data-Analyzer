# GIS demo layers ------------------------------------------------------------
# The four layers offered by "Load selected layers" in the Spatial Data
# Manager. They are built on first use, not at app start, and cached in
# `gis_demo_cache` (gis_demo.R) so one copy is shared by every session in the
# R process. Everything is deterministic: no RNG is drawn, so the demo looks
# the same on every visit and a session cannot disturb another's RNG state.

# Three facilities per city, on a small ring around it. Illustrative only.
gis_demo_facilities <- function() {
  d <- gis_demo_cities()
  per_city <- 3L
  city_i <- rep(seq_len(nrow(d)), each = per_city)
  ring_i <- rep(seq_len(per_city), times = nrow(d))
  angle <- (ring_i - 1L) * 2 * pi / per_city
  fac <- data.frame(
    facility = sprintf("Facility %02d", seq_along(city_i)),
    city = d$city[city_i],
    type = c("Hospital", "Health centre", "Clinic")[ring_i],
    beds = c(60L, 16L, 4L)[ring_i],
    longitude = d$longitude[city_i] + 0.12 * cos(angle),
    latitude = d$latitude[city_i] + 0.12 * sin(angle),
    stringsAsFactors = FALSE
  )
  pts <- sf::st_as_sf(fac, coords = c("longitude", "latitude"), crs = 4326,
                      remove = FALSE)
  gis_prepare(pts)
}

# Eight monthly reports per city (96 rows), offset around each city.
gis_demo_reports <- function() {
  d <- gis_demo_cities()
  grid <- expand.grid(city = d$city, month = 1:8, stringsAsFactors = FALSE)
  city_i <- match(grid$city, d$city)
  month_i <- grid$month
  angle <- (month_i - 1L) * 2 * pi / 8
  rep_df <- data.frame(
    report_id = sprintf("R%03d", seq_len(nrow(grid))),
    city = grid$city,
    date = as.Date("2026-01-01") + (month_i - 1L) * 30,
    cases = as.integer(4L + (city_i * 7L + month_i * 5L) %% 25L),
    longitude = d$longitude[city_i] + 0.20 * cos(angle),
    latitude = d$latitude[city_i] + 0.20 * sin(angle),
    stringsAsFactors = FALSE
  )
  rep_df <- rep_df[order(rep_df$date, rep_df$report_id), ]
  pts <- sf::st_as_sf(rep_df, coords = c("longitude", "latitude"), crs = 4326,
                      remove = FALSE)
  gis_prepare(pts)
}

# All four layers, keyed by the ids gis_demo_choices() hands to the UI.
gis_build_demo <- function() {
  if (!gis_ready()) return(NULL)
  list(
    cities = gis_demo_points(),
    zones = gis_demo_polygons(),
    facilities = gis_demo_facilities(),
    reports = gis_demo_reports()
  )
}

# Built once per R process, on the first request from any session.
gis_demo <- function() {
  if (is.null(gis_demo_cache$layers)) gis_demo_cache$layers <- gis_build_demo()
  gis_demo_cache$layers
}

gis_demo_choices <- function() {
  c(
    "Cities (points)" = "cities",
    "Synthetic zones (polygons)" = "zones",
    "Health facilities (points, synthetic)" = "facilities",
    "Monthly case reports (points, synthetic)" = "reports"
  )
}
