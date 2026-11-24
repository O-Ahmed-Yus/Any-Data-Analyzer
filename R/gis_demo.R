# GIS example data ----------------------------------------------------------
# Two small layers so every tool in the tab can be tried without uploading a
# file: twelve city locations and a set of district areas around them.
# City coordinates are approximate real positions. Every attribute, and every
# district boundary, is illustrative and labelled as such in the interface.
# Nothing is fetched from the internet.

gis_demo_cache <- new.env(parent = emptyenv())

gis_demo_cities <- function() {
  data.frame(
    city = c("Mogadishu", "Hargeisa", "Bosaso", "Kismayo", "Baidoa", "Gaalkacyo",
             "Garoowe", "Beledweyne", "Marka", "Jowhar", "Berbera", "Burao"),
    region = c("Banadir", "Woqooyi Galbeed", "Bari", "Lower Juba", "Bay", "Mudug",
               "Nugaal", "Hiiraan", "Lower Shabelle", "Middle Shabelle",
               "Woqooyi Galbeed", "Togdheer"),
    latitude = c(2.0469, 9.5600, 11.2842, -0.3582, 3.1167, 6.7697,
                 8.4084, 4.7358, 1.7159, 2.7809, 10.4396, 9.5221),
    longitude = c(45.3182, 44.0650, 49.1816, 42.5454, 43.6500, 47.4308,
                  48.4845, 45.2036, 43.9078, 45.5005, 45.0143, 45.5336),
    population = c(2610000, 1200000, 700000, 600000, 500000, 400000,
                   350000, 300000, 250000, 220000, 200000, 190000),
    health_facilities = c(48L, 31L, 22L, 18L, 20L, 16L, 14L, 12L, 11L, 10L, 9L, 8L),
    stringsAsFactors = FALSE
  )
}

gis_demo_points <- function() {
  if (is.null(gis_demo_cache$points)) {
    d <- gis_demo_cities()
    pts <- sf::st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326,
                        remove = FALSE, agr = "constant")
    gis_demo_cache$points <- gis_prepare(pts)
  }
  gis_demo_cache$points
}

gis_demo_polygons <- function() {
  if (!is.null(gis_demo_cache$polygons)) return(gis_demo_cache$polygons)

  d <- gis_demo_cities()
  pts <- gis_demo_points()

  # Voronoi cells around the cities give a set of non-overlapping areas. They
  # are built in UTM zone 38N so the tessellation is planar, then clipped to a
  # box around the cities.
  pts_m <- try(sf::st_transform(pts, 32638), silent = TRUE)
  if (inherits(pts_m, "try-error")) return(NULL)
  env <- sf::st_as_sfc(sf::st_bbox(pts_m) + c(-80000, -80000, 80000, 80000))
  vor <- try(sf::st_voronoi(sf::st_union(sf::st_geometry(pts_m)), envelope = env), silent = TRUE)
  if (inherits(vor, "try-error")) return(NULL)
  vor <- try(sf::st_collection_extract(vor, "POLYGON"), silent = TRUE)
  if (inherits(vor, "try-error")) return(NULL)
  cells <- sf::st_sf(geometry = sf::st_cast(vor, "POLYGON"))
  cells <- suppressWarnings(sf::st_intersection(cells, env))
  if (!nrow(cells)) return(NULL)

  # st_voronoi does not promise to keep the order of the input points, so each
  # cell takes its attributes from the city it actually contains.
  hit <- sf::st_intersects(cells, pts_m)
  k <- vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
  cells <- cells[!is.na(k), , drop = FALSE]
  k <- k[!is.na(k)]
  if (!nrow(cells)) return(NULL)

  rate <- c(6.2, 4.1, 3.4, 9.8, 7.5, 5.1, 2.8, 6.6, 8.9, 4.4, 3.9, 5.7)[k]
  cells$zone <- sprintf("Zone %02d", seq_len(nrow(cells)))
  cells$anchor_city <- d$city[k]
  cells$region <- d$region[k]
  cells$population <- d$population[k]
  cells$cases_per_10000 <- rate
  cells$cases <- as.integer(round(d$population[k] / 10000 * rate))

  out <- gis_prepare(sf::st_transform(cells, 4326))
  gis_demo_cache$polygons <- out
  out
}

# The template offered next to the upload control: the minimum a CSV needs.
gis_demo_csv <- function() {
  d <- gis_demo_cities()
  data.frame(
    place = d$city,
    latitude = d$latitude,
    longitude = d$longitude,
    population = d$population,
    stringsAsFactors = FALSE
  )
}
