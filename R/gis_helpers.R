# GIS helpers ---------------------------------------------------------------
# Pure functions behind the GIS tab: capability checks, coordinate detection,
# file readers, measurements, spatial summaries and colour scales. Nothing here
# reads Shiny inputs, so every function can be checked on its own.
# All spatial calls are namespace-qualified so the rest of the app still starts
# when sf or leaflet is absent.

# ---- capability checks ----------------------------------------------------
gis_pkgs <- function() c("sf", "leaflet")

gis_missing <- function() {
  p <- gis_pkgs()
  p[!vapply(p, requireNamespace, logical(1), quietly = TRUE)]
}

gis_ready <- function() length(gis_missing()) == 0L

gis_have <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# ---- geometry basics ------------------------------------------------------
gis_has_crs <- function(x) !is.na(sf::st_crs(x))

gis_crs_text <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) return("Not set")
  code <- if (!is.null(crs$epsg) && !is.na(crs$epsg)) {
    paste0("EPSG:", crs$epsg)
  } else if (nzchar(crs$input %||% "")) {
    crs$input
  } else {
    "Custom"
  }
  nm <- crs$Name
  if (length(nm) && !is.na(nm) && nzchar(nm)) paste0(code, " (", nm, ")") else code
}

gis_is_geographic <- function(x) isTRUE(sf::st_is_longlat(x))

# Everything is mapped and exported in WGS 84. Layers without a CRS are taken
# as WGS 84, which is the only assumption a plain CSV justifies.
gis_lonlat <- function(x) {
  if (!gis_has_crs(x)) return(sf::st_set_crs(x, 4326))
  if (isTRUE(sf::st_crs(x)$epsg == 4326L)) return(x)
  sf::st_transform(x, 4326)
}

# Drop empty shapes, repair invalid ones and reproject. Returns an sf object
# with a "gis_repairs" attribute listing what changed, for the load message.
gis_prepare <- function(x) {
  notes <- character(0)
  if (!inherits(x, "sf")) stop("The file does not contain spatial features.", call. = FALSE)
  if (!gis_has_crs(x)) {
    x <- sf::st_set_crs(x, 4326)
    notes <- c(notes, "No coordinate system was recorded, so WGS 84 (EPSG:4326) was assumed.")
  }
  keep <- !sf::st_is_empty(x)
  if (!all(keep)) {
    notes <- c(notes, sprintf("%d empty feature(s) removed.", sum(!keep)))
    x <- x[keep, , drop = FALSE]
  }
  invalid <- tryCatch(sum(!sf::st_is_valid(x)), error = function(e) 0L)
  if (isTRUE(invalid > 0)) {
    fixed <- tryCatch(sf::st_make_valid(x), error = function(e) NULL)
    if (!is.null(fixed)) {
      x <- fixed
      notes <- c(notes, sprintf("%d invalid shape(s) repaired.", invalid))
    }
  }
  x <- gis_lonlat(x)
  attr(x, "gis_repairs") <- notes
  x
}

gis_type_word <- function(x) {
  gt <- unique(as.character(sf::st_geometry_type(x)))
  if (all(gt %in% c("POINT", "MULTIPOINT"))) return("point")
  if (all(gt %in% c("LINESTRING", "MULTILINESTRING"))) return("line")
  if (all(gt %in% c("POLYGON", "MULTIPOLYGON"))) return("polygon")
  "mixed"
}

gis_type_label <- function(kind) {
  switch(kind, point = "points", line = "lines", polygon = "areas", "mixed shapes")
}

# ---- coordinate detection -------------------------------------------------
# Column names are matched after stripping punctuation and case, so
# "Longitude (deg)" and "longitude_deg" both normalise to "longitudedeg".
gis_detect_coords <- function(nms) {
  key <- gsub("[^a-z0-9]", "", tolower(nms))
  exact <- function(set) {
    i <- which(key %in% set)
    if (length(i)) nms[i[1]] else NA_character_
  }
  guess <- function(pattern) {
    i <- which(grepl(pattern, key))
    if (!length(i)) return(NA_character_)
    nms[i[order(nchar(key[i]))][1]]
  }

  lon <- exact(c("longitude", "lon", "lng", "long", "longitud", "londeg",
                 "longdeg", "longitudedeg", "longitudedd", "xlong"))
  lat <- exact(c("latitude", "lat", "latitud", "latdeg", "latitudedeg",
                 "latitudedd", "ylat"))
  if (!is.na(lon) && !is.na(lat)) {
    return(list(lon = lon, lat = lat, confidence = "named"))
  }

  lon <- guess("^(lon|lng)|longitude")
  lat <- guess("^lat|latitude")
  if (!is.na(lon) && !is.na(lat)) {
    return(list(lon = lon, lat = lat, confidence = "named"))
  }

  lon <- exact(c("x", "xcoord", "coordx", "easting", "east", "xcoordinate", "utmx"))
  lat <- exact(c("y", "ycoord", "coordy", "northing", "north", "ycoordinate", "utmy"))
  if (!is.na(lon) && !is.na(lat)) {
    return(list(lon = lon, lat = lat, confidence = "xy"))
  }
  list(lon = NA_character_, lat = NA_character_, confidence = "none")
}

gis_numeric_columns <- function(df) {
  names(df)[vapply(df, function(v) is.numeric(v) || is.integer(v), logical(1))]
}

gis_char_columns <- function(df) {
  names(df)[vapply(df, function(v) is.character(v) || is.factor(v), logical(1))]
}

# Coordinates outside the degree ranges usually mean a projected system was
# supplied without its EPSG code.
gis_looks_projected <- function(df, lon, lat) {
  if (is.null(df) || is.na(lon) || is.na(lat)) return(FALSE)
  lo <- suppressWarnings(as.numeric(df[[lon]]))
  la <- suppressWarnings(as.numeric(df[[lat]]))
  ok <- is.finite(lo) & is.finite(la)
  if (!any(ok)) return(FALSE)
  any(abs(lo[ok]) > 180 | abs(la[ok]) > 90)
}

gis_coord_issues <- function(df, lon, lat) {
  if (is.null(df) || is.na(lon) || is.na(lat)) return(NULL)
  lo <- suppressWarnings(as.numeric(df[[lon]]))
  la <- suppressWarnings(as.numeric(df[[lat]]))
  rows <- c(
    "Longitude is not a number" = sum(is.na(lo) & !is.na(df[[lon]])),
    "Latitude is not a number"  = sum(is.na(la) & !is.na(df[[lat]])),
    "Longitude missing"         = sum(is.na(lo) & is.na(df[[lon]])),
    "Latitude missing"          = sum(is.na(la) & is.na(df[[lat]])),
    "Longitude outside -180 to 180" = sum(!is.na(lo) & (lo < -180 | lo > 180)),
    "Latitude outside -90 to 90"    = sum(!is.na(la) & (la < -90 | la > 90))
  )
  rows <- rows[rows > 0]
  if (!length(rows)) return(NULL)
  data.frame(Check = names(rows), Rows = as.integer(rows),
             row.names = NULL, stringsAsFactors = FALSE)
}

# Build an sf point layer from two columns. Rows without a usable pair are
# dropped and counted, never silently repaired into (0, 0).
gis_points_from_coords <- function(df, lon, lat, crs = 4326) {
  if (identical(lon, lat)) stop("Choose two different columns for longitude and latitude.", call. = FALSE)
  crs_obj <- try(sf::st_crs(crs), silent = TRUE)
  if (inherits(crs_obj, "try-error") || is.na(crs_obj)) {
    stop("That EPSG code is not recognised. Try 4326 for latitude/longitude or 32638 for UTM zone 38N.", call. = FALSE)
  }
  lo <- suppressWarnings(as.numeric(df[[lon]]))
  la <- suppressWarnings(as.numeric(df[[lat]]))
  geographic <- isTRUE(sf::st_is_longlat(crs_obj))
  in_range <- !is.na(lo) & !is.na(la)
  if (geographic) in_range <- in_range & abs(lo) <= 180 & abs(la) <= 90
  out_of_range <- sum(!is.na(lo) & !is.na(la) & !in_range)

  if (!any(in_range)) {
    stop(if (out_of_range > 0) {
      "Every coordinate falls outside the valid range for this coordinate system. Check the columns and the EPSG code."
    } else {
      "No row has both a longitude and a latitude, so no points could be created."
    }, call. = FALSE)
  }

  d <- sf::st_drop_geometry(df)[in_range, , drop = FALSE]
  d$.gis_lon <- lo[in_range]
  d$.gis_lat <- la[in_range]
  pts <- sf::st_as_sf(d, coords = c(".gis_lon", ".gis_lat"), crs = crs_obj,
                      remove = TRUE, agr = "constant")
  pts <- gis_lonlat(pts)

  list(sf = pts, used = sum(in_range), total = nrow(df),
       dropped = sum(!in_range), out_of_range = out_of_range)
}

# ---- reading files --------------------------------------------------------
gis_table_exts <- c("csv", "tsv", "txt", "xlsx", "xls")
gis_vector_exts <- c("geojson", "json", "gpkg", "kml", "shp", "zip")

gis_read_any <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% gis_table_exts) {
    return(list(kind = "table", data = gis_read_tabular(path)))
  }
  if (ext %in% gis_vector_exts) {
    return(list(kind = "vector", data = gis_read_vector(path)))
  }
  stop(sprintf(paste0("A .%s file cannot be mapped. Supported formats: CSV, Excel, ",
                      "GeoJSON and zipped shapefiles."), ext), call. = FALSE)
}

gis_guess_sep <- function(path) {
  line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
  if (!length(line) || !nzchar(line)) return(",")
  counts <- vapply(c(",", ";", "\t"), function(s) {
    hits <- gregexpr(s, line, fixed = TRUE)[[1]]
    sum(hits > 0)
  }, integer(1))
  if (max(counts) == 0) "," else c(",", ";", "\t")[which.max(counts)]
}

gis_read_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!gis_have("readxl")) stop("Reading Excel files needs the readxl package.", call. = FALSE)
    d <- as.data.frame(readxl::read_excel(path))
  } else {
    sep <- if (ext == "tsv") "\t" else gis_guess_sep(path)
    d <- tryCatch(
      utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                      check.names = FALSE, fileEncoding = "UTF-8"),
      error = function(e) utils::read.csv(path, sep = sep, stringsAsFactors = FALSE,
                                          check.names = FALSE)
    )
  }
  if (!ncol(d)) stop("The file has no columns.", call. = FALSE)
  if (!nrow(d)) stop("The file has a header but no rows.", call. = FALSE)
  d
}

# A CSV exported from QGIS or ArcGIS often carries a WKT geometry column.
gis_wkt_column <- function(d) {
  cand <- names(d)[grepl("^(geometry|geom|wkt|thegeom|shape)$",
                         gsub("[^a-z0-9]", "", tolower(names(d))))]
  for (cn in cand) {
    v <- as.character(d[[cn]])
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) && grepl("^\\s*(POINT|LINESTRING|POLYGON|MULTIPOINT|MULTILINESTRING|MULTIPOLYGON|GEOMETRYCOLLECTION)",
                           toupper(v[1]))) {
      return(cn)
    }
  }
  NA_character_
}

gis_sf_from_wkt <- function(d, column, crs = 4326) {
  wkt <- as.character(d[[column]])
  ok <- !is.na(wkt) & nzchar(trimws(wkt))
  if (!any(ok)) stop("The geometry column is empty.", call. = FALSE)
  geom <- try(sf::st_as_sfc(wkt[ok], crs = crs), silent = TRUE)
  if (inherits(geom, "try-error")) {
    stop("The geometry column could not be read as WKT text.", call. = FALSE)
  }
  d <- d[ok, , drop = FALSE]
  out <- sf::st_sf(sf::st_drop_geometry(d), geometry = geom)
  out <- gis_prepare(out)
  attr(out, "gis_used_rows") <- sum(ok)
  out
}

gis_read_vector <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "zip") {
    exdir <- file.path(tempdir(), paste0("gis_unzip_", as.integer(stats::runif(1, 1, 1e8))))
    dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
    ok <- tryCatch({ utils::unzip(path, exdir = exdir); TRUE }, error = function(e) FALSE)
    if (!ok) stop("The ZIP archive could not be opened.", call. = FALSE)
    cand <- list.files(exdir, pattern = "\\.(shp|gpkg|geojson|json|kml)$",
                       recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (!length(cand)) {
      stop("The ZIP archive contains no shapefile, GeoPackage, GeoJSON or KML file.", call. = FALSE)
    }
    shp <- grep("\\.shp$", cand, ignore.case = TRUE, value = TRUE)
    path <- if (length(shp)) shp[1] else cand[1]
    ext <- tolower(tools::file_ext(path))
  }

  if (ext == "shp") {
    base <- tools::file_path_sans_ext(path)
    missing <- c(".dbf", ".shx")[!file.exists(paste0(base, c(".dbf", ".shx")))]
    if (length(missing)) {
      stop(sprintf("This .shp file is missing its %s file(s). Zip the whole shapefile and upload the ZIP instead.",
                   paste(missing, collapse = " and ")), call. = FALSE)
    }
  }

  layers <- try(sf::st_layers(path), silent = TRUE)
  layer <- NULL
  if (!inherits(layers, "try-error") && length(layers$name) > 1) layer <- layers$name[1]

  x <- try(sf::st_read(path, layer = layer, quiet = TRUE, stringsAsFactors = FALSE),
           silent = TRUE)
  if (inherits(x, "try-error")) {
    msg <- conditionMessage(attr(x, "condition"))
    stop(sprintf("This file could not be read as a spatial layer. %s", msg), call. = FALSE)
  }
  if (!inherits(x, "sf") || !nrow(x)) stop("The file contains no features.", call. = FALSE)
  x
}

# Internal helper columns are never shown or exported.
gis_tidy_attrs <- function(x) {
  bad <- names(x)[startsWith(names(x), ".")]
  if (length(bad)) x <- x[, setdiff(names(x), bad), drop = FALSE]
  x
}

# ---- measurements ---------------------------------------------------------
# Distances and areas on WGS 84 use the s2 engine (geodesic, metres). When s2
# is unavailable the layer is projected to its UTM zone first.
gis_point_km <- function(lon1, lat1, lon2, lat2) {
  pts <- sf::st_sfc(sf::st_point(c(lon1, lat1)), sf::st_point(c(lon2, lat2)), crs = 4326)
  as.numeric(sf::st_distance(pts[1], pts[2]))[1] / 1000
}

gis_area_km2 <- function(x) {
  x <- gis_lonlat(x)
  a <- tryCatch(as.numeric(sf::st_area(x)), error = function(e) rep(NA_real_, nrow(x)))
  a / 1e6
}

gis_length_km <- function(x) {
  x <- gis_lonlat(x)
  a <- tryCatch(as.numeric(sf::st_length(x)), error = function(e) rep(NA_real_, nrow(x)))
  a / 1000
}

# UTM zone from the centre of the extent; NA when the extent is too wide.
gis_utm_epsg <- function(x) {
  if (!gis_has_crs(x)) return(NA_integer_)
  y <- try(gis_lonlat(x), silent = TRUE)
  if (inherits(y, "try-error") || !nrow(y)) return(NA_integer_)
  bb <- sf::st_bbox(y)
  lon <- mean(c(bb[["xmin"]], bb[["xmax"]]))
  lat <- mean(c(bb[["ymin"]], bb[["ymax"]]))
  if (!is.finite(lon) || !is.finite(lat) || abs(lat) > 84) return(NA_integer_)
  zone <- floor((lon + 180) / 6) + 1
  as.integer(if (lat >= 0) 32600 + zone else 32700 + zone)
}

gis_planar <- function(x) {
  x <- gis_lonlat(x)
  epsg <- gis_utm_epsg(x)
  if (is.na(epsg)) epsg <- 3857
  tryCatch(sf::st_transform(x, epsg), error = function(e) sf::st_transform(x, 3857))
}

# Buffer in kilometres, built in a metric projection and returned in WGS 84.
gis_buffer_km <- function(x, km) {
  if (!is.finite(km) || km <= 0) stop("Enter a buffer distance greater than zero.", call. = FALSE)
  planar <- gis_planar(x)
  buf <- try(sf::st_buffer(sf::st_geometry(planar), dist = km * 1000), silent = TRUE)
  if (inherits(buf, "try-error")) {
    stop("The buffer could not be built. The shapes may be invalid; try another distance.", call. = FALSE)
  }
  out <- gis_lonlat(sf::st_sf(geometry = buf))
  attr(out, "gis_buffer_detail") <- list(
    crs = gis_crs_text(sf::st_crs(planar)),
    approximate = identical(as.integer(sf::st_crs(planar)$epsg), 3857L)
  )
  out
}

gis_bearing <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlon <- (lon2 - lon1) * rad
  y <- sin(dlon) * cos(lat2 * rad)
  x <- cos(lat1 * rad) * sin(lat2 * rad) -
    sin(lat1 * rad) * cos(lat2 * rad) * cos(dlon)
  (atan2(y, x) / rad + 360) %% 360
}

gis_compass <- function(bearing) {
  pts <- c("north", "north-east", "east", "south-east",
           "south", "south-west", "west", "north-west")
  pts[(round(bearing / 45) %% 8) + 1]
}

# Nearest-neighbour distance per point. Exact up to max_n points; beyond that a
# random subset is used and the sample size is reported.
gis_nn_km <- function(x, max_n = 2000) {
  pts <- sf::st_geometry(gis_lonlat(x))
  n <- length(pts)
  if (n < 2) return(NULL)
  sampled <- FALSE
  if (n > max_n) {
    idx <- sort(sample.int(n, max_n))
    pts <- pts[idx]
    sampled <- TRUE
  }
  d <- tryCatch(as.matrix(sf::st_distance(pts)), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  diag(d) <- NA_real_
  nn <- apply(d, 1, min, na.rm = TRUE) / 1000
  nn <- nn[is.finite(nn)]
  if (!length(nn)) return(NULL)
  list(mean = mean(nn), median = stats::median(nn), max = max(nn),
       sampled = sampled, n_used = length(pts), n_total = n)
}

# ---- formatting -----------------------------------------------------------
gis_fmt_int <- function(x) {
  if (!length(x) || is.na(x)) return("-")
  formatC(x, big.mark = ",", format = "d")
}

gis_fmt <- function(x, digits = 1) {
  if (!length(x) || !is.finite(x)) return("-")
  formatC(x, big.mark = ",", format = "f", digits = digits)
}

gis_fmt_km <- function(x) {
  if (!length(x) || !is.finite(x)) return("-")
  if (abs(x) >= 1000) paste0(gis_fmt(x / 1000, 1), "k km") else paste0(gis_fmt(x, 1), " km")
}

gis_fmt_area <- function(x) {
  if (!length(x) || !is.finite(x)) return("-")
  if (abs(x) >= 1e6) return(paste0(gis_fmt(x / 1e6, 2), "M km2"))
  if (abs(x) >= 1000) return(paste0(gis_fmt(x / 1000, 1), "k km2"))
  if (abs(x) < 1) return(paste0(gis_fmt(x * 100, 1), " ha"))
  paste0(gis_fmt(x, 1), " km2")
}

# ---- spatial summary ------------------------------------------------------
# Numbers behind the summary tiles. Distances and areas are geodesic.
gis_summary <- function(x, max_nn = 2000) {
  x <- gis_lonlat(x)
  kind <- gis_type_word(x)
  n <- nrow(x)
  bb <- sf::st_bbox(x)
  lon_span <- bb[["xmax"]] - bb[["xmin"]]
  lat_span <- bb[["ymax"]] - bb[["ymin"]]
  mid_lon <- mean(c(bb[["xmin"]], bb[["xmax"]]))
  mid_lat <- mean(c(bb[["ymin"]], bb[["ymax"]]))
  width_km <- if (lon_span > 0) gis_point_km(bb[["xmin"]], mid_lat, bb[["xmax"]], mid_lat) else 0
  height_km <- if (lat_span > 0) gis_point_km(mid_lon, bb[["ymin"]], mid_lon, bb[["ymax"]]) else 0

  out <- list(
    kind = kind, n = n, bbox = bb,
    lon_span = lon_span, lat_span = lat_span,
    width_km = width_km, height_km = height_km,
    mid_lon = mid_lon, mid_lat = mid_lat,
    extent_text = sprintf("Longitude %.2f to %.2f, latitude %.2f to %.2f",
                          bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]),
    invalid = tryCatch(sum(!sf::st_is_valid(x)), error = function(e) NA_integer_),
    empty = tryCatch(sum(sf::st_is_empty(x)), error = function(e) NA_integer_),
    crs = gis_crs_text(x),
    tiles = list()
  )

  if (kind == "point") {
    cc <- sf::st_coordinates(x)
    out$unique_coords <- nrow(unique(round(cc[, 1:2], 6)))
    out$nn <- gis_nn_km(x, max_n = max_nn)
    area_km2 <- max(width_km, 0.5) * max(height_km, 0.5)
    out$density <- if (n > 1) n / area_km2 * 1000 else NA_real_
    spacing <- if (!is.null(out$nn)) gis_fmt_km(out$nn$mean) else "-"
    out$tiles <- list(
      list(label = "Locations", value = gis_fmt_int(n),
           hint = if (isTRUE(out$unique_coords < n)) {
             sprintf("%s distinct coordinate pairs", gis_fmt_int(out$unique_coords))
           } else "one point per row"),
      list(label = "Area covered",
           value = paste0(gis_fmt(width_km, 0), " x ", gis_fmt(height_km, 0), " km"),
           hint = "bounding box, not travel distance"),
      list(label = "Typical spacing", value = spacing,
           hint = "mean distance to the nearest other point"),
      list(label = "Density",
           value = if (isTRUE(is.finite(out$density))) paste0(gis_fmt(out$density, 1), " /1,000 km2") else "-",
           hint = "points per 1,000 km2 of the bounding box")
    )
  } else if (kind == "polygon") {
    areas <- gis_area_km2(x)
    out$areas <- areas
    best <- which.max(areas)
    out$tiles <- list(
      list(label = "Areas", value = gis_fmt_int(n), hint = "polygon features"),
      list(label = "Total area", value = gis_fmt_area(sum(areas, na.rm = TRUE)),
           hint = "sum of all polygons"),
      list(label = "Largest area", value = gis_fmt_area(max(areas, na.rm = TRUE)),
           hint = if (length(best)) sprintf("feature %d", best) else NULL),
      list(label = "Median area", value = gis_fmt_area(stats::median(areas, na.rm = TRUE)),
           hint = "half the polygons are smaller")
    )
  } else if (kind == "line") {
    lens <- gis_length_km(x)
    out$lengths <- lens
    out$tiles <- list(
      list(label = "Lines", value = gis_fmt_int(n), hint = "line features"),
      list(label = "Total length", value = gis_fmt_km(sum(lens, na.rm = TRUE)),
           hint = "sum of all lines"),
      list(label = "Longest line", value = gis_fmt_km(max(lens, na.rm = TRUE)), hint = NULL),
      list(label = "Area covered",
           value = paste0(gis_fmt(width_km, 0), " x ", gis_fmt(height_km, 0), " km"),
           hint = "bounding box")
    )
  } else {
    out$tiles <- list(
      list(label = "Features", value = gis_fmt_int(n), hint = "mixed geometry types"),
      list(label = "Area covered",
           value = paste0(gis_fmt(width_km, 0), " x ", gis_fmt(height_km, 0), " km"),
           hint = "bounding box")
    )
  }
  out
}

# ---- colour scales --------------------------------------------------------
gis_ramps <- function() {
  list(
    "Blue"   = c("#EFF6FF", "#93C5FD", "#3B82F6", "#1D4ED8"),
    "Teal"   = c("#F0FDFA", "#5EEAD4", "#14B8A6", "#0F766E"),
    "Amber"  = c("#FFFBEB", "#FCD34D", "#F59E0B", "#B45309"),
    "Purple" = c("#F5F3FF", "#C4B5FD", "#8B5CF6", "#5B21B6"),
    "Red"    = c("#FEF2F2", "#FCA5A5", "#EF4444", "#991B1B")
  )
}

gis_ramp <- function(name = "Blue", n = 100) {
  cols <- gis_ramps()[[name]] %||% gis_ramps()[["Blue"]]
  grDevices::colorRampPalette(cols)(n)
}

gis_cat_colors <- function() {
  c("#2563EB", "#0D9488", "#7C3AED", "#EA580C", "#DB2777",
    "#0891B2", "#65A30D", "#CA8A04", "#4F46E5", "#BE123C")
}

# Class breaks for a choropleth. Quantile bins put the same number of areas in
# each colour; equal intervals keep the value ranges equal.
gis_classify <- function(v, n = 5, method = c("quantile", "equal")) {
  method <- match.arg(method)
  v <- suppressWarnings(as.numeric(v))
  ok <- is.finite(v)
  if (!any(ok)) return(NULL)
  u <- unique(v[ok])
  if (length(u) < 2) return(list(single = u[1], brks = NULL))

  style <- if (identical(method, "quantile")) "quantile" else "equal"
  brks <- tryCatch({
    if (gis_have("classInt")) {
      classInt::classIntervals(v[ok], n = min(n, length(u)), style = style)$brks
    } else {
      pretty(range(v[ok]), n = n)
    }
  }, error = function(e) pretty(range(v[ok]), n = n))
  brks <- sort(unique(brks))
  if (length(brks) < 2) return(list(single = u[1], brks = NULL))
  list(brks = brks, single = NULL)
}

# ---- presentation ---------------------------------------------------------
gis_safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x %||% "map")
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "map" else x
}

# A column whose values can label features: unique text, then any short text.
gis_label_column <- function(x) {
  d <- sf::st_drop_geometry(x)
  keep <- names(d)[!startsWith(names(d), ".")]
  if (!length(keep)) return(NA_character_)
  chars <- keep[vapply(d[keep], function(v) is.character(v) || is.factor(v), logical(1))]
  if (!length(chars)) return(NA_character_)
  for (v in chars) {
    u <- as.character(d[[v]])
    if (!anyNA(u) && all(nzchar(u)) && length(unique(u)) == length(u) &&
        length(u) <= 5000) {
      return(v)
    }
  }
  for (v in chars) {
    u <- unique(as.character(d[[v]]))
    if (length(u) <= 100 && !anyNA(u)) return(v)
  }
  chars[1]
}

# Display names for rows, unique so select inputs stay readable.
gis_place_labels <- function(x, column = NA_character_) {
  d <- sf::st_drop_geometry(x)
  n <- nrow(d)
  lab <- if (!is.na(column) && column %in% names(d)) as.character(d[[column]]) else rep(NA_character_, n)
  if (length(lab) != n) lab <- rep(NA_character_, n)
  blank <- is.na(lab) | !nzchar(trimws(lab))
  lab[blank] <- sprintf("Row %d", which(blank))
  if (anyDuplicated(lab)) lab <- make.unique(lab, sep = " #")
  lab
}

# Columns worth showing in a popup: six at most, no internal helpers.
gis_popup_html <- function(d, max_fields = 6) {
  if (!ncol(d)) return(rep("", nrow(d)))
  cols <- utils::head(names(d)[!startsWith(names(d), ".")], max_fields)
  if (!length(cols)) return(rep("", nrow(d)))
  vals <- lapply(cols, function(cl) {
    v <- as.character(d[[cl]])
    v[is.na(v)] <- ""
    htmltools::htmlEscape(v)
  })
  vapply(seq_len(nrow(d)), function(i) {
    paste0("<div class='gis-pop'>",
           paste(sprintf("<div><span class='gis-pop-key'>%s</span> %s</div>",
                         cols, vapply(vals, `[`, character(1), i)),
                 collapse = ""),
           "</div>")
  }, character(1))
}

# ---- session state --------------------------------------------------------
# One dataset at a time, plus an optional buffer overlay. Nothing is written to
# disk or to the global environment.
gis_new_state <- function() {
  shiny::reactiveValues(
    data = NULL, buffer = NULL, error = NULL, note = NULL,
    pending = NULL, fit = 0L
  )
}
