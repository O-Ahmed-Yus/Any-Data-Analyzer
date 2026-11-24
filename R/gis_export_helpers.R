# GIS export helpers --------------------------------------------------------
# Flat tables and files produced by the GIS tab: CSV frames, GeoJSON, a static
# PNG image of the current view and a self-contained HTML map. Kept apart from
# the readers and measurements in gis_helpers.R.

# ---- tables ---------------------------------------------------------------
# Attributes plus coordinates: exact for points, centres for areas and lines.
gis_csv_frame <- function(x) {
  x <- gis_lonlat(x)
  d <- sf::st_drop_geometry(x)
  d <- d[, setdiff(names(d), names(d)[startsWith(names(d), ".")]), drop = FALSE]
  kind <- gis_type_word(x)

  if (kind == "point") {
    cc <- sf::st_coordinates(x)
    d$longitude <- round(cc[, 1], 6)
    d$latitude <- round(cc[, 2], 6)
    return(d)
  }

  cc <- suppressWarnings(sf::st_coordinates(sf::st_centroid(x)))
  d$centre_longitude <- round(cc[, 1], 6)
  d$centre_latitude <- round(cc[, 2], 6)
  if (kind %in% c("polygon", "mixed")) d$area_km2 <- round(gis_area_km2(x), 4)
  if (kind %in% c("line", "mixed")) d$length_km <- round(gis_length_km(x), 4)
  d
}

gis_feature_labels <- function(x, column = NA_character_) {
  gis_place_labels(x, column)
}

gis_area_table <- function(x, labels = NULL) {
  x <- gis_lonlat(x)
  a <- gis_area_km2(x)
  labels <- labels %||% sprintf("Feature %d", seq_len(nrow(x)))
  total <- sum(a, na.rm = TRUE)
  out <- data.frame(
    Feature = labels,
    `Area (km2)` = round(a, 2),
    `Area (hectares)` = round(a * 100, 0),
    `Area (square miles)` = round(a * 0.386102, 2),
    `Share of total (%)` = if (total > 0) round(a / total * 100, 1) else NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[order(-out[["Area (km2)"]]), , drop = FALSE]
}

gis_length_table <- function(x, labels = NULL) {
  x <- gis_lonlat(x)
  len <- gis_length_km(x)
  labels <- labels %||% sprintf("Feature %d", seq_len(nrow(x)))
  out <- data.frame(
    Feature = labels,
    `Length (km)` = round(len, 2),
    `Length (miles)` = round(len * 0.621371, 2),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[order(-out[["Length (km)"]]), , drop = FALSE]
}

# Distances from one place to every other place in the layer.
gis_distance_table <- function(x, from, labels = NULL) {
  x <- gis_lonlat(x)
  n <- nrow(x)
  labels <- labels %||% sprintf("Row %d", seq_len(n))
  pts <- sf::st_geometry(x)
  cc <- sf::st_coordinates(pts)

  km <- tryCatch(as.numeric(sf::st_distance(pts[from], pts)) / 1000,
                 error = function(e) rep(NA_real_, n))
  bearing <- gis_bearing(cc[from, 1], cc[from, 2], cc[, 1], cc[, 2])

  out <- data.frame(
    Place = labels,
    `Distance (km)` = round(km, 2),
    `Distance (miles)` = round(km * 0.621371, 2),
    Direction = gis_compass(bearing),
    `Bearing (degrees)` = round(bearing, 0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out$Direction[from] <- "start"
  out$`Bearing (degrees)`[from] <- NA_real_
  out <- out[order(out[["Distance (km)"]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

gis_buffer_table <- function(buffer, labels, km) {
  a <- gis_area_km2(buffer)
  data.frame(
    Place = labels,
    `Radius (km)` = km,
    `Buffer area (km2)` = round(a, 1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---- files ----------------------------------------------------------------
gis_write_csv <- function(frame, path) {
  utils::write.csv(frame, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  path
}

gis_write_geojson <- function(x, path) {
  x <- gis_lonlat(x)
  names(x) <- make.names(names(x), unique = TRUE)
  sf::st_write(x, path, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  path
}

# Saving a self-contained widget needs pandoc; without it the map plus its
# dependency folder are zipped instead, so the download is never broken.
gis_pandoc <- function() {
  if (gis_have("rmarkdown")) {
    ok <- tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)
    if (isTRUE(ok)) return(TRUE)
  }
  nzchar(Sys.which("pandoc"))
}

gis_zip_available <- function() nzchar(Sys.which("zip"))

gis_html_ext <- function() {
  if (gis_pandoc()) return(".html")
  if (gis_zip_available()) return(".zip")
  NA_character_
}

gis_save_map_html <- function(map, file) {
  if (gis_pandoc()) {
    htmlwidgets::saveWidget(map, file, selfcontained = TRUE)
    return(invisible("html"))
  }
  if (!gis_zip_available()) {
    stop("Saving an interactive map needs pandoc or the zip utility.", call. = FALSE)
  }
  tmp <- file.path(tempdir(), paste0("gis_html_", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(map, file.path(tmp, "map.html"),
                          selfcontained = FALSE, libdir = "lib")
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile = file, files = c("map.html", "lib"), flags = "-r9X")
  invisible("zip")
}

# ---- static image ---------------------------------------------------------
# A ggplot version of the current view, used for the PNG download. Shapes are
# drawn from plain coordinate data rather than with geom_sf: geom_sf evaluates
# theme-relative default aesthetics that a running theme can leave unresolved,
# and plain geometry renders the same in every session. There is no basemap,
# which is why the caption points at the HTML download.

# Coordinates in plotting order, plus a group id for rings and line parts.
gis_plot_frame <- function(x) {
  x <- gis_lonlat(x)
  kind <- gis_type_word(x)
  if (kind == "point") {
    cc <- sf::st_coordinates(x)
    return(list(kind = "point",
                df = data.frame(lon = cc[, 1], lat = cc[, 2],
                                feature = seq_len(nrow(x)))))
  }
  cc <- tryCatch(as.data.frame(sf::st_coordinates(x)), error = function(e) NULL)
  if (is.null(cc) || !nrow(cc)) return(NULL)
  # st_coordinates labels parts L1..Lk and puts the feature id last, so the
  # final L column identifies the feature and the earlier ones are rings.
  levels <- grep("^L[0-9]+$", names(cc), value = TRUE)
  feat_col <- if (length(levels)) levels[length(levels)] else NULL
  group_cols <- setdiff(levels, feat_col)
  feature <- if (is.null(feat_col)) rep(1L, nrow(cc)) else cc[[feat_col]]
  grp <- if (length(group_cols)) {
    do.call(paste, c(cc[group_cols], sep = "."))
  } else {
    rep("1", nrow(cc))
  }
  list(kind = kind,
       df = data.frame(lon = cc$X, lat = cc$Y, feature = feature,
                       group = paste(feature, grp, sep = ".")))
}

gis_static_map <- function(spec) {
  x <- gis_lonlat(spec$x)
  kind <- spec$kind %||% "points"
  cols <- gis_ramp(spec$palette %||% "Blue", 7)
  legend_title <- spec$legend_title %||% NULL
  values <- spec$values
  frame <- gis_plot_frame(x)
  df <- frame$df
  if (!is.null(values)) df$value <- values[df$feature]

  theme_gis <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#E2E8F0", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#F8FAFC", colour = NA),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "#64748B", size = 8),
      plot.title = ggplot2::element_text(face = "bold", colour = "#0F172A", size = 13),
      plot.subtitle = ggplot2::element_text(colour = "#475569", size = 9),
      plot.caption = ggplot2::element_text(colour = "#94A3B8", size = 7.5),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8)
    )

  p <- ggplot2::ggplot() + theme_gis

  if (kind == "heat" && nrow(df) >= 3) {
    p <- p +
      ggplot2::stat_density_2d(
        data = df, ggplot2::aes(x = .data$lon, y = .data$lat,
                                fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = 120) +
      ggplot2::scale_fill_gradientn(colours = cols, name = "Density")
    if (nrow(df) <= 5000) {
      p <- p + ggplot2::geom_point(data = df, ggplot2::aes(x = .data$lon, y = .data$lat),
                                   size = 0.8, colour = "#0F172A", alpha = 0.35,
                                   inherit.aes = FALSE)
    }
  } else if (kind %in% c("choropleth", "area") && !is.null(values)) {
    df$value <- as.numeric(df$value)
    p <- p +
      ggplot2::geom_polygon(data = df,
                            ggplot2::aes(x = .data$lon, y = .data$lat,
                                         group = .data$group, fill = .data$value),
                            colour = "white", linewidth = 0.25) +
      ggplot2::scale_fill_gradientn(colours = cols, name = legend_title,
                                    na.value = "#CBD5E1")
  } else if (kind == "distance" && !is.null(spec$line)) {
    line_df <- gis_plot_frame(spec$line)$df
    pair_df <- df[spec$pair, , drop = FALSE]
    if (!is.null(spec$labels)) pair_df$label <- spec$labels
    p <- p +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$lon, y = .data$lat),
                          size = 2, colour = "#94A3B8", alpha = 0.8) +
      ggplot2::geom_path(data = line_df,
                         ggplot2::aes(x = .data$lon, y = .data$lat),
                         colour = "#2563EB", linewidth = 1) +
      ggplot2::geom_point(data = pair_df, ggplot2::aes(x = .data$lon, y = .data$lat),
                          size = 3.2, colour = "#1D4ED8")
    if (!is.null(pair_df$label)) {
      p <- p + ggplot2::geom_text(data = pair_df,
                                  ggplot2::aes(x = .data$lon, y = .data$lat,
                                               label = .data$label),
                                  nudge_y = 0.4, size = 3.2, colour = "#0F172A")
    }
  } else if (kind == "buffer" && !is.null(spec$buffer)) {
    buf_df <- gis_plot_frame(spec$buffer)$df
    p <- p +
      ggplot2::geom_polygon(data = buf_df,
                            ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$group),
                            fill = "#93C5FD", colour = "#2563EB",
                            alpha = 0.35, linewidth = 0.4) +
      ggplot2::geom_point(data = df, ggplot2::aes(x = .data$lon, y = .data$lat),
                          size = 2.2, colour = "#1E3A8A")
  } else if (kind == "lines") {
    p <- p + ggplot2::geom_path(data = df,
                                ggplot2::aes(x = .data$lon, y = .data$lat,
                                             group = .data$group),
                                colour = "#2563EB", linewidth = 0.8)
  } else if (!is.null(values) && is.numeric(values)) {
    p <- p +
      ggplot2::geom_point(data = df,
                          ggplot2::aes(x = .data$lon, y = .data$lat, colour = .data$value),
                          size = 2.2, alpha = 0.9) +
      ggplot2::scale_colour_gradientn(colours = cols, name = legend_title)
  } else if (!is.null(values)) {
    df$value <- as.character(df$value)
    n_cat <- length(unique(stats::na.omit(df$value)))
    p <- p +
      ggplot2::geom_point(data = df,
                          ggplot2::aes(x = .data$lon, y = .data$lat, colour = .data$value),
                          size = 2.2, alpha = 0.9) +
      ggplot2::scale_colour_manual(
        values = grDevices::colorRampPalette(gis_cat_colors())(max(n_cat, 2)),
        name = legend_title)
  } else {
    p <- p + ggplot2::geom_point(data = df, ggplot2::aes(x = .data$lon, y = .data$lat),
                                 size = 2.2, colour = "#2563EB")
  }

  p + ggplot2::coord_quickmap() + ggplot2::labs(
    title = spec$title,
    subtitle = spec$subtitle,
    caption = paste("Static image of the data, without a basemap.",
                    "The HTML download keeps the interactive map.")
  )
}

gis_save_map_png <- function(spec, path) {
  p <- gis_static_map(spec)
  ggplot2::ggsave(path, p, width = 8, height = 5.5, dpi = 150, bg = "white")
  path
}
