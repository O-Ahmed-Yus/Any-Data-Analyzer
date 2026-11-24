# GIS layer store ------------------------------------------------------------
# The per-session spatial state used by every GIS submodule: one
# reactiveValues holding the spatial layers, plus the small helpers that add,
# read, restyle and remove them. `layers` is a plain list keyed by layer id, so
# a layer reads as store$layers[[id]]; `rev` is bumped whenever the set of
# layers changes and drives the map rebuild in mod_gis_map.R.
#
# Nothing is written to disk or to .GlobalEnv, so two visitors never see each
# other's layers. All spatial calls stay namespace-qualified so the rest of the
# app still starts when sf or leaflet is absent.

# Layer colours, recycled in order as layers are added.
gis_palette <- c("#2563EB", "#059669", "#D97706", "#7C3AED",
                 "#DC2626", "#0891B2", "#CA8A04", "#DB2777")

gis_palette_color <- function(i) gis_palette[((i - 1L) %% length(gis_palette)) + 1L]

gis_new_store <- function() {
  shiny::reactiveValues(
    layers = list(), rasters = list(), results = list(),
    active = NULL, counter = 0L, rev = 0L
  )
}

# ---- extent and layer summaries -------------------------------------------
gis_extent_text <- function(x) {
  if (!gis_has_crs(x)) {
    bb <- sf::st_bbox(x)
    return(sprintf("x %.4f to %.4f, y %.4f to %.4f (unprojected units)",
                   bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]))
  }
  bb <- try(sf::st_bbox(gis_bbox_4326(x)), silent = TRUE)
  if (inherits(bb, "try-error")) return("Unavailable")
  sprintf("Longitude %.3f to %.3f, latitude %.3f to %.3f",
          bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])
}

# Always returns lon/lat bounds so Leaflet can fit a layer recorded in any CRS.
gis_bbox_4326 <- function(x) {
  y <- if (isTRUE(sf::st_crs(x)$epsg == 4326)) x else sf::st_transform(x, 4326)
  sf::st_bbox(y)
}

gis_geom_summary <- function(x) {
  g <- sf::st_geometry(x)
  valid <- try(sf::st_is_valid(g), silent = TRUE)
  invalid <- if (inherits(valid, "try-error")) NA_integer_ else sum(!valid, na.rm = TRUE)
  list(
    n = nrow(x),
    geom = paste(sort(unique(as.character(sf::st_geometry_type(g)))), collapse = ", "),
    crs = gis_crs_text(x),
    extent = gis_extent_text(x),
    empty = sum(sf::st_is_empty(g)),
    invalid = invalid,
    duplicates = sum(duplicated(g))
  )
}

# First character column with a handful of distinct, complete values: a sane
# default for hover labels.
gis_default_label <- function(x) {
  d <- sf::st_drop_geometry(x)
  if (!ncol(d)) return(NA_character_)
  chars <- names(d)[vapply(d, function(v) is.character(v) || is.factor(v), logical(1))]
  for (v in chars) {
    u <- unique(as.character(d[[v]]))
    if (length(u) <= 50 && all(!is.na(u))) return(v)
  }
  NA_character_
}

# ---- layer operations ------------------------------------------------------
# Every mutator bumps `rev`, which is the single signal the map listens to.

# Returns the new layer id, or NA when there is nothing addable.
gis_layer_add <- function(store, x, name = NULL, source = "upload",
                          synthetic = FALSE) {
  if (is.null(x) || !inherits(x, "sf") || !nrow(x)) return(NA_character_)
  store$counter <- store$counter + 1L
  id <- paste0("L", store$counter)
  nm <- trimws(name %||% "")
  if (!nzchar(nm)) nm <- paste("Layer", store$counter)
  existing <- vapply(store$layers, `[[`, character(1), "name")
  if (nm %in% existing) nm <- paste0(nm, " (", store$counter, ")")
  store$layers[[id]] <- list(
    id = id, name = nm, sf = x, source = source, synthetic = synthetic,
    visible = TRUE, opacity = 1, color = gis_palette_color(store$counter),
    label = gis_default_label(x), created = Sys.time()
  )
  store$active <- id
  store$rev <- store$rev + 1L
  id
}

gis_layer <- function(store, id = NULL) {
  id <- id %||% store$active
  if (is.null(id)) return(NULL)
  store$layers[[id]]
}

# Named vector of ids keyed by layer name: what selectInput() wants.
gis_layer_choices <- function(store, kind = NULL) {
  lay <- store$layers
  if (!length(lay)) return(stats::setNames(character(0), character(0)))
  keep <- vapply(lay, function(l) {
    if (is.null(kind)) TRUE else gis_type_word(l$sf) %in% kind
  }, logical(1))
  lay <- lay[keep]
  stats::setNames(vapply(lay, `[[`, character(1), "id"),
                  vapply(lay, `[[`, character(1), "name"))
}

gis_layer_update <- function(store, id, ...) {
  if (is.null(store$layers[[id]])) return(invisible(FALSE))
  fields <- list(...)
  for (nm in names(fields)) store$layers[[id]][[nm]] <- fields[[nm]]
  store$rev <- store$rev + 1L
  invisible(TRUE)
}

gis_layer_remove <- function(store, id) {
  store$layers[[id]] <- NULL
  if (identical(store$active, id)) {
    store$active <- if (length(store$layers)) names(store$layers)[length(store$layers)] else NULL
  }
  store$rev <- store$rev + 1L
  invisible(TRUE)
}

gis_store_clear <- function(store) {
  store$layers <- list()
  store$rasters <- list()
  store$results <- list()
  store$active <- NULL
  store$rev <- store$rev + 1L
  invisible(TRUE)
}
