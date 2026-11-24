# Interactive map explorer ---------------------------------------------------
# Leaflet map, basemap switching, layer manager, popups, measurement tools and
# a table linked to the map in both directions.

gis_basemaps <- function() {
  p <- leaflet::providers
  list(
    "OpenStreetMap"       = p$OpenStreetMap,
    "Carto Positron"      = p$CartoDB.Positron,
    "Carto Dark Matter"   = p$CartoDB.DarkMatter,
    "Esri World Imagery"  = p$Esri.WorldImagery,
    "OpenTopoMap"         = p$OpenTopoMap
  )
}

gis_popup_html <- function(d, max_fields = 8) {
  if (!ncol(d)) return(rep("", nrow(d)))
  cols <- utils::head(names(d), max_fields)
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

# Add one layer to a leaflet map. Group names come from the layer name so the
# built-in layer control labels match the layer manager.
gis_map_layer <- function(map, lay, cluster = FALSE) {
  x <- lay$sf
  if (gis_has_crs(x) && !identical(sf::st_crs(x)$epsg, 4326L)) {
    x <- try(sf::st_transform(x, 4326), silent = TRUE)
    if (inherits(x, "try-error")) return(map)
  }
  d <- sf::st_drop_geometry(x)
  pop <- gis_popup_html(d)
  lab <- if (!is.na(lay$label) && lay$label %in% names(d)) as.character(d[[lay$label]]) else NULL
  op <- lay$opacity %||% 1
  ids <- as.character(seq_len(nrow(x)))
  tw <- gis_type_word(x)

  if (tw == "point") {
    map <- leaflet::addCircleMarkers(
      map, data = x, radius = 5, color = "#ffffff", weight = 1,
      fillColor = lay$color, fillOpacity = op, opacity = 1,
      popup = pop, label = lab, group = lay$name, layerId = ids,
      clusterOptions = if (isTRUE(cluster) && nrow(x) > 1) {
        leaflet::markerClusterOptions(showCoverageOnHover = FALSE)
      } else NULL
    )
  } else if (tw == "line") {
    map <- leaflet::addPolylines(
      map, data = x, color = lay$color, weight = 3, opacity = op,
      popup = pop, label = lab, group = lay$name, layerId = ids
    )
  } else {
    map <- leaflet::addPolygons(
      map, data = x, color = "#ffffff", weight = 1,
      fillColor = lay$color, fillOpacity = op, fill = TRUE, opacity = 1,
      popup = pop, label = lab, group = lay$name, layerId = ids
    )
  }
  map
}

gis_map_highlight <- function(map, lay, rows) {
  x <- lay$sf[rows, ]
  if (gis_has_crs(x) && !identical(sf::st_crs(x)$epsg, 4326L)) x <- sf::st_transform(x, 4326)
  tw <- gis_type_word(x)
  if (tw == "point") {
    leaflet::addCircleMarkers(map, data = x, radius = 9, color = "#111827", weight = 3,
                              fillColor = lay$color, fillOpacity = 0.9,
                              group = "__gis_sel", layerId = as.character(rows))
  } else if (tw == "line") {
    leaflet::addPolylines(map, data = x, color = "#111827", weight = 6,
                          group = "__gis_sel", layerId = as.character(rows))
  } else {
    leaflet::addPolygons(map, data = x, color = "#111827", weight = 4,
                         fillOpacity = 0.35, fillColor = lay$color,
                         group = "__gis_sel", layerId = as.character(rows))
  }
}

# Geodesic area of a polygon drawn on the map. The drawn feature arrives as
# GeoJSON coordinates in WGS 84, so sf measures it on the ellipsoid via s2.
gis_drawn_area_km2 <- function(feature) {
  coords <- feature$geometry$coordinates[[1]]
  m <- do.call(rbind, lapply(coords, function(p) {
    c(as.numeric(p[[1]]), as.numeric(p[[2]]))
  }))
  if (nrow(m) < 4) return(NA_real_)
  poly <- sf::st_sfc(sf::st_polygon(list(m)), crs = 4326)
  as.numeric(sf::st_area(poly)) / 1e6
}

gis_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(tagList(icon("map"), " Interactive map")),
      div(class = "d-flex flex-wrap gap-2 align-items-end mb-2",
          selectInput(ns("basemap"), "Basemap", choices = names(gis_basemaps()), width = "190px"),
          selectInput(ns("zoom_layer"), "Zoom to layer",
                      choices = c("(no layers yet)" = ""), width = "170px"),
          actionButton(ns("zoom"), "Zoom", icon = icon("magnifying-glass-plus")),
          actionButton(ns("reset"), "Reset view", icon = icon("arrows-rotate")),
          checkboxInput(ns("cluster"), "Cluster points", value = FALSE),
          div(class = "ms-auto small-note", id = ns("coords"),
              "Move the cursor over the map")
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel("Active layer style", icon = icon("sliders"),
                               uiOutput(ns("style_ui")))
      ),
      leaflet::leafletOutput(ns("map"), height = "560px"),
      div(class = "small-note mt-2",
          "Click a feature to select the matching row. Draw a polygon with the toolbar to measure its area; ",
          "the ruler tool measures distances. Esri imagery is subject to Esri's terms of use.")
    ),
    bslib::card(
      bslib::card_header(tagList(icon("table-list"), " Attribute table")),
      uiOutput(ns("table_head")),
      DTOutput(ns("attrib"))
    )
  )
}

gis_map_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    structure <- reactiveVal(0L)
    fit_done <- reactiveVal(FALSE)
    observeEvent(store$rev, structure(structure() + 1L), ignoreInit = TRUE)

    last_zoom_choices <- reactiveVal(NULL)
    observe({
      ch <- gis_layer_choices(store)
      if (identical(ch, last_zoom_choices())) return()
      last_zoom_choices(ch)
      updateSelectInput(session, "zoom_layer", choices = ch)
    })

    # ---- map rendering ----------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      structure()
      layers <- isolate(store$layers)
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, minZoom = 2)) |>
        leaflet::addProviderTiles(
          gis_basemaps()[[isolate(input$basemap) %||% "OpenStreetMap"]],
          group = "basemap") |>
        leaflet::setView(46.5, 6, 5) |>
        leaflet::addScaleBar(position = "bottomleft") |>
        leaflet.extras::addFullscreenControl(pseudoFullscreen = TRUE) |>
        leaflet.extras::addMeasurePathToolbar(
          leaflet.extras::measurePathOptions(showDistances = TRUE, showArea = TRUE)) |>
        leaflet.extras::addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE, circleOptions = FALSE, markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()) |>
        htmlwidgets::onRender(sprintf(
          "function(el, x) {
             var m = this;
             var out = document.getElementById('%s');
             m.on('mousemove', function(e) {
               if (out) { out.innerHTML = 'Lat ' + e.latlng.lat.toFixed(5) + ', Lon ' + e.latlng.lng.toFixed(5); }
             });
           }", ns("coords")))

      visible <- Filter(function(l) isTRUE(l$visible), layers)
      for (lay in visible) m <- gis_map_layer(m, lay, cluster = isolate(input$cluster))

      if (length(visible) && !isolate(fit_done())) {
        bb <- try(gis_bbox_4326(visible[[1]]$sf), silent = TRUE)
        if (!inherits(bb, "try-error")) {
          m <- leaflet::fitBounds(m, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
          fit_done(TRUE)
        }
      }
      if (length(visible)) {
        m <- leaflet::addLayersControl(
          m, overlayGroups = vapply(visible, `[[`, character(1), "name"),
          options = leaflet::layersControlOptions(collapsed = TRUE))
      }
      m
    })

    observeEvent(input$basemap, {
      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearTiles() |>
        leaflet::addProviderTiles(gis_basemaps()[[input$basemap]], group = "basemap")
    }, ignoreInit = TRUE)

    # ---- movement ---------------------------------------------------------
    observeEvent(input$zoom, {
      lay <- isolate(store$layers[[input$zoom_layer]])
      if (is.null(lay)) return()
      bb <- try(gis_bbox_4326(lay$sf), silent = TRUE)
      if (inherits(bb, "try-error")) {
        showNotification("This layer cannot be located because it has no usable CRS.",
                         type = "warning")
        return()
      }
      leaflet::leafletProxy(ns("map")) |>
        leaflet::fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    observeEvent(input$reset, {
      leaflet::leafletProxy(ns("map")) |> leaflet::setView(46.5, 6, 5)
    })

    # ---- drawn area measurement ------------------------------------------
    observeEvent(input$map_draw_new_feature, {
      f <- input$map_draw_new_feature
      if (!identical(f$geometry$type %||% "", "Polygon")) return()
      km2 <- try(gis_drawn_area_km2(f), silent = TRUE)
      if (inherits(km2, "try-error") || is.na(km2)) {
        showNotification("The drawn shape could not be measured.", type = "warning")
        return()
      }
      showNotification(
        sprintf("Drawn polygon area: %.3f km2 (%.1f hectares). Measured geodesically from the drawn outline.",
                km2, km2 * 100),
        type = "message", duration = 10)
    }, ignoreInit = TRUE)

    # ---- active layer style ----------------------------------------------
    output$style_ui <- renderUI({
      store$active
      store$rev
      lay <- isolate(gis_layer(store))
      if (is.null(lay)) return(div(class = "small-note", "Add a layer to edit its style."))
      d <- sf::st_drop_geometry(lay$sf)
      chars <- names(d)[vapply(d, function(v) is.character(v) || is.factor(v), logical(1))]
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        checkboxInput(ns("vis"), "Visible", value = isTRUE(lay$visible)),
        sliderInput(ns("opac"), "Opacity", min = 0, max = 1,
                    value = lay$opacity %||% 1, step = 0.05),
        selectInput(ns("color"), "Colour", choices = gis_palette, selected = lay$color),
        selectInput(ns("labelfield"), "Hover label",
                    choices = c("(none)" = "", stats::setNames(chars, chars)),
                    selected = if (is.na(lay$label)) "" else lay$label)
      )
    })

    observeEvent(input$cluster, structure(structure() + 1L), ignoreInit = TRUE)

    # Restyle one layer in place: clearing and re-adding the group avoids a
    # full map rebuild, so the view, zoom and drawings are preserved.
    restyle <- function(lay) {
      proxy <- leaflet::clearGroup(leaflet::leafletProxy(ns("map")), lay$name)
      if (isTRUE(lay$visible)) gis_map_layer(proxy, lay, cluster = input$cluster)
    }

    observeEvent(input$vis, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      store$layers[[lay$id]]$visible <- isTRUE(input$vis)
      lay$visible <- isTRUE(input$vis)
      restyle(lay)
    }, ignoreInit = TRUE)

    observeEvent(input$opac, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      store$layers[[lay$id]]$opacity <- input$opac
      lay$opacity <- input$opac
      restyle(lay)
    }, ignoreInit = TRUE)

    observeEvent(input$color, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      gis_layer_update(store, lay$id, color = input$color)
    }, ignoreInit = TRUE)

    observeEvent(input$labelfield, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      gis_layer_update(store, lay$id,
                       label = if (nzchar(input$labelfield)) input$labelfield else NA_character_)
    }, ignoreInit = TRUE)

    # ---- linked attribute table ------------------------------------------
    output$table_head <- renderUI({
      lay <- gis_layer(store)
      if (is.null(lay)) return(div(class = "small-note", "No layer selected."))
      div(class = "small-note mb-2",
          sprintf("%s - %s features. Selecting a row highlights the feature; clicking a feature selects its row.",
                  lay$name, gis_fmt_int(nrow(lay$sf))))
    })

    output$attrib <- renderDT({
      lay <- gis_layer(store)
      if (is.null(lay)) return(datatable(data.frame(Message = "No layer selected"), rownames = FALSE))
      datatable(sf::st_drop_geometry(lay$sf), rownames = FALSE, selection = "single",
                options = list(pageLength = 8, scrollX = TRUE, dom = "ftip"))
    })

    observeEvent(input$attrib_rows_selected, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      proxy <- leaflet::clearGroup(leaflet::leafletProxy(ns("map")), "__gis_sel")
      rows <- input$attrib_rows_selected
      if (length(rows)) gis_map_highlight(proxy, lay, rows)
    })

    observeEvent(input$map_shape_click, {
      e <- input$map_shape_click
      if (is.null(e$id)) return()
      i <- suppressWarnings(as.integer(e$id))
      if (is.na(i)) return()
      DT::selectRows(DT::dataTableProxy("attrib"), i)
    })
  })
}
