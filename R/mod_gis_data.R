# Spatial Data Manager ------------------------------------------------------
# Imports vector layers (demo, coordinates from the main dataset or an upload,
# and spatial files), keeps them in the session store, and exposes CRS and
# geometry tools plus format exports.

gis_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        card_header(tagList(icon("plus"), " Add a spatial layer")),
        radioButtons(ns("source"), NULL, inline = TRUE,
                     choices = c("Demo data" = "demo",
                                 "Coordinates" = "coords",
                                 "Spatial file" = "file")),
        uiOutput(ns("add_panel"))
      ),
      bslib::card(
        card_header(tagList(icon("layer-group"), " Layers in this session")),
        selectInput(ns("active"), "Active layer", choices = NULL),
        uiOutput(ns("layer_meta")),
        div(class = "d-flex flex-wrap gap-2 mt-3",
            actionButton(ns("rename"), "Rename", icon = icon("pen")),
            actionButton(ns("remove"), "Remove", icon = icon("trash")),
            actionButton(ns("clear"), "Clear all", icon = icon("broom")))
      )
    ),

    bslib::card(
      card_header(tagList(icon("table"), " Attribute table")),
      div(class = "small-note mb-2",
          "The first 100 features of the active layer. Use the Map Explorer tab for a table linked to the map."),
      DTOutput(ns("preview"))
    ),

    bslib::card(
      card_header(tagList(icon("sliders"), " Geometry, CRS and export")),
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          h6(tags$b("Coordinate reference system")),
          p(class = "small-note", "Reproject the active layer. Planar area, distance and buffer work needs a projected CRS."),
          numericInput(ns("epsg"), "Target EPSG code", value = NA, min = 2000, max = 99999),
          actionButton(ns("transform"), "Reproject layer", icon = icon("globe"))
        ),
        div(
          h6(tags$b("Geometry repair")),
          p(class = "small-note", "Fix invalid geometry and remove duplicated geometries. The geometry column is rebuilt; attributes are kept."),
          actionButton(ns("repair"), "Repair invalid geometry", icon = icon("wrench")),
          actionButton(ns("dedupe"), "Remove duplicate geometry", icon = icon("clone"))
        ),
        div(
          h6(tags$b("Export the active layer")),
          p(class = "small-note", "GeoJSON and GeoPackage keep geometry and attributes; CSV keeps the attributes and coordinates."),
          downloadButton(ns("dl_geojson"), "GeoJSON"),
          downloadButton(ns("dl_gpkg"), "GeoPackage"),
          downloadButton(ns("dl_csv"), "CSV")
        )
      )
    )
  )
}

gis_data_server <- function(id, store, app_data = NULL, data_label = NULL,
                            add_history = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pending_df <- reactiveVal(NULL)

    # ---- add-layer panels -------------------------------------------------
    output$add_panel <- renderUI({
      switch(input$source %||% "demo",
        demo = tagList(
          checkboxGroupInput(ns("demo_layers"), "Layers to load",
                             choices = gis_demo_choices(),
                             selected = unname(gis_demo_choices())),
          div(class = "small-note mb-2",
              "City coordinates are approximate real locations. Every attribute, ",
              "the zone polygons and the case reports are synthetic."),
          actionButton(ns("add_demo"), "Load selected layers",
                       icon = icon("download"), class = "btn-primary")
        ),
        coords = tagList(
          actionButton(ns("use_app"), "Use the current dataset",
                       icon = icon("table")),
          fileInput(ns("coord_file"), "Or upload CSV / Excel with coordinates",
                    accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
          uiOutput(ns("coord_form"))
        ),
        file = tagList(
          fileInput(ns("vfile"), "Spatial file",
                    accept = c(".geojson", ".json", ".gpkg", ".kml", ".zip", ".shp")),
          div(class = "small-note mb-2",
              "GeoJSON, GeoPackage, KML or a zipped shapefile. Zip the .shp, ",
              ".shx, .dbf and .prj together; a loose .shp alone will not load."),
          actionButton(ns("add_file"), "Add layer", icon = icon("plus"),
                       class = "btn-primary")
        )
      )
    })

    # ---- coordinate flow --------------------------------------------------
    observeEvent(input$use_app, {
      pending_df(NULL)
      if (is.null(app_data())) {
        showNotification("The main app has no dataset loaded. Upload a file below or pick another source.",
                         type = "warning", duration = 8)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$coord_file, {
      f <- input$coord_file
      req(f)
      d <- try(gis_read_tabular(f$datapath), silent = TRUE)
      if (inherits(d, "try-error") || is.null(d) || !ncol(d)) {
        showNotification("That file could not be read as a table with a header row.",
                         type = "error", duration = 8)
        return()
      }
      pending_df(list(data = d, label = f$name))
    }, ignoreInit = TRUE)

    coord_data <- reactive({
      if (!is.null(pending_df())) return(pending_df())
      d <- app_data()
      if (is.null(d)) return(NULL)
      list(data = d, label = data_label() %||% "current dataset")
    })

    output$coord_form <- renderUI({
      cd <- coord_data()
      if (is.null(cd)) {
        return(div(class = "small-note",
                   "No table available yet. Load data in the main app, or upload a file above."))
      }
      nums <- gis_numeric_columns(cd$data)
      if (length(nums) < 2) {
        return(div(class = "alert alert-warning mb-0",
                   "This table has fewer than two numeric columns, so it cannot hold coordinates."))
      }
      det <- gis_detect_coords(names(cd$data))
      lon_choice <- if (!is.na(det$lon)) det$lon else nums[1]
      lat_choice <- if (!is.na(det$lat)) det$lat else nums[min(2, length(nums))]
      tagList(
        p(class = "small-note", tags$b(cd$label), sprintf(" (%d rows)", nrow(cd$data))),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("coord_lon"), "Longitude / X column", choices = nums, selected = lon_choice),
          selectInput(ns("coord_lat"), "Latitude / Y column", choices = nums, selected = lat_choice)
        ),
        numericInput(ns("coord_epsg"), "EPSG code of these coordinates",
                     value = if (identical(det$confidence, "named")) 4326 else NA,
                     min = 2000, max = 99999),
        if (identical(det$confidence, "xy")) {
          div(class = "alert alert-warning py-2 mb-2",
              "Column names look like x/y, which may be projected metres rather than degrees. ",
              "Enter the EPSG code that the file was recorded in; it is not assumed to be WGS 84.")
        } else if (identical(det$confidence, "named")) {
          div(class = "small-note mb-2",
              "Named latitude/longitude columns default to WGS 84 (EPSG:4326). Change the code if the source used another datum.")
        },
        textInput(ns("coord_name"), "Layer name", value = tools::file_path_sans_ext(cd$label)),
        tableOutput(ns("coord_check")),
        actionButton(ns("add_coords"), "Create point layer", icon = icon("location-dot"),
                     class = "btn-primary")
      )
    })

    output$coord_check <- renderTable({
      cd <- coord_data()
      req(cd, input$coord_lon, input$coord_lat)
      iss <- gis_coord_issues(cd$data, input$coord_lon, input$coord_lat)
      if (is.null(iss)) return(NULL)
      iss
    }, striped = TRUE, spacing = "xs", width = "100%")

    observeEvent(input$add_coords, {
      cd <- coord_data()
      req(cd, input$coord_lon, input$coord_lat)
      epsg <- suppressWarnings(as.integer(input$coord_epsg))
      if (is.na(epsg) || is.na(sf::st_crs(epsg))) {
        showNotification("Enter a valid EPSG code for the coordinates before creating the layer.",
                         type = "error", duration = 8)
        return()
      }
      res <- try(gis_points_from_coords(cd$data, input$coord_lon, input$coord_lat, epsg),
                 silent = TRUE)
      if (inherits(res, "try-error")) {
        showNotification(conditionMessage(attr(res, "condition")), type = "error", duration = 10)
        return()
      }
      nm <- trimws(input$coord_name %||% "")
      if (!nzchar(nm)) nm <- "Point layer"
      id <- gis_layer_add(store, res$sf, name = nm, source = cd$label)
      msg <- sprintf("%s features added.", gis_fmt_int(res$used))
      if (res$dropped > 0) {
        msg <- paste0(msg, sprintf(" %d row(s) dropped: %d without a usable coordinate pair, %d outside the valid range for the selected CRS.",
                                   res$dropped, res$dropped - res$out_of_range, res$out_of_range))
      }
      showNotification(msg, type = "message", duration = 10)
      if (!is.null(add_history)) {
        add_history("GIS", sprintf("Point layer from coordinates: %s", nm),
                    sprintf("%d features, %s", res$used, gis_crs_text(res$sf)),
                    NA_real_, "Layer created")
      }
    })

    # ---- demo and file import ---------------------------------------------
    observeEvent(input$add_demo, {
      chosen <- input$demo_layers %||% character(0)
      if (!length(chosen)) {
        showNotification("Select at least one demo layer.", type = "warning")
        return()
      }
      demo <- gis_demo()
      if (is.null(demo)) {
        showNotification("The demo layers need the sf package.", type = "error")
        return()
      }
      for (k in chosen) {
        nm <- c(cities = "Demo cities", zones = "Demo zones (synthetic)",
                facilities = "Demo health facilities (synthetic)",
                reports = "Demo case reports (synthetic)")[[k]]
        gis_layer_add(store, demo[[k]], name = nm, source = "Demo data",
                      synthetic = k %in% c("zones", "facilities", "reports"))
      }
      showNotification(sprintf("%d demo layer(s) loaded.", length(chosen)), type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$add_file, {
      f <- input$vfile
      req(f)
      x <- try(gis_read_vector(f$datapath), silent = TRUE)
      if (inherits(x, "try-error")) {
        showNotification(conditionMessage(attr(x, "condition")), type = "error", duration = 10)
        return()
      }
      nm <- tools::file_path_sans_ext(f$name)
      gis_layer_add(store, x, name = nm, source = f$name)
      showNotification(sprintf("%s layer added: %s features.",
                               gis_type_word(x), gis_fmt_int(nrow(x))),
                       type = "message")
    }, ignoreInit = TRUE)

    # ---- active layer -----------------------------------------------------
    last_choices <- reactiveVal(NULL)
    observe({
      ch <- gis_layer_choices(store)
      if (identical(ch, last_choices())) return()
      last_choices(ch)
      sel <- if (!is.null(store$active) && store$active %in% unname(ch)) store$active else NULL
      updateSelectInput(session, "active", choices = ch, selected = sel)
    })

    observeEvent(input$active, {
      if (!is.null(input$active) && nzchar(input$active)) store$active <- input$active
    }, ignoreInit = TRUE)

    output$layer_meta <- renderUI({
      lay <- gis_layer(store)
      if (is.null(lay)) {
        return(div(class = "small-note",
                   "No layers yet. Load a demo layer, turn coordinates into points, or upload a spatial file."))
      }
      s <- gis_geom_summary(lay$sf)
      tagList(
        p(tags$b("Features: "), gis_fmt_int(s$n), tags$span(class = "badge bg-secondary ms-2", gis_type_word(lay$sf))),
        p(tags$b("Geometry: "), s$geom),
        p(tags$b("CRS: "), s$crs),
        p(tags$b("Extent: "), s$extent),
        p(tags$b("Source: "), lay$source,
          if (isTRUE(lay$synthetic)) tags$span(class = "badge bg-warning text-dark ms-2", "synthetic")),
        if (!is.na(s$invalid) && s$invalid > 0) {
          div(class = "alert alert-warning py-1 px-2 mb-1",
              sprintf("%d feature(s) have invalid geometry. Use 'Repair invalid geometry'.", s$invalid))
        },
        if (s$empty > 0) {
          div(class = "alert alert-warning py-1 px-2 mb-1",
              sprintf("%d empty geometry(ies).", s$empty))
        },
        if (s$duplicates > 0) {
          div(class = "alert alert-info py-1 px-2 mb-0",
              sprintf("%d duplicated geometry(ies) detected.", s$duplicates))
        }
      )
    })

    output$preview <- renderDT({
      lay <- gis_layer(store)
      if (is.null(lay)) return(datatable(data.frame(Message = "No layer selected"), rownames = FALSE))
      d <- sf::st_drop_geometry(lay$sf)
      d <- utils::head(d, 100)
      datatable(d, rownames = FALSE,
                options = list(pageLength = 5, scrollX = TRUE, dom = "tip"))
    })

    # ---- layer management -------------------------------------------------
    observeEvent(input$rename, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      showModal(modalDialog(
        title = "Rename layer",
        textInput(ns("new_name"), "Layer name", value = lay$name),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("rename_ok"), "Save", class = "btn-primary"))
      ))
    })

    observeEvent(input$rename_ok, {
      lay <- gis_layer(store)
      nm <- trimws(input$new_name %||% "")
      if (!is.null(lay) && nzchar(nm)) gis_layer_update(store, lay$id, name = nm)
      removeModal()
    })

    observeEvent(input$remove, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      gis_layer_remove(store, lay$id)
      showNotification(sprintf("Layer '%s' removed from this session.", lay$name), type = "message")
    })

    observeEvent(input$clear, {
      if (!length(store$layers)) return()
      showModal(modalDialog(
        title = "Clear all layers?",
        "Layers live only in this browser session and are not saved anywhere.",
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("clear_ok"), "Remove all", class = "btn-danger"))
      ))
    })

    observeEvent(input$clear_ok, {
      gis_store_clear(store)
      removeModal()
      showNotification("All spatial layers cleared.", type = "message")
    })

    # ---- CRS and geometry tools -------------------------------------------
    observeEvent(input$transform, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      epsg <- suppressWarnings(as.integer(input$epsg))
      if (is.na(epsg) || is.na(sf::st_crs(epsg))) {
        showNotification("Enter a valid EPSG code.", type = "error")
        return()
      }
      if (!gis_has_crs(lay$sf)) {
        showNotification("This layer has no CRS, so it cannot be reprojected. Recreate it from coordinates with an explicit EPSG code.",
                         type = "error", duration = 10)
        return()
      }
      old <- gis_crs_text(lay$sf)
      out <- try(sf::st_transform(lay$sf, epsg), silent = TRUE)
      if (inherits(out, "try-error")) {
        showNotification("The reprojection failed: the target code may be incompatible with this geometry.",
                         type = "error", duration = 10)
        return()
      }
      gis_layer_update(store, lay$id, sf = out)
      if (is.function(add_history)) {
        add_history("GIS", sprintf("Reproject layer: %s", lay$name),
                    sprintf("%s to %s", old, gis_crs_text(out)), NA_real_, "CRS changed")
      }
      showNotification(sprintf("Reprojected from %s to %s.", old, gis_crs_text(out)),
                       type = "message", duration = 8)
    })

    observeEvent(input$repair, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      before <- sum(!sf::st_is_valid(lay$sf), na.rm = TRUE)
      out <- try(sf::st_make_valid(lay$sf), silent = TRUE)
      if (inherits(out, "try-error")) {
        showNotification("Geometry repair failed for this layer.", type = "error")
        return()
      }
      after <- sum(!sf::st_is_valid(out), na.rm = TRUE)
      gis_layer_update(store, lay$id, sf = out)
      showNotification(sprintf("Geometry rebuilt: %d invalid feature(s) before, %d after.", before, after),
                       type = "message", duration = 8)
    })

    observeEvent(input$dedupe, {
      lay <- gis_layer(store)
      if (is.null(lay)) return()
      keep <- !duplicated(sf::st_geometry(lay$sf))
      removed <- sum(!keep)
      if (!removed) {
        showNotification("No duplicated geometries found.", type = "message")
        return()
      }
      gis_layer_update(store, lay$id, sf = lay$sf[keep, ])
      showNotification(sprintf("%d duplicated feature(s) removed.", removed), type = "message")
    })

    # ---- exports ----------------------------------------------------------
    output$dl_geojson <- downloadHandler(
      filename = function() paste0(gis_safe_name(gis_layer(store)$name %||% "layer"), ".geojson"),
      content = function(file) {
        lay <- gis_layer(store)
        req(lay)
        sf::st_write(sf::st_transform(lay$sf, 4326), file, driver = "GeoJSON", quiet = TRUE)
      }
    )

    output$dl_gpkg <- downloadHandler(
      filename = function() paste0(gis_safe_name(gis_layer(store)$name %||% "layer"), ".gpkg"),
      content = function(file) {
        lay <- gis_layer(store)
        req(lay)
        sf::st_write(lay$sf, file, driver = "GPKG",
                     layer = gis_safe_name(lay$name), quiet = TRUE)
      }
    )

    output$dl_csv <- downloadHandler(
      filename = function() paste0(gis_safe_name(gis_layer(store)$name %||% "layer"), ".csv"),
      content = function(file) {
        lay <- gis_layer(store)
        req(lay)
        utils::write.csv(gis_csv_frame(lay$sf), file, row.names = FALSE, na = "")
      }
    )
  })
}
