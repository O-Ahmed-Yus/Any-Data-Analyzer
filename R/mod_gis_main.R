# GIS workspace container ---------------------------------------------------
# One nav_panel in the main app. Submodules appear here as each phase of the
# GIS build is finished; nothing is listed before it works.

gis_unavailable_ui <- function() {
  bslib::card(
    card_header(tagList(icon("triangle-exclamation"), " GIS code is not available")),
    p(sprintf("This build of the app needs the %s package(s), which are not installed.",
              paste(gis_missing(), collapse = " and "))),
    p(class = "small-note",
      "Install them and restart the app. Every statistical feature outside this tab works without them.")
  )
}

gis_ui <- function(id) {
  ns <- NS(id)
  if (!gis_ready()) return(gis_unavailable_ui())

  navset_pill_list(
    id = ns("gis_tabs"),
    widths = c(2, 10),
    nav_panel(tagList(icon("database"), " Spatial data"),
              gis_data_ui(ns("data"))),
    nav_panel(tagList(icon("map"), " Map explorer"),
              gis_map_ui(ns("map")))
  )
}

gis_server <- function(id, app_data = NULL, data_label = NULL, add_history = NULL) {
  moduleServer(id, function(input, output, session) {
    if (!gis_ready()) return(invisible(NULL))

    store <- gis_new_store()

    gis_data_server("data", store = store, app_data = app_data,
                    data_label = data_label, add_history = add_history)
    gis_map_server("map", store = store)
  })
}
