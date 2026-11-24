# Visualisation studio - server ----------------------------------------------
# The server holds one mapping (chart type plus variables) and rebuilds one
# ggplot object. Controls, summary values, notes and exports are all derived
# from that single build.

# ---- export helpers -------------------------------------------------------

viz_save_svg <- function(p, file, bg) {
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggplot2::ggsave(file, p, device = svglite::svglite, width = 10, height = 6.4,
                    units = "in", bg = bg)
    return(invisible(TRUE))
  }
  grDevices::svg(file, width = 10, height = 6.4, bg = bg)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(TRUE)
}

viz_html_available <- function() {
  viz_have_plotly() && requireNamespace("htmlwidgets", quietly = TRUE) &&
    requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
}

viz_save_html <- function(b, file, th) {
  if (!viz_html_available()) stop("Interactive HTML export is not available here.")
  w <- viz_interactive(b, th)
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  htmlwidgets::saveWidget(w, tmp, selfcontained = TRUE)
  if (!file.copy(tmp, file, overwrite = TRUE)) stop("The HTML file could not be written.")
  invisible(TRUE)
}

# ---- small UI pieces ------------------------------------------------------

viz_chip_ui <- function(chips) {
  if (!length(chips)) return(NULL)
  htmltools::div(
    class = "viz-chips",
    lapply(chips, function(c) {
      htmltools::div(class = "viz-chip",
                     htmltools::span(class = "viz-chip-value", c$value),
                     htmltools::span(class = "viz-chip-label", c$label))
    })
  )
}

viz_notes_ui <- function(notes) {
  notes <- notes[nzchar(notes)]
  if (!length(notes)) return(NULL)
  htmltools::div(
    class = "viz-notes",
    lapply(notes, function(n)
      htmltools::div(class = "viz-note",
                     htmltools::tags$i(class = "fa fa-circle-info"), n))
  )
}

# Message shown instead of a chart: needs more data, or the mapping is invalid.
viz_state_ui <- function(ns, title, text, alternatives = character(),
                         icon_name = "circle-info", overlay = TRUE) {
  ch <- viz_charts()
  alts <- alternatives[alternatives %in% names(ch)]
  htmltools::div(
    class = if (overlay) "viz-plot-state" else "viz-state-inline",
    htmltools::div(
      class = "viz-empty",
      htmltools::div(class = "viz-empty-ic", shiny::icon(icon_name)),
      htmltools::tags$h5(title),
      htmltools::tags$p(text),
      if (length(alts))
        htmltools::div(
          class = "viz-empty-actions",
          lapply(alts, function(k)
            shiny::actionButton(ns(paste0("alt_", k)), ch[[k]]$label,
                                class = "btn-sm btn-outline-primary"))
        )
    )
  )
}

# ---- field values ---------------------------------------------------------

# Validated values for every field of the chart. `get_input` is either the live
# input list or an isolated snapshot when the controls are being rendered.
viz_field_values <- function(p, type, get_input) {
  ch <- viz_chart(type)
  def <- viz_default_fields(p, type)
  out <- lapply(names(ch$fields), function(role) {
    cand <- viz_candidates(p, ch$fields[[role]]$types)
    v <- get_input(paste0("f_", role))
    if (is.null(v) || !length(v) || !all(v %in% cand)) v <- def[[role]]
    if (is.null(v)) v <- character(0)
    as.character(v)
  })
  names(out) <- names(ch$fields)
  out
}

# ---- chart-specific options ----------------------------------------------

viz_option_controls <- function(ns, type, f, current) {
  out <- list()
  add <- function(x) out[[length(out) + 1]] <<- x
  has_y <- !is.null(nz(f$y))
  has_color <- !is.null(nz(f$color))

  if (type == "bar") {
    add(shiny::radioButtons(ns("o_orient"), "Orientation",
                            c("Vertical" = "v", "Horizontal" = "h"),
                            selected = current("o_orient", "v"), inline = TRUE))
    add(shiny::selectInput(ns("o_sort"), "Order categories",
                           c("Largest first" = "desc", "Smallest first" = "asc",
                             "Name (A to Z)" = "name", "Data order" = "none"),
                           selected = current("o_sort", "desc")))
    if (has_y)
      add(shiny::selectInput(ns("o_agg"), "Values are",
                             c("Totals" = "sum", "Means" = "mean", "Medians" = "median"),
                             selected = current("o_agg", "sum")))
    if (has_color)
      add(shiny::selectInput(ns("o_pos"), "Grouped bars",
                             c("Side by side" = "dodge", "Stacked" = "stack"),
                             selected = current("o_pos", "dodge")))
  }
  if (type == "line") {
    add(shiny::selectInput(ns("o_line_agg"), "Repeated x values",
                           c("Average" = "mean", "Add up" = "sum"),
                           selected = current("o_line_agg", "mean")))
    add(shiny::checkboxInput(ns("o_points"), "Show points",
                             isTRUE(current("o_points", TRUE))))
  }
  if (type == "scatter") {
    add(shiny::selectInput(ns("o_trend"), "Trend line",
                           c("None" = "none", "Straight line" = "lm",
                             "Smoothed curve" = "loess"),
                           selected = current("o_trend", "none")))
  }
  if (type == "hist") {
    add(shiny::sliderInput(ns("o_bins"), "Number of bins", 5, 100, 30, 1))
  }
  if (type == "pie") {
    add(shiny::checkboxInput(ns("o_donut"), "Donut (hole in the middle)",
                             isTRUE(current("o_donut", FALSE))))
    if (has_y)
      add(shiny::selectInput(ns("o_agg"), "Values are",
                             c("Totals" = "sum", "Means" = "mean"),
                             selected = current("o_agg", "sum")))
  }
  if (type == "corr") {
    add(shiny::radioButtons(ns("o_method"), "Correlation method",
                            c("Pearson" = "pearson", "Spearman" = "spearman"),
                            selected = current("o_method", "pearson"), inline = TRUE))
  }
  if (type == "errorbar") {
    add(shiny::selectInput(ns("o_err"), "Error bars show",
                           c("95% confidence interval" = "ci", "Standard error" = "se",
                             "Standard deviation" = "sd"),
                           selected = current("o_err", "ci")))
    add(shiny::radioButtons(ns("o_orient"), "Orientation",
                            c("Vertical" = "v", "Horizontal" = "h"),
                            selected = current("o_orient", "v"), inline = TRUE))
  }
  if (type == "confmat") {
    add(shiny::selectInput(ns("o_normalize"), "Show as",
                           c("Counts" = "none", "Percent of each actual class" = "row",
                             "Percent of each predicted class" = "col",
                             "Percent of all rows" = "all"),
                           selected = current("o_normalize", "none")))
  }
  if (length(out)) out else NULL
}

viz_labels_supported <- function(type) {
  type %in% c("bar", "pie", "errorbar", "importance")
}

# ---- server ---------------------------------------------------------------

viz_studio_server <- function(id, data, data_label = NULL, add_history = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    input_val <- function(name, default = NULL) {
      v <- input[[name]]
      if (is.null(v)) default else v
    }

    # ---- state ------------------------------------------------------------
    prof <- shiny::reactive({ viz_profile(data()) })

    type <- shiny::reactive({
      t <- input$type
      if (is.null(t) || !t %in% viz_chart_types()) viz_default_type() else t
    })

    fields <- shiny::reactive({
      viz_field_values(prof(), type(), function(n) input[[n]])
    })

    opt <- shiny::reactive({
      list(orient = input_val("o_orient", "v"),
           sort = input_val("o_sort", "desc"),
           agg = input_val("o_agg", "sum"),
           pos = input_val("o_pos", "dodge"),
           bins = input_val("o_bins", 30),
           donut = isTRUE(input_val("o_donut", FALSE)),
           trend = input_val("o_trend", "none"),
           method = input_val("o_method", "pearson"),
           err = input_val("o_err", "ci"),
           normalize = input_val("o_normalize", "none"),
           line_agg = input_val("o_line_agg", "mean"),
           points = isTRUE(input_val("o_points", TRUE)),
           labels = isTRUE(input_val("o_labels", FALSE)))
    })

    theme_key <- shiny::reactive({ input_val("theme", viz_theme_default) })
    auto_title <- shiny::reactive({ viz_auto_title(type(), fields(), prof()) })
    title_now <- shiny::reactive({
      t <- trimws(input_val("title", ""))
      if (!nzchar(t)) auto_title() else t
    })

    spec <- shiny::reactive({
      p <- prof()
      if (!viz_chart_ready(p, type())) return(NULL)
      list(type = type(), data = p$data, f = fields(), opt = opt(),
           theme = theme_key(), title = title_now(),
           xlab = input_val("xlab", ""), ylab = input_val("ylab", ""),
           legend = isTRUE(input_val("legend", TRUE)))
    })

    spec_d <- shiny::debounce(spec, 120)

    built <- shiny::reactive({
      s <- spec_d()
      if (is.null(s)) return(NULL)
      viz_build_chart(s)
    })

    # ---- header and suggestions ------------------------------------------
    output$data_pill <- shiny::renderUI({
      p <- prof()
      label <- if (is.function(data_label)) data_label() else "Data"
      htmltools::div(
        class = "viz-data-pill",
        htmltools::span(class = "viz-dot"),
        paste0(label, " · ", format(p$n, big.mark = ","), " rows · ",
               p$p, " columns")
      )
    })

    output$suggest_ui <- shiny::renderUI({
      sug <- viz_suggestions(prof())
      if (!length(sug)) return(NULL)
      htmltools::div(
        class = "viz-suggest",
        htmltools::span(class = "viz-suggest-label", "Suggested for your data"),
        lapply(sug, function(s) {
          ch <- viz_chart(s$type)
          shiny::actionButton(
            ns(paste0("suggest_", s$type)),
            htmltools::HTML(paste0("<b>", ch$label, "</b>&nbsp; ",
                                   htmltools::htmlEscape(s$reason))),
            class = "viz-suggest-btn")
        })
      )
    })

    output$help_ui <- shiny::renderUI({
      h <- viz_chart_help(type())
      if (is.null(h)) return(NULL)
      htmltools::span(class = "viz-help", h)
    })

    # ---- variable controls ------------------------------------------------
    output$fields_ui <- shiny::renderUI({
      p <- prof()
      ty <- type()
      ch <- viz_chart(ty)
      if (!viz_chart_ready(p, ty)) {
        r <- viz_chart_requirement(p, ty)
        return(viz_state_ui(ns, r$title, r$text, overlay = FALSE))
      }
      vals <- viz_field_values(p, ty, function(n) shiny::isolate(input[[n]]))
      htmltools::tagList(
        lapply(names(ch$fields), function(role) {
          f <- ch$fields[[role]]
          viz_field_input(ns, role, f, viz_candidates(p, f$types), vals[[role]])
        })
      )
    })

    output$options_ui <- shiny::renderUI({
      ty <- type()
      if (!viz_chart_ready(prof(), ty)) return(NULL)
      f <- shiny::isolate(fields())
      cur <- function(name, default) {
        v <- shiny::isolate(input[[name]])
        if (is.null(v)) default else v
      }
      ctl <- viz_option_controls(ns, ty, f, cur)
      if (is.null(ctl))
        return(htmltools::div(class = "viz-help", "This chart uses automatic settings."))
      htmltools::tagList(ctl)
    })

    output$labels_option <- shiny::renderUI({
      if (!viz_labels_supported(type())) return(NULL)
      shiny::checkboxInput(ns("o_labels"), "Show values on the chart",
                           isTRUE(input_val("o_labels", FALSE)))
    })

    # ---- chart ------------------------------------------------------------
    output$plot_state <- shiny::renderUI({
      p <- prof()
      ty <- type()
      if (!viz_chart_ready(p, ty)) {
        r <- viz_chart_requirement(p, ty)
        return(viz_state_ui(ns, r$title, r$text, r$alternatives))
      }
      b <- built()
      if (is.null(b)) return(NULL)
      if (!is.null(b$error))
        return(viz_state_ui(ns, "Nothing to show yet", b$error,
                            viz_alternatives(p), "triangle-exclamation"))
      NULL
    })

    if (viz_have_plotly()) {
      output$plot <- plotly::renderPlotly({
        b <- built()
        shiny::req(b$plot)
        viz_interactive(b, viz_theme_get(theme_key()))
      })
    } else {
      output$plot <- shiny::renderPlot({
        b <- built()
        shiny::req(b$plot)
        b$plot
      })
    }

    output$chips_ui <- shiny::renderUI({
      b <- built()
      if (is.null(b)) return(NULL)
      viz_chip_ui(b$chips)
    })

    output$note_ui <- shiny::renderUI({
      b <- built()
      if (is.null(b)) return(NULL)
      viz_notes_ui(b$note)
    })

    # ---- interactions -----------------------------------------------------
    shiny::observeEvent(type(), {
      shiny::updateTextInput(session, "title", value = "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$full, {
      session$sendCustomMessage("viz-fullscreen", list(id = ns("canvas_card")))
    }, ignoreInit = TRUE)

    for (k in viz_chart_types()) {
      local({
        key <- k
        shiny::observeEvent(input[[paste0("suggest_", key)]], {
          session$sendCustomMessage("viz-select", list(picker = ns("picker"), value = key))
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        shiny::observeEvent(input[[paste0("alt_", key)]], {
          session$sendCustomMessage("viz-select", list(picker = ns("picker"), value = key))
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }

    # ---- export -----------------------------------------------------------
    fmt <- shiny::reactive({
      f <- input_val("fmt", "png")
      if (!f %in% c("png", "svg", "html")) "png" else f
    })

    if (!viz_html_available()) {
      shiny::updateSelectInput(session, "fmt", choices = c(PNG = "png", SVG = "svg"))
    }

    output$dl <- shiny::downloadHandler(
      filename = function() paste0(viz_slug(title_now()), ".", fmt()),
      content = function(file) {
        b <- built()
        shiny::req(b$plot)
        th <- viz_theme_get(theme_key())
        switch(fmt(),
          png = ggplot2::ggsave(file, b$plot, width = 10, height = 6.4, dpi = 300,
                                units = "in", bg = th$bg),
          svg = viz_save_svg(b$plot, file, th$bg),
          html = viz_save_html(b, file, th)
        )
        if (is.function(add_history)) {
          try(add_history("Visualise", paste0("Chart: ", title_now()),
                          toupper(fmt()), NA_real_, "Chart downloaded"),
              silent = TRUE)
        }
      }
    )
  })
}
