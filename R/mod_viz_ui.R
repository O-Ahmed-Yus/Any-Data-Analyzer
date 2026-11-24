# Visualisation studio - user interface --------------------------------------
# One page, three steps: pick a chart, pick the variables, read the chart.
# Everything optional lives in the collapsed "Customize chart" accordion.

# ---- miniature chart icons ------------------------------------------------

viz_icon <- function(key) {
  open <- '<svg viewBox="0 0 28 22" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">'
  body <- switch(key,
    bar = '<path d="M6 18V10M12 18V5M18 18V13M24 18V8"/>',
    line = '<path d="M3 16l6-6 5 4 6-8 5 4"/><circle cx="9" cy="10" r="1.1" fill="currentColor"/><circle cx="20" cy="6" r="1.1" fill="currentColor"/>',
    scatter = '<circle cx="7" cy="15" r="1.5"/><circle cx="12" cy="11" r="1.5"/><circle cx="16" cy="13" r="1.5"/><circle cx="20" cy="7" r="1.5"/><circle cx="23" cy="10" r="1.5"/>',
    hist = '<path d="M4 18V11h4v7M8 18V6h4v12M12 18V9h4v9M16 18V13h4v5M20 18V8h4v10"/>',
    box = '<path d="M9 6h10v10H9z"/><path d="M14 3v3M14 16v3M11.5 11h5"/>',
    pie = '<circle cx="14" cy="11" r="7.5"/><path d="M14 11V3.5M14 11l6.5 3.5"/>',
    density = '<path d="M3 17c4 0 4-9 8-9s4 9 8 9 3-3 5-3"/>',
    corr = '<rect x="5" y="3" width="5" height="5" opacity=".9"/><rect x="10" y="3" width="5" height="5" opacity=".45"/><rect x="15" y="3" width="5" height="5" opacity=".2"/><rect x="5" y="8" width="5" height="5" opacity=".45"/><rect x="10" y="8" width="5" height="5" opacity=".9"/><rect x="15" y="8" width="5" height="5" opacity=".45"/><rect x="5" y="13" width="5" height="5" opacity=".2"/><rect x="10" y="13" width="5" height="5" opacity=".45"/><rect x="15" y="13" width="5" height="5" opacity=".9"/>',
    qq = '<path d="M4 18L24 4" stroke-dasharray="2.5 2.5"/><circle cx="8" cy="16" r="1.3"/><circle cx="12" cy="12" r="1.3"/><circle cx="16" cy="10" r="1.3"/><circle cx="20" cy="6" r="1.3"/>',
    error = '<path d="M14 7v8M11 10h6M11 14h6"/><circle cx="14" cy="11" r="1.6" fill="currentColor" stroke="none"/>',
    confmat = '<rect x="5" y="3" width="8" height="7" opacity=".85"/><rect x="13" y="3" width="8" height="7" opacity=".2"/><rect x="5" y="10" width="8" height="7" opacity=".2"/><rect x="13" y="10" width="8" height="7" opacity=".85"/>',
    roc = '<path d="M4 18L9 12c3-4 6-6 15-7"/><path d="M4 18L24 5" stroke-dasharray="2.5 2.5" opacity=".6"/>',
    importance = '<path d="M4 5h16M4 10h11M4 15h7M4 19h4"/>',
    actualpred = '<path d="M4 18L24 4" stroke-dasharray="2.5 2.5"/><circle cx="9" cy="15" r="1.4"/><circle cx="14" cy="10" r="1.4"/><circle cx="19" cy="8" r="1.4"/>',
    residual = '<path d="M4 11h20" stroke-dasharray="2.5 2.5"/><circle cx="8" cy="6" r="1.4"/><circle cx="13" cy="15" r="1.4"/><circle cx="18" cy="7" r="1.4"/><circle cx="22" cy="14" r="1.4"/>',
    '<path d="M4 18h20"/>'
  )
  htmltools::HTML(paste0(open, body, "</svg>"))
}

# ---- chart picker ---------------------------------------------------------

viz_pick_card <- function(key, ch, active = FALSE) {
  htmltools::tags$button(
    type = "button",
    class = paste("viz-pick-card", if (active) "active"),
    `data-value` = key,
    role = "radio",
    `aria-checked` = if (active) "true" else "false",
    title = ch$desc,
    htmltools::span(class = "viz-pick-ic", viz_icon(ch$icon)),
    htmltools::span(class = "viz-pick-name", ch$label)
  )
}

viz_picker <- function(ns) {
  ch <- viz_charts()
  groups <- c("Basic", "Statistical", "ML")
  htmltools::div(
    class = "viz-picker", id = ns("picker"),
    `data-input` = ns("type"), `data-default` = viz_default_type(),
    role = "radiogroup", `aria-label` = "Chart type",
    lapply(groups, function(g) {
      keys <- viz_group_types(g)
      if (!length(keys)) return(NULL)
      htmltools::div(
        class = "viz-pick-row",
        htmltools::span(class = "viz-pick-label",
                        if (identical(g, "ML")) "ML charts" else g),
        htmltools::div(class = "viz-pick-cards",
                       lapply(keys, function(k)
                         viz_pick_card(k, ch[[k]], active = k == viz_default_type())))
      )
    })
  )
}

# ---- field controls -------------------------------------------------------

viz_chart_help <- function(type) {
  switch(type,
    confmat = "True labels and predicted labels, one row per case.",
    roc = "True labels and a numeric score, usually the predicted probability.",
    importance = "A column of feature names and their importance values.",
    actualpred = "Observed values and the model's predicted values.",
    residual = "Observed and predicted values; residual = actual - predicted.",
    corr = "Pick between two and ten numeric variables.",
    line = "A date column is used automatically when one exists.",
    pie = "Categories beyond the eight largest are merged into 'Other'.",
    NULL)
}

viz_field_input <- function(ns, role, f, cand, selected) {
  id <- ns(paste0("f_", role))
  sel <- selected[selected %in% cand]
  if (f$multiple) {
    return(shiny::selectizeInput(
      id, f$label, choices = cand, selected = utils::head(sel, 10),
      multiple = TRUE,
      options = list(plugins = list("remove_button"), maxItems = 10,
                     placeholder = "Choose variables")))
  }
  if (!f$required) {
    sel <- if (length(sel)) sel[[1]] else ""
    return(shiny::selectInput(id, f$label,
                              choices = c(stats::setNames("", f$none %||% "None"),
                                          stats::setNames(cand, cand)),
                              selected = sel))
  }
  shiny::selectizeInput(
    id, f$label, choices = cand,
    selected = if (length(sel)) sel[[1]] else character(0),
    options = list(placeholder = "Choose a column", allowEmptyOption = FALSE))
}

# ---- main UI --------------------------------------------------------------

viz_canvas_output <- function(ns) { if (viz_have_plotly()) plotly::plotlyOutput(ns("plot"), height = "100%") else shiny::plotOutput(ns("plot"), height = "100%") }

viz_studio_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    viz_css(),
    htmltools::tags$script(htmltools::HTML(viz_js())),
    htmltools::div(
      class = "viz-studio",

      htmltools::div(
        class = "viz-head",
        htmltools::div(
          class = "viz-head-text",
          htmltools::tags$h3("Visualise"),
          htmltools::tags$p("Pick a chart, choose the variables, and the chart is ready.")
        ),
        shiny::uiOutput(ns("data_pill"), inline = TRUE)
      ),

      shiny::uiOutput(ns("suggest_ui")),

      htmltools::div(
        class = "viz-step-row",
        htmltools::span(class = "viz-step", "1"),
        htmltools::span(class = "viz-step-text", "Chart type")
      ),
      viz_picker(ns),

      htmltools::div(
        class = "viz-body",
        htmltools::div(
          class = "viz-controls",
          htmltools::div(
            class = "viz-panel",
            htmltools::div(
              class = "viz-panel-head",
              htmltools::span(class = "viz-step", "2"),
              htmltools::span(class = "viz-panel-title", "Variables"),
              shiny::uiOutput(ns("help_ui"), inline = TRUE)
            ),
            shiny::uiOutput(ns("fields_ui"))
          ),
          htmltools::div(
            class = "viz-panel viz-panel-plain",
            bslib::accordion(
              id = ns("customize"), open = FALSE, multiple = TRUE,
              class = "viz-accordion",
              bslib::accordion_panel(
                "Customize chart",
                icon = shiny::icon("sliders"),
                htmltools::div(
                  class = "viz-acc-group",
                  htmltools::span(class = "viz-acc-title", "Appearance"),
                  shiny::textInput(ns("title"), "Title", placeholder = "Automatic title"),
                  shiny::selectInput(ns("theme"), "Colour theme",
                                     choices = viz_theme_choices(),
                                     selected = viz_theme_default),
                  shiny::checkboxInput(ns("legend"), "Show legend", TRUE)
                ),
                htmltools::div(
                  class = "viz-acc-group",
                  htmltools::span(class = "viz-acc-title", "Labels and axes"),
                  shiny::textInput(ns("xlab"), "X axis title", placeholder = "Automatic"),
                  shiny::textInput(ns("ylab"), "Y axis title", placeholder = "Automatic"),
                  shiny::uiOutput(ns("labels_option"))
                ),
                htmltools::div(
                  class = "viz-acc-group",
                  htmltools::span(class = "viz-acc-title", "Chart options"),
                  shiny::uiOutput(ns("options_ui"))
                )
              )
            )
          )
        ),

        htmltools::div(
          class = "viz-canvas", id = ns("canvas_card"),
          htmltools::div(
            class = "viz-canvas-head",
            htmltools::div(
              class = "viz-canvas-title",
              htmltools::span(class = "viz-step", "3"),
              htmltools::span("Preview")
            ),
            htmltools::div(
              class = "viz-toolbar",
              shiny::selectInput(ns("fmt"), NULL,
                                 choices = c(PNG = "png", SVG = "svg", HTML = "html"),
                                 selectize = FALSE, width = "94px"),
              shiny::downloadButton(ns("dl"),
                                    htmltools::tagList(shiny::icon("download"), " Download"),
                                    class = "btn-sm btn-primary viz-dl"),
              shiny::actionButton(ns("full"), shiny::icon("expand"),
                                  class = "btn-sm btn-outline-secondary viz-icon-btn",
                                  title = "Full screen")
            )
          ),
          htmltools::div(class = "viz-plot", htmltools::div(class = "viz-plot-holder", viz_canvas_output(ns)), shiny::uiOutput(ns("plot_state"))),
          shiny::uiOutput(ns("chips_ui")),
          shiny::uiOutput(ns("note_ui"))
        )
      )
    )
  )
}

# ---- JavaScript -----------------------------------------------------------
# Chart cards behave like a radio group without a server round trip; the server
# can select a card (suggestions) through a custom message. Full screen uses the
# browser API, so no extra package is needed.

viz_js <- function() {
  "
  (function () {
    if (window.__vizStudioReady) return;
    window.__vizStudioReady = true;

    function selectCard(picker, value) {
      var cards = picker.querySelectorAll('.viz-pick-card');
      for (var i = 0; i < cards.length; i++) {
        var on = cards[i].getAttribute('data-value') === value;
        cards[i].classList.toggle('active', on);
        cards[i].setAttribute('aria-checked', on ? 'true' : 'false');
      }
      var id = picker.getAttribute('data-input');
      if (id && window.Shiny) Shiny.setInputValue(id, value, {priority: 'event'});
    }

    document.addEventListener('click', function (ev) {
      var card = ev.target.closest ? ev.target.closest('.viz-pick-card') : null;
      if (!card) return;
      var picker = card.closest('.viz-picker');
      if (picker) selectCard(picker, card.getAttribute('data-value'));
    });

    document.addEventListener('keydown', function (ev) {
      if (ev.key !== 'Enter' && ev.key !== ' ') return;
      var card = ev.target.closest ? ev.target.closest('.viz-pick-card') : null;
      if (!card) return;
      ev.preventDefault();
      var picker = card.closest('.viz-picker');
      if (picker) selectCard(picker, card.getAttribute('data-value'));
    });

    function initPickers() {
      var pickers = document.querySelectorAll('.viz-picker');
      for (var i = 0; i < pickers.length; i++) {
        var p = pickers[i];
        if (p.getAttribute('data-init')) continue;
        p.setAttribute('data-init', '1');
        selectCard(p, p.getAttribute('data-default'));
      }
    }

    document.addEventListener('DOMContentLoaded', initPickers);
    document.addEventListener('shiny:connected', initPickers);
    document.addEventListener('shiny:value', initPickers);
    document.addEventListener('shiny:inputchanged', function (e) {
      if (e.name && /-suggest_/.test(e.name)) initPickers();
    });

    if (window.Shiny) Shiny.addCustomMessageHandler('viz-select', function (msg) {
      var picker = document.getElementById(msg.picker);
      if (picker) selectCard(picker, msg.value);
    });

    if (window.Shiny) Shiny.addCustomMessageHandler('viz-fullscreen', function (msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      if (document.fullscreenElement) {
        document.exitFullscreen();
        return;
      }
      if (el.requestFullscreen) el.requestFullscreen();
      else if (el.webkitRequestFullscreen) el.webkitRequestFullscreen();
    });
  })();
  "
}
