# =========================================================================
# Visitor tracking - server
# =========================================================================
# track_session()      one anonymous tracker per Shiny session
# track_admin_server() the administrator dashboard and its access gate

track_chr <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(x)) NA_character_ else x
}

track_session <- function(input, session, store = track_store()) {
  session_id <- tryCatch(session$token, error = function(e) NULL)
  session_id <- track_chr(session_id)
  if (is.na(session_id)) {
    session_id <- paste0("s", format(Sys.time(), "%Y%m%d%H%M%S"),
                         sample(1000:9999, 1))
  }

  started <- Sys.time()
  last_seen <- started
  beats <- 0L

  state <- new.env(parent = emptyenv())
  state$started <- FALSE
  state$client_ready <- FALSE
  state$visitor_id <- NA_character_
  state$new_visitor <- NA
  state$client <- list(device = NA_character_, browser = NA_character_,
                       os = NA_character_, screen = NA_character_)
  state$country <- track_country(session)

  # Writes are unconditional; the client always sends its details.
  record <- function(event, module = NA_character_, analysis = NA_character_,
                     dataset = NA_character_, page = NA_character_,
                     duration_sec = NA_real_) {
    track_record(store, track_row(
      event = event, session_id = session_id,
      visitor_id = state$visitor_id, new_visitor = state$new_visitor,
      module = module, analysis = analysis, dataset = dataset, page = page,
      device = state$client$device, browser = state$client$browser,
      os = state$client$os, screen = state$client$screen,
      country = state$country, duration_sec = duration_sec))
  }

  current_page <- function() {
    v <- tryCatch(isolate(input$tabs), error = function(e) NULL)
    track_chr(v)
  }

  maybe_start <- function() {
    if (state$started || !isTRUE(state$client_ready)) return(invisible())
    state$started <- TRUE
    record("session_start", page = current_page())
  }

  observeEvent(input$trk_client, {
    cl <- input$trk_client
    if (!is.list(cl)) return()
    state$client <- list(
      device  = track_chr(cl$device),
      browser = track_chr(cl$browser),
      os      = track_chr(cl$os),
      screen  = track_chr(cl$screen)
    )
    vid <- track_chr(cl$visitor_id)
    if (!is.na(vid)) state$visitor_id <- substr(vid, 1, 64)
    nv <- cl$new_visitor
    if (!is.null(nv) && length(nv)) state$new_visitor <- isTRUE(as.logical(nv)[1])
    state$client_ready <- TRUE
    maybe_start()
  })

  # The client sends a beat each minute while the tab is visible. A row is
  # written every five minutes so duration survives a browser that is closed
  # without a session_end.
  observeEvent(input$trk_beat, {
    last_seen <<- Sys.time()
    beats <<- beats + 1L
    if (beats %% 5L == 0L) {
      record("heartbeat",
             duration_sec = as.numeric(difftime(Sys.time(), started, units = "secs")))
    }
  })

  observeEvent(input$tabs, {
    if (!state$started) return()
    record("page_view", page = track_chr(input$tabs))
  })

  session$onSessionEnded(function() {
    if (!isTRUE(state$started)) return(invisible())
    try(record("session_end",
               duration_sec = as.numeric(difftime(last_seen, started, units = "secs"))),
        silent = TRUE)
  })

  list(
    session_id = session_id,
    store = store,
    # Called from add_history(), i.e. only when a result was produced.
    record_analysis = function(module, analysis, dataset = NA_character_) {
      if (!isTRUE(state$started)) return(invisible(FALSE))
      record("analysis", module = track_chr(module),
             analysis = track_chr(analysis), dataset = track_chr(dataset))
    },
    record_dataset = function(name, source = "upload") {
      if (!isTRUE(state$started)) return(invisible(FALSE))
      record("dataset", dataset = track_chr(name), module = track_chr(source))
    }
  )
}

# ---- activity table ------------------------------------------------------
# Pure helpers, so the formatting can be checked without a browser.
track_session_seconds <- function(ev) {
  if (!nrow(ev)) return(numeric(0))
  when <- as.POSIXct(ev$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  spl <- split(seq_len(nrow(ev)), ev$session_id)
  vapply(spl, function(i) {
    t <- when[i]
    t <- t[!is.na(t)]
    if (length(t) < 2) return(0)
    as.numeric(difftime(max(t), min(t), units = "secs"))
  }, numeric(1))
}

track_format_duration <- function(s) {
  s <- suppressWarnings(as.numeric(s))
  ifelse(is.na(s), "",
    ifelse(s >= 3600, sprintf("%.1f h", s / 3600),
      ifelse(s >= 60, sprintf("%d min", round(s / 60)),
             sprintf("%d s", round(s)))))
}

track_activity_frame <- function(ev, tz = "UTC") {
  if (!nrow(ev)) {
    return(data.frame(Note = "No records for this period."))
  }
  when <- as.POSIXct(ev$ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  label <- c(session_start = "Session start", session_end = "Session end",
             heartbeat = "Active", page_view = "Section",
             analysis = "Analysis", dataset = "Dataset")
  ev_label <- unname(label[ev$event])
  ev_label[is.na(ev_label)] <- ev$event[is.na(ev_label)]

  detail <- rep("", nrow(ev))
  detail[ev$event == "analysis"] <- ev$analysis[ev$event == "analysis"]
  detail[ev$event == "dataset"] <-
    ifelse(is.na(ev$dataset[ev$event == "dataset"]), "",
           paste0(ev$dataset[ev$event == "dataset"], " (",
                  ev$module[ev$event == "dataset"], ")"))
  detail[ev$event == "page_view"] <- ev$page[ev$event == "page_view"]
  # Mark first-time browsers on the session_start row.
  detail[ev$event == "session_start"] <-
    ifelse(ev$new_visitor[ev$event == "session_start"] %in% TRUE,
           "New visitor", "Returning visitor")

  secs <- track_session_seconds(ev)
  dur <- unname(secs[ev$session_id])
  dur[ev$event != "session_end"] <- NA

  short <- function(x, n = 8) ifelse(is.na(x), "", substr(as.character(x), 1, n))
  data.frame(
    Time = format(when, "%Y-%m-%d %H:%M", tz = tz),
    Event = ev_label,
    Detail = detail,
    Visitor = short(ev$visitor_id),
    Session = short(ev$session_id),
    Duration = track_format_duration(dur),
    Device = ifelse(is.na(ev$device), "Unknown", ev$device),
    Browser = ifelse(is.na(ev$browser), "Unknown", ev$browser),
    OS = ifelse(is.na(ev$os), "Unknown", ev$os),
    Screen = ifelse(is.na(ev$screen), "-", ev$screen),
    Country = ifelse(is.na(ev$country), "-", ev$country),
    check.names = FALSE
  )
}

# ---- administrator dashboard ---------------------------------------------
track_admin_server <- function(input, output, session, tracker) {
  cfg <- track_config()
  store <- tracker$store
  admin_ok <- reactiveVal(FALSE)
  attempts <- reactiveVal(0L)
  blocked_until <- reactiveVal(as.POSIXct(NA))

  ask_key <- function() {
    showModal(modalDialog(
      title = tagList(icon("user-shield"), " Administrator access"),
      p("The visitor dashboard is restricted to the app owner."),
      passwordInput("trk_admin_key", NULL, placeholder = "Passphrase"),
      div(class = "trk-note",
          "Set the passphrase with the TRACKING_ADMIN_TOKEN environment
           variable on the server."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("trk_admin_go", "Unlock",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  }

  observeEvent(input$trk_admin_open, {
    if (isTRUE(admin_ok())) nav_select("tabs", "admin") else ask_key()
  }, ignoreInit = TRUE)

  observeEvent(input$trk_admin_go, {
    until <- blocked_until()
    if (!is.na(until) && Sys.time() < until) {
      showNotification("Too many attempts. Try again in a few minutes.",
                       type = "error")
      return()
    }
    token <- cfg$admin_token
    if (!nzchar(token)) {
      showNotification(
        "No administrator passphrase is configured on the server
         (TRACKING_ADMIN_TOKEN).", type = "error", duration = 10)
      return()
    }
    if (identical(track_chr(input$trk_admin_key), token)) {
      admin_ok(TRUE)
      attempts(0L)
      removeModal()
      nav_select("tabs", "admin")
    } else {
      attempts(attempts() + 1L)
      if (attempts() >= 5L) {
        blocked_until(Sys.time() + 300)
        attempts(0L)
      }
      showNotification("Incorrect passphrase.", type = "error")
    }
  }, ignoreInit = TRUE)

  # ?admin=1 opens the passphrase prompt without advertising the dashboard.
  observeEvent(session$clientData$url_search, {
    q <- session$clientData$url_search
    if (is.null(q) || !nzchar(q) || isTRUE(admin_ok())) return()
    params <- tryCatch(shiny::parseQueryString(sub("^\\?", "", q)),
                       error = function(e) list())
    if (!is.null(params$admin)) ask_key()
  }, ignoreInit = TRUE)

  observeEvent(input$trk_lock, {
    admin_ok(FALSE)
    nav_select("tabs", "Dashboard")
  }, ignoreInit = TRUE)

  # ---- data ---------------------------------------------------------------
  events <- reactive({
    req(isTRUE(admin_ok()))
    input$trk_refresh
    ev <- track_read(store)
    if (nrow(ev)) {
      ev$date <- as.Date(substr(ev$ts, 1, 10))
    } else {
      ev$date <- as.Date(character(0))
    }
    ev
  })

  filtered <- reactive({
    ev <- events()
    if (!nrow(ev)) return(ev)
    days <- suppressWarnings(as.integer(input$trk_days %||% 30))
    if (!is.na(days) && days > 0) {
      ev <- ev[!is.na(ev$date) & ev$date >= Sys.Date() - (days - 1), , drop = FALSE]
    }
    ev
  })

  # ---- key figures --------------------------------------------------------
  output$trk_kpis <- renderUI({
    ev <- filtered()
    starts <- ev[ev$event == "session_start", , drop = FALSE]
    sessions <- length(unique(stats::na.omit(starts$session_id)))
    visitors <- length(unique(stats::na.omit(starts$visitor_id)))
    analyses <- sum(ev$event == "analysis", na.rm = TRUE)
    datasets <- sum(ev$event == "dataset", na.rm = TRUE)
    sections <- length(unique(stats::na.omit(ev$page[ev$event == "page_view"])))

    div(class = "kpi-grid",
      kpi_card("Visits (sessions)", format(sessions, big.mark = ","),
               "Sessions in this period", "fa-users", ""),
      kpi_card("Unique visitors", format(visitors, big.mark = ","),
               "Browsers that kept a random ID", "fa-fingerprint", "kpi-teal"),
      kpi_card("Analyses run", format(analyses, big.mark = ","),
               "Tests, models and charts", "fa-flask", "kpi-violet"),
      kpi_card("Datasets used", format(datasets, big.mark = ","),
               "File names only", "fa-file-arrow-up", "kpi-amber"),
      kpi_card("Sections used", format(sections, big.mark = ","),
               "Distinct tabs opened", "fa-compass", "kpi-green")
    )
  })

  output$trk_period <- renderUI({
    ev <- events()
    starts <- ev[ev$event == "session_start", , drop = FALSE]
    d <- starts$date
    tagList(
      div("Today ", tags$b(sum(d == Sys.Date(), na.rm = TRUE))),
      div("Last 7 days ", tags$b(sum(d >= Sys.Date() - 6, na.rm = TRUE))),
      div("Last 30 days ", tags$b(sum(d >= Sys.Date() - 29, na.rm = TRUE))),
      div("Analyses (all time) ",
          tags$b(sum(ev$event == "analysis", na.rm = TRUE))),
      div("Datasets (all time) ",
          tags$b(sum(ev$event == "dataset", na.rm = TRUE)))
    )
  })

  output$trk_status <- renderText({
    req(isTRUE(admin_ok()))
    msg <- store$message
    if (!isTRUE(store$persistent)) {
      msg <- paste(msg, "Records are lost when the server restarts;",
                   "configure a persistent backend before deploying.")
    }
    msg
  })

  output$trk_store_note <- renderText({
    req(isTRUE(admin_ok()))
    sprintf("Storage: %s. Times are shown in %s.", store$label, cfg$tz)
  })

  # ---- charts -------------------------------------------------------------
  empty_plot <- function(msg) {
    ggplot() +
      annotate("text", x = 0, y = 0, label = msg, colour = "#94A3B8",
               size = 4) +
      theme_void()
  }

  daily_counts <- function(dates, days) {
    as.integer(table(factor(dates, levels = as.character(days))))
  }

  output$trk_trend <- renderPlot({
    ev <- filtered()
    ev <- ev[!is.na(ev$date), , drop = FALSE]
    if (!nrow(ev)) return(empty_plot("No activity recorded in this period."))
    days <- seq(min(ev$date), max(ev$date), by = "day")
    starts <- unique(ev[ev$event == "session_start", c("date", "session_id")])
    long <- rbind(
      data.frame(date = days, series = "Visits",
                 n = daily_counts(starts$date, days)),
      data.frame(date = days, series = "Analyses",
                 n = daily_counts(ev$date[ev$event == "analysis"], days))
    )
    p <- ggplot(long, aes(date, n, colour = series)) +
      geom_point(size = 1.8) +
      scale_colour_manual(values = c(Visits = "#2563EB",
                                     Analyses = "#14B8A6")) +
      scale_x_date(date_labels = "%d %b") +
      labs(x = NULL, y = NULL, colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            panel.grid.minor = element_blank())
    if (length(days) > 1) p <- p + geom_line(linewidth = 0.9)
    p
  })

  output$trk_devices <- renderPlot({
    ev <- filtered()
    starts <- unique(ev[ev$event == "session_start",
                        c("session_id", "device")])
    if (!nrow(starts)) return(empty_plot("No device data yet."))
    starts$device[is.na(starts$device) | !nzchar(starts$device)] <- "Unknown"
    d <- as.data.frame(table(Device = starts$device), stringsAsFactors = FALSE)
    d <- d[d$Freq > 0, , drop = FALSE]
    ggplot(d, aes(y = reorder(Device, Freq), x = Freq)) +
      geom_col(fill = "#8B5CF6", width = 0.6) +
      geom_text(aes(label = Freq), hjust = -0.25, size = 3.4,
                colour = theme_col("fg", 0.7)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())
  })

  output$trk_methods <- renderPlot({
    ev <- filtered()
    a <- ev[ev$event == "analysis" & !is.na(ev$analysis), , drop = FALSE]
    if (!nrow(a)) return(empty_plot("No analyses in this period."))
    tab <- utils::head(sort(table(a$analysis), decreasing = TRUE), 8)
    d <- data.frame(method = names(tab), n = as.integer(tab),
                    stringsAsFactors = FALSE)
    ggplot(d, aes(y = reorder(method, n), x = n)) +
      geom_col(fill = "#2563EB", width = 0.62) +
      scale_y_discrete(labels = function(x) ifelse(nchar(x) > 34,
                                                   paste0(substr(x, 1, 32), "..."),
                                                   x)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })

  output$trk_sections <- renderPlot({
    ev <- filtered()
    p <- ev[ev$event == "page_view" & !is.na(ev$page), , drop = FALSE]
    if (!nrow(p)) return(empty_plot("No section visits recorded."))
    tab <- utils::head(sort(table(p$page), decreasing = TRUE), 8)
    d <- data.frame(page = names(tab), n = as.integer(tab),
                    stringsAsFactors = FALSE)
    ggplot(d, aes(y = reorder(page, n), x = n)) +
      geom_col(fill = "#14B8A6", width = 0.62) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })

  output$trk_activity <- renderDT({
    d <- track_activity_frame(filtered(), cfg$tz)
    datatable(d, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             order = list(list(0, "desc"))),
              class = "compact stripe hover")
  })

  # ---- exports ------------------------------------------------------------
  output$trk_dl_events <- downloadHandler(
    filename = function() paste0("mr_somali_tracking_events_", Sys.Date(), ".csv"),
    content = function(file) {
      ev <- filtered()
      ev <- ev[, setdiff(names(ev), "date"), drop = FALSE]
      utils::write.csv(ev, file, row.names = FALSE, na = "")
    }
  )

  output$trk_dl_summary <- downloadHandler(
    filename = function() paste0("mr_somali_tracking_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      ev <- filtered()
      starts <- ev[ev$event == "session_start", , drop = FALSE]
      counts <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) return(data.frame(item = character(0), n = integer(0)))
        t <- sort(table(x), decreasing = TRUE)
        data.frame(item = names(t), n = as.integer(t), stringsAsFactors = FALSE)
      }
      block <- function(section, df, first = NULL) {
        if (!is.null(first)) {
          df <- rbind(data.frame(item = names(first), n = as.integer(first),
                                 stringsAsFactors = FALSE), df)
        }
        if (!nrow(df)) return(NULL)
        data.frame(section = section, item = df$item, count = df$n,
                   stringsAsFactors = FALSE)
      }
      parts <- list(
        block("Totals", counts(NULL),
              c(Sessions = length(unique(stats::na.omit(starts$session_id))),
                `Unique visitors` = length(unique(stats::na.omit(starts$visitor_id))),
                Analyses = sum(ev$event == "analysis", na.rm = TRUE),
                Datasets = sum(ev$event == "dataset", na.rm = TRUE))),
        block("Method", counts(ev$analysis[ev$event == "analysis"])),
        block("Device", counts(ev$device[ev$event == "session_start"])),
        block("Browser", counts(ev$browser[ev$event == "session_start"])),
        block("Section", counts(ev$page[ev$event == "page_view"]))
      )
      parts <- Filter(Negate(is.null), parts)
      out <- if (length(parts)) do.call(rbind, parts) else
        data.frame(section = character(0), item = character(0), count = integer(0))
      utils::write.csv(out, file, row.names = FALSE)
    }
  )
}

