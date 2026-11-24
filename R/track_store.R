# =========================================================================
# Visitor tracking - storage layer
# =========================================================================
# Append-only event log for anonymous usage statistics. One flat table keeps
# both backends simple and makes the CSV export a straight copy of the data.
#
# Backends
#   local    a CSV file. Fine for development; the shinyapps.io filesystem is
#            wiped whenever the instance restarts, so do not use it in
#            production.
#   gsheets  a Google Sheet shared with a service account. Survives restarts
#            and redeploys.
#
# Configuration (options or environment variables; options win)
#   tracking.backend          TRACKING_BACKEND          local | gsheets
#   tracking.dir              TRACKING_DIR              local backend directory
#   tracking.sheet            TRACKING_SHEET            Sheet id or URL
#   tracking.sheet_tab        TRACKING_SHEET_TAB        worksheet name
#   tracking.admin_token      TRACKING_ADMIN_TOKEN      admin passphrase
#   tracking.tz               TRACKING_TZ               display time zone
#
# Service-account credentials are read from GOOGLE_APPLICATION_CREDENTIALS
# (a path) or GCP_SERVICE_ACCOUNT_JSON (the JSON itself, written to a temp file
# at startup - the only workable route on shinyapps.io).
# Nothing here is required for the app to run: every failure is caught and
# surfaced in the admin dashboard instead of breaking a visitor session.

track_fields <- function() {
  c("event_id", "ts", "session_id", "visitor_id", "new_visitor", "event",
    "module", "analysis", "dataset", "page", "device", "browser", "os",
    "screen", "country", "duration_sec")
}

track_setting <- function(name, default = NULL) {
  opt <- getOption(paste0("tracking.", name))
  if (!is.null(opt)) return(opt)
  env <- Sys.getenv(paste0("TRACKING_", toupper(name)), unset = NA_character_)
  if (is.na(env) || !nzchar(env)) default else env
}

track_config <- function() {
  dir <- track_setting("dir", NULL)
  if (is.null(dir)) {
    dir <- tryCatch(file.path(tools::R_user_dir("mr-somali", "data"), "tracking"),
                    error = function(e) file.path("~", ".mr-somali", "tracking"))
  }
  list(
    backend     = tolower(track_setting("backend", "local")),
    dir         = dir,
    sheet       = track_setting("sheet", ""),
    sheet_tab   = track_setting("sheet_tab", "events"),
    admin_token = track_setting("admin_token", ""),
    tz          = track_setting("tz", "UTC")
  )
}

track_empty <- function() {
  data.frame(
    event_id     = character(0), ts = character(0),
    session_id   = character(0), visitor_id = character(0),
    new_visitor  = logical(0),   event = character(0),
    module       = character(0), analysis = character(0),
    dataset      = character(0), page = character(0),
    device       = character(0), browser = character(0),
    os           = character(0), screen = character(0),
    country      = character(0), duration_sec = numeric(0),
    stringsAsFactors = FALSE
  )
}

# One event row. Missing values stay NA so the dashboard can tell "unknown"
# apart from an empty string.
track_row <- function(event, session_id, visitor_id = NA_character_,
                      new_visitor = NA, module = NA_character_,
                      analysis = NA_character_, dataset = NA_character_,
                      page = NA_character_, device = NA_character_,
                      browser = NA_character_, os = NA_character_,
                      screen = NA_character_, country = NA_character_,
                      duration_sec = NA_real_) {
  id <- tryCatch(uuid::UUIDgenerate(), error = function(e) NA_character_)
  if (is.na(id)) {
    id <- paste0(format(Sys.time(), "%Y%m%d%H%M%OS3"), "-",
                 paste(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))
  }
  data.frame(
    event_id = id,
    ts = format(as.POSIXct(Sys.time()), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    session_id = session_id,
    visitor_id = visitor_id,
    new_visitor = as.logical(new_visitor),
    event = event,
    module = module,
    analysis = analysis,
    dataset = dataset,
    page = page,
    device = device,
    browser = browser,
    os = os,
    screen = screen,
    country = country,
    duration_sec = as.numeric(duration_sec),
    stringsAsFactors = FALSE
  )
}

track_coerce <- function(d) {
  if (is.null(d) || !nrow(d)) return(track_empty())
  for (nm in setdiff(track_fields(), names(d))) d[[nm]] <- NA
  d <- d[, track_fields(), drop = FALSE]
  d$new_visitor <- as.logical(d$new_visitor)
  d$duration_sec <- suppressWarnings(as.numeric(d$duration_sec))
  d$ts <- as.character(d$ts)
  d
}

# ---- local CSV backend ---------------------------------------------------
track_store_local <- function(cfg) {
  path <- file.path(cfg$dir, "events.csv")

  append <- function(df) {
    if (!dir.exists(cfg$dir)) {
      dir.create(cfg$dir, recursive = TRUE, showWarnings = FALSE)
    }
    lock <- NULL
    if (requireNamespace("filelock", quietly = TRUE)) {
      lock <- tryCatch(
        filelock::lock(file.path(cfg$dir, "events.lock"), timeout = 5000),
        error = function(e) NULL)
    }
    on.exit(if (!is.null(lock)) try(filelock::unlock(lock), silent = TRUE),
            add = TRUE)
    has_file <- file.exists(path)
    utils::write.table(df, path, sep = ",", row.names = FALSE,
                       col.names = !has_file, append = has_file, quote = TRUE,
                       na = "", fileEncoding = "UTF-8")
    invisible(TRUE)
  }

  read <- function() {
    if (!file.exists(path)) return(track_empty())
    d <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE,
                      na.strings = c("", "NA"), check.names = FALSE),
      error = function(e) track_empty())
    track_coerce(d)
  }

  list(
    key = paste("local", cfg$dir),
    label = "Local CSV",
    persistent = FALSE,
    available = TRUE,
    message = sprintf("Recording to %s (this file does not survive a shinyapps.io restart).", path),
    append = append,
    read = read
  )
}

# ---- Google Sheets backend ------------------------------------------------
track_gsheets_auth <- function() {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' is not installed.")
  }
  path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")
  json <- Sys.getenv("GCP_SERVICE_ACCOUNT_JSON", "")
  if (!nzchar(path) && nzchar(json)) {
    path <- file.path(tempdir(), "tracking-service-account.json")
    writeLines(json, path, useBytes = TRUE)
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("No service-account credentials: set GOOGLE_APPLICATION_CREDENTIALS or GCP_SERVICE_ACCOUNT_JSON.")
  }
  googlesheets4::gs4_auth(path = path, cache = FALSE)
  invisible(TRUE)
}

track_store_gsheets <- function(cfg) {
  if (!nzchar(cfg$sheet)) {
    return(list(
      key = "gsheets", label = "Google Sheets", persistent = TRUE,
      available = FALSE, message = "TRACKING_SHEET is not set.",
      append = function(df) stop("Google Sheets backend is not configured."),
      read = function() track_empty()
    ))
  }
  ready <- FALSE
  error_msg <- NULL
  ensure <- function() {
    if (ready) return(invisible(TRUE))
    track_gsheets_auth()
    ss <- cfg$sheet
    sheets <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) NULL)
    if (is.null(sheets)) stop("Cannot open the tracking spreadsheet.")
    if (!cfg$sheet_tab %in% sheets) {
      googlesheets4::sheet_add(ss, sheet = cfg$sheet_tab)
    }
    # Write the header on first use so sheet_append() has somewhere to go.
    head <- tryCatch(
      googlesheets4::read_sheet(ss, sheet = cfg$sheet_tab, n_max = 1,
                                col_names = FALSE),
      error = function(e) NULL)
    if (is.null(head) || !nrow(head)) {
      googlesheets4::write_sheet(track_empty(), ss, sheet = cfg$sheet_tab)
    }
    ready <<- TRUE
    invisible(TRUE)
  }

  append <- function(df) {
    ensure()
    df[] <- lapply(df, function(x) if (is.logical(x)) as.character(x) else x)
    googlesheets4::sheet_append(cfg$sheet, df, sheet = cfg$sheet_tab)
    invisible(TRUE)
  }

  read <- function() {
    tryCatch({
      ensure()
      d <- googlesheets4::read_sheet(cfg$sheet, sheet = cfg$sheet_tab,
                                     col_types = "c")
      track_coerce(as.data.frame(d, stringsAsFactors = FALSE))
    }, error = function(e) {
      error_msg <<- conditionMessage(e)
      track_empty()
    })
  }

  list(
    key = paste("gsheets", cfg$sheet, cfg$sheet_tab),
    label = "Google Sheets",
    persistent = TRUE,
    available = TRUE,
    message = sprintf("Recording to Google Sheet '%s' (%s).", cfg$sheet, cfg$sheet_tab),
    error = function() error_msg,
    append = append,
    read = read
  )
}

# The store is built once per R process; shinyapps.io starts one process per
# instance, so the credential handshake happens on the first tracked event.
track_store <- function(cfg = track_config()) {
  key <- paste(cfg$backend, cfg$dir, cfg$sheet, cfg$sheet_tab, sep = "|")
  cached <- getOption("tracking.store")
  if (!is.null(cached) && identical(cached$key, key)) return(cached)
  store <- switch(
    cfg$backend,
    gsheets = track_store_gsheets(cfg),
    local = track_store_local(cfg),
    {
      store <- track_store_local(cfg)
      store$message <- sprintf("Unknown backend '%s'; falling back to the local CSV.", cfg$backend)
      store
    }
  )
  options(tracking.store = store)
  store
}

track_record <- function(store, row) {
  ok <- tryCatch({ store$append(row); TRUE },
                 error = function(e) {
                   message("tracking: could not record event - ", conditionMessage(e))
                   FALSE
                 })
  invisible(ok)
}

track_read <- function(store) {
  tryCatch(store$read(), error = function(e) track_empty())
}

# ---- server-side context --------------------------------------------------
# Country is only recorded when a proxy or CDN in front of the app supplies it
# as a header. No IP lookup is performed: the request address is a proxy
# address on shinyapps.io, so a lookup would be neither reliable nor fair.
track_country <- function(session) {
  hdr <- tryCatch(session$request, error = function(e) NULL)
  if (is.null(hdr)) return(NA_character_)
  for (key in c("HTTP_CF_IPCOUNTRY", "HTTP_X_COUNTRY_CODE",
                "HTTP_X_APPENGINE_COUNTRY", "HTTP_X_GEO_COUNTRY")) {
    v <- hdr[[key]]
    if (!is.null(v) && nzchar(v) && !toupper(v) %in% c("XX", "ZZ", "T1")) {
      return(toupper(v))
    }
  }
  NA_character_
}
