# =========================================================================
# Visitor tracking - UI
# =========================================================================
# Two pieces:
#   track_assets()      CSS + client script (place once, at the end of the UI)
#   track_admin_ui()    the administrator dashboard, inside a hidden nav panel

track_css <- "
  /* ---- admin dashboard ---- */
  .trk-page { padding: 4px 2px 28px; }
  .trk-head {
    display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-end;
    justify-content: space-between; margin-bottom: 14px;
  }
  .trk-head h3 { margin: 0; font-weight: 700; letter-spacing: -0.2px; }
  .trk-head .trk-sub { color: #64748B; font-size: 0.86rem; margin-top: 2px; }
  .trk-actions { display: flex; gap: 8px; flex-wrap: wrap; }
  .trk-status {
    font-size: 0.78rem; color: #64748B; margin-top: 6px;
  }
  .trk-status .trk-warn { color: #B45309; }
  .trk-statline {
    display: flex; gap: 22px; flex-wrap: wrap; margin: 2px 0 10px;
  }
  .trk-statline div { font-size: 0.86rem; color: #64748B; }
  .trk-statline b { color: #0F172A; font-size: 1.02rem; }
  [data-bs-theme='dark'] .trk-head .trk-sub,
  [data-bs-theme='dark'] .trk-status { color: #94A3B8; }
  [data-bs-theme='dark'] .trk-statline div { color: #94A3B8; }
  [data-bs-theme='dark'] .trk-statline b { color: #F1F5F9; }
  .trk-empty {
    display: flex; align-items: center; justify-content: center;
    height: 220px; color: #94A3B8; font-size: 0.9rem; text-align: center;
  }
  .trk-note { font-size: 0.8rem; color: #64748B; }
"

# Client script. Colours and layout stay in R; this only collects what the
# browser will disclose and forwards it once the session is connected.
track_js <- paste(
  "(function () {",
  "  var VISITOR_KEY = 'mrsomali_visitor_v1';",
  "  var pending = [];",
  "  var timer = null;",
  "  var booted = false;",
  "",
  "  function read(k) { try { return window.localStorage.getItem(k); } catch (e) { return null; } }",
  "  function write(k, v) { try { window.localStorage.setItem(k, v); } catch (e) {} }",
  "",
  "  function uuid() {",
  "    try {",
  "      if (window.crypto && window.crypto.randomUUID) return window.crypto.randomUUID();",
  "      if (window.crypto && window.crypto.getRandomValues) {",
  "        var b = new Uint8Array(16);",
  "        window.crypto.getRandomValues(b);",
  "        var s = '';",
  "        for (var i = 0; i < b.length; i++) s += ('0' + b[i].toString(16)).slice(-2);",
  "        return s;",
  "      }",
  "    } catch (e) {}",
  "    return 'v' + Date.now().toString(36) + Math.random().toString(36).slice(2, 10);",
  "  }",
  "",
  "  function send(name, value) {",
  "    if (window.Shiny && window.Shiny.setInputValue) {",
  "      window.Shiny.setInputValue(name, value, { priority: 'event' });",
  "    } else {",
  "      pending.push([name, value]);",
  "    }",
  "  }",
  "",
  "  function info() {",
  "    var ua = navigator.userAgent || '';",
  "    var w = window.screen ? window.screen.width : 0;",
  "    var h = window.screen ? window.screen.height : 0;",
  "    var touch = ('ontouchstart' in window) || (navigator.maxTouchPoints > 0);",
  "    var tablet = /iPad/.test(ua) || /Tablet|PlayBook|Silk/i.test(ua) ||",
  "                 (/Macintosh/.test(ua) && touch);",
  "    var device = tablet ? 'Tablet'",
  "               : (/Mobi|Android|iPhone|iPod|Windows Phone/i.test(ua) ? 'Mobile' : 'Desktop');",
  "    var os = 'Unknown';",
  "    if (/Windows NT/.test(ua)) os = 'Windows';",
  "    else if (/Android/.test(ua)) os = 'Android';",
  "    else if (/iPhone|iPad|iPod/.test(ua)) os = 'iOS';",
  "    else if (/Mac OS X/.test(ua)) os = 'macOS';",
  "    else if (/CrOS/.test(ua)) os = 'ChromeOS';",
  "    else if (/Linux/.test(ua)) os = 'Linux';",
  "    var browser = 'Unknown';",
  "    if (/Edg\\//.test(ua)) browser = 'Edge';",
  "    else if (/OPR\\/|Opera/.test(ua)) browser = 'Opera';",
  "    else if (/Chrome\\//.test(ua)) browser = 'Chrome';",
  "    else if (/Firefox\\//.test(ua)) browser = 'Firefox';",
  "    else if (/Safari\\//.test(ua)) browser = 'Safari';",
  "    return {",
  "      device: device, browser: browser, os: os,",
  "      screen: (w && h) ? (w + 'x' + h) : 'unknown'",
  "    };",
  "  }",
  "",
  "  function beat() {",
  "    if (document.visibilityState && document.visibilityState !== 'visible') return;",
  "    send('trk_beat', Date.now());",
  "  }",
  "",
  "  function startTimer() {",
  "    if (timer) return;",
  "    timer = window.setInterval(beat, 60000);",
  "  }",
  "",
  "  function init() {",
  "    var existing = read(VISITOR_KEY);",
  "    var payload = info();",
  "    payload.visitor_id = existing || uuid();",
  "    if (!existing) write(VISITOR_KEY, payload.visitor_id);",
  "    payload.new_visitor = !existing;",
  "    send('trk_client', payload);",
  "    startTimer();",
  "  }",
  "",
  "  function boot() {",
  "    if (booted) return;",
  "    booted = true;",
  "    if (window.Shiny && window.Shiny.setInputValue) {",
  "      while (pending.length) {",
  "        var p = pending.shift();",
  "        window.Shiny.setInputValue(p[0], p[1], { priority: 'event' });",
  "      }",
  "    }",
  "    init();",
  "  }",
  "",
  "  if (window.Shiny && window.Shiny.initializedPromise) {",
  "    window.Shiny.initializedPromise.then(boot);",
  "  } else if (window.jQuery) {",
  "    window.jQuery(document).on('shiny:connected', boot);",
  "  } else {",
  "    document.addEventListener('shiny:connected', boot);",
  "  }",
  "",
  "  document.addEventListener('visibilitychange', function () {",
  "    if (document.visibilityState === 'visible') beat();",
  "  });",
  "})();",
  sep = "\n"
)

track_assets <- function() {
  tagList(
    tags$style(htmltools::HTML(track_css)),
    tags$script(htmltools::HTML(track_js))
  )
}

# ---- administrator dashboard ---------------------------------------------
track_admin_ui <- function() {
  div(class = "trk-page",
    div(class = "trk-head",
      div(
        h3(icon("chart-line"), " Visitor analytics"),
        div(class = "trk-sub",
            "Anonymous usage of Mr Somali. Sessions are visits; unique visitors
             are browsers carrying the same random ID."),
        div(class = "trk-status", textOutput("trk_status", inline = TRUE))
      ),
      div(class = "trk-actions",
        selectInput("trk_days", NULL, width = "150px",
                    choices = c("Last 7 days" = 7, "Last 30 days" = 30,
                                "Last 90 days" = 90, "All time" = 0),
                    selected = 30),
        actionButton("trk_refresh", "Refresh", icon = icon("rotate")),
        actionButton("trk_lock", "Lock", icon = icon("lock"))
      )
    ),

    uiOutput("trk_kpis"),

    div(class = "trk-statline", uiOutput("trk_period", inline = TRUE)),

    layout_columns(
      col_widths = c(8, 4),
      card(card_header(tagList(icon("chart-line"), " Visits and analyses per day")),
           plotOutput("trk_trend", height = "260px")),
      card(card_header(tagList(icon("mobile-screen"), " Devices")),
           plotOutput("trk_devices", height = "260px"))
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(card_header(tagList(icon("flask"), " Most used methods")),
           plotOutput("trk_methods", height = "240px")),
      card(card_header(tagList(icon("compass"), " Sections used")),
           plotOutput("trk_sections", height = "240px"))
    ),

    card(
      card_header(
        class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
        tagList(icon("clock-rotate-left"), " Visitor activity"),
        div(class = "d-flex gap-2",
            downloadButton("trk_dl_events", "Export events (CSV)",
                           class = "btn-sm btn-outline-primary"),
            downloadButton("trk_dl_summary", "Export summary (CSV)",
                           class = "btn-sm btn-outline-primary"))
      ),
      DTOutput("trk_activity"),
      div(class = "trk-note mt-2",
          "Uploaded data, results and free-text input are never recorded;
           only the file name of a dataset is kept.")
    ),

    div(class = "trk-note mt-3",
        textOutput("trk_store_note"))
  )
}
