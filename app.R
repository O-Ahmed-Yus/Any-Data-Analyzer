# Mr Somali - statistical analysis platform
# Run with: shiny::runApp("Myapp/analysis_app.R")
# Analyses an uploaded data file, a bundled sample, or My.data from the session.

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(ggplot2)
library(thematic)

# Used throughout the app but not exported by any attached package. It used to
# resolve only because a developer had it in .GlobalEnv, which a fresh R
# process on shinyapps.io does not, so it is defined here explicitly.
`%||%` <- function(x, y) if (is.null(x)) y else x

# GIS and visualisation modules live in R/. They are sourced rather than added
# to this file so the main source stays navigable; missing files or missing
# packages leave the rest of the app untouched.
local({
  dir <- if (dir.exists("R")) "R" else if (dir.exists("Myapp/R")) "Myapp/R" else NA_character_
  if (!is.na(dir)) {
    files <- list.files(dir,
                        pattern = "^(gis_|mod_gis_|viz_|mod_viz_|track_|mod_track_).*\\.R$",
                        full.names = TRUE)
    for (f in files) sys.source(f, envir = .GlobalEnv)
  }
})

# Tracking secrets live in an app-local .Renviron so they stay out of the
# source. An environment variable set on the server wins over the file.
# readRenviron() is explicit because the app can be launched with the working
# directory either at Myapp/ or one level above it.
local({
  if (!nzchar(Sys.getenv("TRACKING_ADMIN_TOKEN"))) {
    env_file <- c(".Renviron", "Myapp/.Renviron")
    env_file <- env_file[file.exists(env_file)][1]
    if (!is.na(env_file)) readRenviron(env_file)
  }
})

# Upload cap. Shiny copies uploads into a per-session temporary directory and
# removes them when the session ends; nothing persists between sessions.
options(shiny.maxRequestSize = 50 * 1024^2)

# ---- design tokens -------------------------------------------------------
brand <- list(
  primary   = "#2563EB",
  secondary = "#14B8A6",
  accent    = "#8B5CF6",
  success   = "#10B981",
  warning   = "#F59E0B",
  danger    = "#EF4444",
  ink       = "#0F172A",
  muted     = "#64748B",
  surface   = "#FFFFFF",
  canvas    = "#F8FAFC",
  sidebar   = "#111827"
)

# Categorical palette used by plots (thematic installs it as palette())
pal <- c("#2563EB", "#14B8A6", "#8B5CF6", "#F59E0B", "#10B981",
         "#EF4444", "#0EA5E9", "#EC4899")

app_css <- "
  body, .navbar, .card, .btn, .form-control, .form-select, .table {
    font-family: 'Inter', 'Segoe UI', system-ui, -apple-system, sans-serif;
  }
  body { background-color: #F8FAFC; }

  /* ---- brand / navbar ---- */
  .app-title {
    font-weight: 700; letter-spacing: -0.15px; color: #0F172A;
    display: inline-flex; align-items: center;
  }
  .app-logo {
    height: 1.7em; width: auto; margin-right: 0.5rem;
    display: inline-block; border-radius: 6px;
  }
  .navbar {
    position: sticky; top: 0; z-index: 1030;
    background-color: #FFFFFF;
    box-shadow: 0 1px 3px rgba(15, 23, 42, 0.08);
  }
  .navbar::after {
    content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 3px;
    background-image: linear-gradient(90deg, #2563EB, #14B8A6, #8B5CF6, #2563EB);
  }
  .navbar .navbar-nav .nav-link {
    color: #475569; border-radius: 8px; padding: 0.4rem 0.75rem;
    margin: 0.1rem; font-size: 0.9rem; font-weight: 500;
    transition: background-color .15s ease, color .15s ease;
  }
  .navbar .navbar-nav .nav-link .fa, .navbar .navbar-nav .nav-link .fas {
    margin-right: 0.35rem; opacity: 0.85;
  }
  .navbar .navbar-nav .nav-link:hover {
    background-color: rgba(37, 99, 235, 0.08); color: #1D4ED8;
  }
  .navbar .navbar-nav .nav-link.active {
    color: #FFFFFF !important; font-weight: 600;
    background-image: linear-gradient(135deg, #2563EB 0%, #1D4ED8 100%);
    box-shadow: 0 2px 8px rgba(37, 99, 235, 0.35);
  }

  /* ---- dark sidebar ---- */
  .bslib-sidebar-layout > .sidebar {
    background-color: #111827;
    border-right: none;
    color: #E5E7EB;
  }
  .sidebar h6 {
    color: #94A3B8; font-size: 0.72rem; letter-spacing: 1.1px;
    text-transform: uppercase; font-weight: 700; margin-top: 0.4rem;
  }
  .sidebar .form-control, .sidebar .form-select {
    background-color: #1F2937; color: #E5E7EB;
    border: 1px solid #374151; border-radius: 8px;
  }
  .sidebar .form-control:focus, .sidebar .form-select:focus {
    background-color: #1F2937; color: #E5E7EB;
    border-color: #2563EB; box-shadow: 0 0 0 0.2rem rgba(37, 99, 235, .3);
  }
  .sidebar .form-control::placeholder { color: #9CA3AF; }
  .sidebar .control-label, .sidebar label, .sidebar .help-block,
  .sidebar .shiny-input-container > label { color: #CBD5E1; }
  .sidebar a { color: #93C5FD; }
  .sidebar hr { border-color: #374151; opacity: 1; }
  .sidebar .accordion-item {
    background-color: #1F2937; border: 1px solid #374151;
    border-radius: 10px; margin-bottom: 0.4rem; overflow: hidden;
  }
  .sidebar .accordion-button {
    background-color: #1F2937; color: #E5E7EB; font-size: 0.85rem;
    padding: 0.55rem 0.75rem;
  }
  .sidebar .accordion-button:not(.collapsed) {
    background-color: #24354F; color: #BFDBFE; box-shadow: none;
  }
  .sidebar .accordion-button::after { filter: invert(1) brightness(1.6); }
  .sidebar .btn-default:not(.btn-primary), .sidebar .btn-outline-secondary {
    color: #E5E7EB; border-color: #4B5563; background-color: transparent;
  }
  .sidebar .btn-default:not(.btn-primary):hover, .sidebar .btn-outline-secondary:hover {
    background-color: #374151; color: #FFFFFF;
  }
  .sidebar .selectize-control.multi .selectize-input,
  .sidebar .selectize-input {
    background-color: #1F2937; color: #E5E7EB; border-color: #374151;
  }
  .sidebar .selectize-dropdown {
    background-color: #1F2937; color: #E5E7EB; border-color: #374151;
  }
  .sidebar .selectize-dropdown .active { background-color: #24354F; }
  .sidebar .selectize-control.multi .selectize-input > div {
    background: #2563EB; color: #FFFFFF; border-radius: 6px;
  }

  /* ---- layout / cards ---- */
  .card {
    border: 1px solid #E2E8F0; border-radius: 14px;
    box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
    transition: box-shadow .18s ease;
  }
  .card:hover { box-shadow: 0 10px 24px rgba(15, 23, 42, 0.08); }
  .card-header {
    background-color: #FFFFFF; border-bottom: 1px solid #E2E8F0;
    font-weight: 600; color: #0F172A; border-radius: 14px 14px 0 0 !important;
  }
  h5, .section-title {
    color: #0F172A; font-weight: 650; font-size: 1.05rem;
    margin-top: 1.4rem; margin-bottom: 0.6rem;
    display: flex; align-items: center; gap: 0.5rem;
  }
  h5 .fa, .section-title .fa { color: #2563EB; }
  .muted { color: #64748B; }
  .small-note { font-size: 0.83rem; color: #64748B; }

  /* ---- KPI cards ---- */
  .kpi-grid {
    display: grid; grid-template-columns: repeat(auto-fit, minmax(165px, 1fr));
    gap: 0.9rem; margin-bottom: 0.5rem;
  }
  .kpi {
    background: #FFFFFF; border: 1px solid #E2E8F0; border-radius: 14px;
    padding: 0.95rem 1.05rem; position: relative; overflow: hidden;
  }
  .kpi::before {
    content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 4px;
    background: #2563EB;
  }
  .kpi.kpi-teal::before { background: #14B8A6; }
  .kpi.kpi-violet::before { background: #8B5CF6; }
  .kpi.kpi-amber::before { background: #F59E0B; }
  .kpi.kpi-green::before { background: #10B981; }
  .kpi.kpi-red::before { background: #EF4444; }
  .kpi-label {
    font-size: 0.73rem; text-transform: uppercase; letter-spacing: 0.8px;
    color: #64748B; font-weight: 700;
  }
  .kpi-value { font-size: 1.6rem; font-weight: 700; color: #0F172A; line-height: 1.2; }
  .kpi-sub { font-size: 0.78rem; color: #64748B; }
  .kpi-icon { position: absolute; right: 0.9rem; top: 0.9rem; font-size: 1.1rem; color: #CBD5E1; }

  /* ---- empty state ---- */
  .empty-state {
    text-align: center; padding: 2.4rem 1.5rem; border-radius: 16px;
    border: 1px dashed #CBD5E1; background: #FFFFFF;
  }
  .empty-state .empty-icon { font-size: 2.2rem; color: #2563EB; margin-bottom: 0.6rem; }

  /* ---- decision boxes ---- */
  .decision {
    border-left: 6px solid; border-radius: 10px;
    padding: 0.75rem 1rem; margin: 0.4rem 0 1rem 0;
  }
  .decision-verdict { font-weight: 700; font-size: 1.02rem; }
  .decision-detail { font-size: 0.83rem; opacity: 0.85; }
  .decision-hyp { font-size: 0.9rem; margin: 0.45rem 0; }
  .decision-reject { border-color: #10B981; background-color: #ECFDF5; color: #064E3B; }
  .decision-reject .decision-verdict { color: #047857; }
  .decision-fail { border-color: #F59E0B; background-color: #FFFBEB; color: #78350F; }
  .decision-fail .decision-verdict { color: #B45309; }

  /* ---- console / interpretation blocks ---- */
  pre.shiny-text-output {
    background-color: #F8FAFC; border: 1px solid #E2E8F0;
    border-left: 4px solid #2563EB; border-radius: 10px;
    padding: 0.75rem 0.9rem; font-size: 0.85rem; color: #0F172A;
  }
  .interp { line-height: 1.65; font-size: 0.95rem; color: #1E293B; }
  .interp h6 { font-weight: 700; color: #0F172A; margin-top: 1rem; }
  .interp code { background: #EFF6FF; color: #1D4ED8; padding: 0 0.25rem; border-radius: 4px; }

  /* ---- tables ---- */
  table.dataTable thead th {
    background-color: #F1F5F9 !important; color: #0F172A;
    font-size: 0.82rem; border-bottom: 2px solid #2563EB !important;
  }
  table.table > thead > tr > th {
    background-color: #F1F5F9; border-bottom: 2px solid #2563EB;
    font-size: 0.85rem;
  }
  .table-striped > tbody > tr:nth-of-type(odd) > * { background-color: #F8FAFC; }

  /* ---- buttons ---- */
  .btn-primary { background-color: #2563EB; border-color: #2563EB; }
  .btn-primary:hover { background-color: #1D4ED8; border-color: #1D4ED8; }
  /* Action buttons carry both .btn-default and .btn-primary, so these rules
     must not reach the primary ones: a blue label on the blue gradient is
     unreadable. Primary labels are pinned to white below instead. */
  .btn-default:not(.btn-primary) { border-color: #2563EB; color: #2563EB; }
  .btn-default:not(.btn-primary):hover { background-color: #2563EB; color: #FFFFFF; }
  .btn-primary, .btn-primary:hover, .btn-primary:focus, .btn-primary:focus-visible,
  .btn-primary:active, .btn-primary.active, .btn-primary:disabled,
  .btn-primary .fa, .btn-primary .fas {
    color: #FFFFFF !important;
  }
  .shiny-download-link {
    background-color: #2563EB; border-color: #2563EB; color: #FFFFFF;
  }
  .shiny-download-link:hover { background-color: #1D4ED8; color: #FFFFFF; }

  /* ---- About / badges ---- */
  .about-card { max-width: 860px; margin-left: auto; margin-right: auto; }
  .about-badge {
    display: inline-block; margin: 0.16rem 0.32rem 0.16rem 0;
    padding: 0.34rem 0.8rem; border-radius: 999px;
    font-size: 0.82rem; font-weight: 600; color: #FFFFFF;
    background-image: linear-gradient(135deg, #2563EB 0%, #1D4ED8 100%);
  }
  .glossary-term { font-weight: 600; color: #1D4ED8; }

  /* ---- motion ---- */
  @keyframes app-fade-up {
    from { opacity: 0; transform: translateY(6px); }
    to   { opacity: 1; transform: translateY(0); }
  }
  .tab-pane.active { animation: app-fade-up .24s ease-out both; }
  @media (prefers-reduced-motion: reduce) {
    .tab-pane.active { animation: none !important; }
    .card, .card:hover { transition: none !important; }
  }

  /* ---- polish (additive) ---- */
  ::selection { background: rgba(37, 99, 235, 0.18); }
  body {
    background-image:
      radial-gradient(900px 420px at 8% -10%, rgba(37, 99, 235, 0.07), transparent 60%),
      radial-gradient(760px 380px at 100% 0%, rgba(20, 184, 166, 0.06), transparent 55%);
    background-repeat: no-repeat;
    background-attachment: fixed;
  }
  .navbar { backdrop-filter: saturate(160%) blur(6px); }
  .bslib-sidebar-layout > .sidebar {
    background-image: linear-gradient(180deg, #111827 0%, #0B1220 100%);
  }
  .card { transition: box-shadow .18s ease, transform .18s ease; }
  .card:hover { transform: translateY(-2px); }
  .kpi { transition: transform .18s ease, box-shadow .18s ease; }
  .kpi:hover { transform: translateY(-2px); box-shadow: 0 10px 22px rgba(15, 23, 42, 0.09); }
  .btn-primary, .shiny-download-link {
    background-image: linear-gradient(135deg, #2563EB 0%, #1D4ED8 100%);
    border-color: transparent;
    box-shadow: 0 1px 2px rgba(37, 99, 235, 0.25);
    transition: transform .15s ease, box-shadow .15s ease, filter .15s ease;
  }
  .btn-primary:hover, .shiny-download-link:hover {
    filter: brightness(1.06); transform: translateY(-1px);
    box-shadow: 0 6px 14px rgba(37, 99, 235, 0.28);
  }
  .btn:active { transform: translateY(0); }
  .btn:focus-visible, .form-control:focus-visible, .form-select:focus-visible {
    outline: 2px solid rgba(37, 99, 235, 0.55); outline-offset: 2px;
  }
  table.dataTable tbody tr:hover > * { background-color: rgba(37, 99, 235, 0.06) !important; }
  ::-webkit-scrollbar { width: 10px; height: 10px; }
  ::-webkit-scrollbar-track { background: transparent; }
  ::-webkit-scrollbar-thumb {
    background: #CBD5E1; border-radius: 999px;
    border: 2px solid transparent; background-clip: content-box;
  }
  ::-webkit-scrollbar-thumb:hover { background: #94A3B8; background-clip: content-box; }
  @media (prefers-reduced-motion: reduce) {
    .card, .card:hover, .kpi, .kpi:hover, .btn, .btn:hover {
      transition: none !important; transform: none !important;
    }
  }

  /* ---- dark mode ---- */
  [data-bs-theme='dark'] body { background-color: #0B1220; color: #E5E7EB; }
  [data-bs-theme='dark'] .navbar { background-color: #0F172A; }
  [data-bs-theme='dark'] .navbar .navbar-nav .nav-link { color: #CBD5E1; }
  [data-bs-theme='dark'] .navbar .navbar-nav .nav-link:hover {
    background-color: rgba(37, 99, 235, 0.2); color: #BFDBFE;
  }
  [data-bs-theme='dark'] .app-title { color: #F8FAFC; }
  [data-bs-theme='dark'] .card { background-color: #111C2E; border-color: #1F2A3C; color: #E5E7EB; }
  [data-bs-theme='dark'] .card-header {
    background-color: #111C2E; border-color: #1F2A3C; color: #E5E7EB;
  }
  [data-bs-theme='dark'] h5, [data-bs-theme='dark'] .section-title { color: #F1F5F9; }
  [data-bs-theme='dark'] h5 .fa { color: #60A5FA; }
  [data-bs-theme='dark'] .kpi { background: #111C2E; border-color: #1F2A3C; }
  [data-bs-theme='dark'] .kpi-value { color: #F8FAFC; }
  [data-bs-theme='dark'] .kpi-icon { color: #334155; }
  [data-bs-theme='dark'] .empty-state { background: #111C2E; border-color: #1F2A3C; }
  [data-bs-theme='dark'] .decision-reject { background-color: #06281C; color: #D1FAE5; border-color: #10B981; }
  [data-bs-theme='dark'] .decision-reject .decision-verdict { color: #6EE7B7; }
  [data-bs-theme='dark'] .decision-fail { background-color: #2A1F06; color: #FDE68A; border-color: #F59E0B; }
  [data-bs-theme='dark'] .decision-fail .decision-verdict { color: #FCD34D; }
  [data-bs-theme='dark'] pre.shiny-text-output {
    background-color: #0B1220; border-color: #1F2A3C; color: #E2E8F0;
  }
  [data-bs-theme='dark'] .interp { color: #CBD5E1; }
  [data-bs-theme='dark'] .interp h6 { color: #F1F5F9; }
  [data-bs-theme='dark'] .interp code { background: #1E3A8A; color: #DBEAFE; }
  [data-bs-theme='dark'] table.dataTable thead th {
    background-color: #1F2A3C !important; color: #E2E8F0;
  }
  [data-bs-theme='dark'] table.table > thead > tr > th {
    background-color: #1F2A3C; color: #E2E8F0;
  }
  [data-bs-theme='dark'] .table-striped > tbody > tr:nth-of-type(odd) > * {
    background-color: #0F172A;
  }
  [data-bs-theme='dark'] .small-note, [data-bs-theme='dark'] .muted { color: #94A3B8; }
  [data-bs-theme='dark'] body {
    background-image:
      radial-gradient(900px 420px at 8% -10%, rgba(37, 99, 235, 0.22), transparent 60%),
      radial-gradient(760px 380px at 100% 0%, rgba(20, 184, 166, 0.14), transparent 55%);
  }
  [data-bs-theme='dark'] .bslib-sidebar-layout > .sidebar {
    background-image: linear-gradient(180deg, #0F172A 0%, #0B1220 100%);
  }
  [data-bs-theme='dark'] ::-webkit-scrollbar-thumb {
    background: #334155; background-clip: content-box;
  }
  [data-bs-theme='dark'] table.dataTable tbody tr:hover > * {
    background-color: rgba(37, 99, 235, 0.16) !important;
  }

  /* ===================================================================== */
  /* ---- GIS workspace ---- */
  .gis-meta p { margin-bottom: .2rem; font-size: .88rem; color: #334155; }
  .gis-pop { font-size: .82rem; line-height: 1.4; max-width: 320px; }
  .gis-pop .gis-pop-key { color: #64748B; font-weight: 600; }
  .leaflet-container { border-radius: 14px; font-family: inherit; }
  .leaflet-popup-content { margin: .55rem .75rem; }
  [data-bs-theme='dark'] .gis-meta p { color: #CBD5E1; }
  [data-bs-theme='dark'] .gis-pop .gis-pop-key { color: #94A3B8; }
  [data-bs-theme='dark'] .leaflet-container { background: #0B1220; }

  /* vivid layer: richer colour, depth and personality                     */
  /* Light rules first, dark overrides re-applied at the end of this block. */
  /* ===================================================================== */
  :root {
    --mr-blue: #2563EB;
    --mr-indigo: #4F46E5;
    --mr-violet: #7C3AED;
    --mr-cyan: #06B6D4;
    --mr-teal: #0D9488;
    --mr-emerald: #10B981;
    --mr-amber: #F59E0B;
    --mr-pink: #EC4899;
    --mr-grad: linear-gradient(135deg, #2563EB 0%, #7C3AED 52%, #06B6D4 100%);
    --mr-grad-warm: linear-gradient(135deg, #F59E0B 0%, #EC4899 100%);
    --mr-radius: 16px;
  }

  /* ---- canvas: soft multi-colour mesh ---- */
  body {
    background-color: #F6F8FD;
    background-image:
      radial-gradient(1050px 480px at 4% -14%, rgba(37, 99, 235, .15), transparent 62%),
      radial-gradient(900px 430px at 104% -8%, rgba(124, 58, 237, .14), transparent 60%),
      radial-gradient(820px 520px at 92% 108%, rgba(6, 182, 212, .13), transparent 62%),
      radial-gradient(680px 420px at -6% 104%, rgba(236, 72, 153, .10), transparent 60%);
    background-repeat: no-repeat;
    background-attachment: fixed;
  }
  ::selection { background: rgba(124, 58, 237, .22); }

  /* ---- navbar ---- */
  .navbar {
    background-image: linear-gradient(96deg,
      rgba(255, 255, 255, .96) 0%,
      rgba(238, 242, 255, .94) 58%,
      rgba(236, 254, 255, .94) 100%);
    border-bottom: 1px solid rgba(226, 232, 240, .9);
    box-shadow: 0 8px 24px -20px rgba(15, 23, 42, .6);
  }
  .navbar::after {
    height: 3px; opacity: 1;
    background-image: linear-gradient(90deg,
      #2563EB 0%, #7C3AED 35%, #EC4899 65%, #06B6D4 100%);
    box-shadow: 0 1px 12px rgba(124, 58, 237, .45);
  }
  .app-title {
    font-weight: 800; letter-spacing: -.35px;
    background-image: linear-gradient(115deg, #0F172A 0%, #1D4ED8 48%, #7C3AED 100%);
    -webkit-background-clip: text; background-clip: text;
    -webkit-text-fill-color: transparent;
  }
  .app-logo {
    height: 1.95em; padding: 2px; border-radius: 13px;
    background-image: var(--mr-grad);
    box-shadow: 0 10px 20px -12px rgba(37, 99, 235, 1);
  }
  .navbar .navbar-nav .nav-link { border-radius: 10px; }
  .navbar .navbar-nav .nav-link.active {
    background-image: linear-gradient(135deg, #2563EB 0%, #7C3AED 100%);
    box-shadow: 0 8px 18px -10px rgba(79, 70, 229, 1);
  }

  /* ---- sidebar: glows and coloured section dots ---- */
  .bslib-sidebar-layout > .sidebar {
    background-image:
      radial-gradient(460px 260px at 0% -10%, rgba(37, 99, 235, .48), transparent 62%),
      radial-gradient(420px 260px at 108% 10%, rgba(124, 58, 237, .38), transparent 62%),
      radial-gradient(400px 320px at 20% 108%, rgba(6, 182, 212, .30), transparent 62%),
      linear-gradient(180deg, #111827 0%, #0B1220 100%);
    border-right: 1px solid rgba(148, 163, 184, .16);
    box-shadow: inset -1px 0 0 rgba(15, 23, 42, .45);
  }
  .sidebar h6 { color: #CBD5E1; display: flex; align-items: center; gap: .45rem; }
  .sidebar h6::before {
    content: ''; flex: 0 0 auto; width: 7px; height: 7px; border-radius: 999px;
    background-image: linear-gradient(135deg, #93C5FD, #2563EB);
    box-shadow: 0 0 10px rgba(96, 165, 250, .9);
  }
  .sidebar h6:nth-of-type(2)::before {
    background-image: linear-gradient(135deg, #34D399, #0D9488);
    box-shadow: 0 0 10px rgba(52, 211, 153, .85);
  }
  .sidebar h6:nth-of-type(3)::before {
    background-image: linear-gradient(135deg, #C4B5FD, #7C3AED);
    box-shadow: 0 0 10px rgba(167, 139, 250, .9);
  }

  /* ---- cards ---- */
  .card {
    border-color: rgba(226, 232, 240, .95);
    border-radius: var(--mr-radius);
    background-color: #FFFFFF;
    box-shadow: 0 1px 2px rgba(15, 23, 42, .04),
                0 20px 40px -34px rgba(37, 99, 235, .9);
  }
  .card:hover {
    transform: translateY(-3px);
    border-color: rgba(199, 210, 254, .95);
    box-shadow: 0 24px 48px -30px rgba(79, 70, 229, .8);
  }
  .card-header {
    position: relative;
    background-color: #FFFFFF;
    background-image: linear-gradient(96deg,
      rgba(37, 99, 235, .11) 0%,
      rgba(124, 58, 237, .07) 48%,
      rgba(6, 182, 212, .10) 100%);
    border-bottom: 1px solid rgba(226, 232, 240, .9);
    border-radius: var(--mr-radius) var(--mr-radius) 0 0 !important;
    padding: .7rem 1rem .7rem 1.15rem;
  }
  .card-header::after {
    content: ''; position: absolute; left: 0; right: 0; bottom: -1px; height: 2px;
    background-image: linear-gradient(90deg, #2563EB, #7C3AED 45%, #06B6D4);
    opacity: .6;
  }
  /* icon chip, only for icons that sit directly in the header (not in tabs) */
  .card-header > .fa, .card-header > .fas, .card-header > .far {
    display: inline-grid; place-items: center;
    width: 1.6rem; height: 1.6rem; margin-right: .15rem;
    font-size: .72rem; color: #FFFFFF; vertical-align: middle;
    border-radius: 9px; background-image: var(--mr-grad);
    box-shadow: 0 7px 16px -10px rgba(79, 70, 229, 1);
  }
  /* card tab pills */
  .card-header .nav-tabs, .card-header .card-header-tabs {
    border-bottom: 0; margin: 0; gap: .3rem;
  }
  .card-header .nav-tabs .nav-link {
    border: 1px solid transparent; border-radius: 999px;
    padding: .34rem .9rem; margin: 0;
    font-size: .87rem; font-weight: 500; color: #475569;
    transition: background-color .15s ease, color .15s ease, box-shadow .15s ease;
  }
  .card-header .nav-tabs .nav-link:hover {
    background-color: rgba(37, 99, 235, .10); color: #1D4ED8;
  }
  .card-header .nav-tabs .nav-link.active {
    color: #FFFFFF !important; border-color: transparent;
    background-image: linear-gradient(135deg, #2563EB 0%, #7C3AED 100%);
    box-shadow: 0 10px 20px -14px rgba(79, 70, 229, 1);
  }
  .card-header .nav-tabs .nav-link.active .fa { color: #FFFFFF; }

  /* ---- section headings ---- */
  h5, .section-title {
    position: relative; padding-bottom: .5rem;
    font-weight: 700; color: #0F172A;
  }
  h5::after, .section-title::after {
    content: ''; position: absolute; left: 0; bottom: 0;
    width: 54px; height: 3px; border-radius: 999px;
    background-image: linear-gradient(90deg, #2563EB, #7C3AED 60%, #06B6D4);
  }
  h5 .fa, .section-title .fa { color: var(--mr-indigo); }

  /* ---- KPI cards ---- */
  .kpi {
    --kpi: #2563EB;
    --kpi-2: #60A5FA;
    --kpi-soft: rgba(37, 99, 235, .12);
    background-color: #FFFFFF;
    background-image: linear-gradient(160deg,
      var(--kpi-soft) 0%, rgba(255, 255, 255, .97) 58%, #FFFFFF 100%);
    border: 1px solid rgba(226, 232, 240, .9);
    border-radius: var(--mr-radius);
    padding: 1rem 1.1rem 1rem 1.3rem;
    box-shadow: 0 10px 24px -22px rgba(15, 23, 42, .9);
  }
  .kpi::before {
    width: 5px; border-radius: 999px;
    background-image: linear-gradient(180deg, var(--kpi), var(--kpi-2));
  }
  .kpi::after {
    content: ''; position: absolute; right: -30px; top: -40px;
    width: 130px; height: 130px; border-radius: 50%;
    background-image: radial-gradient(circle, var(--kpi-soft), transparent 70%);
    pointer-events: none;
  }
  .kpi.kpi-teal   { --kpi: #0D9488; --kpi-2: #5EEAD4; --kpi-soft: rgba(13, 148, 136, .14); }
  .kpi.kpi-violet { --kpi: #7C3AED; --kpi-2: #C4B5FD; --kpi-soft: rgba(124, 58, 237, .14); }
  .kpi.kpi-amber  { --kpi: #D97706; --kpi-2: #FCD34D; --kpi-soft: rgba(245, 158, 11, .16); }
  .kpi.kpi-green  { --kpi: #059669; --kpi-2: #6EE7B7; --kpi-soft: rgba(16, 185, 129, .14); }
  .kpi.kpi-red    { --kpi: #DC2626; --kpi-2: #FCA5A5; --kpi-soft: rgba(239, 68, 68, .14); }
  .kpi:hover {
    transform: translateY(-3px);
    box-shadow: 0 22px 44px -26px var(--kpi);
    border-color: var(--kpi-2);
  }
  .kpi-label { color: var(--kpi); }
  .kpi-sub { color: #475569; }
  .kpi-icon {
    position: absolute; right: .9rem; top: .9rem; z-index: 1;
    width: 2rem; height: 2rem; border-radius: 12px;
    display: grid; place-items: center; font-size: .9rem;
    color: #FFFFFF; opacity: 1;
    background-image: linear-gradient(135deg, var(--kpi), var(--kpi-2));
    box-shadow: 0 10px 20px -12px var(--kpi);
  }

  /* ---- dashboard hero ---- */
  .dash-hero {
    position: relative; overflow: hidden;
    display: flex; align-items: center; justify-content: space-between;
    gap: 1rem; padding: 1.15rem 1.4rem; margin-bottom: 1.1rem;
    border-radius: 20px; color: #FFFFFF;
    background-image:
      radial-gradient(560px 220px at 100% 0%, rgba(255, 255, 255, .22), transparent 62%),
      radial-gradient(420px 240px at 0% 120%, rgba(255, 255, 255, .12), transparent 60%),
      linear-gradient(118deg, #2563EB 0%, #4F46E5 42%, #7C3AED 74%, #06B6D4 100%);
    box-shadow: 0 22px 44px -28px rgba(79, 70, 229, 1);
  }
  .dash-hero-title {
    color: #FFFFFF; font-size: 1.2rem; font-weight: 700;
    letter-spacing: -.25px; margin: 0 0 .18rem 0;
  }
  .dash-hero-sub { color: rgba(255, 255, 255, .88); font-size: .85rem; }
  .dash-hero-art {
    flex: 0 0 auto; display: grid; place-items: center;
    width: 3rem; height: 3rem; border-radius: 16px; font-size: 1.35rem;
    color: #FFFFFF;
    background-color: rgba(255, 255, 255, .18);
    border: 1px solid rgba(255, 255, 255, .35);
    backdrop-filter: blur(4px);
  }

  /* ---- empty state ---- */
  .empty-state {
    border: 1px solid transparent; border-radius: 22px;
    background-color: #FFFFFF;
    background-image:
      linear-gradient(#FFFFFF, #FFFFFF),
      linear-gradient(135deg, #2563EB, #7C3AED 50%, #06B6D4);
    background-origin: border-box;
    background-clip: padding-box, border-box;
    box-shadow: 0 26px 56px -38px rgba(79, 70, 229, 1);
  }
  .empty-state .empty-icon {
    display: grid; place-items: center;
    width: 4rem; height: 4rem; margin: 0 auto .9rem;
    border-radius: 22px; font-size: 1.6rem; color: #FFFFFF;
    background-image: var(--mr-grad);
    box-shadow: 0 18px 34px -18px rgba(79, 70, 229, 1);
  }

  /* ---- decision boxes ---- */
  .decision {
    border-radius: 14px;
    box-shadow: 0 14px 30px -26px rgba(15, 23, 42, .9);
  }
  .decision-reject {
    background-image: linear-gradient(120deg, #D1FAE5 0%, #ECFDF5 60%, #F0FDFA 100%);
  }
  .decision-fail {
    background-image: linear-gradient(120deg, #FEF3C7 0%, #FFFBEB 60%, #FEF9C3 100%);
  }

  /* ---- code / console blocks ---- */
  pre.shiny-text-output {
    background-color: #F8FAFC;
    background-image: linear-gradient(135deg, rgba(37, 99, 235, .06), rgba(6, 182, 212, .06));
    border-color: rgba(199, 210, 254, .9);
    border-left: 4px solid var(--mr-indigo);
    border-radius: 12px;
  }
  .interp code { background: #EEF2FF; color: #4338CA; }

  /* ---- tables ---- */
  table.dataTable thead th {
    background-color: #FFFFFF !important;
    background-image: linear-gradient(180deg, #EEF2FF 0%, #E0F2FE 100%);
    color: #1E293B;
    border-bottom: 2px solid var(--mr-indigo) !important;
  }
  table.table > thead > tr > th {
    background-color: #FFFFFF;
    background-image: linear-gradient(180deg, #EEF2FF 0%, #E0F2FE 100%);
    color: #1E293B; border-bottom: 2px solid var(--mr-indigo);
  }
  .table-striped > tbody > tr:nth-of-type(odd) > * { background-color: #F8FAFF; }

  /* ---- buttons ---- */
  .btn { border-radius: 10px; }
  .btn-primary, .shiny-download-link {
    background-image: linear-gradient(135deg, #2563EB 0%, #4F46E5 60%, #7C3AED 100%);
    box-shadow: 0 10px 22px -14px rgba(79, 70, 229, 1);
  }
  .shiny-download-link {
    background-image: linear-gradient(135deg, #0D9488 0%, #0891B2 100%);
    box-shadow: 0 10px 22px -14px rgba(13, 148, 136, 1);
  }
  .btn-primary:hover, .shiny-download-link:hover { filter: brightness(1.07); }
  .btn-outline-primary, .btn-outline-secondary {
    border-color: rgba(199, 210, 254, 1); color: #4338CA;
  }
  .btn-outline-primary:hover, .btn-outline-secondary:hover {
    background-image: linear-gradient(135deg, #2563EB, #7C3AED);
    border-color: transparent; color: #FFFFFF;
  }

  /* ---- About / badges ---- */
  .about-badge {
    background-image: linear-gradient(135deg, #2563EB 0%, #7C3AED 70%, #06B6D4 100%);
    box-shadow: 0 8px 18px -12px rgba(79, 70, 229, 1);
  }
  .glossary-term { color: #4338CA; }

  /* ---- dark mode for the vivid layer ---- */
  [data-bs-theme='dark'] body {
    background-color: #070D19;
    background-image:
      radial-gradient(1050px 480px at 4% -14%, rgba(37, 99, 235, .42), transparent 62%),
      radial-gradient(900px 430px at 104% -8%, rgba(124, 58, 237, .38), transparent 60%),
      radial-gradient(820px 520px at 92% 108%, rgba(6, 182, 212, .32), transparent 62%),
      radial-gradient(680px 420px at -6% 104%, rgba(236, 72, 153, .22), transparent 60%);
  }
  [data-bs-theme='dark'] .navbar {
    background-image: linear-gradient(96deg,
      rgba(15, 23, 42, .96) 0%,
      rgba(30, 27, 75, .94) 58%,
      rgba(12, 45, 60, .94) 100%);
    border-bottom-color: rgba(51, 65, 85, .8);
  }
  [data-bs-theme='dark'] .app-title {
    background-image: linear-gradient(115deg, #F8FAFC 0%, #BFDBFE 45%, #DDD6FE 100%);
    -webkit-background-clip: text; background-clip: text;
    -webkit-text-fill-color: transparent;
  }
  [data-bs-theme='dark'] .app-logo {
    box-shadow: 0 10px 22px -12px rgba(96, 165, 250, .9);
  }
  [data-bs-theme='dark'] .card {
    background-color: #111C2E;
    border-color: #1F2A3C;
    box-shadow: 0 1px 2px rgba(0, 0, 0, .35), 0 22px 44px -34px rgba(0, 0, 0, 1);
  }
  [data-bs-theme='dark'] .card:hover {
    border-color: #3B4E6B;
    box-shadow: 0 24px 48px -30px rgba(96, 165, 250, .55);
  }
  [data-bs-theme='dark'] .card-header {
    background-color: #111C2E;
    background-image: linear-gradient(96deg,
      rgba(37, 99, 235, .28) 0%,
      rgba(124, 58, 237, .20) 48%,
      rgba(6, 182, 212, .18) 100%);
    border-bottom-color: #1F2A3C;
  }
  [data-bs-theme='dark'] .card-header .nav-tabs .nav-link { color: #CBD5E1; }
  [data-bs-theme='dark'] .card-header .nav-tabs .nav-link:hover {
    background-color: rgba(96, 165, 250, .16); color: #DBEAFE;
  }
  [data-bs-theme='dark'] .kpi {
    background-color: #111C2E;
    background-image: linear-gradient(160deg,
      var(--kpi-soft) 0%, rgba(17, 28, 46, .96) 58%, #111C2E 100%);
    border-color: #1F2A3C;
    box-shadow: 0 10px 24px -22px rgba(0, 0, 0, 1);
  }
  [data-bs-theme='dark'] .kpi.kpi-teal   { --kpi-soft: rgba(45, 212, 191, .18); }
  [data-bs-theme='dark'] .kpi.kpi-violet { --kpi-soft: rgba(167, 139, 250, .20); }
  [data-bs-theme='dark'] .kpi.kpi-amber  { --kpi-soft: rgba(251, 191, 36, .18); }
  [data-bs-theme='dark'] .kpi.kpi-green  { --kpi-soft: rgba(52, 211, 153, .18); }
  [data-bs-theme='dark'] .kpi.kpi-red    { --kpi-soft: rgba(248, 113, 113, .18); }
  [data-bs-theme='dark'] .kpi:hover { box-shadow: 0 22px 44px -26px var(--kpi); }
  [data-bs-theme='dark'] .kpi-label { color: var(--kpi-2); }
  [data-bs-theme='dark'] .kpi-icon { color: #FFFFFF; }
  [data-bs-theme='dark'] .kpi-sub { color: #94A3B8; }
  [data-bs-theme='dark'] .empty-state {
    background-color: #111C2E;
    background-image:
      linear-gradient(#111C2E, #111C2E),
      linear-gradient(135deg, #3B82F6, #8B5CF6 50%, #22D3EE);
    border-color: transparent;
  }
  [data-bs-theme='dark'] .decision-reject {
    background-image: linear-gradient(120deg, #06281C 0%, #052E22 100%);
  }
  [data-bs-theme='dark'] .decision-fail {
    background-image: linear-gradient(120deg, #2A1F06 0%, #291D08 100%);
  }
  [data-bs-theme='dark'] pre.shiny-text-output {
    background-color: #0B1220;
    background-image: linear-gradient(135deg, rgba(37, 99, 235, .16), rgba(6, 182, 212, .12));
    border-color: #1F2A3C; border-left: 4px solid #60A5FA;
  }
  [data-bs-theme='dark'] .interp code { background: #1E293B; color: #C7D2FE; }
  [data-bs-theme='dark'] table.dataTable thead th {
    background-color: #111C2E !important;
    background-image: linear-gradient(180deg, #1B2A44 0%, #152238 100%);
    color: #E2E8F0;
  }
  [data-bs-theme='dark'] table.table > thead > tr > th {
    background-color: #111C2E;
    background-image: linear-gradient(180deg, #1B2A44 0%, #152238 100%);
    color: #E2E8F0;
  }
  [data-bs-theme='dark'] .table-striped > tbody > tr:nth-of-type(odd) > * {
    background-color: #0F172A;
  }
  [data-bs-theme='dark'] .btn-outline-primary,
  [data-bs-theme='dark'] .btn-outline-secondary {
    border-color: #334155; color: #BFDBFE;
  }
  [data-bs-theme='dark'] .glossary-term { color: #A5B4FC; }
"

app_theme <- bs_theme(
  version = 5,
  primary = brand$primary,
  secondary = brand$secondary,
  success = brand$success,
  info = brand$accent,
  warning = brand$warning,
  danger = brand$danger,
  bg = brand$canvas,
  fg = brand$ink
) |>
  bs_add_rules(app_css)

# Theme colour with transparency, resolved by thematic at plot time.
# Falls back to the brand colours when thematic cannot resolve the app's CSS
# (for example outside a browser session).
theme_col <- function(name = c("accent", "fg", "bg"), alpha = 1) {
  nm <- match.arg(name)
  col <- tryCatch(thematic::thematic_get_option(nm), error = function(e) NULL)
  if (is.null(col) || !length(col) || is.na(col) || identical(as.character(col), "auto")) {
    col <- switch(nm, accent = brand$primary, fg = brand$ink, bg = brand$surface)
  }
  grDevices::adjustcolor(col, alpha.f = alpha)
}

# ---- small formatting helpers -------------------------------------------
num_cols <- function(d) names(d)[vapply(d, is.numeric, logical(1))]

cat_cols <- function(d, max_levels = 30) {
  v <- names(d)[vapply(d, function(x) is.character(x) || is.factor(x) ||
                         is.logical(x), logical(1))]
  v[vapply(d[v], function(x) length(unique(x)) <= max_levels, logical(1))]
}

binary_cols <- function(d) {
  names(d)[vapply(d, function(x) {
    if (!(is.character(x) || is.factor(x) || is.numeric(x) || is.logical(x)))
      return(FALSE)
    length(unique(stats::na.omit(x))) == 2
  }, logical(1))]
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0) return(character(0))
  ifelse(is.na(x), "NA",
         formatC(x, format = "f", digits = digits, big.mark = ","))
}

fmt_p <- function(p, digits = 4) {
  if (length(p) != 1) return(NA_character_)
  if (is.na(p)) return("NA")
  if (p < 1e-4) return("< 1e-04")
  formatC(p, format = "f", digits = digits)
}

pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

sig_word <- function(p, a = 0.05) {
  if (is.na(p)) return("not computed")
  if (p < a) "statistically significant" else "not statistically significant"
}

mode_of <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  m <- mean(x)
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}

kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(NA_real_)
  m <- mean(x)
  z <- (x - m) / s
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(z^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

cohens_d <- function(x, y) {
  n1 <- length(x); n2 <- length(y)
  if (n1 < 2 || n2 < 2) return(NA_real_)
  sp <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) /
               (n1 + n2 - 2))
  if (!is.finite(sp) || sp == 0) return(NA_real_)
  (mean(x) - mean(y)) / sp
}

d_magnitude <- function(d) {
  if (is.na(d)) return("undetermined")
  a <- abs(d)
  if (a < 0.2) "negligible" else if (a < 0.5) "small"
  else if (a < 0.8) "medium" else "large"
}

# Levene's test (Brown-Forsythe, median-centred); avoids a car dependency
levene_p <- function(y, g) {
  ok <- stats::complete.cases(y, g)
  y <- y[ok]; g <- factor(g[ok])
  if (nlevels(g) < 2) return(NA_real_)
  z <- abs(y - stats::ave(y, g, FUN = stats::median))
  fit <- try(stats::aov(z ~ g), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  summary(fit)[[1]][["Pr(>F)"]][1]
}

shapiro_p <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000) return(NA_real_)
  if (stats::sd(x) == 0) return(NA_real_)
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}

# ---- UI helpers ----------------------------------------------------------
copy_button <- function(label = "Copy interpretation") {
  tags$button(
    type = "button", class = "btn btn-sm btn-outline-secondary mt-2",
    onclick = paste0("navigator.clipboard.writeText(",
                     "this.closest('.interp').innerText); ",
                     "this.innerText = 'Copied';"),
    icon("copy"), " ", label
  )
}

decision_box <- function(p, h0, ha, alpha = 0.05, note = NULL) {
  reject <- !is.na(p) && p < alpha
  tags$div(
    class = paste("decision", if (reject) "decision-reject" else "decision-fail"),
    tags$div(class = "decision-verdict",
             if (is.na(p)) "Test not available"
             else if (reject) "Reject H\u2080" else "Fail to reject H\u2080"),
    tags$div(class = "decision-detail",
             sprintf("p-value = %s \u00b7 \u03b1 = %s", fmt_p(p), format(alpha))),
    tags$div(class = "decision-hyp",
             tags$b("H\u2080: "), h0, tags$br(),
             tags$b("H\u2081: "), ha),
    tags$div(class = "decision-detail",
             if (is.na(p)) "The test could not be computed for this data."
             else if (reject) "Evidence favours H\u2081 at this significance level."
             else "Not enough evidence to support H\u2081 at this significance level."),
    if (!is.null(note)) tags$div(class = "decision-detail", note)
  )
}

result_tabs <- function(results, interpretation, assumptions) {
  navset_card_tab(
    nav_panel(title = tagList(icon("table"), " Results"), results),
    nav_panel(title = tagList(icon("comment-dots"), " Interpretation"),
              div(class = "interp", interpretation, copy_button())),
    nav_panel(title = tagList(icon("check-double"), " Assumptions"), assumptions)
  )
}

# ---- built-in demo data --------------------------------------------------
# Generated once with a fixed seed, then the session RNG state is restored.
demo_students <- local({
  had_seed <- exists(".Random.seed", envir = globalenv())
  old <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  set.seed(20260101)
  n <- 220
  dept <- sample(c("Statistics", "Computer Science", "Economics", "Biology"),
                 n, replace = TRUE)
  year <- sample(1:4, n, replace = TRUE)
  hours <- round(stats::rnorm(n, 11 + 1.6 * year, 4), 1)
  hours[hours < 0] <- 0
  gpa <- round(2.1 + 0.11 * hours + stats::rnorm(n, 0, 0.42), 2)
  gpa <- pmin(pmax(gpa, 0.5), 4)
  exam <- round(40 + 2.0 * hours + stats::rnorm(n, 0, 9))
  d <- data.frame(
    id = sprintf("S%03d", seq_len(n)),
    department = dept,
    year = year,
    study_hours = hours,
    gpa = gpa,
    exam_score = exam,
    passed = ifelse(exam >= 70, "Yes", "No"),
    stringsAsFactors = FALSE
  )
  if (had_seed) assign(".Random.seed", old, envir = globalenv())
  d
})

# Larger samples ship as CSV in demo/ next to the app. They are read once at
# startup and skipped if the folder was not bundled with the deployment.
demo_csv <- function(name) {
  for (p in c(file.path("demo", name), file.path("www", "demo", name))) {
    if (file.exists(p)) return(utils::read.csv(p, stringsAsFactors = FALSE))
  }
  NULL
}

demo_sets <- list(
  "Student survey (simulated, 220 x 7)" = demo_students,
  "Iris (150 x 5)" = iris,
  "mtcars (32 x 11)" = mtcars,
  "ToothGrowth (60 x 3)" = ToothGrowth,
  "mpg (234 x 11)" = as.data.frame(ggplot2::mpg),
  "Employees: salaries (397 x 6)" = demo_csv("salaries.csv"),
  "Pima Indians diabetes (768 x 9)" = demo_csv("pima.csv")
)
demo_sets <- demo_sets[!vapply(demo_sets, is.null, logical(1))]

# ---- reading uploaded files ---------------------------------------------
clean_haven <- function(d) {
  d <- as.data.frame(haven::zap_labels(d))
  d[] <- lapply(d, function(x) {
    if (inherits(x, "haven_labelled")) as.numeric(x) else x
  })
  d
}

read_upload <- function(file, delim = ",", encoding = "UTF-8") {
  ext <- tolower(tools::file_ext(file$name))
  enc <- if (nzchar(encoding)) encoding else "UTF-8"
  d <- switch(
    ext,
    csv = utils::read.csv(file$datapath, sep = delim, fileEncoding = enc,
                          stringsAsFactors = FALSE, check.names = FALSE),
    tsv = utils::read.delim(file$datapath, fileEncoding = enc,
                            stringsAsFactors = FALSE, check.names = FALSE),
    txt = utils::read.delim(file$datapath, sep = delim, fileEncoding = enc,
                            stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = ,
    xls  = as.data.frame(readxl::read_excel(file$datapath)),
    sav  = {
      if (!requireNamespace("haven", quietly = TRUE))
        stop("Reading .sav files needs the haven package.", call. = FALSE)
      clean_haven(haven::read_sav(file$datapath))
    },
    dta  = {
      if (!requireNamespace("haven", quietly = TRUE))
        stop("Reading .dta files needs the haven package.", call. = FALSE)
      clean_haven(haven::read_dta(file$datapath))
    },
    rds  = as.data.frame(readRDS(file$datapath)),
    stop("Unsupported file type: .", ext, call. = FALSE)
  )
  if (!is.data.frame(d)) stop("The file did not contain a data frame.", call. = FALSE)
  if (ncol(d) == 0) stop("The file has no columns.", call. = FALSE)
  d
}

# ---- cleaning operations -------------------------------------------------
# Each operation is a closure plus a human-readable label, so the history can
# show exactly what was applied and undo can drop the last step.
apply_ops <- function(d, ops) {
  for (op in ops) d <- op$fn(d)
  d
}

op_impute <- function(vars, method) {
  list(
    label = sprintf("Imputed %s in %d variable(s)", method, length(vars)),
    fn = function(d) {
      for (v in intersect(vars, names(d))) {
        x <- d[[v]]
        if (!anyNA(x)) next
        fill <- switch(
          method,
          mean = if (is.numeric(x)) mean(x, na.rm = TRUE) else mode_of(x),
          median = if (is.numeric(x)) stats::median(x, na.rm = TRUE) else mode_of(x),
          mode = mode_of(x)
        )
        x[is.na(x)] <- fill
        d[[v]] <- x
      }
      d
    }
  )
}

op_complete_cases <- function(vars) {
  list(
    label = if (is.null(vars) || !length(vars)) "Dropped rows with any missing value"
            else sprintf("Dropped rows missing %s", paste(vars, collapse = ", ")),
    fn = function(d) {
      keep <- stats::complete.cases(if (is.null(vars) || !length(vars)) d else d[vars])
      d[keep, , drop = FALSE]
    }
  )
}

op_drop_duplicates <- function() {
  list(
    label = "Removed duplicate rows",
    fn = function(d) d[!duplicated(d), , drop = FALSE]
  )
}

op_transform <- function(var, how, new_name) {
  label_map <- c(log = "natural log", log10 = "log10", sqrt = "square root",
                 z = "z-score", minmax = "min-max scale",
                 factor = "to factor", numeric = "to numeric",
                 character = "to character")
  list(
    label = sprintf("Transformed %s: %s -> %s", var, label_map[[how]],
                    if (nzchar(new_name)) new_name else var),
    fn = function(d) {
      if (!var %in% names(d)) return(d)
      x <- d[[var]]
      out <- switch(
        how,
        log = if (is.numeric(x)) log(ifelse(x > 0, x, NA)) else x,
        log10 = if (is.numeric(x)) log10(ifelse(x > 0, x, NA)) else x,
        sqrt = if (is.numeric(x)) sqrt(ifelse(x >= 0, x, NA)) else x,
        z = if (is.numeric(x)) as.numeric(scale(x)) else x,
        minmax = if (is.numeric(x)) {
          r <- range(x, na.rm = TRUE)
          if (diff(r) == 0) x else (x - r[1]) / diff(r)
        } else x,
        factor = factor(x),
        numeric = suppressWarnings(as.numeric(x)),
        character = as.character(x)
      )
      nm <- if (nzchar(new_name)) new_name else var
      d[[nm]] <- out
      d
    }
  )
}

op_drop_outliers <- function(var, method, k) {
  list(
    label = sprintf("Removed %s outliers on %s (k = %s)",
                    method, var, format(k)),
    fn = function(d) {
      if (!var %in% names(d) || !is.numeric(d[[var]])) return(d)
      x <- d[[var]]
      keep <- if (method == "IQR") {
        q <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
        iqr <- diff(q)
        !is.na(x) & x >= q[1] - k * iqr & x <= q[2] + k * iqr
      } else {
        s <- stats::sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        if (!is.finite(s) || s == 0) rep(TRUE, length(x))
        else !is.na(x) & abs((x - m) / s) <= k
      }
      d[keep, , drop = FALSE]
    }
  )
}

# Turn dummy coefficient names ("departmentComputer Science") into labels
nice_terms <- function(nms, d, outcome) {
  out <- nms
  done <- rep(FALSE, length(nms))
  vars <- setdiff(names(d), outcome)
  vars <- vars[order(-nchar(vars))]
  for (v in vars) {
    if (is.numeric(d[[v]])) next
    hit <- !done & startsWith(nms, v) & nchar(nms) > nchar(v)
    out[hit] <- paste0(v, ": ", substring(nms[hit], nchar(v) + 1))
    done[hit] <- TRUE
  }
  out
}

# ---- descriptive helpers -------------------------------------------------
describe_numeric <- function(x, digits = 3) {
  x_ok <- x[is.finite(x)]
  n <- length(x_ok)
  out <- data.frame(
    n = length(x), missing = sum(is.na(x)), mean = NA_real_, se = NA_real_,
    sd = NA_real_, cv = NA_real_, min = NA_real_, q1 = NA_real_,
    median = NA_real_, q3 = NA_real_, max = NA_real_, iqr = NA_real_,
    range = NA_real_, skewness = NA_real_, kurtosis = NA_real_,
    mode = NA_real_, stringsAsFactors = FALSE
  )
  if (n == 0) return(out)
  out$mean <- mean(x_ok)
  out$sd <- stats::sd(x_ok)
  out$se <- out$sd / sqrt(n)
  out$cv <- if (out$mean != 0) out$sd / abs(out$mean) else NA_real_
  out$min <- min(x_ok)
  q <- stats::quantile(x_ok, c(.25, .5, .75), names = FALSE)
  out$q1 <- q[1]; out$median <- q[2]; out$q3 <- q[3]
  out$max <- max(x_ok)
  out$iqr <- q[3] - q[1]
  out$range <- out$max - out$min
  out$skewness <- skewness(x_ok)
  out$kurtosis <- kurtosis(x_ok)
  out$mode <- mode_of(x_ok)
  out
}

frequency_table <- function(x) {
  x <- as.character(x)
  tab <- table(x, useNA = "no")
  n_valid <- sum(tab)
  out <- data.frame(
    level = names(tab),
    count = as.integer(tab),
    percent = 100 * as.integer(tab) / length(x),
    valid_percent = if (n_valid) 100 * as.integer(tab) / n_valid else NA_real_,
    stringsAsFactors = FALSE
  )
  out$cumulative_percent <- cumsum(out$valid_percent)
  attr(out, "missing") <- sum(is.na(x))
  out
}

# =========================================================================
# UI
# =========================================================================
ui <- tagList(
tags$head(
  tags$link(rel = "icon", type = "image/png", href = "favicon.png")
),
page_navbar(
  id = "tabs",
  theme = app_theme,
  title = tags$span(class = "app-title",
                    tags$img(src = "app-logo.png",
                             class = "app-logo",
                             alt = "Mr Somali"),
                    "Mr Somali"),
  navbar_options = navbar_options(underline = FALSE, collapsible = TRUE),
  fillable = FALSE,
  sidebar = sidebar(
    width = 320,
    h6("Data source"),
    fileInput("file", NULL,
              buttonLabel = tagList(icon("folder-open"), " Choose file"),
              placeholder = "CSV, TSV, XLSX, SAV, DTA, RDS",
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".sav",
                         ".dta", ".rds")),
    selectInput("sample", "Or use a sample dataset",
                choices = c("(none)" = "", names(demo_sets))),
    conditionalPanel(
      "input.file !== null",
      accordion(
        open = FALSE,
        accordion_panel(
          title = tagList(icon("gear"), " CSV / text options"),
          value = "csv_options",
          selectInput("delim", "Delimiter",
                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t",
                                  Pipe = "|")),
          selectInput("encoding", "Encoding",
                      choices = c("UTF-8", "latin1", "Windows-1252"))
        )
      )
    ),
    div(class = "small-note mb-2",
        "With no file and no sample, the app analyses My.data from the R session."),
    actionButton("reload", "Reload data source", icon = icon("sync"),
                 width = "100%"),
    hr(),
    h6("Filters"),
    uiOutput("filters"),
    div(class = "small-note mb-2", textOutput("filter_status")),
    actionButton("reset", "Reset filters", icon = icon("undo"),
                 width = "100%"),
    hr(),
    h6("Analysis settings"),
    selectInput("alpha", "Significance level (\u03b1)",
                choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                selected = 0.05),
    selectInput("digits", "Decimal places", choices = 0:5, selected = 3),
    hr(),
    downloadButton("dl", "Download filtered data", width = "100%")
  ),
  nav_spacer(),

  # ---- Dashboard ---------------------------------------------------------
  nav_panel("Dashboard", icon = icon("tachometer-alt"),
    uiOutput("dash_ui")
  ),

  # ---- Data --------------------------------------------------------------
  nav_panel("Data", icon = icon("table"),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header(tagList(icon("table"), " Preview")), DTOutput("tab")),
      card(card_header(tagList(icon("columns"), " Variables")),
           tableOutput("var_info"))
    ),
    h5(icon("list"), " Structure"),
    verbatimTextOutput("str"),
    h5(icon("triangle-exclamation"), " Missing values per column"),
    tableOutput("nas")
  ),

  # ---- Cleaning ----------------------------------------------------------
  nav_panel("Cleaning", icon = icon("broom"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tagList(icon("user-slash"), " Missing values")),
        selectInput("cl_vars", "Variables", choices = NULL, multiple = TRUE),
        selectInput("cl_method", "Treatment",
                    choices = c("Complete cases only (drop rows)" = "drop",
                                "Mean imputation" = "mean",
                                "Median imputation" = "median",
                                "Mode imputation" = "mode")),
        actionButton("cl_apply_missing", "Apply", icon = icon("check"),
                     class = "btn-primary"),
        div(class = "small-note mt-2",
            "Imputation is a convenience for exploration. It cannot recover
             information that was never collected, and it biases standard errors
             downward. Report any imputation you use.")
      ),
      card(
        card_header(tagList(icon("clone"), " Duplicates")),
        textOutput("dup_count"),
        DTOutput("dup_preview"),
        actionButton("cl_drop_dups", "Remove duplicate rows",
                     icon = icon("trash"), class = "btn-primary mt-2")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tagList(icon("wand-magic-sparkles"), " Transform a variable")),
        selectInput("tr_var", "Variable", choices = NULL),
        selectInput("tr_op", "Transformation",
                    choices = c("Natural log" = "log", "log10" = "log10",
                                "Square root" = "sqrt", "Z-score" = "z",
                                "Min-max scale" = "minmax",
                                "To factor" = "factor",
                                "To numeric" = "numeric",
                                "To character" = "character")),
        textInput("tr_name", "New variable name (optional)"),
        actionButton("tr_apply", "Add transformation", icon = icon("plus"),
                     class = "btn-primary")
      ),
      card(
        card_header(tagList(icon("filter"), " Outliers")),
        selectInput("out_var", "Numeric variable", choices = NULL),
        selectInput("out_method", "Method",
                    choices = c("IQR rule" = "IQR", "Z-score" = "z")),
        sliderInput("out_k", "Threshold (k)", min = 1, max = 5, value = 3,
                    step = 0.5),
        plotOutput("out_plot", height = "220px"),
        actionButton("out_remove", "Remove flagged rows", icon = icon("trash"),
                     class = "btn-primary")
      )
    ),
    h5(icon("history"), " Transformation history"),
    tableOutput("clean_log"),
    div(class = "d-flex gap-2 mt-2",
        actionButton("clean_undo", "Undo last step", icon = icon("rotate-left")),
        actionButton("clean_reset", "Reset all cleaning",
                     icon = icon("arrows-rotate"))),
    h5(icon("eye"), " Result preview"),
    DTOutput("clean_preview")
  ),

  # ---- Descriptive -------------------------------------------------------
  nav_panel("Descriptive", icon = icon("chart-bar"),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("desc_y", "Numeric variable", choices = NULL),
      selectInput("desc_g", "Group by (optional)", choices = NULL),
      selectInput("desc_cat", "Categorical variable", choices = NULL)
    ),
    h5(icon("calculator"), " Numeric summary"),
    tableOutput("desc_tab"),
    layout_columns(col_widths = c(6, 6),
                   plotOutput("desc_hist"), plotOutput("desc_box")),
    h5(icon("list-ol"), " Frequency table"),
    tableOutput("desc_freq"),
    h5(icon("layer-group"), " Grouped summary"),
    tableOutput("desc_group")
  ),

  # ---- Tests -------------------------------------------------------------
  nav_panel("Tests", icon = icon("flask"),
    layout_columns(
      col_widths = c(5, 4, 3),
      selectInput("test_type", "Statistical test",
                  choices = c(
                    "One-sample t-test" = "t1",
                    "Independent-samples t-test (Welch)" = "t2",
                    "Paired-samples t-test" = "paired_t",
                    "One-way ANOVA" = "anova",
                    "Welch ANOVA" = "welch_anova",
                    "Mann-Whitney U test" = "mw",
                    "Wilcoxon signed-rank test" = "wilcox",
                    "Kruskal-Wallis test" = "kw",
                    "Friedman test" = "friedman"
                  )),
      selectInput("t_y", "Numeric variable", choices = NULL),
      selectInput("t_g", "Grouping variable", choices = NULL)
    ),
    uiOutput("test_extra"),
    actionButton("run_test", "Run test", icon = icon("play"),
                 class = "btn-primary"),
    br(), br(),
    uiOutput("test_out")
  ),

  # ---- Categories --------------------------------------------------------
  nav_panel("Categories", icon = icon("th-large"),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("ct_v1", "Row variable", choices = NULL),
      selectInput("ct_v2", "Column variable", choices = NULL),
      selectInput("ct_type", "Test",
                  choices = c("Chi-square test of independence" = "chisq",
                              "Fisher's exact test (2x2)" = "fisher",
                              "McNemar's test (paired 2x2)" = "mcnemar"))
    ),
    actionButton("run_ct", "Run test", icon = icon("play"),
                 class = "btn-primary"),
    br(), br(),
    uiOutput("ct_out")
  ),

  # ---- Correlation -------------------------------------------------------
  nav_panel("Correlation", icon = icon("exchange-alt"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("cor_x", "X variable", choices = NULL),
      selectInput("cor_y", "Y variable", choices = NULL),
      selectInput("cor_method", "Method",
                  choices = c("Pearson" = "pearson",
                              "Spearman (rank)" = "spearman",
                              "Kendall (rank)" = "kendall")),
      div(class = "d-flex align-items-end",
          actionButton("run_cor", "Analyse", icon = icon("play"),
                       class = "btn-primary w-100"))
    ),
    uiOutput("cor_out"),
    h5(icon("table"), " Correlation matrix of all numeric variables"),
    tableOutput("cor_mat"),
    h5(icon("fire"), " Heatmap"),
    plotOutput("cor_heat", height = "440px")
  ),

  # ---- Regression --------------------------------------------------------
  nav_panel("Regression", icon = icon("chart-line"),
    layout_columns(
      col_widths = c(4, 8),
      selectInput("reg_y", "Outcome variable (numeric)", choices = NULL),
      checkboxGroupInput("reg_x", "Predictors", choices = NULL,
                         inline = TRUE)
    ),
    actionButton("run_reg", "Fit linear model", icon = icon("play"),
                 class = "btn-primary"),
    br(), br(),
    uiOutput("reg_out")
  ),

  # ---- Logistic regression ----------------------------------------------
  nav_panel("Logistic", icon = icon("project-diagram"),
    layout_columns(
      col_widths = c(4, 6, 2),
      selectInput("log_y", "Binary outcome", choices = NULL),
      checkboxGroupInput("log_x", "Predictors", choices = NULL, inline = TRUE),
      sliderInput("log_cut", "Classification cut-off", min = 0.05, max = 0.95,
                  value = 0.5, step = 0.05)
    ),
    actionButton("run_log", "Fit logistic model", icon = icon("play"),
                 class = "btn-primary"),
    br(), br(),
    uiOutput("log_out")
  ),

  # ---- Visualisation studio ---------------------------------------------
  nav_panel("Visualise", icon = icon("palette"),
    viz_studio_ui("viz")
  ),

  # ---- Test finder -------------------------------------------------------
  nav_panel("Test Finder", icon = icon("compass"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(tagList(icon("question-circle"), " Which test should I use?")),
        radioButtons("fd_goal", "1. What is your research objective?",
                     choices = c("Compare groups or conditions" = "compare",
                                 "Test whether two variables are associated" = "assoc",
                                 "Predict or explain an outcome" = "predict",
                                 "Describe one variable" = "describe")),
        radioButtons("fd_dep", "2. What type is the outcome variable?",
                     choices = c("Numeric (continuous)" = "num",
                                 "Categorical (group membership)" = "cat")),
        radioButtons("fd_ngroups", "3. How many groups or conditions?",
                     choices = c("One sample" = "one", "Two groups" = "two",
                                 "Three or more groups" = "many")),
        radioButtons("fd_paired", "4. Are the observations paired or repeated?",
                     choices = c("Independent observations" = "ind",
                                 "Paired / repeated measures" = "paired")),
        radioButtons("fd_assume", "5. Are normality and equal-variance assumptions plausible?",
                     choices = c("Yes, or not sure" = "yes",
                                 "No, or clearly violated" = "no"))
      ),
      card(
        card_header(tagList(icon("lightbulb"), " Recommendation")),
        uiOutput("finder_out")
      )
    )
  ),

  # ---- GIS & spatial analytics -------------------------------------------
  nav_panel("GIS & Spatial Analytics", icon = icon("globe"),
    gis_ui("gis")
  ),

  # ---- Report ------------------------------------------------------------
  nav_panel("Report", icon = icon("file-download"),
    card(
      card_header(tagList(icon("file-lines"), " Research report")),
      layout_columns(
        col_widths = c(6, 6),
        textInput("rep_title", "Report title",
                  value = "Statistical Analysis Report"),
        textInput("rep_author", "Author")
      ),
      checkboxGroupInput(
        "rep_parts", "Include in the report",
        choices = c("Dataset overview" = "overview",
                    "Descriptive statistics" = "describe",
                    "Analysis history and results" = "history",
                    "Methods and limitations" = "methods"),
        selected = c("overview", "describe", "history", "methods"),
        inline = TRUE
      ),
      downloadButton("dl_report", "Download HTML report",
                     class = "btn-primary mt-2"),
      div(class = "small-note mt-2",
          "The report is a self-contained HTML file (open it in a browser and
           print to PDF if you need one). It records computations and
           interpretations only. Conclusions and academic judgements remain
           yours to write.")
    ),
    h5(icon("clock-rotate-left"), " Analysis history"),
    tableOutput("hist_tab"),
    div(class = "d-flex gap-2 mt-2",
        downloadButton("dl_hist", "Export history (CSV)"),
        actionButton("hist_clear", "Clear history", icon = icon("trash")))
  ),

  # ---- Help / About ------------------------------------------------------
  nav_panel("Help", icon = icon("question-circle"),
    div(class = "about-card",
      card(
        card_header(tagList(icon("book"), " How this app works")),
        card_body(
          p("Mr Somali follows the usual research workflow:"),
          tags$ol(
            tags$li(tags$b("Load data"), " - upload a file, choose a sample, or use My.data."),
            tags$li(tags$b("Check and clean"), " - inspect missing values, duplicates and outliers."),
            tags$li(tags$b("Describe"), " - summarise variables numerically and graphically."),
            tags$li(tags$b("Choose a test"), " - the Test Finder explains which test fits your design."),
            tags$li(tags$b("Run the analysis"), " - read the results, interpretation and assumptions tabs."),
            tags$li(tags$b("Report"), " - download a report of what was computed.")
          ),
          h5("Glossary"),
          tags$dl(
            tags$dt(class = "glossary-term", "p-value"),
            tags$dd("The probability of observing a result at least as extreme as
                     yours if the null hypothesis were true. It is not the
                     probability that the null hypothesis is true."),
            tags$dt(class = "glossary-term", "Significance level (alpha)"),
            tags$dd("The threshold you set before looking at the data. A result
                     is called statistically significant when p < alpha."),
            tags$dt(class = "glossary-term", "Effect size"),
            tags$dd("How large the observed difference or association is, on a
                     standard scale. It is what practical importance is judged on."),
            tags$dt(class = "glossary-term", "Confidence interval"),
            tags$dd("A range of plausible values for the population quantity,
                     constructed so that it covers the true value in 95% of
                     repeated samples."),
            tags$dt(class = "glossary-term", "Statistical power"),
            tags$dd("The chance of detecting an effect of a given size when it
                     really exists. Small samples often lack power."),
            tags$dt(class = "glossary-term", "Odds ratio"),
            tags$dd("The odds of the outcome in one group divided by the odds in
                     another. It is not a risk ratio and not a probability.")
          ),
          h5("Frequently asked questions"),
          tags$dl(
            tags$dt(class = "glossary-term", "My result is not significant. Does that prove no effect?"),
            tags$dd("No. It means the data are compatible with the null
                     hypothesis; a small sample may simply lack the power to
                     detect a real effect."),
            tags$dt(class = "glossary-term", "Can I compare more than two groups with a t-test?"),
            tags$dd("Running repeated t-tests inflates the false-positive rate.
                     Use ANOVA (or Kruskal-Wallis) and a post-hoc procedure."),
            tags$dt(class = "glossary-term", "When should I use a nonparametric test?"),
            tags$dd("When the outcome is ordinal, or when normality or equal
                     variances are clearly violated and the sample is small."),
            tags$dt(class = "glossary-term", "Is my data stored on the server?"),
            tags$dd("Uploads are written to a per-session temporary directory
                     and removed when the session ends. Nothing is shared
                     between sessions.")
          ),
          h5("About me"),
          p(class = "mb-1", tags$b("Ahmed Yusuf"), " - 4th-year BSc Statistics
            student, Dire Dawa University. I work with R, Shiny, Python, SPSS,
            Stata and Excel, and create educational content through MrSoomaali."),
          div(class = "d-flex flex-wrap gap-2 mt-2",
              tags$a(class = "btn btn-primary", href = "https://mrsoomaali.com/",
                     target = "_blank", rel = "noopener noreferrer",
                     icon("globe"), " mrsoomaali.com"),
              tags$a(class = "btn btn-outline-secondary",
                     href = "https://youtube.com/@Mrsoomaali", target = "_blank",
                     rel = "noopener noreferrer", icon("youtube"), " YouTube"))
        )
      )
    )
  ),

  # ---- visitor analytics: hidden panel, administrator only ---------------
  nav_panel_hidden("admin", track_admin_ui()),
  nav_item(actionLink("trk_admin_open", icon("user-shield"),
                      class = "nav-link",
                      title = "Administrator dashboard")),
  nav_item(input_dark_mode(id = "mode", mode = "light"))
),
track_assets()
)

# ---- shared UI helpers ---------------------------------------------------
# Minimal HTML table renderer, so computed tables can live inside renderUI()
html_table <- function(df, digits = 3) {
  if (is.null(df) || !nrow(df)) return(NULL)
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) fmt_num(col, digits) else as.character(col)
  })
  esc <- htmltools::htmlEscape
  head <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) {
    paste0("<tr>", paste0("<td>", esc(as.character(r)), "</td>", collapse = ""), "</tr>")
  })
  htmltools::HTML(sprintf(
    '<table class="table table-sm table-striped table-hover" style="width:100%%"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
    head, paste(rows, collapse = "")
  ))
}

kpi_card <- function(label, value, sub = NULL, icon = NULL, tone = "") {
  div(class = paste("kpi", tone),
      div(class = "kpi-label", label),
      div(class = "kpi-value", value),
      if (!is.null(sub)) div(class = "kpi-sub", sub),
      if (!is.null(icon)) tags$i(class = paste("kpi-icon", "fa", icon))
  )
}

welcome_ui <- function() {
  tagList(
    div(class = "empty-state mb-3",
      div(class = "empty-icon", icon("chart-pie")),
      h3("Welcome to Mr Somali"),
      p(class = "muted mb-3",
        "Upload a dataset to get descriptive statistics, hypothesis tests,
         regression models, publication-ready charts and plain-language
         interpretations - no code required."),
      p(class = "small-note",
        "Use the sidebar to choose a file, or pick one of the built-in sample
         datasets to explore the app first.")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header(tagList(icon("upload"), " 1. Bring your data")),
           card_body(
             p("CSV, TSV, Excel, SPSS, Stata and RDS files are supported, with
                configurable delimiter and encoding for text files."),
             p(class = "small-note", "Files are read into a temporary location
                for this session only.")
           )),
      card(card_header(tagList(icon("broom"), " 2. Check and clean")),
           card_body(
             p("Inspect missing values, duplicates and outliers, and apply
                transformations you can undo at any time."),
             p(class = "small-note", "The original file is never modified.")
           )),
      card(card_header(tagList(icon("flask"), " 3. Analyse and report")),
           card_body(
             p("Run tests and models, read the interpretation and assumption
                tabs, then download a report of what was computed."),
             p(class = "small-note", "Every number is calculated from your data.")
           ))
    ),
    h5(icon("lightbulb"), " Not sure which test to use?"),
    p(class = "muted", "Open the Test Finder tab and answer five short questions
      about your design. It recommends a method and explains why.")
  )
}

missing_plot <- function(d) {
  counts <- vapply(d, function(x) sum(is.na(x)), integer(1))
  df <- data.frame(variable = names(counts), missing = as.integer(counts))
  df <- df[df$missing > 0, , drop = FALSE]
  if (!nrow(df)) {
    return(ggplot() +
             annotate("text", x = 0, y = 0, label = "No missing values",
                      size = 5, colour = theme_col("fg", 0.6)) +
             theme_void())
  }
  df$variable <- factor(df$variable, levels = df$variable[order(df$missing)])
  ggplot(df, aes(x = missing, y = variable)) +
    geom_col(fill = "#7C3AED", width = 0.65) +
    geom_text(aes(label = missing), hjust = -0.25, size = 3.5,
              colour = theme_col("fg", 0.75)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = "Missing values", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major.y = element_blank())
}

# =========================================================================
# SERVER
# =========================================================================
server <- function(input, output, session) {

  # ---- visitor tracking (anonymous; see R/mod_track_*.R) -----------------
  # Tracking is decorative: a storage problem must never end a visitor session.
  tracker <- tryCatch(track_session(input, session), error = function(e) {
    message("tracking: disabled for this session - ", conditionMessage(e))
    list(session_id = NA_character_, store = NULL,
         record_analysis = function(...) invisible(FALSE),
         record_dataset = function(...) invisible(FALSE))
  })

  # A feature module that fails to start is logged and reported to the visitor,
  # but must not take the websocket down with it: an error raised directly in
  # the server body ends the session with "Disconnected from the server".
  safe_module <- function(label, expr) {
    tryCatch(force(expr), error = function(e) {
      message(sprintf("%s module did not start: %s", label, conditionMessage(e)))
      try(showNotification(
        sprintf("The %s tab could not start. The rest of the app is unaffected.", label),
        type = "error", duration = 15), silent = TRUE)
      NULL
    })
  }

  # ---- data source -------------------------------------------------------
  reload_n <- reactiveVal(0)
  observeEvent(input$reload, {
    reload_n(reload_n() + 1)
    showNotification("Data source reloaded.", type = "message")
  }, ignoreInit = TRUE)

  upload_error <- reactiveVal(NULL)

  source_data <- reactive({
    reload_n()
    if (!is.null(input$file)) {
      d <- tryCatch(
        read_upload(input$file,
                    delim = input$delim %||% ",",
                    encoding = input$encoding %||% "UTF-8"),
        error = function(e) {
          upload_error(conditionMessage(e))
          NULL
        }
      )
      return(d)
    }
    if (nzchar(input$sample %||% "")) return(demo_sets[[input$sample]])
    if (exists("My.data", envir = .GlobalEnv)) {
      d <- tryCatch(as.data.frame(get("My.data", envir = .GlobalEnv)),
                    error = function(e) NULL)
      if (!is.null(d) && ncol(d) > 0) return(d)
    }
    NULL
  })

  data_label <- reactive({
    if (!is.null(input$file)) return(input$file$name)
    if (nzchar(input$sample %||% "")) return(input$sample)
    if (!is.null(source_data())) return("My.data (R session)")
    "No data loaded"
  })

  # ---- tracking: data sources (file name only, never the data) -----------
  observeEvent(input$file, {
    f <- input$file
    if (is.null(f) || !nrow(f)) return()
    tracker$record_dataset(f$name[1], source = "upload")
  }, ignoreNULL = TRUE)

  observeEvent(input$sample, {
    s <- input$sample %||% ""
    if (nzchar(s)) tracker$record_dataset(s, source = "sample")
  }, ignoreInit = TRUE)

  observeEvent(upload_error(), {
    if (is.null(upload_error())) return()
    showNotification(paste("Could not read the file:", upload_error()),
                     type = "error", duration = 10)
  })

  # ---- filters -----------------------------------------------------------
  reset_n <- reactiveVal(0)
  observeEvent(input$reset, reset_n(reset_n() + 1), ignoreInit = TRUE)

  output$filters <- renderUI({
    reset_n()
    d <- source_data()
    req(d)
    panels <- lapply(names(d), function(v) {
      x <- d[[v]]
      hidden_label <- tags$span(class = "visually-hidden", v)
      if (is.numeric(x)) {
        rng <- range(x, na.rm = TRUE)
        if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)
        accordion_panel(
          title = v,
          sliderInput(paste0("f_", v), hidden_label, min = rng[1],
                      max = rng[2], value = rng)
        )
      } else if (length(unique(x)) <= 20) {
        lv <- sort(unique(x))
        accordion_panel(
          title = v,
          selectInput(paste0("f_", v), hidden_label, choices = lv,
                      selected = lv, multiple = TRUE)
        )
      } else {
        NULL
      }
    })
    panels <- Filter(Negate(is.null), panels)
    if (!length(panels)) return(helpText("No filterable columns in this data set."))
    do.call(accordion,
            c(list(id = "filter_acc", open = FALSE, multiple = TRUE), panels))
  })

  filtered_data <- reactive({
    d <- source_data()
    req(d)
    for (v in names(d)) {
      f <- input[[paste0("f_", v)]]
      if (is.null(f)) next
      x <- d[[v]]
      if (is.numeric(x)) {
        d <- d[!is.na(x) & x >= f[1] & x <= f[2], , drop = FALSE]
      } else {
        d <- d[x %in% f, , drop = FALSE]
      }
    }
    d
  })

  output$filter_status <- renderText({
    d <- source_data()
    req(d)
    n <- nrow(filtered_data())
    if (n == nrow(d)) sprintf("%d rows selected", n)
    else sprintf("%d of %d rows selected", n, nrow(d))
  })

  # ---- cleaning ----------------------------------------------------------
  clean_ops <- reactiveVal(list())
  observeEvent(source_data(), clean_ops(list()), ignoreInit = TRUE)

  dat <- reactive({
    d <- filtered_data()
    ops <- clean_ops()
    if (!length(ops)) return(d)
    tryCatch(apply_ops(d, ops), error = function(e) {
      showNotification(paste("A cleaning step could not be applied:",
                             conditionMessage(e)), type = "error")
      d
    })
  })

  output$clean_log <- renderTable({
    ops <- clean_ops()
    if (!length(ops)) {
      return(data.frame(Step = integer(0), Action = character(0)))
    }
    data.frame(Step = seq_along(ops),
               Action = vapply(ops, function(o) o$label, character(1)),
               check.names = FALSE)
  })

  observeEvent(input$clean_undo, {
    ops <- clean_ops()
    if (!length(ops)) {
      showNotification("Nothing to undo.", type = "warning")
      return()
    }
    clean_ops(ops[-length(ops)])
    showNotification("Last cleaning step undone.", type = "message")
  })

  observeEvent(input$clean_reset, {
    clean_ops(list())
    showNotification("All cleaning steps removed.", type = "message")
  })

  observeEvent(input$cl_apply_missing, {
    d <- filtered_data()
    req(d)
    vars <- input$cl_vars
    if (is.null(vars) || !length(vars)) {
      showNotification("Select at least one variable.", type = "warning")
      return()
    }
    op <- if (identical(input$cl_method, "drop")) {
      op_complete_cases(vars)
    } else {
      op_impute(vars, input$cl_method)
    }
    clean_ops(c(clean_ops(), list(op)))
    showNotification(paste(op$label, "applied."), type = "message")
  })

  observeEvent(input$cl_drop_dups, {
    d <- filtered_data()
    req(d)
    if (!sum(duplicated(d))) {
      showNotification("No duplicate rows were found.", type = "message")
      return()
    }
    op <- op_drop_duplicates()
    clean_ops(c(clean_ops(), list(op)))
    showNotification(paste(op$label, "applied."), type = "message")
  })

  observeEvent(input$tr_apply, {
    req(input$tr_var)
    op <- op_transform(input$tr_var, input$tr_op, trimws(input$tr_name %||% ""))
    clean_ops(c(clean_ops(), list(op)))
    updateTextInput(session, "tr_name", value = "")
    showNotification(paste(op$label, "applied."), type = "message")
  })

  outlier_state <- reactive({
    d <- dat()
    req(d, input$out_var)
    v <- input$out_var
    if (!v %in% names(d) || !is.numeric(d[[v]])) return(NULL)
    x <- d[[v]]
    k <- input$out_k
    flag <- if (identical(input$out_method, "IQR")) {
      q <- stats::quantile(x, c(.25, .75), na.rm = TRUE)
      iqr <- diff(q)
      !is.na(x) & (x < q[1] - k * iqr | x > q[2] + k * iqr)
    } else {
      s <- stats::sd(x, na.rm = TRUE)
      m <- mean(x, na.rm = TRUE)
      if (!is.finite(s) || s == 0) rep(FALSE, length(x))
      else !is.na(x) & abs((x - m) / s) > k
    }
    list(var = v, values = x, flag = flag)
  })

  output$out_plot <- renderPlot({
    o <- outlier_state()
    if (is.null(o)) {
      return(ggplot() + annotate("text", x = 0, y = 0,
                                 label = "Select a numeric variable",
                                 colour = "#64748B", size = 4.5) + theme_void())
    }
    df <- data.frame(value = o$values,
                     status = factor(ifelse(o$flag, "Flagged", "Within range"),
                                     levels = c("Within range", "Flagged")))
    ggplot(df, aes(x = status, y = value, colour = status)) +
      geom_boxplot(width = 0.35, outlier.shape = NA, colour = "#94A3B8") +
      geom_jitter(width = 0.12, size = 1.5, alpha = 0.6) +
      scale_colour_manual(values = c("Within range" = "#2563EB",
                                     "Flagged" = "#EF4444")) +
      labs(x = NULL, y = o$var, colour = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })

  observeEvent(input$out_remove, {
    o <- outlier_state()
    req(o)
    n_flag <- sum(o$flag)
    if (!n_flag) {
      showNotification("No rows are flagged at this threshold.", type = "message")
      return()
    }
    op <- op_drop_outliers(o$var, input$out_method, input$out_k)
    clean_ops(c(clean_ops(), list(op)))
    showNotification(sprintf("%d flagged row(s) removed.", n_flag), type = "message")
  })

  output$clean_preview <- renderDT({
    d <- dat()
    datatable(d, options = list(pageLength = 5, scrollX = TRUE,
                                dom = "ftip"),
              rownames = FALSE, class = "compact stripe")
  })

  output$dup_count <- renderText({
    d <- dat()
    n <- sum(duplicated(d))
    if (n == 0) "No duplicate rows in the current data."
    else sprintf("%d duplicate row(s) found (after filters and cleaning).", n)
  })

  output$dup_preview <- renderDT({
    d <- dat()
    dups <- d[duplicated(d) | duplicated(d, fromLast = TRUE), , drop = FALSE]
    datatable(dups, options = list(pageLength = 5, scrollX = TRUE, dom = "ft"),
              rownames = FALSE, class = "compact stripe")
  })

  # ---- dashboard ---------------------------------------------------------
  output$dash_ui <- renderUI({
    d <- source_data()
    if (is.null(d)) return(welcome_ui())
    dd <- dat()
    n <- nrow(dd); p <- ncol(dd)
    n_num <- length(num_cols(dd))
    n_cat <- length(cat_cols(dd))
    n_miss <- sum(is.na(dd))
    total_cells <- n * p
    miss_pct <- if (total_cells) n_miss / total_cells else 0
    n_dup <- sum(duplicated(dd))
    complete_rows <- if (p) sum(stats::complete.cases(dd)) else 0
    size <- format(utils::object.size(dd), units = "auto")
    quality <- if (miss_pct < 0.01 && n_dup == 0) "Good"
               else if (miss_pct < 0.1) "Fair" else "Needs attention"

    tagList(
      div(class = "dash-hero",
          div(
            div(class = "dash-hero-title", data_label()),
            div(class = "dash-hero-sub",
                sprintf("%d rows x %d columns after filters and cleaning",
                        n, p))
          ),
          div(class = "dash-hero-art", icon("chart-line"))),
      div(class = "kpi-grid",
        kpi_card("Observations", format(n, big.mark = ","),
                 sprintf("%s complete rows", format(complete_rows, big.mark = ",")),
                 "fa-users", ""),
        kpi_card("Variables", p,
                 sprintf("%d numeric / %d categorical", n_num, n_cat),
                 "fa-columns", "kpi-teal"),
        kpi_card("Missing cells", format(n_miss, big.mark = ","),
                 sprintf("%s of all cells", pct(miss_pct)), "fa-triangle-exclamation",
                 "kpi-amber"),
        kpi_card("Duplicate rows", format(n_dup, big.mark = ","),
                 if (n_dup) "Consider removing them" else "None detected",
                 "fa-clone", "kpi-violet"),
        kpi_card("Data quality", quality,
                 sprintf("In memory: %s", size), "fa-shield-alt",
                 if (identical(quality, "Good")) "kpi-green" else "kpi-red")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header(tagList(icon("chart-simple"), " Missing values by variable")),
             plotOutput("dash_missing", height = "260px")),
        card(card_header(tagList(icon("list-check"), " Quick checks")),
             card_body(
               tags$ul(class = "mb-0",
                 tags$li(sprintf("%d rows have at least one missing value.",
                                 n - complete_rows)),
                 tags$li(sprintf("%d constant column(s).",
                                 sum(vapply(dd, function(x)
                                   length(unique(stats::na.omit(x))) <= 1,
                                   logical(1))))),
                 tags$li(sprintf("%d variable(s) with more than 20 categories.",
                                 sum(vapply(dd, function(x)
                                   length(unique(stats::na.omit(x))) > 20,
                                   logical(1))))),
                 tags$li(sprintf("%d binary variable(s) available for logistic
                                 regression.", length(binary_cols(dd))))
               )
             ))
      )
    )
  })

  output$dash_missing <- renderPlot({
    d <- dat()
    req(d)
    missing_plot(d)
  })

  # ---- data tab ----------------------------------------------------------
  output$tab <- renderDT({
    datatable(dat(), filter = "top",
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, class = "compact stripe")
  })

  output$var_info <- renderTable({
    d <- dat()
    data.frame(
      variable = names(d),
      type = vapply(d, function(x) class(x)[1], character(1)),
      unique = vapply(d, function(x) length(unique(x)), integer(1)),
      missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
      check.names = FALSE
    )
  })

  output$str <- renderPrint({
    str(dat())
  })

  output$nas <- renderTable({
    d <- dat()
    data.frame(
      column = names(d),
      missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
      percent = vapply(d, function(x) round(100 * mean(is.na(x)), 2),
                       numeric(1)),
      check.names = FALSE
    )
  })

  # ---- keep variable choices in step with the data -----------------------
  observeEvent(source_data(), {
    d <- source_data()
    req(d)
    n <- num_cols(d)
    cc <- cat_cols(d)
    b <- binary_cols(d)

    updateSelectInput(session, "desc_y", choices = n)
    updateSelectInput(session, "desc_g", choices = c("(none)", cc))
    updateSelectInput(session, "desc_cat", choices = cc)

    updateSelectInput(session, "t_y", choices = n)
    updateSelectInput(session, "t_g", choices = cc)

    updateSelectInput(session, "ct_v1", choices = cc)
    updateSelectInput(session, "ct_v2", choices = cc,
                      selected = cc[min(2, length(cc))])

    updateSelectInput(session, "cor_x", choices = n)
    updateSelectInput(session, "cor_y", choices = n,
                      selected = n[min(2, length(n))])

    updateSelectInput(session, "reg_y", choices = n)
    updateCheckboxGroupInput(session, "reg_x", choices = n,
                             selected = if (length(n) > 1) n[2] else character(0))
    updateSelectInput(session, "log_y", choices = b)

    updateSelectInput(session, "cl_vars", choices = names(d))
    updateSelectInput(session, "tr_var", choices = names(d))
    updateSelectInput(session, "out_var", choices = n)
  })

  # Logistic predictors follow the chosen outcome
  observeEvent(list(source_data(), input$log_y), {
    d <- source_data()
    req(d)
    num <- setdiff(num_cols(d), input$log_y)
    catv <- setdiff(cat_cols(d), input$log_y)
    # setNames() errors on a zero-length vector, so build the list conditionally
    choices <- c(
      if (length(num)) setNames(num, paste0(num, " (numeric)")),
      if (length(catv)) setNames(catv, paste0(catv, " (categorical)"))
    )
    keep <- intersect(isolate(input$log_x), unname(choices))
    updateCheckboxGroupInput(session, "log_x", choices = choices,
                             selected = keep)
  }, ignoreNULL = FALSE)

  # ---- shared settings ---------------------------------------------------
  alpha_now <- reactive({
    a <- suppressWarnings(as.numeric(input$alpha)[1])
    if (length(a) == 0 || is.na(a)) 0.05 else a
  })

  digits_now <- reactive({
    d <- suppressWarnings(as.integer(input$digits)[1])
    if (length(d) == 0 || is.na(d)) 3 else d
  })

  # ---- descriptive statistics -------------------------------------------
  output$desc_tab <- renderTable({
    d <- dat()
    req(input$desc_y)
    s <- describe_numeric(d[[input$desc_y]])
    stats <- c("n", "Missing", "Mean", "Std. error", "Std. deviation",
               "Coefficient of variation", "Minimum", "1st quartile (Q1)",
               "Median", "3rd quartile (Q3)", "Maximum", "Interquartile range",
               "Range", "Skewness", "Kurtosis (excess)", "Mode")
    vals <- c(s$n, s$missing, s$mean, s$se, s$sd, s$cv, s$min, s$q1, s$median,
              s$q3, s$max, s$iqr, s$range, s$skewness, s$kurtosis, s$mode)
    data.frame(Statistic = stats,
               Value = c(format(s$n), format(s$missing),
                         fmt_num(vals[-(1:2)], digits_now())),
               check.names = FALSE)
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$desc_hist <- renderPlot({
    d <- dat()
    req(input$desc_y)
    x <- d[[input$desc_y]]
    hist(x, main = paste("Distribution of", input$desc_y),
         xlab = input$desc_y, col = theme_col("accent", 0.28),
         border = theme_col("bg"), las = 1)
  })

  output$desc_box <- renderPlot({
    d <- dat()
    req(input$desc_y)
    x <- d[[input$desc_y]]
    g <- input$desc_g
    if (is.null(g) || g == "(none)") {
      boxplot(x, ylab = input$desc_y, col = theme_col("accent", 0.28),
              border = theme_col("fg"), las = 1)
    } else {
      gf <- factor(d[[g]])
      boxplot(x ~ gf, las = 2, ylab = input$desc_y, xlab = g,
              col = seq_len(nlevels(gf)), border = theme_col("fg"))
    }
  })

  output$desc_freq <- renderTable({
    d <- dat()
    req(input$desc_cat)
    f <- frequency_table(d[[input$desc_cat]])
    out <- data.frame(
      Level = f$level,
      Count = f$count,
      Percent = round(f$percent, 1),
      `Valid percent` = round(f$valid_percent, 1),
      `Cumulative percent` = round(f$cumulative_percent, 1),
      check.names = FALSE
    )
    out
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$desc_group <- renderTable({
    d <- dat()
    req(input$desc_y)
    g <- input$desc_g
    if (is.null(g) || g == "(none)") {
      return(data.frame(Group = "(no grouping variable selected)"))
    }
    gf <- factor(d[[g]])
    x <- d[[input$desc_y]]
    rows <- lapply(levels(gf), function(l) {
      s <- describe_numeric(x[gf == l])
      data.frame(
        Group = l, n = s$n, Missing = s$missing,
        Mean = round(s$mean, digits_now()), SD = round(s$sd, digits_now()),
        Median = round(s$median, digits_now()),
        Min = round(s$min, digits_now()), Max = round(s$max, digits_now()),
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---- analysis history --------------------------------------------------
  hist_log <- reactiveVal(data.frame(
    time = character(0), module = character(0), analysis = character(0),
    statistic = character(0), p_value = character(0),
    decision = character(0), stringsAsFactors = FALSE
  ))

  add_history <- function(module, analysis, statistic, p, decision) {
    hist_log(rbind(hist_log(), data.frame(
      time = format(Sys.time(), "%Y-%m-%d %H:%M"),
      module = module, analysis = analysis, statistic = statistic,
      p_value = fmt_p(p), decision = decision, stringsAsFactors = FALSE)))
    # Recorded only when a result was actually produced, never on tab open.
    tracker$record_analysis(module, analysis,
                            dataset = tryCatch(isolate(data_label()),
                                               error = function(e) NA_character_))
  }

  # ---- GIS & spatial analytics -------------------------------------------
  safe_module("GIS and Spatial Analytics",
              gis_server("gis", app_data = source_data, data_label = data_label,
                         add_history = add_history))

  # ---- statistical tests: controls ---------------------------------------
  output$test_extra <- renderUI({
    d <- dat()
    extra <- switch(
      input$test_type,
      "t1" = numericInput("t_mu", "Hypothesised mean under H0", value = 0),
      "paired_t" = selectInput("t_y2", "Second numeric variable (paired)",
                               choices = setdiff(num_cols(d), input$t_y)),
      "wilcox" = selectInput("t_y2", "Second numeric variable (paired)",
                             choices = setdiff(num_cols(d), input$t_y)),
      "friedman" = checkboxGroupInput(
        "fr_vars", "Three or more numeric variables (wide format)",
        choices = num_cols(d), inline = TRUE),
      helpText("This test uses the numeric and grouping variables selected above.")
    )
    if (is.null(extra)) helpText("This test uses the variables selected above.") else extra
  })

  test_res <- reactiveVal(NULL)

  # ---- statistical tests: computation ------------------------------------
  compute_test <- function() {
    d <- dat()
    a <- alpha_now()
    dg <- digits_now()
    tt <- input$test_type
    yname <- input$t_y
    gname <- input$t_g
    need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
    verdict <- function(p) if (p < a) "Reject H0" else "Fail to reject H0"
    ci_txt <- function(ci) sprintf("[%s, %s]", fmt_num(ci[1], dg), fmt_num(ci[2], dg))
    p_txt <- function(p) sprintf("p = %s", fmt_p(p))
    assump <- function(required, checks, advice) {
      tagList(
        h6("Assumptions required"),
        tags$ul(lapply(required, tags$li)),
        h6("Checks on your data"),
        checks,
        h6("If an assumption is violated"),
        p(advice)
      )
    }
    sig_txt <- function(p) {
      if (p < a) sprintf("At alpha = %s the result is statistically significant.", format(a))
      else sprintf("At alpha = %s the result is not statistically significant. This is not evidence that the null hypothesis is true.", format(a))
    }

    if (tt == "t1") {
      need(!is.null(yname) && is.numeric(d[[yname]]), "Choose a numeric variable.")
      x <- d[[yname]]
      x <- x[is.finite(x)]
      need(length(x) >= 2, "At least two non-missing observations are needed.")
      mu0 <- suppressWarnings(as.numeric(input$t_mu %||% 0))
      if (!is.finite(mu0)) mu0 <- 0
      fit <- stats::t.test(x, mu = mu0)
      eff <- (mean(x) - mu0) / stats::sd(x)
      ci <- fit$conf.int
      tab <- data.frame(
        Quantity = c("n", "Mean", "SD", "Std. error", "Hypothesised mean",
                     "Difference", "t statistic", "df", "p-value", "95% CI for the mean"),
        Value = c(length(x), fmt_num(mean(x), dg), fmt_num(stats::sd(x), dg),
                  fmt_num(stats::sd(x) / sqrt(length(x)), dg), fmt_num(mu0, dg),
                  fmt_num(mean(x) - mu0, dg), fmt_num(fit$statistic, 3),
                  format(round(fit$parameter, 2)), fmt_p(fit$p.value),
                  ci_txt(ci)),
        check.names = FALSE
      )
      return(list(
        name = sprintf("One-sample t-test: %s", yname),
        p = fit$p.value,
        stat_text = sprintf("t(%s) = %s", format(round(fit$parameter, 2)),
                            fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the population mean of %s equals %s", yname, fmt_num(mu0, dg)),
        ha = sprintf("the population mean of %s differs from %s", yname, fmt_num(mu0, dg)),
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("A one-sample t-test compared the mean of %s in your data (M = %s, SD = %s) with the hypothesised value %s.",
                    yname, fmt_num(mean(x), dg), fmt_num(stats::sd(x), dg), fmt_num(mu0, dg))),
          h6("Statistical result"),
          p(sprintf("t(%s) = %s, %s. The mean is %s points away from the hypothesised value, with a 95%% confidence interval for the mean of %s.",
                    format(round(fit$parameter, 2)), fmt_num(fit$statistic, 3), p_txt(fit$p.value),
                    fmt_num(mean(x) - mu0, dg), ci_txt(ci))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Cohen's d = %s, a %s standardised difference (0.2 small, 0.5 medium, 0.8 large).",
                    fmt_num(eff, dg), d_magnitude(eff))),
          h6("How to write it up"),
          p(sprintf("A one-sample t-test showed that the mean of %s (M = %s, SD = %s) was %s the hypothesised value of %s, t(%s) = %s, %s.",
                    yname, fmt_num(mean(x), dg), fmt_num(stats::sd(x), dg),
                    if (fit$p.value < a) "significantly different from" else "not significantly different from",
                    fmt_num(mu0, dg), format(round(fit$parameter, 2)),
                    fmt_num(fit$statistic, 3), p_txt(fit$p.value)))
        ),
        assumptions = assump(
          c("Observations are independent of each other.",
            "The variable is approximately normally distributed, or the sample is large."),
          html_table(data.frame(
            Check = c("Shapiro-Wilk normality", "Sample size"),
            Result = c(paste0(fmt_p(shapiro_p(x)),
                              if (is.na(shapiro_p(x))) "" else if (shapiro_p(x) < 0.05) " (evidence against normality)" else " (no evidence against normality)"),
                       sprintf("%d observations%s", length(x),
                               if (length(x) >= 30) " - CLT makes the t-test fairly robust" else " - small sample")),
            check.names = FALSE)),
          "With clear non-normality and a small sample, use the Wilcoxon signed-rank test on the differences from the hypothesised value."
        )
      ))
    }

    if (tt == "t2") {
      need(!is.null(yname) && is.numeric(d[[yname]]), "Choose a numeric outcome variable.")
      need(!is.null(gname) && gname %in% names(d), "Choose a grouping variable.")
      g <- factor(d[[gname]])
      y <- d[[yname]]
      need(nlevels(g) == 2, sprintf("An independent-samples t-test needs exactly two groups; '%s' has %d.", gname, nlevels(g)))
      ok <- stats::complete.cases(y, g)
      y <- y[ok]; g <- droplevels(g[ok])
      x1 <- y[g == levels(g)[1]]; x2 <- y[g == levels(g)[2]]
      need(length(x1) >= 2 && length(x2) >= 2, "Both groups need at least two observations.")
      fit <- stats::t.test(x1, x2)
      eff <- cohens_d(x1, x2)
      ci <- fit$conf.int
      tab <- data.frame(
        Group = c(levels(g), "Difference"),
        n = c(length(x1), length(x2), NA),
        Mean = c(fmt_num(mean(x1), dg), fmt_num(mean(x2), dg), fmt_num(mean(x1) - mean(x2), dg)),
        SD = c(fmt_num(stats::sd(x1), dg), fmt_num(stats::sd(x2), dg), ""),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Welch independent-samples t-test: %s by %s", yname, gname),
        p = fit$p.value,
        stat_text = sprintf("t(%s) = %s", format(round(fit$parameter, 2)), fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the mean of %s is the same in both %s groups", yname, gname),
        ha = sprintf("the mean of %s differs between the %s groups", yname, gname),
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("A Welch independent-samples t-test compared the mean of %s between the two levels of %s (%s vs %s). Welch's version does not assume equal variances.",
                    yname, gname, levels(g)[1], levels(g)[2])),
          h6("Statistical result"),
          p(sprintf("t(%s) = %s, %s. The difference in means is %s (95%% CI %s).",
                    format(round(fit$parameter, 2)), fmt_num(fit$statistic, 3), p_txt(fit$p.value),
                    fmt_num(mean(x1) - mean(x2), dg), ci_txt(ci))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Cohen's d = %s (%s). The sign indicates the direction: %s.",
                    fmt_num(eff, dg), d_magnitude(eff),
                    if (!is.na(eff) && eff > 0) paste(levels(g)[1], "is higher") else paste(levels(g)[2], "is higher"))),
          h6("Practical meaning"),
          p("A statistically significant difference is not automatically an important one. Compare the effect size and the confidence interval with what would matter in your field."),
          h6("How to write it up"),
          p(sprintf("An independent-samples t-test found %s in %s between %s and %s, t(%s) = %s, %s, d = %s.",
                    if (fit$p.value < a) "a significant difference" else "no significant difference",
                    yname, levels(g)[1], levels(g)[2], format(round(fit$parameter, 2)),
                    fmt_num(fit$statistic, 3), p_txt(fit$p.value), fmt_num(eff, 2)))
        ),
        assumptions = assump(
          c("Independent observations (not paired or repeated).",
            "Approximately normal outcome within each group, or large groups.",
            "Equal variances are not required for Welch's t-test, but are reported below."),
          html_table(data.frame(
            Check = c(sprintf("Shapiro-Wilk: %s", levels(g)[1]),
                      sprintf("Shapiro-Wilk: %s", levels(g)[2]),
                      "Levene equal-variance test",
                      "Group sizes"),
            Result = c(fmt_p(shapiro_p(x1)), fmt_p(shapiro_p(x2)),
                       fmt_p(levene_p(y, g)),
                       sprintf("%d vs %d", length(x1), length(x2))),
            check.names = FALSE)),
          "For clearly non-normal data with small groups, use the Mann-Whitney U test; it compares distributions rather than means."
        )
      ))
    }

    if (tt == "paired_t") {
      y2name <- input$t_y2
      need(!is.null(input$t_y2) && nzchar(input$t_y2), "Choose the second numeric variable.")
      need(input$t_y2 != yname, "Choose two different variables.")
      x1 <- d[[yname]]; x2 <- d[[y2name]]
      need(is.numeric(x1) && is.numeric(x2), "Both variables must be numeric.")
      ok <- is.finite(x1) & is.finite(x2)
      x1 <- x1[ok]; x2 <- x2[ok]
      need(length(x1) >= 2, "At least two complete pairs are needed.")
      diff <- x1 - x2
      fit <- stats::t.test(x1, x2, paired = TRUE)
      eff <- mean(diff) / stats::sd(diff)
      ci <- fit$conf.int
      tab <- data.frame(
        Quantity = c("Pairs", "Mean difference", "SD of differences", "t statistic",
                     "df", "p-value", "95% CI for the mean difference"),
        Value = c(length(x1), fmt_num(mean(diff), dg), fmt_num(stats::sd(diff), dg),
                  fmt_num(fit$statistic, 3), format(round(fit$parameter, 2)),
                  fmt_p(fit$p.value), ci_txt(ci)),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Paired-samples t-test: %s vs %s", yname, y2name),
        p = fit$p.value,
        stat_text = sprintf("t(%s) = %s", format(round(fit$parameter, 2)), fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the mean difference between %s and %s is zero", yname, y2name),
        ha = sprintf("the mean difference between %s and %s is not zero", yname, y2name),
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("A paired-samples t-test compared %s and %s on the same %d cases. It tests the mean of the within-case differences, which removes between-person variability.",
                    yname, y2name, length(x1))),
          h6("Statistical result"),
          p(sprintf("Mean difference = %s (95%% CI %s), t(%s) = %s, %s.",
                    fmt_num(mean(diff), dg), ci_txt(ci), format(round(fit$parameter, 2)),
                    fmt_num(fit$statistic, 3), p_txt(fit$p.value))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Cohen's d for paired data = %s (%s), based on the standardised mean difference.",
                    fmt_num(eff, dg), d_magnitude(eff))),
          h6("How to write it up"),
          p(sprintf("A paired-samples t-test showed %s between %s (M = %s) and %s (M = %s), t(%s) = %s, %s.",
                    if (fit$p.value < a) "a significant difference" else "no significant difference",
                    yname, fmt_num(mean(x1), dg), y2name, fmt_num(mean(x2), dg),
                    format(round(fit$parameter, 2)), fmt_num(fit$statistic, 3), p_txt(fit$p.value)))
        ),
        assumptions = assump(
          c("The two measurements are paired within the same unit.",
            "The differences are approximately normally distributed.",
            "Observations are independent across units."),
          html_table(data.frame(
            Check = c("Shapiro-Wilk on the differences", "Pairs", "Mean difference"),
            Result = c(fmt_p(shapiro_p(diff)), as.character(length(diff)),
                       fmt_num(mean(diff), dg)),
            check.names = FALSE)),
          "If the differences are clearly non-normal, use the Wilcoxon signed-rank test instead."
        )
      ))
    }

    if (tt %in% c("anova", "welch_anova")) {
      need(!is.null(yname) && is.numeric(d[[yname]]), "Choose a numeric outcome variable.")
      need(!is.null(gname), "Choose a grouping variable.")
      g <- factor(d[[gname]])
      y <- d[[yname]]
      need(nlevels(g) >= 2, sprintf("ANOVA needs at least two groups; '%s' has %d.", gname, nlevels(g)))
      ok <- stats::complete.cases(y, g)
      y <- y[ok]; g <- droplevels(g[ok])
      need(nlevels(g) >= 2, "At least two groups with data are required.")
      aov_fit <- stats::aov(y ~ g)
      s <- summary(aov_fit)[[1]]
      eta2 <- s[["Sum Sq"]][1] / sum(s[["Sum Sq"]])
      welch <- identical(tt, "welch_anova")
      fit <- if (welch) stats::oneway.test(y ~ g, var.equal = FALSE) else NULL
      p <- if (welch) fit$p.value else s[["Pr(>F)"]][1]
      df1 <- if (welch) fit$parameter[1] else s[["Df"]][1]
      df2 <- if (welch) fit$parameter[2] else s[["Df"]][2]
      stat_v <- if (welch) fit$statistic else s[["F value"]][1]
      means <- data.frame(
        Group = levels(g),
        n = as.integer(table(g)),
        Mean = fmt_num(tapply(y, g, mean), dg),
        SD = fmt_num(tapply(y, g, stats::sd), dg),
        Median = fmt_num(tapply(y, g, stats::median), dg),
        check.names = FALSE
      )
      tk_txt <- ""
      if (!welch) {
        tk <- try(stats::TukeyHSD(aov_fit), silent = TRUE)
        if (!inherits(tk, "try-error")) {
          tk_txt <- paste(c("Tukey HSD pairwise comparisons",
                            utils::capture.output(print(tk))), collapse = "\n")
        }
      }
      return(list(
        name = sprintf("%s: %s by %s", if (welch) "Welch ANOVA" else "One-way ANOVA", yname, gname),
        p = p,
        stat_text = sprintf("F(%s, %s) = %s", format(round(df1, 2)),
                            format(round(df2, 2)), fmt_num(stat_v, 3)),
        decision_text = verdict(p),
        h0 = sprintf("all %d group means of %s are equal", nlevels(g), yname),
        ha = "at least one group mean differs",
        results = tagList(
          html_table(means),
          html_table(data.frame(
            Source = c(gname, "Residuals"),
            Df = c(df1, if (welch) NA else df2),
            `Sum Sq` = if (welch) NA else format(s[["Sum Sq"]], digits = 5),
            `Mean Sq` = if (welch) NA else format(s[["Mean Sq"]], digits = 5),
            `F` = c(fmt_num(stat_v, 3), ""),
            `p` = c(fmt_p(p), ""),
            check.names = FALSE
          )),
          tags$pre(class = "shiny-text-output",
                   paste(c(if (welch) utils::capture.output(print(fit)) else utils::capture.output(print(s)),
                           if (nzchar(tk_txt)) tk_txt else character(0)), collapse = "\n"))
        ),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("%s compared the mean of %s across %d levels of %s.",
                    if (welch) "A Welch ANOVA" else "A one-way ANOVA",
                    yname, nlevels(g), gname)),
          h6("Statistical result"),
          p(sprintf("F(%s, %s) = %s, %s.", format(round(df1, 2)), format(round(df2, 2)),
                    fmt_num(stat_v, 3), p_txt(p))),
          p(sig_txt(p)),
          h6("Effect size"),
          p(sprintf("Eta squared = %s, so about %s of the variability in %s is associated with %s. Benchmarks: 0.01 small, 0.06 medium, 0.14 large.",
                    fmt_num(eta2, dg), pct(eta2), yname, gname)),
          h6("Next step"),
          if (is.na(p) || p >= a) {
            p("The omnibus test is not significant, so there is no justification for post-hoc pairwise comparisons.")
          } else if (welch) {
            p("Welch's test does not come with Tukey comparisons. Use Games-Howell, or pairwise t-tests with a multiple-comparison correction.")
          } else {
            p("The omnibus test is significant. Use the Tukey HSD table in the Results tab to see which pairs differ, and remember that those intervals are already adjusted for multiple comparisons.")
          },
          h6("What not to conclude"),
          p("A significant F says at least one mean differs - not which one, and not that the differences are large enough to matter.")
        ),
        assumptions = assump(
          c("Independent observations.",
            "Approximately normal residuals within groups.",
            "Equal variances across groups (not required for the Welch version)."),
          html_table(data.frame(
            Check = c("Shapiro-Wilk on residuals", "Levene equal-variance test",
                      "Smallest group n"),
            Result = c(fmt_p(shapiro_p(stats::residuals(aov_fit))),
                       fmt_p(levene_p(y, g)),
                       as.character(min(table(g)))),
            check.names = FALSE)),
          "With unequal variances use Welch ANOVA; with clear non-normality use Kruskal-Wallis."
        )
      ))
    }

    if (tt == "mw") {
      need(!is.null(yname) && is.numeric(d[[yname]]), "Choose a numeric outcome variable.")
      need(!is.null(gname), "Choose a grouping variable.")
      g <- factor(d[[gname]]); y <- d[[yname]]
      need(nlevels(g) == 2, sprintf("Mann-Whitney needs two groups; '%s' has %d.", gname, nlevels(g)))
      ok <- stats::complete.cases(y, g); y <- y[ok]; g <- droplevels(g[ok])
      fit <- stats::wilcox.test(y ~ g, exact = FALSE, correct = TRUE)
      n1 <- sum(g == levels(g)[1]); n2 <- sum(g == levels(g)[2])
      U <- unname(fit$statistic)
      r <- 1 - (2 * U) / (n1 * n2)
      med <- tapply(y, g, stats::median)
      tab <- data.frame(
        Group = levels(g), n = c(n1, n2),
        Median = fmt_num(as.numeric(med), dg),
        `Mean rank` = fmt_num(as.numeric(tapply(rank(y), g, mean)), 2),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Mann-Whitney U test: %s by %s", yname, gname),
        p = fit$p.value,
        stat_text = sprintf("U = %s", fmt_num(U, 1)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the distribution of %s is the same in both %s groups", yname, gname),
        ha = sprintf("the distribution of %s differs between the %s groups", yname, gname),
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("The Mann-Whitney U test compared the ranks of %s between the two levels of %s. It does not assume normality, but it tests whole distributions, so a significant result can reflect a difference in spread rather than in the median.",
                    yname, gname)),
          h6("Statistical result"),
          p(sprintf("U = %s, %s. Median: %s = %s, %s = %s.",
                    fmt_num(U, 1), p_txt(fit$p.value), levels(g)[1],
                    fmt_num(med[[1]], dg), levels(g)[2], fmt_num(med[[2]], dg))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Rank-biserial correlation r = %s. Benchmarks: 0.1 small, 0.3 medium, 0.5 large.",
                    fmt_num(r, dg))),
          h6("Note"),
          p("Report medians and interquartile ranges rather than means and standard deviations for this test.")
        ),
        assumptions = assump(
          c("Independent observations.", "At least ordinal measurement.",
            "Similar distribution shapes if the result is to be read as a difference in medians."),
          html_table(data.frame(
            Check = c("Group sizes", "Shapiro-Wilk (reference only)"),
            Result = c(sprintf("%d vs %d", n1, n2),
                       sprintf("%s vs %s", fmt_p(shapiro_p(y[g == levels(g)[1]])),
                               fmt_p(shapiro_p(y[g == levels(g)[2]])))),
            check.names = FALSE)),
          "If the shapes differ substantially, describe the result as a difference in distributions and show both densities."
        )
      ))
    }

    if (tt == "wilcox") {
      need(!is.null(input$t_y2) && nzchar(input$t_y2), "Choose the second numeric variable.")
      need(input$t_y2 != yname, "Choose two different variables.")
      x1 <- d[[yname]]; x2 <- d[[input$t_y2]]
      ok <- is.finite(x1) & is.finite(x2); x1 <- x1[ok]; x2 <- x2[ok]
      need(length(x1) >= 2, "At least two complete pairs are needed.")
      diff <- x1 - x2
      fit <- stats::wilcox.test(x1, x2, paired = TRUE, exact = FALSE)
      n <- length(diff)
      z <- stats::qnorm(fit$p.value / 2)
      r <- abs(z) / sqrt(n)
      tab <- data.frame(
        Quantity = c("Pairs", "Median difference", "Positive differences",
                     "Negative differences", "V statistic", "p-value"),
        Value = c(n, fmt_num(stats::median(diff), dg), sum(diff > 0),
                  sum(diff < 0), fmt_num(fit$statistic, 1), fmt_p(fit$p.value)),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Wilcoxon signed-rank test: %s vs %s", yname, input$t_y2),
        p = fit$p.value,
        stat_text = sprintf("V = %s", fmt_num(fit$statistic, 1)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the median difference between %s and %s is zero", yname, input$t_y2),
        ha = sprintf("the median difference between %s and %s is not zero", yname, input$t_y2),
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("The Wilcoxon signed-rank test compared paired measurements of %s and %s using the ranks of the within-case differences.",
                    yname, input$t_y2)),
          h6("Statistical result"),
          p(sprintf("V = %s, %s, based on %d pairs. Median difference = %s.",
                    fmt_num(fit$statistic, 1), p_txt(fit$p.value), n,
                    fmt_num(stats::median(diff), dg))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("r = %s from the normal approximation (0.1 small, 0.3 medium, 0.5 large).", fmt_num(r, dg))),
          h6("Note"),
          p("Zero differences are dropped before ranking, so the effective sample can be smaller than the number of pairs.")
        ),
        assumptions = assump(
          c("Paired observations.", "Differences roughly symmetric.", "Independent pairs."),
          html_table(data.frame(
            Check = c("Pairs", "Non-zero differences"),
            Result = c(n, sum(diff != 0)), check.names = FALSE)),
          "If the differences are roughly normal, the paired t-test has more power; report both if they disagree."
        )
      ))
    }

    if (tt == "kw") {
      need(!is.null(yname) && is.numeric(d[[yname]]), "Choose a numeric outcome variable.")
      need(!is.null(gname), "Choose a grouping variable.")
      g <- factor(d[[gname]]); y <- d[[yname]]
      ok <- stats::complete.cases(y, g); y <- y[ok]; g <- droplevels(g[ok])
      need(nlevels(g) >= 2, "At least two groups are required.")
      fit <- stats::kruskal.test(y ~ g)
      k <- nlevels(g); n <- length(y)
      eps2 <- (unname(fit$statistic) - k + 1) / (n - k)
      med <- tapply(y, g, stats::median)
      tab <- data.frame(
        Group = levels(g), n = as.integer(table(g)),
        Median = fmt_num(as.numeric(med), dg),
        IQR = fmt_num(as.numeric(tapply(y, g, stats::IQR)), dg),
        `Mean rank` = fmt_num(as.numeric(tapply(rank(y), g, mean)), 2),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Kruskal-Wallis test: %s by %s", yname, gname),
        p = fit$p.value,
        stat_text = sprintf("H(%d) = %s", k - 1, fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("the distribution of %s is the same across all %s groups", yname, gname),
        ha = "at least one group differs",
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("The Kruskal-Wallis test is the nonparametric alternative to one-way ANOVA. It compares the ranks of %s across %d groups.",
                    yname, k)),
          h6("Statistical result"),
          p(sprintf("H(%d) = %s, %s.", k - 1, fmt_num(fit$statistic, 3), p_txt(fit$p.value))),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Epsilon squared = %s, the proportion of rank variance explained by group.",
                    fmt_num(eps2, dg))),
          h6("Next step"),
          if (is.na(fit$p.value) || fit$p.value >= a) {
            p("No post-hoc comparisons are justified by a non-significant omnibus test.")
          } else {
            p("Follow up with pairwise Mann-Whitney tests using a multiple-comparison correction such as Holm or Benjamini-Hochberg.")
          }
        ),
        assumptions = assump(
          c("Independent observations.", "At least ordinal outcome.",
            "Similar distribution shapes for a median interpretation."),
          html_table(data.frame(
            Check = c("Groups", "Smallest group"),
            Result = c(k, min(table(g))), check.names = FALSE)),
          "If distribution shapes differ, interpret the result as a difference in distributions rather than medians."
        )
      ))
    }

    if (tt == "friedman") {
      vars <- input$fr_vars
      need(length(vars) >= 3, "Select three or more numeric variables measured on the same units.")
      m <- as.matrix(d[vars])
      m <- m[stats::complete.cases(m), , drop = FALSE]
      need(nrow(m) >= 2, "At least two complete rows are needed.")
      fit <- stats::friedman.test(m)
      k <- ncol(m); n <- nrow(m)
      W <- unname(fit$statistic) / (n * (k - 1))
      tab <- data.frame(
        Variable = colnames(m),
        Median = fmt_num(apply(m, 2, stats::median), dg),
        check.names = FALSE
      )
      return(list(
        name = sprintf("Friedman test across %d repeated measurements", k),
        p = fit$p.value,
        stat_text = sprintf("chi-squared(%d) = %s", k - 1, fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = "the outcome is the same at every measurement occasion",
        ha = "at least one occasion differs",
        results = tagList(html_table(tab),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("The Friedman test is the nonparametric equivalent of a repeated-measures ANOVA. It ranks each case's values across the %d variables and asks whether the rank sums differ.", k)),
          h6("Statistical result"),
          p(sprintf("chi-squared(%d) = %s, %s on %d complete cases.",
                    k - 1, fmt_num(fit$statistic, 3), p_txt(fit$p.value), n)),
          p(sig_txt(fit$p.value)),
          h6("Effect size"),
          p(sprintf("Kendall's W = %s, where 0 means no agreement and 1 means perfect agreement in the rankings across occasions.",
                    fmt_num(W, dg))),
          h6("Note"),
          p("Only complete rows are used; the case count above tells you how much was dropped.")
        ),
        assumptions = assump(
          c("Measurements are repeated on the same units.",
            "The outcome is at least ordinal.", "Independent cases."),
          html_table(data.frame(Check = c("Variables", "Complete cases"),
                                Result = c(k, n), check.names = FALSE)),
          "For two occasions, use the paired t-test or the Wilcoxon signed-rank test."
        )
      ))
    }

  }

  observeEvent(input$run_test, {
    res <- tryCatch(compute_test(), error = function(e) e)
    if (inherits(res, "error")) {
      test_res(NULL)
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    test_res(res)
    add_history("Tests", res$name, res$stat_text, res$p, res$decision_text)
    showNotification("Test completed.", type = "message")
  })

  output$test_out <- renderUI({
    res <- test_res()
    if (is.null(res)) {
      return(div(class = "small-note",
        "Pick a test, choose your variables, then press Run test. The results,
         a plain-language interpretation and the assumption checks appear here."))
    }
    tagList(
      decision_box(res$p, res$h0, res$ha, alpha_now()),
      result_tabs(res$results, res$interpretation, res$assumptions)
    )
  })

  # ---- categorical analysis ---------------------------------------------
  ct_res <- reactiveVal(NULL)

  compute_ct <- function() {
    d <- dat()
    a <- alpha_now(); dg <- digits_now()
    v1 <- input$ct_v1; v2 <- input$ct_v2
    need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
    verdict <- function(p) if (p < a) "Reject H0" else "Fail to reject H0"
    need(!is.null(v1) && !is.null(v2), "Choose two categorical variables.")
    need(v1 != v2, "Choose two different variables.")
    tb <- table(d[[v1]], d[[v2]])
    need(all(dim(tb) >= 2), "Both variables need at least two observed categories.")
    observed <- data.frame(Level = NA, stringsAsFactors = FALSE)

    cramers_v <- function(tab) {
      ch <- suppressWarnings(stats::chisq.test(tab))
      sqrt(unname(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
    }
    strength <- function(v) {
      if (is.na(v)) "unknown"
      else if (v < 0.1) "negligible" else if (v < 0.3) "weak"
      else if (v < 0.5) "moderate" else "strong"
    }

    if (input$ct_type == "fisher") {
      need(all(dim(tb) == 2), "Fisher's exact test here is limited to 2 x 2 tables.")
      fit <- stats::fisher.test(tb)
      or <- unname(fit$estimate)
      ci <- fit$conf.int
      return(list(
        name = sprintf("Fisher's exact test: %s by %s", v1, v2),
        p = fit$p.value,
        stat_text = sprintf("odds ratio = %s", fmt_num(or, 3)),
        decision_text = verdict(fit$p.value),
        h0 = sprintf("%s and %s are independent", v1, v2),
        ha = sprintf("%s and %s are associated", v1, v2),
        results = tagList(html_table(as.data.frame.matrix(tb), dg),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p("Fisher's exact test asks whether the two categories are independent. It is exact, so it remains valid when expected counts are small."),
          h6("Statistical result"),
          p(sprintf("p = %s. Odds ratio = %s (95%% CI %s).",
                    fmt_p(fit$p.value), fmt_num(or, dg),
                    sprintf("[%s, %s]", fmt_num(ci[1], dg), fmt_num(ci[2], dg)))),
          p(sprintf("At alpha = %s this association is %s.", format(a),
                    if (fit$p.value < a) "statistically significant" else "not statistically significant")),
          h6("Reading the odds ratio"),
          p("An odds ratio of 1 means no association. Values above 1 mean higher odds in the first row category; values below 1 mean lower odds. An odds ratio is not a risk ratio and not a probability.")
        ),
        assumptions = tagList(
          h6("Assumptions required"),
          tags$ul(tags$li("Independent observations."),
                  tags$li("Both variables are categorical."),
                  tags$li("A 2 x 2 table (for this implementation).")),
          h6("Checks on your data"),
          html_table(data.frame(Check = c("Cells", "Minimum expected count"),
                                Result = c(prod(dim(tb)),
                                           fmt_num(min(stats::chisq.test(tb)$expected), 2)),
                                check.names = FALSE))
        )
      ))
    }

    if (input$ct_type == "mcnemar") {
      need(all(dim(tb) == 2), "McNemar's test needs a 2 x 2 table of paired binary measurements.")
      fit <- stats::mcnemar.test(tb, correct = TRUE)
      b <- tb[1, 2]; cc <- tb[2, 1]
      return(list(
        name = sprintf("McNemar's test: %s vs %s", v1, v2),
        p = fit$p.value,
        stat_text = sprintf("chi-squared(%d) = %s", fit$parameter, fmt_num(fit$statistic, 3)),
        decision_text = verdict(fit$p.value),
        h0 = "the two paired measurements have the same marginal proportions",
        ha = "the marginal proportions differ",
        results = tagList(html_table(as.data.frame.matrix(tb), dg),
                          tags$pre(class = "shiny-text-output",
                                   paste(utils::capture.output(print(fit)), collapse = "\n"))),
        interpretation = tagList(
          h6("What was tested"),
          p(sprintf("McNemar's test compares two paired binary measurements. It uses only the discordant pairs: %d cases changed from category 1 to 2, and %d changed the other way.", b, cc)),
          h6("Statistical result"),
          p(sprintf("chi-squared(%d) = %s, %s.", fit$parameter, fmt_num(fit$statistic, 3), fmt_p(fit$p.value))),
          p(sprintf("At alpha = %s the change is %s.", format(a),
                    if (fit$p.value < a) "statistically significant" else "not statistically significant")),
          h6("Caution"),
          p("A significant result means the proportions differ, which is not the same as agreement between the two measurements. Consider also reporting Cohen's kappa.")
        ),
        assumptions = tagList(
          h6("Assumptions required"),
          tags$ul(tags$li("The two measurements are paired on the same units."),
                  tags$li("Both variables are binary."),
                  tags$li("Discordant pairs are not too few.")),
          h6("Checks on your data"),
          html_table(data.frame(Check = c("Concordant pairs", "Discordant pairs"),
                                Result = c(sum(diag(tb)), b + cc),
                                check.names = FALSE))
        )
      ))
    }

    # chi-square of independence (default)
    ch <- suppressWarnings(stats::chisq.test(tb, correct = FALSE))
    small <- any(ch$expected < 5)
    if (small) {
      ch_sim <- suppressWarnings(stats::chisq.test(tb, simulate.p.value = TRUE, B = 10000))
    }
    v <- cramers_v(tb)
    expected <- data.frame(ch$expected, check.names = FALSE)
    # cbind.data.frame() already passes check.names = FALSE internally
    expected <- cbind(Category = rownames(tb), expected)
    observed <- cbind(Category = rownames(tb), as.data.frame.matrix(tb))
    row_pct <- cbind(Category = rownames(tb),
                     as.data.frame.matrix(round(100 * prop.table(tb, 1), 1)))
    note <- if (small) {
      "Some expected counts are below 5. The p-value from a 10,000-replicate simulation is shown alongside the asymptotic one."
    } else NULL

    # Effect measures for 2 x 2 tables: odds ratio, risk ratio, risk difference
    effect_block <- NULL
    if (all(dim(tb) == 2) && all(tb > 0)) {
      a11 <- tb[1, 1]; a12 <- tb[1, 2]; a21 <- tb[2, 1]; a22 <- tb[2, 2]
      zc <- stats::qnorm(1 - a / 2)
      p1 <- a11 / (a11 + a12); p2 <- a21 / (a21 + a22)
      orv <- (a11 * a22) / (a12 * a21)
      se_or <- sqrt(1 / a11 + 1 / a12 + 1 / a21 + 1 / a22)
      rrv <- p1 / p2
      se_rr <- sqrt(1 / a11 - 1 / (a11 + a12) + 1 / a21 - 1 / (a21 + a22))
      rdv <- p1 - p2
      se_rd <- sqrt(p1 * (1 - p1) / (a11 + a12) + p2 * (1 - p2) / (a21 + a22))
      effect_block <- tagList(
        h6("Effect measures (2 x 2 table)"),
        html_table(data.frame(
          Measure = c(sprintf("Risk when %s = %s", v1, rownames(tb)[1]),
                      sprintf("Risk when %s = %s", v1, rownames(tb)[2]),
                      sprintf("Odds ratio (%s vs %s)", rownames(tb)[1], rownames(tb)[2]),
                      sprintf("Risk ratio (%s vs %s)", rownames(tb)[1], rownames(tb)[2]),
                      "Risk difference"),
          Estimate = c(pct(p1), pct(p2), fmt_num(orv, dg), fmt_num(rrv, dg),
                       fmt_num(rdv, dg)),
          `Confidence interval` = c("", "",
                                    sprintf("[%s, %s]", fmt_num(exp(log(orv) - zc * se_or), dg),
                                            fmt_num(exp(log(orv) + zc * se_or), dg)),
                                    sprintf("[%s, %s]", fmt_num(exp(log(rrv) - zc * se_rr), dg),
                                            fmt_num(exp(log(rrv) + zc * se_rr), dg)),
                                    sprintf("[%s, %s]", fmt_num(rdv - zc * se_rd, dg),
                                            fmt_num(rdv + zc * se_rd, dg))),
          check.names = FALSE
        )),
        p(class = "small-note",
          "The odds ratio overstates the risk ratio when the outcome is common.
           The risk difference is on the scale of the outcome itself and is
           often the most interpretable measure for decisions.")
      )
    }

    return(list(
      name = sprintf("Chi-square test of independence: %s by %s", v1, v2),
      p = ch$p.value,
      stat_text = sprintf("chi-squared(%d) = %s", ch$parameter, fmt_num(ch$statistic, 3)),
      decision_text = verdict(ch$p.value),
      h0 = sprintf("%s and %s are independent", v1, v2),
      ha = sprintf("%s and %s are associated", v1, v2),
      note = note,
      results = tagList(
        h6("Observed counts"), html_table(observed, dg),
        h6("Row percentages"), html_table(row_pct, 1),
        h6("Expected counts under independence"), html_table(expected, 2),
        effect_block,
        tags$pre(class = "shiny-text-output",
                 paste(utils::capture.output(print(ch)),
                       if (small) paste("\nSimulated p-value (10,000 replications):",
                                        fmt_p(ch_sim$p.value)) else "",
                       collapse = "\n"))
      ),
      interpretation = tagList(
        h6("What was tested"),
        p(sprintf("The chi-square test of independence asked whether %s and %s are related in the population, comparing observed counts with the counts expected if the two variables were unrelated.",
                  v1, v2)),
        h6("Statistical result"),
        p(sprintf("chi-squared(%d) = %s, %s.", ch$parameter, fmt_num(ch$statistic, 3), fmt_p(ch$p.value))),
        p(sprintf("At alpha = %s the association is %s.", format(a),
                  if (ch$p.value < a) "statistically significant" else "not statistically significant")),
        h6("Effect size"),
        p(sprintf("Cramer's V = %s, a %s association (0.1 small, 0.3 moderate, 0.5 strong).", fmt_num(v, dg), strength(v))),
        h6("Direction"),
        p("Chi-square does not have a sign. Read the row percentages to describe which categories co-occur, and report the largest standardised residuals as the cells driving the result."),
        if (small) p("Because some expected counts are small, treat the asymptotic p-value cautiously; the simulated p-value is more reliable here.")
      ),
      assumptions = tagList(
        h6("Assumptions required"),
        tags$ul(tags$li("Independent observations; each case counted once."),
                tags$li("Categorical variables."),
                tags$li("Expected counts of about 5 or more in most cells.")),
        h6("Checks on your data"),
        html_table(data.frame(
          Check = c("Cells with expected count < 5", "Total cells",
                    "Smallest expected count"),
          Result = c(sum(ch$expected < 5), length(ch$expected),
                     fmt_num(min(ch$expected), 2)),
          check.names = FALSE)),
        h6("If an assumption is violated"),
        p("Use Fisher's exact test for 2 x 2 tables, or the simulated p-value reported here. Collapsing sparse categories is another option, if it is substantively defensible.")
      )
    ))
  }

  observeEvent(input$run_ct, {
    res <- tryCatch(compute_ct(), error = function(e) e)
    if (inherits(res, "error")) {
      ct_res(NULL)
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    ct_res(res)
    add_history("Categories", res$name, res$stat_text, res$p, res$decision_text)
    showNotification("Test completed.", type = "message")
  })

  output$ct_out <- renderUI({
    res <- ct_res()
    if (is.null(res)) {
      return(div(class = "small-note",
        "Choose two categorical variables and press Run test."))
    }
    tagList(
      decision_box(res$p, res$h0, res$ha, alpha_now(), note = res$note),
      result_tabs(res$results, res$interpretation, res$assumptions)
    )
  })

  # ---- correlation -------------------------------------------------------
  cor_res <- reactiveVal(NULL)

  compute_cor <- function() {
    d <- dat()
    a <- alpha_now(); dg <- digits_now()
    xv <- input$cor_x; yv <- input$cor_y
    method <- input$cor_method %||% "pearson"
    need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
    need(!is.null(xv) && !is.null(yv), "Choose two numeric variables.")
    need(xv != yv, "Choose two different variables.")
    need(is.numeric(d[[xv]]) && is.numeric(d[[yv]]),
         "Both variables must be numeric for correlation.")
    x <- d[[xv]]; y <- d[[yv]]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    need(length(x) >= 3, "At least three complete pairs are needed.")

    fit <- suppressWarnings(stats::cor.test(x, y, method = method,
                                            conf.level = 1 - a))
    r <- unname(fit$estimate)
    label <- switch(method, pearson = "Pearson r", spearman = "Spearman rho",
                    kendall = "Kendall tau")
    direction <- if (is.na(r)) "undetermined" else if (r > 0) "positive" else if (r < 0) "negative" else "none"
    magnitude <- if (is.na(r)) "undetermined" else {
      ar <- abs(r)
      if (ar < 0.1) "negligible" else if (ar < 0.3) "weak"
      else if (ar < 0.5) "moderate" else if (ar < 0.7) "fairly strong"
      else "strong"
    }
    ci_txt <- if (!is.null(fit$conf.int)) {
      sprintf("[%s, %s]", fmt_num(fit$conf.int[1], dg), fmt_num(fit$conf.int[2], dg))
    } else "not available for this method"

    tab <- data.frame(
      Quantity = c("Variable pair", "Method", "n", label, "p-value",
                   "95% confidence interval"),
      Value = c(paste(xv, "and", yv), method,
                as.character(length(x)), fmt_num(r, dg), fmt_p(fit$p.value), ci_txt),
      check.names = FALSE
    )

    return(list(
      name = sprintf("%s correlation: %s and %s", tools::toTitleCase(method), xv, yv),
      p = fit$p.value,
      stat_text = sprintf("%s = %s", label, fmt_num(r, dg)),
      decision_text = if (fit$p.value < a) "Reject H0" else "Fail to reject H0",
      x = xv, y = yv,
      h0 = sprintf("there is no %s correlation between %s and %s", method, xv, yv),
      ha = sprintf("there is a %s correlation between %s and %s", method, xv, yv),
      results = tagList(
        html_table(tab),
        h6("Scatterplot"),
        plotOutput("cor_scatter", height = "340px"),
        tags$pre(class = "shiny-text-output",
                 paste(utils::capture.output(print(fit)), collapse = "\n"))
      ),
      interpretation = tagList(
        h6("What was tested"),
        p(sprintf("The %s correlation measures how closely %s and %s move together on a monotonic scale from -1 to 1.",
                  method, xv, yv)),
        h6("Statistical result"),
        p(sprintf("%s = %s, %s, based on %d complete pairs. The 95%% confidence interval is %s.",
                  label, fmt_num(r, dg), sprintf("p = %s", fmt_p(fit$p.value)),
                  length(x), ci_txt)),
        p(sprintf("The correlation is %s and %s, and at alpha = %s it is %s.",
                  direction, magnitude, format(a),
                  if (fit$p.value < a) "statistically significant" else "not statistically significant")),
        h6("What it means"),
        if (method == "pearson") {
          p(sprintf("R squared for this pair is %s, so %s of the variance in one variable is linearly shared with the other.",
                    fmt_num(r^2, dg), pct(r^2)))
        } else {
          p("Rank correlations measure monotonic association. They are less sensitive to outliers and do not require normality.")
        },
        h6("Cautions"),
        p("Correlation does not establish causation. A third variable, reverse causation, or selection effects can produce any correlation you observe."),
        p("A non-significant correlation is not proof of no relationship: the test detects linear (or monotonic) association only, and small samples have little power.")
      ),
      assumptions = tagList(
        h6("Assumptions required"),
        tags$ul(
          tags$li("Independent pairs of observations."),
          tags$li("Pearson: both variables roughly normal, relationship roughly linear, no extreme outliers."),
          tags$li("Spearman and Kendall: at least ordinal data, monotonic relationship.")
        ),
        h6("Checks on your data"),
        html_table(data.frame(
          Check = c(sprintf("Shapiro-Wilk: %s", xv), sprintf("Shapiro-Wilk: %s", yv),
                    "Pairs", "Share of extreme pairs (|z| > 3.5)"),
          Result = c(fmt_p(shapiro_p(x)), fmt_p(shapiro_p(y)),
                     length(x),
                     pct(mean(abs(scale(x)) > 3.5 | abs(scale(y)) > 3.5))),
          check.names = FALSE
        )),
        h6("If an assumption is violated"),
        p("Use Spearman's rho when the relationship is monotonic but not linear, or when outliers dominate Pearson's r.")
      )
    ))
  }

  observeEvent(input$run_cor, {
    res <- tryCatch(compute_cor(), error = function(e) e)
    if (inherits(res, "error")) {
      cor_res(NULL)
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    cor_res(res)
    add_history("Correlation", res$name, res$stat_text, res$p, res$decision_text)
    showNotification("Correlation computed.", type = "message")
  })

  output$cor_out <- renderUI({
    res <- cor_res()
    if (is.null(res)) {
      return(div(class = "small-note",
        "Choose two numeric variables and a method, then press Analyse."))
    }
    tagList(
      decision_box(res$p, res$h0, res$ha, alpha_now()),
      result_tabs(res$results, res$interpretation, res$assumptions)
    )
  })

  output$cor_scatter <- renderPlot({
    res <- cor_res()
    req(res)
    d <- dat()
    df <- data.frame(x = d[[res$x]], y = d[[res$y]])
    df <- df[stats::complete.cases(df), , drop = FALSE]
    ggplot(df, aes(x = x, y = y)) +
      geom_point(colour = "#2563EB", alpha = 0.65, size = 2) +
      geom_smooth(method = "lm", formula = y ~ x, colour = "#14B8A6",
                  fill = "#14B8A6", alpha = 0.15, linewidth = 1) +
      labs(x = res$x, y = res$y) +
      theme_minimal(base_size = 13)
  })

  output$cor_mat <- renderTable({
    d <- dat()
    n <- num_cols(d)
    req(length(n) >= 2)
    m <- round(stats::cor(d[n], use = "pairwise.complete.obs"), 2)
    cbind(variable = rownames(m), as.data.frame(m))
  }, rownames = FALSE, striped = TRUE, hover = TRUE, width = "100%")

  output$cor_heat <- renderPlot({
    d <- dat()
    n <- num_cols(d)
    req(length(n) >= 2)
    m <- stats::cor(d[n], use = "pairwise.complete.obs")
    df <- expand.grid(Var1 = rownames(m), Var2 = colnames(m),
                      stringsAsFactors = FALSE)
    df$r <- as.vector(m)
    ggplot(df, aes(x = Var2, y = Var1, fill = r)) +
      geom_tile(colour = "white", linewidth = 0.6) +
      geom_text(aes(label = fmt_num(r, 2)),
                size = 3.2, colour = ifelse(abs(df$r) > 0.6, "white", "#1E293B")) +
      scale_fill_gradient2(low = "#EF4444", mid = "#F8FAFC", high = "#2563EB",
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1),
            panel.grid = element_blank())
  })

  # ---- linear regression -------------------------------------------------
  reg_res <- reactiveVal(NULL)

  compute_reg <- function() {
    d <- dat()
    a <- alpha_now(); dg <- digits_now()
    yv <- input$reg_y; xv <- input$reg_x
    need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
    need(!is.null(yv) && is.numeric(d[[yv]]), "Choose a numeric outcome.")
    need(length(xv) >= 1, "Choose at least one predictor.")
    need(!yv %in% xv, "The outcome cannot also be a predictor.")
    vars <- c(yv, xv)
    dd <- d[stats::complete.cases(d[vars]), vars, drop = FALSE]
    need(nrow(dd) > length(xv) + 1, "Not enough complete rows to fit the model.")
    fit <- stats::lm(stats::reformulate(xv, yv), data = dd)
    s <- summary(fit)
    co <- s$coefficients
    ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
    p_col <- co[, ncol(co)]
    coef_tab <- data.frame(
      Predictor = rownames(co),
      B = fmt_num(co[, 1], dg),
      `Std. error` = fmt_num(co[, 2], dg),
      t = fmt_num(co[, 3], 3),
      p = vapply(p_col, fmt_p, character(1)),
      `95% CI` = if (!is.null(ci)) {
        sprintf("[%s, %s]", fmt_num(ci[, 1], dg), fmt_num(ci[, 2], dg))
      } else "NA",
      decision = ifelse(p_col < a, "Reject H0", "Fail to reject H0"),
      check.names = FALSE
    )
    rownames(coef_tab) <- NULL

    vif_tab <- local({
      mm <- stats::model.matrix(fit)[, -1, drop = FALSE]
      if (ncol(mm) < 2) return(NULL)
      vals <- vapply(colnames(mm), function(v) {
        yv2 <- mm[, v]
        X <- mm[, setdiff(colnames(mm), v), drop = FALSE]
        r2 <- summary(stats::lm(yv2 ~ X))$r.squared
        if (!is.finite(r2) || r2 >= 1) NA_real_ else 1 / (1 - r2)
      }, numeric(1))
      data.frame(Predictor = names(vals), VIF = round(vals, 2),
                 check.names = FALSE)
    })

    fstat <- s$fstatistic
    p_f <- if (is.null(fstat)) NA_real_ else
      stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

    fit_tab <- data.frame(
      Measure = c("Rows used", "R squared", "Adjusted R squared", "Residual standard error",
                  "F statistic", "Model p-value", "AIC"),
      Value = c(nrow(dd), fmt_num(s$r.squared, dg), fmt_num(s$adj.r.squared, dg),
                fmt_num(s$sigma, dg),
                if (is.null(fstat)) "NA" else
                  sprintf("F(%d, %d) = %s", fstat[2], fstat[3], fmt_num(fstat[1], 3)),
                fmt_p(p_f), fmt_num(stats::AIC(fit), 1)),
      check.names = FALSE
    )

    sig_rows <- coef_tab[p_col < a & rownames(co) != "(Intercept)", , drop = FALSE]

    return(list(
      name = sprintf("Linear regression: %s", yv),
      p = p_f,
      stat_text = if (is.null(fstat)) "F not available" else
        sprintf("F(%d, %d) = %s", fstat[2], fstat[3], fmt_num(fstat[1], 3)),
      decision_text = if (is.na(p_f)) "Not computed" else if (p_f < a) "Reject H0" else "Fail to reject H0",
      h0 = "all slopes are zero",
      ha = "at least one predictor is linearly related to the outcome",
      results = tagList(
        h6("Model fit"), html_table(fit_tab),
        h6("Coefficients"), html_table(coef_tab),
        if (!is.null(vif_tab)) tagList(h6("Multicollinearity (VIF)"), html_table(vif_tab)),
        tags$pre(class = "shiny-text-output",
                 paste(utils::capture.output(print(s)), collapse = "\n"))
      ),
      interpretation = tagList(
        h6("What was fitted"),
        p(sprintf("An ordinary least squares regression predicted %s from %s, using %d complete rows.",
                  yv, paste(xv, collapse = ", "), nrow(dd))),
        h6("Overall model"),
        p(sprintf("The model explains %s of the variance in %s (adjusted R squared = %s), %s.",
                  pct(s$r.squared), yv, fmt_num(s$adj.r.squared, dg),
                  if (is.na(p_f)) "" else if (p_f < a)
                    sprintf("and the overall F test is significant (p = %s)", fmt_p(p_f))
                  else sprintf("but the overall F test is not significant (p = %s)", fmt_p(p_f)))),
        h6("Individual predictors"),
        if (!nrow(sig_rows)) {
          p(sprintf("No slope is significant at alpha = %s. Report this honestly rather than trimming predictors until something becomes significant.", format(a)))
        } else {
          tagList(
            p(sprintf("At alpha = %s, %d predictor(s) have slopes distinguishable from zero:", format(a), nrow(sig_rows))),
            tags$ul(lapply(seq_len(nrow(sig_rows)), function(i) {
              tags$li(sprintf("%s: B = %s (95%% CI %s), p = %s. Holding the other predictors fixed, a one-unit increase is associated with a change of %s in %s.",
                              sig_rows$Predictor[i], sig_rows$B[i], sig_rows$`95% CI`[i],
                              sig_rows$p[i], sig_rows$B[i], yv))
            }))
          )
        },
        h6("Cautions"),
        p("Regression coefficients describe association adjusted for the other predictors, not causal effects. Check the residual plots before trusting the standard errors, and watch the VIF table for predictors that duplicate each other.")
      ),
      assumptions = tagList(
        h6("Assumptions required"),
        tags$ul(
          tags$li("Linearity between each predictor and the outcome."),
          tags$li("Independent errors with constant variance (homoscedasticity)."),
          tags$li("Approximately normal residuals for inference in small samples."),
          tags$li("No severe multicollinearity.")
        ),
        h6("Checks on your data"),
        html_table(data.frame(
          Check = c("Shapiro-Wilk on residuals", "Breusch-Pagan (heteroscedasticity)",
                    "Largest VIF", "Rows used"),
          Result = c(fmt_p(shapiro_p(stats::residuals(fit))),
                     fmt_p(bp_p(fit)),
                     if (is.null(vif_tab)) "n/a" else
                       fmt_num(max(vif_tab$VIF, na.rm = TRUE), 2),
                     nrow(dd)),
          check.names = FALSE
        )),
        h6("Residual diagnostics"),
        plotOutput("reg_diag", height = "420px"),
        h6("If an assumption is violated"),
        p("Non-constant variance calls for robust standard errors or a transformation of the outcome. Severe non-normality with a small sample argues for bootstrap intervals. VIF above about 10 means the coefficient standard errors are inflated by collinearity.")
      )
    ))
  }

  bp_p <- function(fit) {
    e2 <- stats::residuals(fit)^2
    aux <- stats::lm(e2 ~ stats::fitted(fit))
    stat <- length(e2) * summary(aux)$r.squared
    stats::pchisq(stat, df = 1, lower.tail = FALSE)
  }

  observeEvent(input$run_reg, {
    res <- tryCatch(compute_reg(), error = function(e) e)
    if (inherits(res, "error")) {
      reg_res(NULL)
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    reg_res(res)
    add_history("Regression", res$name, res$stat_text, res$p, res$decision_text)
    showNotification("Model fitted.", type = "message")
  })

  output$reg_out <- renderUI({
    res <- reg_res()
    if (is.null(res)) {
      return(div(class = "small-note",
        "Choose an outcome and one or more predictors, then press Fit linear model."))
    }
    tagList(
      decision_box(res$p, res$h0, res$ha, alpha_now()),
      result_tabs(res$results, res$interpretation, res$assumptions)
    )
  })

  output$reg_diag <- renderPlot({
    d <- dat(); yv <- input$reg_y; xv <- input$reg_x
    req(yv, length(xv) >= 1)
    dd <- d[stats::complete.cases(d[c(yv, xv)]), c(yv, xv), drop = FALSE]
    fit <- stats::lm(stats::reformulate(xv, yv), data = dd)
    op <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1))
    on.exit(graphics::par(op), add = TRUE)
    plot(fit, which = 1:4)
  })

  # ---- logistic regression ----------------------------------------------
  log_res <- reactiveVal(NULL)

  auc <- function(p, y) {
    n1 <- sum(y == 1); n0 <- sum(y == 0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }

  compute_log <- function() {
    d <- dat()
    a <- alpha_now(); dg <- digits_now()
    yv <- input$log_y; xv <- input$log_x
    cut <- input$log_cut %||% 0.5
    need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
    need(!is.null(yv) && yv %in% names(d), "Choose a binary outcome.")
    need(length(xv) >= 1, "Choose at least one predictor.")
    need(!yv %in% xv, "The outcome cannot also be a predictor.")
    vars <- unique(c(yv, xv))
    dd <- d[stats::complete.cases(d[vars]), vars, drop = FALSE]
    dd[[yv]] <- factor(dd[[yv]])
    need(nlevels(dd[[yv]]) == 2, "The outcome must have exactly two observed categories.")
    for (v in setdiff(vars, yv)) {
      if (!is.numeric(dd[[v]])) {
        dd[[v]] <- droplevels(factor(dd[[v]]))
        need(nlevels(dd[[v]]) >= 2,
             sprintf("'%s' has a single level once missing values are dropped.", v))
      }
    }
    need(nrow(dd) > length(xv) + 1, "Not enough complete rows to fit the model.")

    fit <- suppressWarnings(
      stats::glm(stats::reformulate(xv, yv), data = dd, family = stats::binomial())
    )
    need(fit$converged, "The model did not converge. Simplify the predictors or check for separation.")
    s <- summary(fit)
    co <- s$coefficients
    z <- stats::qnorm(1 - a / 2)
    event <- levels(dd[[yv]])[2]
    obs <- as.integer(dd[[yv]] == event)
    prob <- unname(stats::fitted(fit))

    keep <- rownames(co) != "(Intercept)"
    co <- co[keep, , drop = FALSE]
    or_tab <- data.frame(
      Term = nice_terms(rownames(co), dd, yv),
      B = fmt_num(co[, 1], dg),
      `Std. error` = fmt_num(co[, 2], dg),
      Wald = fmt_num((co[, 1] / co[, 2])^2, 3),
      p = vapply(co[, 4], fmt_p, character(1)),
      `Exp(B)` = fmt_num(exp(co[, 1]), dg),
      `CI for Exp(B)` = sprintf("[%s, %s]",
                                fmt_num(exp(co[, 1] - z * co[, 2]), dg),
                                fmt_num(exp(co[, 1] + z * co[, 2]), dg)),
      decision = ifelse(co[, 4] < a, "Reject H0", "Fail to reject H0"),
      check.names = FALSE
    )
    rownames(or_tab) <- NULL

    # Term-level likelihood-ratio tests: more reliable than Wald tests when a
    # predictor has three or more categories
    lr_tab <- local({
      d1 <- try(stats::drop1(fit, test = "Chisq"), silent = TRUE)
      if (inherits(d1, "try-error")) return(NULL)
      d1 <- d1[rownames(d1) != "<none>", , drop = FALSE]
      pv <- d1[["Pr(>Chi)"]]
      data.frame(Predictor = rownames(d1), Df = d1[["Df"]],
                 `LR chi-square` = round(d1[["LRT"]], 3),
                 p = vapply(pv, fmt_p, character(1)),
                 decision = ifelse(pv < a, "Reject H0", "Fail to reject H0"),
                 check.names = FALSE)
    })

    pred <- as.integer(prob >= cut)
    conf <- table(Predicted = factor(pred, levels = c(0, 1),
                                     labels = c("Predicted 0", "Predicted 1")),
                  Observed = factor(obs, levels = c(0, 1),
                                    labels = c("Observed 0", "Observed 1")))
    accuracy <- mean(pred == obs)
    base_rate <- max(mean(obs == 1), mean(obs == 0))
    k <- nrow(co)
    events <- sum(obs)
    auc_v <- auc(prob, obs)
    lr <- fit$null.deviance - fit$deviance
    df_lr <- fit$df.null - fit$df.residual
    p_lr <- stats::pchisq(lr, df_lr, lower.tail = FALSE)

    fit_tab <- data.frame(
      Measure = c("Rows used", "Events (modelled level)", "Null deviance",
                  "Residual deviance", "Likelihood-ratio chi-square", "Model p-value",
                  "McFadden pseudo R squared", "AIC", "AUC",
                  sprintf("Accuracy at cut-off %s", format(cut)),
                  "Majority-class rate (no-model benchmark)"),
      Value = c(nrow(dd), sprintf("%d (%s)", events, event),
                fmt_num(fit$null.deviance, 2), fmt_num(fit$deviance, 2),
                sprintf("chi-squared(%d) = %s", df_lr, fmt_num(lr, 2)),
                fmt_p(p_lr), fmt_num(1 - fit$deviance / fit$null.deviance, dg),
                fmt_num(stats::AIC(fit), 1), fmt_num(auc_v, 3),
                pct(accuracy), pct(base_rate)),
      check.names = FALSE
    )

    coef_lines <- lapply(seq_len(nrow(co)), function(i) {
      orv <- exp(co[i, 1])
      tags$li(sprintf(
        "%s: Exp(B) = %s (95%% CI %s). A one-unit increase in %s multiplies the odds of %s by %s, holding the other predictors constant.",
        rownames(co)[i], fmt_num(orv, dg),
        sprintf("[%s, %s]", fmt_num(exp(co[i, 1] - z * co[i, 2]), dg),
                fmt_num(exp(co[i, 1] + z * co[i, 2]), dg)),
        rownames(co)[i], event, fmt_num(orv, dg)))
    })

    warnings <- c(
      if (events < 10 * k)
        sprintf("Only %d events for %d parameters. Fewer than about 10 events per parameter makes estimates unstable.", events, k),
      if (any(abs(co[, 1]) > 10)) "At least one coefficient is very large, which often signals quasi-separation.",
      if (accuracy < base_rate) "The model classifies no better than always predicting the majority class at this cut-off."
    )

    return(list(
      name = sprintf("Logistic regression: %s", yv),
      p = p_lr,
      stat_text = sprintf("LR chi-squared(%d) = %s", df_lr, fmt_num(lr, 2)),
      decision_text = if (p_lr < a) "Reject H0" else "Fail to reject H0",
      h0 = sprintf("all slopes are zero, so %s is unrelated to the predictors", yv),
      ha = "at least one predictor is associated with the outcome",
      results = tagList(
        h6("Model fit"), html_table(fit_tab),
        h6("Coefficients and odds ratios"), html_table(or_tab),
        if (!is.null(lr_tab)) tagList(
          h6("Term-level likelihood-ratio tests"),
          p(class = "small-note",
            "One test per predictor, comparing the model with and without it.
             Prefer these to the Wald tests for predictors with three or more
             categories."),
          html_table(lr_tab)
        ),
        h6(sprintf("Classification at cut-off %s", format(cut))),
        html_table(as.data.frame.matrix(conf), 0),
        h6("ROC curve"), plotOutput("log_roc", height = "340px"),
        tags$pre(class = "shiny-text-output",
                 paste(utils::capture.output(print(s)), collapse = "\n"))
      ),
      interpretation = tagList(
        h6("What was fitted"),
        p(sprintf("A binary logistic regression modelled the probability that %s = %s using %s, on %d complete rows.",
                  yv, event, paste(xv, collapse = ", "), nrow(dd))),
        h6("Overall model"),
        p(sprintf("The likelihood-ratio test is %s, %s, so the model %s the outcome.",
                  sprintf("chi-squared(%d) = %s", df_lr, fmt_num(lr, 2)),
                  sprintf("p = %s", fmt_p(p_lr)),
                  if (p_lr < a) "is associated with" else "does not show a clear association with")),
        p(sprintf("AUC = %s: 0.5 is chance and 1 is perfect discrimination.", fmt_num(auc_v, 3))),
        h6("Reading the odds ratios"),
        tags$ul(coef_lines),
        p("An odds ratio is a ratio of odds, not of risks. It is not the probability of the outcome, and it overstates the risk ratio when the outcome is common. Exp(B) = 1 means no association."),
        h6("Classification"),
        p(sprintf("At a cut-off of %s the model classifies %s of cases correctly, compared with %s for always predicting the majority class.",
                  format(cut), pct(accuracy), pct(base_rate))),
        if (length(warnings)) tagList(h6("Warnings"), tags$ul(lapply(warnings, tags$li)))
      ),
      assumptions = tagList(
        h6("Assumptions required"),
        tags$ul(
          tags$li("Binary outcome with independent observations."),
          tags$li("Linearity of the log-odds for continuous predictors."),
          tags$li("No extreme multicollinearity or complete separation."),
          tags$li("Enough events per parameter.")
        ),
        h6("Checks on your data"),
        html_table(data.frame(
          Check = c("Events per parameter", "Largest |B|", "Converged",
                    "AUC"),
          Result = c(fmt_num(events / k, 1), fmt_num(max(abs(co[, 1])), 2),
                     if (fit$converged) "yes" else "no", fmt_num(auc_v, 3)),
          check.names = FALSE
        )),
        h6("If an assumption is violated"),
        p("With separation, drop the offending predictor or use a penalised method such as Firth's correction. With non-linearity, add a polynomial or spline term for that predictor. With few events, collect more data or reduce the number of predictors.")
      )
    ))
  }

  observeEvent(input$run_log, {
    res <- tryCatch(compute_log(), error = function(e) e)
    if (inherits(res, "error")) {
      log_res(NULL)
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    log_res(res)
    add_history("Logistic", res$name, res$stat_text, res$p, res$decision_text)
    showNotification("Model fitted.", type = "message")
  })

  output$log_out <- renderUI({
    res <- log_res()
    if (is.null(res)) {
      return(div(class = "small-note",
        "Choose a binary outcome and predictors, then press Fit logistic model."))
    }
    tagList(
      decision_box(res$p, res$h0, res$ha, alpha_now()),
      result_tabs(res$results, res$interpretation, res$assumptions)
    )
  })

  output$log_roc <- renderPlot({
    res <- log_res()
    req(res)
    d <- dat()
    yv <- input$log_y; xv <- input$log_x
    dd <- d[stats::complete.cases(d[c(yv, xv)]), c(yv, xv), drop = FALSE]
    dd[[yv]] <- factor(dd[[yv]])
    fit <- suppressWarnings(stats::glm(stats::reformulate(xv, yv), data = dd,
                                       family = stats::binomial()))
    obs <- as.integer(dd[[yv]] == levels(dd[[yv]])[2])
    p <- unname(stats::fitted(fit))
    o <- order(p, decreasing = TRUE)
    tpr <- cumsum(obs[o]) / sum(obs)
    fpr <- cumsum(1 - obs[o]) / sum(1 - obs)
    graphics::plot(c(0, fpr), c(0, tpr), type = "l", lwd = 2.2,
                   col = theme_col("accent"), xlim = c(0, 1), ylim = c(0, 1),
                   xlab = "1 - specificity", ylab = "Sensitivity",
                   main = sprintf("ROC curve (AUC = %.3f)", auc(p, obs)), las = 1)
    graphics::abline(0, 1, lty = 2, col = theme_col("fg", 0.45))
  })

  # ---- visualisation studio ---------------------------------------------
  # The studio keeps its own specification and chart state; it only needs the
  # cleaned and filtered data plus a label for provenance.
  safe_module("Visualise",
              viz_studio_server("viz", data = dat, data_label = data_label,
                                add_history = add_history))

  # ---- statistical test finder ------------------------------------------
  finder_rec <- reactive({
    recommend_test(input$fd_goal, input$fd_dep, input$fd_ngroups,
                   input$fd_paired, input$fd_assume)
  })

  output$finder_out <- renderUI({
    r <- finder_rec()
    if (is.null(r)) return(p(class = "small-note", "Answer the questions to see a recommendation."))
    tagList(
      h4(r$test),
      p(class = "muted", r$summary),
      h6("Why this test fits"),
      p(r$why),
      h6("Assumptions to check"),
      tags$ul(lapply(r$assumptions, tags$li)),
      h6("Alternatives"),
      p(r$alternatives),
      h6("Next step"),
      p(sprintf("Run it in the %s tab.", r$tab)),
      actionButton("finder_open", tagList(icon("arrow-right"), " Take me there"),
                   class = "btn-primary")
    )
  })

  observeEvent(input$finder_open, {
    r <- finder_rec()
    req(r)
    bslib::nav_select("tabs", selected = r$tab, session = session)
  })

  # ---- history and report ------------------------------------------------
  output$hist_tab <- renderTable({
    h <- hist_log()
    if (!nrow(h)) {
      return(data.frame(Note = "No analyses recorded yet. Run a test or model to build history."))
    }
    h
  }, striped = TRUE, hover = TRUE, width = "100%")

  observeEvent(input$hist_clear, {
    hist_log(hist_log()[0, , drop = FALSE])
    showNotification("Analysis history cleared.", type = "message")
  })

  output$dl_hist <- downloadHandler(
    filename = function() paste0("analysis_history_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(hist_log(), file, row.names = FALSE)
  )

  output$dl <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(dat(), file, row.names = FALSE)
  )

  report_html <- function() {
    esc <- htmltools::htmlEscape
    parts <- input$rep_parts %||% character(0)
    d <- dat()
    title <- trimws(input$rep_title %||% "Statistical Analysis Report")
    if (!nzchar(title)) title <- "Statistical Analysis Report"
    author <- trimws(input$rep_author %||% "")
    dg <- digits_now()

    tbl <- function(df) {
      h <- html_table(df, dg)
      if (is.null(h)) return("<p><em>No rows.</em></p>")
      as.character(h)
    }

    sections <- c(
      sprintf("<header><h1>%s</h1><p class='meta'>%s%s</p></header>",
              esc(title),
              if (nzchar(author)) paste0(esc(author), " - ") else "",
              format(Sys.Date(), "%d %B %Y")),
      "<p class='note'>This report was generated automatically from the data
       currently loaded in Mr Somali. Every statistic below is computed
       from that data. Interpretations are generated from templates, not by a
       language model, and the conclusions you submit should be your own.</p>"
    )

    if ("overview" %in% parts) {
      sections <- c(sections,
        "<h2>1. Dataset overview</h2>",
        sprintf("<p>Source: %s. <strong>%d</strong> rows and <strong>%d</strong> columns after filters and cleaning. %s missing cells.</p>",
                esc(data_label()), nrow(d), ncol(d),
                pct(mean(is.na(d)))),
        tbl(data.frame(Variable = names(d),
                       Type = vapply(d, function(x) class(x)[1], character(1)),
                       Missing = vapply(d, function(x) sum(is.na(x)), integer(1)),
                       row.names = NULL, check.names = FALSE))
      )
    }

    if ("describe" %in% parts) {
      nums <- num_cols(d)
      desc <- if (length(nums)) {
        do.call(rbind, lapply(nums, function(v) {
          s <- describe_numeric(d[[v]])
          data.frame(Variable = v, n = s$n, Missing = s$missing,
                     Mean = fmt_num(s$mean, dg), SD = fmt_num(s$sd, dg),
                     Median = fmt_num(s$median, dg),
                     Min = fmt_num(s$min, dg), Max = fmt_num(s$max, dg),
                     Skewness = fmt_num(s$skewness, dg), check.names = FALSE)
        }))
      } else {
        data.frame(Note = "No numeric variables in the current data.")
      }
      sections <- c(sections,
        "<h2>2. Descriptive statistics</h2>",
        sprintf("<p>Numeric variables (%d):</p>", length(nums)), tbl(desc))
    }

    if ("history" %in% parts) {
      h <- hist_log()
      sections <- c(sections,
        "<h2>3. Analyses performed</h2>",
        if (!nrow(h)) "<p><em>No analyses were recorded in this session.</em></p>"
        else tbl(h))
    }

    if ("methods" %in% parts) {
      sections <- c(sections,
        "<h2>4. Methods, assumptions and limitations</h2>",
        "<p>Tests were run at the significance level selected in the app. Each
         analysis reports its own assumptions and the checks performed on this
         data in the Assumptions tab of the application.</p>",
        "<p><strong>Limitations.</strong> Results describe the data as filtered
         and cleaned in this session. Missing values, which the analysis treats
         as removed unless imputed, may bias estimates if they are not missing
         completely at random. Statistical significance is not practical
         importance, and tests run on small samples may lack power. Correlation
         and regression coefficients do not establish causation. Automated
         interpretations follow fixed templates and do not replace subject
         knowledge or a statistician's review.</p>")
    }

    paste0(
      "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1'>",
      "<title>", esc(title), "</title><style>",
      "body{font-family:Segoe UI,system-ui,sans-serif;max-width:900px;margin:2.5rem auto;padding:0 1.2rem;color:#0F172A;line-height:1.6}",
      "h1{font-size:1.7rem;margin-bottom:.2rem}h2{font-size:1.15rem;color:#1D4ED8;margin-top:2rem;border-bottom:2px solid #E2E8F0;padding-bottom:.3rem}",
      ".meta{color:#64748B;margin-top:0}.note{background:#F1F5F9;border-left:4px solid #2563EB;padding:.8rem 1rem;font-size:.9rem}",
      "table{border-collapse:collapse;width:100%;font-size:.85rem;margin:.8rem 0}",
      "th,td{border:1px solid #E2E8F0;padding:.35rem .55rem;text-align:left}",
      "th{background:#F1F5F9}tr:nth-child(even) td{background:#F8FAFC}",
      "footer{margin-top:3rem;color:#64748B;font-size:.8rem;border-top:1px solid #E2E8F0;padding-top:1rem}",
      "</style></head><body>",
      paste(sections, collapse = "\n"),
      "<footer>Generated by Mr Somali on ", format(Sys.time(), "%Y-%m-%d %H:%M"), ".</footer>",
      "</body></html>"
    )
  }

  # ---- visitor analytics dashboard (administrator only) ------------------
  track_admin_server(input, output, session, tracker)

  output$dl_report <- downloadHandler(
    filename = function() paste0("analysis_report_", Sys.Date(), ".html"),
    content = function(file) {
      txt <- tryCatch(report_html(), error = function(e) {
        showNotification(paste("Report could not be generated:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
      req(txt)
      writeLines(txt, file, useBytes = TRUE)
    }
  )

}

# ---- test finder decision rules -----------------------------------------
# A pure function, so the advice lives in one place and can be checked directly.
recommend_test <- function(goal = "compare", dep = "num", ngroups = "two",
                           paired = "ind", assume = "yes") {
  goal <- goal %||% "compare"
  dep <- dep %||% "num"
  ngroups <- ngroups %||% "two"
  paired <- paired %||% "ind"
  assume <- assume %||% "yes"

  if (identical(goal, "describe")) {
    return(list(
      test = "Descriptive statistics", tab = "Descriptive",
      summary = "You want to describe a variable rather than test a hypothesis.",
      why = "Means, medians, spread and frequency tables summarise a distribution without a formal test. This is the correct first step and often the only one needed.",
      assumptions = c("Observations should be independent and representative of the population you care about.",
                      "Report the measurement scale: mean and SD suit symmetric numeric data; median and IQR suit skewed or ordinal data."),
      alternatives = "Add a histogram, boxplot or density plot to show the shape, and report missing values explicitly."
    ))
  }

  if (identical(goal, "predict")) {
    if (dep == "num") {
      return(list(
        test = "Linear regression", tab = "Regression",
        summary = "A continuous outcome explained by one or more predictors.",
        why = "Linear regression estimates how much the outcome changes per unit of each predictor while adjusting for the others, and reports R squared with coefficient intervals.",
        assumptions = c("Linear relationship between each predictor and the outcome.",
                        "Independent errors with constant variance.",
                        "Approximately normal residuals for small-sample inference.",
                        "No severe multicollinearity (check VIF)."),
        alternatives = "Transform the outcome or use robust standard errors when variance is not constant; use a generalised linear model for non-normal outcomes."
      ))
    }
    return(list(
      test = "Binary logistic regression", tab = "Logistic",
      summary = "A two-category outcome explained by one or more predictors.",
      why = "Logistic regression models the log-odds of the outcome, so predicted probabilities stay between 0 and 1. Odds ratios express the size of each association.",
      assumptions = c("Binary outcome with independent observations.",
                      "Linearity of the log-odds for continuous predictors.",
                      "Enough events per parameter (about 10 or more).",
                      "No complete separation."),
      alternatives = "With few events, consider Firth's penalised logistic regression; for more than two outcome categories, use multinomial regression."
    ))
  }

  if (identical(goal, "assoc")) {
    if (dep == "num") {
      return(list(
        test = "Correlation analysis", tab = "Correlation",
        summary = "Two numeric variables measured on the same units.",
        why = "Pearson's r measures linear association; Spearman's rho and Kendall's tau measure monotonic association and are more robust to outliers and skew.",
        assumptions = c("Independent pairs.",
                        "Pearson: approximate normality and a linear relationship.",
                        "Spearman and Kendall: at least ordinal data."),
        alternatives = "With a numeric outcome and covariates, use regression instead: correlation cannot adjust for confounders."
      ))
    }
    if (paired == "paired") {
      return(list(
        test = "McNemar's test", tab = "Categories",
        summary = "Two binary measurements on the same cases.",
        why = "McNemar's test compares paired proportions using the discordant pairs, which is exactly the structure of a before/after binary measure.",
        assumptions = c("Paired binary measurements on the same units.",
                        "Enough discordant pairs to estimate the difference."),
        alternatives = "Report Cohen's kappa as well when the question is agreement rather than change."
      ))
    }
    return(list(
      test = "Chi-square test of independence", tab = "Categories",
      summary = "Two categorical variables in independent groups.",
      why = "The chi-square test compares observed cell counts with the counts expected under independence. Cramer's V describes the strength of the association.",
      assumptions = c("Independent observations, each case counted once.",
                      "Expected counts of about 5 or more in most cells."),
      alternatives = "Use Fisher's exact test for 2 x 2 tables with small counts, or the simulated p-value when the table is sparse."
    ))
  }

  # goal: compare groups
  if (dep == "num") {
    if (ngroups == "one") {
      if (assume == "yes") {
        return(list(
          test = "One-sample t-test", tab = "Tests",
          summary = "One numeric variable compared with a hypothesised value.",
          why = "The one-sample t-test compares the sample mean with a fixed value and gives a confidence interval for the mean.",
          assumptions = c("Independent observations.",
                          "Approximately normal data, or a sample of about 30 or more."),
          alternatives = "Use the Wilcoxon signed-rank test on the differences when the data are clearly non-normal and the sample is small."
        ))
      }
      return(list(
        test = "Wilcoxon signed-rank test", tab = "Tests",
        summary = "One numeric variable compared with a hypothesised value, without a normality assumption.",
        why = "The signed-rank test works on the ranks of the differences from the hypothesised value, so it tolerates skew and outliers.",
        assumptions = c("Independent observations.", "Differences roughly symmetric."),
        alternatives = "The one-sample t-test has more power when the data are normal."
      ))
    }
    if (ngroups == "two") {
      if (paired == "paired") {
        if (assume == "yes") {
          return(list(
            test = "Paired-samples t-test", tab = "Tests",
            summary = "Two numeric measurements on the same cases.",
            why = "Pairing removes between-case variability, so the paired t-test is more powerful than treating the two measurements as independent groups.",
            assumptions = c("Paired observations.", "Differences approximately normal."),
            alternatives = "Use the Wilcoxon signed-rank test when the differences are clearly non-normal."
          ))
        }
        return(list(
          test = "Wilcoxon signed-rank test", tab = "Tests",
          summary = "Two paired numeric measurements without a normality assumption.",
          why = "It ranks the within-case differences, so it tolerates skew and outliers.",
          assumptions = c("Paired observations.", "Differences roughly symmetric.",
                          "Independent pairs."),
          alternatives = "The paired t-test is more powerful when the differences are approximately normal."
        ))
      }
      if (assume == "yes") {
        return(list(
          test = "Welch independent-samples t-test", tab = "Tests",
          summary = "A numeric outcome compared between two independent groups.",
          why = "Welch's t-test compares group means without assuming equal variances, which is the safer default.",
          assumptions = c("Independent observations.",
                          "Approximately normal outcome within each group, or groups of 30 or more."),
          alternatives = "Use the Mann-Whitney U test for clearly non-normal data with small groups."
        ))
      }
      return(list(
        test = "Mann-Whitney U test", tab = "Tests",
        summary = "A numeric or ordinal outcome compared between two independent groups, without a normality assumption.",
        why = "It compares ranks between groups, so it remains valid for skewed data and small samples.",
        assumptions = c("Independent observations.", "At least ordinal measurement.",
                        "Similar distribution shapes for a median interpretation."),
        alternatives = "Welch's t-test has more power when the data are approximately normal."
      ))
    }
    if (paired == "paired") {
      return(list(
        test = "Friedman test", tab = "Tests",
        summary = "Three or more measurements of the same numeric outcome on the same cases.",
        why = "The Friedman test is the nonparametric repeated-measures test: it ranks each case's values across occasions and asks whether the rank sums differ.",
        assumptions = c("Repeated measurements on the same units.",
                        "At least ordinal outcome.", "Independent cases."),
        alternatives = "A repeated-measures ANOVA is the parametric counterpart; this app implements Friedman, which does not assume sphericity."
      ))
    }
    if (assume == "yes") {
      return(list(
        test = "One-way ANOVA", tab = "Tests",
        summary = "A numeric outcome compared across three or more independent groups.",
        why = "ANOVA tests all group means in one model, which controls the false-positive rate. Tukey HSD then shows which pairs differ.",
        assumptions = c("Independent observations.", "Approximately normal residuals.",
                        "Equal variances (otherwise use Welch ANOVA)."),
        alternatives = "Use Welch ANOVA for unequal variances and Kruskal-Wallis for non-normal data."
      ))
    }
    return(list(
      test = "Kruskal-Wallis test", tab = "Tests",
      summary = "A numeric or ordinal outcome compared across three or more independent groups.",
      why = "It is the nonparametric analogue of one-way ANOVA and compares rank sums instead of means.",
      assumptions = c("Independent observations.", "At least ordinal outcome.",
                      "Similar distribution shapes for a median interpretation."),
      alternatives = "Follow up with pairwise Mann-Whitney tests using a Holm or Benjamini-Hochberg correction."
    ))
  }

  # categorical outcome
  if (paired == "paired") {
    return(list(
      test = "McNemar's test", tab = "Categories",
      summary = "Two paired binary measurements.",
      why = "McNemar's test uses only the discordant pairs, which is what paired binary data provide.",
      assumptions = c("Paired binary measurements.", "Enough discordant pairs."),
      alternatives = "Report Cohen's kappa when the question is agreement rather than change."
    ))
  }
  list(
    test = "Chi-square test of independence", tab = "Categories",
    summary = "Two categorical variables.",
    why = "The chi-square test compares observed and expected cell counts; Cramer's V describes the strength of the association.",
    assumptions = c("Independent observations.",
                    "Expected counts of about 5 or more in most cells."),
    alternatives = "Use Fisher's exact test for 2 x 2 tables with small counts."
  )
}

# Base R and ggplot2 output inherit the app theme and palette
thematic_shiny(qualitative = pal)

shinyApp(ui, server)



