# Visualisation studio - styles ---------------------------------------------
# Scoped to .viz-studio so the rest of the application is untouched.

viz_css <- function() {
  css <- "
  .viz-studio {
    --viz-line: #E6EBF2;
    --viz-ink: #0F172A;
    --viz-muted: #64748B;
    --viz-soft: #F8FAFC;
    --viz-accent: #2563EB;
    --viz-radius: 16px;
    padding-top: 2px;
  }

  /* header */
  .viz-head {
    display: flex; justify-content: space-between; align-items: flex-end;
    gap: 16px; flex-wrap: wrap; margin-bottom: 4px;
  }
  .viz-head h3 {
    font-size: 1.4rem; font-weight: 700; color: var(--viz-ink); margin: 0;
    letter-spacing: -0.01em;
  }
  .viz-head p { color: var(--viz-muted); margin: 4px 0 0; font-size: 0.9rem; }
  .viz-data-pill {
    background: #FFFFFF; border: 1px solid var(--viz-line); border-radius: 999px;
    padding: 6px 14px; font-size: 0.81rem; color: #475569; white-space: nowrap;
    box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
  }
  .viz-data-pill .viz-dot {
    display: inline-block; width: 7px; height: 7px; border-radius: 50%;
    background: #10B981; margin-right: 7px; vertical-align: middle;
  }

  /* suggestions */
  .viz-suggest {
    display: flex; flex-wrap: wrap; align-items: center; gap: 8px;
    margin: 14px 0 6px;
  }
  .viz-suggest-label {
    font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.07em;
    color: #94A3B8; font-weight: 700;
  }
  .viz-suggest-btn {
    display: inline-flex; align-items: center; gap: 7px; background: #FFFFFF;
    border: 1px solid var(--viz-line); border-radius: 999px; padding: 6px 13px;
    font-size: 0.82rem; color: #475569; cursor: pointer; line-height: 1.2;
    transition: border-color 0.15s ease, box-shadow 0.15s ease, color 0.15s ease;
  }
  .viz-suggest-btn:hover {
    border-color: #BFDBFE; color: var(--viz-accent); background: #F8FAFF;
    box-shadow: 0 2px 8px rgba(37, 99, 235, 0.08);
  }
  .viz-suggest-btn b { font-weight: 650; color: var(--viz-ink); }
  .viz-suggest-btn:hover b { color: var(--viz-accent); }

  /* steps */
  .viz-step-row { display: flex; align-items: center; gap: 8px; margin: 14px 0 8px; }
  .viz-step-text { font-size: 0.82rem; font-weight: 650; color: #334155; }
  .viz-step {
    width: 20px; height: 20px; border-radius: 50%; background: #EFF6FF;
    color: var(--viz-accent); font-size: 0.71rem; font-weight: 700;
    display: inline-flex; align-items: center; justify-content: center;
    flex: 0 0 20px;
  }

  /* chart picker */
  .viz-picker { display: flex; flex-direction: column; gap: 6px; margin-bottom: 18px; }
  .viz-pick-row {
    display: grid; grid-template-columns: 92px minmax(0, 1fr);
    align-items: center; gap: 10px;
  }
  .viz-pick-label {
    font-size: 0.69rem; text-transform: uppercase; letter-spacing: 0.07em;
    color: #94A3B8; font-weight: 700;
  }
  .viz-pick-cards { display: flex; flex-wrap: wrap; gap: 6px; }
  .viz-pick-card {
    display: inline-flex; align-items: center; gap: 7px; padding: 7px 12px 7px 9px;
    border-radius: 11px; border: 1px solid var(--viz-line); background: #FFFFFF;
    color: #475569; font-size: 0.82rem; font-weight: 500; cursor: pointer;
    line-height: 1.15; transition: all 0.15s ease; text-align: left;
  }
  .viz-pick-card:hover {
    border-color: #CBD5E1; box-shadow: 0 2px 8px rgba(15, 23, 42, 0.07);
    transform: translateY(-1px);
  }
  .viz-pick-card:focus-visible {
    outline: none; box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.25);
  }
  .viz-pick-card.active {
    border-color: var(--viz-accent); background: #EFF6FF; color: var(--viz-accent);
    font-weight: 650; box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.10);
  }
  .viz-pick-ic { display: inline-flex; }
  .viz-pick-ic svg { width: 21px; height: 17px; display: block; }

  /* layout */
  .viz-body {
    display: grid; grid-template-columns: 336px minmax(0, 1fr);
    gap: 16px; align-items: start;
  }
  .viz-controls { display: flex; flex-direction: column; gap: 14px; min-width: 0; }
  .viz-panel {
    background: #FFFFFF; border: 1px solid var(--viz-line); border-radius: var(--viz-radius);
    padding: 15px 16px; box-shadow: 0 1px 3px rgba(15, 23, 42, 0.04);
  }
  .viz-panel-plain { padding: 6px; }
  .viz-panel-head { display: flex; align-items: center; gap: 8px; margin-bottom: 12px; }
  .viz-panel-title { font-weight: 650; color: var(--viz-ink); font-size: 0.93rem; }
  .viz-help { font-size: 0.77rem; color: var(--viz-muted); margin: -2px 0 10px; line-height: 1.4; }

  /* form controls inside the studio */
  .viz-studio .form-group, .viz-studio .shiny-input-container { margin-bottom: 10px; }
  .viz-studio .shiny-input-container:last-child { margin-bottom: 0; }
  .viz-studio label, .viz-studio .control-label {
    font-size: 0.79rem; font-weight: 600; color: #475569; margin-bottom: 3px;
  }
  .viz-studio .form-control, .viz-studio .form-select,
  .viz-studio .selectize-input {
    border-radius: 9px; border-color: #E2E8F0; font-size: 0.86rem;
    color: var(--viz-ink); min-height: 36px;
  }
  .viz-studio .form-control:focus, .viz-studio .form-select:focus,
  .viz-studio .selectize-input.focus {
    border-color: #93C5FD; box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.12);
  }
  .viz-studio .selectize-dropdown { font-size: 0.86rem; border-radius: 9px; }
  .viz-studio .btn {
    border-radius: 9px; font-size: 0.83rem; font-weight: 600;
  }

  /* canvas */
  .viz-canvas {
    background: #FFFFFF; border: 1px solid var(--viz-line);
    border-radius: var(--viz-radius); padding: 14px 16px 12px;
    box-shadow: 0 1px 3px rgba(15, 23, 42, 0.04); min-width: 0;
  }
  .viz-canvas-head {
    display: flex; justify-content: space-between; align-items: center;
    gap: 10px; flex-wrap: wrap; margin-bottom: 4px;
  }
  .viz-canvas-title {
    display: flex; align-items: center; gap: 8px; font-weight: 650;
    color: var(--viz-ink); font-size: 0.93rem;
  }
  .viz-toolbar { display: flex; align-items: center; gap: 8px; }
  .viz-toolbar .form-group, .viz-toolbar .shiny-input-container { margin: 0; }
  .viz-toolbar .form-select {
    height: 32px; padding: 2px 28px 2px 10px; font-size: 0.8rem;
    background-color: #FFFFFF;
  }
  .viz-icon-btn { padding: 4px 9px; }
  .viz-dl { display: inline-flex; align-items: center; gap: 6px; padding: 5px 14px; }

  .viz-plot {
    position: relative; height: clamp(400px, 56vh, 620px); margin: 6px 0 2px;
  }
  .viz-plot > div, .viz-plot .html-widget, .viz-plot .js-plotly-plot {
    height: 100% !important; width: 100% !important;
  }
  .viz-plot-holder { position: absolute; inset: 0; }
  .viz-plot-state {
    position: absolute; inset: 0; background: #FFFFFF; display: flex;
    align-items: center; justify-content: center; border-radius: 12px;
    padding: 10px; z-index: 2;
  }
  .viz-state-inline { padding: 2px 0; }
  .viz-state-inline .viz-empty { padding: 10px 2px; text-align: left; }
  .viz-state-inline .viz-empty p { margin: 0; }
  .viz-plot .recalculating { opacity: 0.35; transition: opacity 0.2s ease; }
  .viz-plot:has(.recalculating)::after {
    content: ''; position: absolute; inset: 0; border-radius: 12px;
    background: linear-gradient(100deg, rgba(248,250,252,0) 30%,
                rgba(241,245,249,0.85) 50%, rgba(248,250,252,0) 70%);
    background-size: 200% 100%; animation: viz-shimmer 1.3s linear infinite;
    pointer-events: none;
  }
  @keyframes viz-shimmer {
    from { background-position: 200% 0; }
    to { background-position: -200% 0; }
  }

  /* summary values */
  .viz-chips { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 10px; }
  .viz-chip {
    background: var(--viz-soft); border: 1px solid var(--viz-line);
    border-radius: 11px; padding: 6px 12px; min-width: 78px;
  }
  .viz-chip-value {
    display: block; font-weight: 650; color: var(--viz-ink); font-size: 0.94rem;
    line-height: 1.25;
  }
  .viz-chip-label {
    display: block; font-size: 0.68rem; text-transform: uppercase;
    letter-spacing: 0.05em; color: #94A3B8; font-weight: 700;
  }

  /* notes and empty states */
  .viz-notes { margin-top: 10px; display: flex; flex-direction: column; gap: 3px; }
  .viz-note {
    font-size: 0.77rem; color: #94A3B8; display: flex; gap: 6px;
    align-items: flex-start; line-height: 1.4;
  }
  .viz-note i { margin-top: 2px; font-size: 0.7rem; }
  .viz-empty { text-align: center; padding: 34px 20px; }
  .viz-empty-ic {
    width: 46px; height: 46px; border-radius: 50%; background: #EFF6FF;
    color: var(--viz-accent); display: inline-flex; align-items: center;
    justify-content: center; margin-bottom: 10px; font-size: 1.05rem;
  }
  .viz-empty h5 { font-weight: 650; color: var(--viz-ink); font-size: 0.98rem; margin: 0 0 5px; }
  .viz-empty p { margin: 0 auto; max-width: 430px; font-size: 0.85rem; color: var(--viz-muted); }
  .viz-empty-actions { margin-top: 14px; display: flex; gap: 8px; justify-content: center; flex-wrap: wrap; }

  /* customize accordion */
  .viz-accordion { --bs-accordion-border-width: 0; --bs-accordion-bg: transparent; }
  .viz-accordion .accordion-item {
    border: 1px solid var(--viz-line); border-radius: 12px; overflow: hidden;
    background: #FFFFFF;
  }
  .viz-accordion .accordion-button {
    font-size: 0.86rem; font-weight: 650; color: #334155; background: #FBFCFE;
    padding: 11px 14px; box-shadow: none !important;
  }
  .viz-accordion .accordion-button:not(.collapsed) {
    color: var(--viz-accent); background: #F8FAFF;
  }
  .viz-accordion .accordion-button::after { width: 0.9rem; height: 0.9rem; background-size: 0.9rem; }
  .viz-accordion .accordion-body { padding: 14px; }
  .viz-acc-group { margin-bottom: 16px; }
  .viz-acc-group:last-child { margin-bottom: 4px; }
  .viz-acc-title {
    display: block; font-size: 0.69rem; text-transform: uppercase;
    letter-spacing: 0.07em; color: #94A3B8; font-weight: 700; margin-bottom: 9px;
  }
  .viz-acc-group + .viz-acc-group {
    border-top: 1px solid var(--viz-line); padding-top: 14px;
  }

  /* full screen */
  .viz-canvas:fullscreen {
    background: #FFFFFF; padding: 18px 22px; border-radius: 0;
    display: flex; flex-direction: column; overflow: auto;
  }
  .viz-canvas:fullscreen .viz-plot { height: calc(100vh - 210px); }

  /* responsive */
  @media (max-width: 1200px) {
    .viz-body { grid-template-columns: 300px minmax(0, 1fr); }
  }
  @media (max-width: 991px) {
    .viz-body { grid-template-columns: minmax(0, 1fr); }
    .viz-pick-row { grid-template-columns: minmax(0, 1fr); gap: 4px; }
    .viz-pick-label { margin: 4px 0 0; }
    .viz-plot { height: 400px; }
    .viz-canvas-head { align-items: flex-start; }
  }
  @media (max-width: 575px) {
    .viz-plot { height: 340px; }
    .viz-pick-card { font-size: 0.78rem; padding: 6px 10px 6px 8px; }
    .viz-head h3 { font-size: 1.2rem; }
    .viz-chips { gap: 6px; }
    .viz-chip { min-width: 68px; padding: 5px 9px; }
  }
  "
  htmltools::tags$style(htmltools::HTML(css))
}
