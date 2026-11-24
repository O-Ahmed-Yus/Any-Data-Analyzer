# Visualisation studio - themes, scales and formatting -----------------------
# Five curated palettes. A theme object carries the categorical palette, a
# sequential and a diverging ramp, and the ink / grid colours used by every
# chart. Nothing here needs to be configured by the user.

viz_themes <- list(
  classic = list(
    label = "Classic Blue",
    pal = c("#2563EB", "#E69F00", "#0E9F6E", "#CC79A7", "#56B4E9",
            "#D55E00", "#7C3AED", "#64748B"),
    seq = c("#EFF6FF", "#BFDBFE", "#60A5FA", "#2563EB", "#1E3A8A"),
    div_low = "#2166AC", div_mid = "#F8FAFC", div_high = "#B2182B",
    ink = "#0F172A", muted = "#64748B", grid = "#E6EBF2", bg = "#FFFFFF"
  ),
  purple = list(
    label = "Modern Purple",
    pal = c("#7C3AED", "#A78BFA", "#4F46E5", "#EC4899", "#0EA5E9",
            "#F59E0B", "#10B981", "#EF4444"),
    seq = c("#F5F3FF", "#DDD6FE", "#C4B5FD", "#8B5CF6", "#4C1D95"),
    div_low = "#2563EB", div_mid = "#FAF8FF", div_high = "#DB2777",
    ink = "#1E1B4B", muted = "#6B7280", grid = "#EDE9FE", bg = "#FFFFFF"
  ),
  emerald = list(
    label = "Emerald Green",
    pal = c("#0E9F6E", "#0891B2", "#84CC16", "#0EA5E9", "#F59E0B",
            "#7C3AED", "#EF4444", "#64748B"),
    seq = c("#ECFDF5", "#A7F3D0", "#34D399", "#0E9F6E", "#065F46"),
    div_low = "#0891B2", div_mid = "#F7FCFA", div_high = "#B45309",
    ink = "#052E2B", muted = "#5F6B6A", grid = "#E2F1EC", bg = "#FFFFFF"
  ),
  sunset = list(
    label = "Warm Sunset",
    pal = c("#EA580C", "#F59E0B", "#E11D48", "#FB7185", "#7C3AED",
            "#0EA5E9", "#0E9F6E", "#FACC15"),
    seq = c("#FFF7ED", "#FED7AA", "#FB923C", "#EA580C", "#9A3412"),
    div_low = "#2563EB", div_mid = "#FFFBF5", div_high = "#DC2626",
    ink = "#2A1310", muted = "#78716C", grid = "#F5EBE0", bg = "#FFFFFF"
  ),
  mono = list(
    label = "Professional Monochrome",
    pal = c("#1E293B", "#475569", "#64748B", "#94A3B8", "#0F172A",
            "#7C8798", "#CBD5E1", "#334155"),
    seq = c("#F1F5F9", "#CBD5E1", "#94A3B8", "#475569", "#0F172A"),
    div_low = "#334155", div_mid = "#F8FAFC", div_high = "#0F172A",
    ink = "#0F172A", muted = "#64748B", grid = "#E9EEF4", bg = "#FFFFFF"
  )
)

viz_theme_default <- "classic"

viz_theme_get <- function(key) {
  nm <- if (is.null(key) || !key %in% names(viz_themes)) viz_theme_default else key
  viz_themes[[nm]]
}

viz_theme_choices <- function() {
  stats::setNames(names(viz_themes),
                  vapply(viz_themes, function(t) t$label, character(1)))
}

# Categorical colours for n groups, interpolated when n exceeds the palette.
viz_pal <- function(th, n) {
  n <- max(1L, as.integer(n))
  if (n <= length(th$pal)) th$pal[seq_len(n)] else grDevices::colorRampPalette(th$pal)(n)
}

# Distinct groups in a grouping column, whether it is a factor, character,
# logical or numeric column (droplevels() only accepts factors).
viz_ngroups <- function(x) {
  x <- as.character(x)
  length(unique(x[!is.na(x)]))
}

# ---- ggplot styling -------------------------------------------------------

viz_theme <- function(th, legend = TRUE) {
  ggplot2::theme_minimal(base_size = 14, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = th$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = th$bg, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = th$grid, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 17, colour = th$ink,
                                         margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggplot2::element_text(size = 11, colour = th$muted,
                                            margin = ggplot2::margin(b = 14)),
      axis.title = ggplot2::element_text(size = 11.5, colour = th$muted),
      axis.text = ggplot2::element_text(size = 11, colour = "#475569"),
      legend.position = if (isTRUE(legend)) "bottom" else "none",
      legend.title = ggplot2::element_text(size = 11, colour = th$muted),
      legend.text = ggplot2::element_text(size = 10.5, colour = "#475569"),
      legend.key = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(t = 4),
      plot.margin = ggplot2::margin(16, 22, 12, 14)
    )
}

# Titles, axis labels and theme in one place. `void = TRUE` is used by charts
# without axes (pie / donut).
viz_finish <- function(p, s, th, xlab = NULL, ylab = NULL, sub = NULL,
                       legend_title = NULL, void = FALSE) {
  xl <- if (nzchar(s$xlab %||% "")) s$xlab else xlab
  yl <- if (nzchar(s$ylab %||% "")) s$ylab else ylab
  p <- p + ggplot2::labs(title = s$title, subtitle = sub, x = xl, y = yl,
                         fill = legend_title, colour = legend_title)
  p + viz_theme(th, legend = s$legend)
}

viz_finish_pie <- function(p, s, th, legend_title = NULL) {
  p <- p + ggplot2::labs(title = s$title, subtitle = NULL, fill = legend_title)
  p + viz_theme(th, legend = s$legend) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = th$bg, colour = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 17, colour = th$ink,
                                         margin = ggplot2::margin(b = 10)),
      legend.position = if (isTRUE(s$legend)) "right" else "none",
      legend.title = ggplot2::element_text(size = 11, colour = th$muted),
      legend.text = ggplot2::element_text(size = 10.5, colour = "#475569"),
      plot.margin = ggplot2::margin(16, 18, 12, 14)
    )
}

# ---- plotly styling -------------------------------------------------------

viz_have_plotly <- function() requireNamespace("plotly", quietly = TRUE)

viz_interactive <- function(b, th) {
  if (!is.null(b$widget)) return(b$widget)
  if (!viz_have_plotly() || is.null(b$plot)) return(NULL)
  tooltip <- if (identical(b$tip, "text")) "text" else "all"
  p <- plotly::ggplotly(b$plot, tooltip = tooltip, dynamicTicks = TRUE) |>
    plotly::layout(
      font = list(family = "Inter, Segoe UI, system-ui, sans-serif", size = 12.5,
                  color = "#475569"),
      hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = th$grid,
                        font = list(color = th$ink, size = 12.5,
                                    family = "Inter, Segoe UI, system-ui, sans-serif")),
      margin = list(l = 64, r = 24, t = 16, b = 52)
    ) |>
    plotly::config(
      displaylogo = FALSE, responsive = TRUE, scrollZoom = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                                 "toggleSpikelines", "hoverClosestCartesian",
                                 "hoverCompareCartesian")
    )
  p
}

# ---- formatting -----------------------------------------------------------

viz_num <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("–")
  x <- x[[1]]
  if (!is.finite(x)) return("–")
  if (abs(x) >= 1000) format(round(x), big.mark = ",", trim = TRUE)
  else format(signif(x, digits), big.mark = ",", trim = TRUE)
}

viz_comma <- function(x) {
  ifelse(is.na(x), "–", format(round(x, 2), big.mark = ",", trim = TRUE))
}

viz_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "–", paste0(format(round(100 * x, digits), trim = TRUE), "%"))
}

viz_slug <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", tolower(paste(x, collapse = " ")))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) "chart" else substr(x, 1, 60)
}

# Relative luminance, used to pick readable label colours on filled tiles and
# pie slices.
viz_luminance <- function(cols) {
  rgb <- grDevices::col2rgb(cols) / 255
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

viz_on_colour <- function(cols, th, cutoff = 0.62) {
  ifelse(viz_luminance(cols) > cutoff, th$ink, "#FFFFFF")
}
