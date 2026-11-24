# Visualisation studio - distribution charts --------------------------------
# Histogram, boxplot, pie / donut, density and Q-Q plot.

# ---- histogram ------------------------------------------------------------

viz_plot_hist <- function(s, th) {
  x <- s$f$x
  tk <- viz_take(s$data, x)
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows are available for this variable."))

  bins <- max(2L, min(200L, as.integer(s$opt$bins %||% 30)))
  width <- diff(range(dd[[x]], na.rm = TRUE)) / bins

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[[x]], group = 1)) +
    ggplot2::geom_histogram(bins = bins, fill = th$pal[1], colour = th$bg,
                            linewidth = 0.35, na.rm = TRUE) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = c(0, 0.06)))

  sub <- paste0(bins, " bins of width ", viz_num(width, 2))

  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("hist", s$f),
                         ylab = "Count", sub = sub),
       chips = list(viz_chip("Rows", format(nrow(dd), big.mark = ",")),
                    viz_chip("Median", viz_num(stats::median(dd[[x]], na.rm = TRUE))),
                    viz_chip("Standard deviation", viz_num(stats::sd(dd[[x]], na.rm = TRUE))),
                    viz_chip("Skewness", viz_num(viz_skewness(dd[[x]]), 2))),
       tip = NULL,
       note = viz_dropped_note(tk$dropped))
}

viz_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)
  sum((x - mean(x))^3) / n / s^3
}

# ---- boxplot --------------------------------------------------------------

viz_plot_box <- function(s, th) {
  y <- s$f$y
  x <- nz(s$f$x)
  tk <- viz_take(s$data, c(y, x))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows are available for this variable."))

  if (is.null(x)) {
    dd$.all <- "All rows"
    p <- ggplot2::ggplot(dd, ggplot2::aes(x = .all, y = .data[[y]])) +
      ggplot2::geom_boxplot(fill = th$pal[1], colour = th$ink, width = 0.34,
                            outlier.colour = th$pal[1], outlier.size = 1.6,
                            outlier.alpha = 0.7, linewidth = 0.45) +
      ggplot2::labs(x = NULL)
    chip2 <- list(viz_chip("Median", viz_num(stats::median(dd[[y]], na.rm = TRUE))))
    legend <- NULL
  } else {
    lv <- sort(unique(as.character(dd[[x]])))
    dd[[x]] <- factor(as.character(dd[[x]]), levels = lv)
    p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[[x]], y = .data[[y]],
                                          fill = .data[[x]])) +
      ggplot2::geom_boxplot(colour = th$ink, width = 0.6, outlier.size = 1.6,
                            outlier.alpha = 0.7, linewidth = 0.4) +
      ggplot2::scale_fill_manual(values = viz_pal(th, length(lv))) +
      viz_wrap_discrete(12)
    chip2 <- list(viz_chip("Groups", as.character(length(lv))))
    legend <- x
  }

  p <- p + ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                       expand = ggplot2::expansion(mult = 0.04))

  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("box", s$f),
                         ylab = viz_auto_ylab("box", s$f),
                         sub = "Box = middle 50% of values, line = median, whiskers = 1.5 x IQR",
                         legend_title = legend),
       chips = c(list(viz_chip("Rows", format(nrow(dd), big.mark = ","))), chip2),
       tip = NULL, note = viz_dropped_note(tk$dropped))
}

# ---- pie / donut ----------------------------------------------------------

viz_plot_pie <- function(s, th) {
  x <- s$f$x
  y <- nz(s$f$y)
  dd <- viz_take(s$data, c(x, y))$data
  if (!nrow(dd)) return(viz_fail("No rows are available for this variable."))

  fn <- viz_agg_fun(s$opt$agg)
  agg <- viz_aggregate(dd, x, y, fn)
  agg <- agg[!is.na(agg[[x]]) & agg$value > 0, , drop = FALSE]
  if (!nrow(agg)) return(viz_fail("These values cannot be shown as proportions."))

  agg <- agg[order(-agg$value), , drop = FALSE]
  merged <- 0L
  if (nrow(agg) > 8) {
    merged <- nrow(agg) - 7L
    other <- data.frame(value = sum(agg$value[8:nrow(agg)]), stringsAsFactors = FALSE)
    other[[x]] <- "Other"
    agg <- rbind(agg[1:7, c(x, "value"), drop = FALSE], other[, c(x, "value"), drop = FALSE])
  }

  agg$share <- agg$value / sum(agg$value)
  agg$label <- ifelse(agg$share < 0.03, "", viz_pct(agg$share, 0))

  fills <- viz_pal(th, nrow(agg))
  names(fills) <- as.character(agg[[x]])
  agg$dark_text <- viz_luminance(unname(fills[as.character(agg[[x]])])) > 0.65

  # Native plotly pie: ggplotly cannot follow coord_polar in this stack, and a
  # native trace gives better hover text and rotation for free.
  widget <- NULL
  if (viz_have_plotly()) {
    widget <- plotly::plot_ly(
      labels = as.character(agg[[x]]), values = agg$value, type = "pie",
      hole = if (isTRUE(s$opt$donut)) 0.45 else 0,
      marker = list(colors = unname(fills[as.character(agg[[x]])]),
                    line = list(color = th$bg, width = 2)),
      textinfo = if (isTRUE(s$opt$labels)) "percent" else "none",
      textposition = "inside",
      insidetextfont = list(color = "#FFFFFF", size = 12),
      sort = FALSE, direction = "clockwise",
      hovertemplate = paste0("%{label}<br>Value: %{value}<br>%{percent}<extra></extra>"),
      showlegend = isTRUE(s$legend)
    ) |>
      plotly::layout(
        title = list(text = s$title, x = 0.02, xanchor = "left",
                     font = list(size = 17, color = th$ink,
                                 family = "Inter, Segoe UI, sans-serif")),
        font = list(family = "Inter, Segoe UI, sans-serif", color = "#475569"),
        margin = list(l = 20, r = 20, t = 58, b = 20),
        paper_bgcolor = th$bg, plot_bgcolor = th$bg,
        hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = th$grid,
                          font = list(color = th$ink))
      ) |>
      plotly::config(displaylogo = FALSE, responsive = TRUE,
                     modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScaleX",
                                                "autoScale2d"))
  }

  # Static version for PNG / SVG export.
  agg2 <- agg[rev(seq_len(nrow(agg))), , drop = FALSE]
  agg2$ymax <- cumsum(agg2$share)
  agg2$ymin <- c(0, utils::head(agg2$ymax, -1))
  agg2$mid <- (agg2$ymax + agg2$ymin) / 2
  hole <- if (isTRUE(s$opt$donut)) 1.15 else 0
  rad <- if (hole > 0) 1.58 else 1.22

  p <- ggplot2::ggplot(agg2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = hole, xmax = 2, ymin = ymin, ymax = ymax,
                                    fill = .data[[x]]),
                       colour = th$bg, linewidth = 0.9) +
    ggplot2::scale_fill_manual(values = fills) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(0, 2)

  if (isTRUE(s$opt$labels)) {
    dark <- agg2[agg2$dark_text, , drop = FALSE]
    light <- agg2[!agg2$dark_text, , drop = FALSE]
    if (nrow(dark))
      p <- p + ggplot2::geom_text(data = dark, ggplot2::aes(x = rad, y = mid, label = label),
                                  colour = th$ink, size = 3.2, fontface = "bold")
    if (nrow(light))
      p <- p + ggplot2::geom_text(data = light, ggplot2::aes(x = rad, y = mid, label = label),
                                  colour = "#FFFFFF", size = 3.2, fontface = "bold")
  }

  notes <- if (merged > 0)
    paste0(merged, " smaller categories were merged into 'Other'.") else character()

  list(plot = viz_finish_pie(p, s, th, legend_title = x),
       widget = widget,
       chips = list(viz_chip("Categories", as.character(nrow(agg))),
                    viz_chip(if (is.null(y)) "Rows" else y, viz_num(sum(agg$value), 4))),
       tip = "text", note = notes)
}

# ---- density --------------------------------------------------------------

viz_plot_density <- function(s, th) {
  x <- s$f$x
  g <- nz(s$f$color)
  tk <- viz_take(s$data, c(x, g))
  cap <- viz_cap(tk$data, 20000)
  dd <- cap$data
  if (!nrow(dd)) return(viz_fail("No rows are available for this variable."))

  grouped <- !is.null(g)
  a <- if (grouped) ggplot2::aes(x = .data[[x]], fill = .data[[g]], colour = .data[[g]])
       else ggplot2::aes(x = .data[[x]], group = 1)
  p <- ggplot2::ggplot(dd, a)
  p <- p + if (grouped) ggplot2::geom_density(alpha = 0.28, linewidth = 0.8, na.rm = TRUE)
           else ggplot2::geom_density(fill = th$pal[1], colour = th$pal[1], alpha = 0.28,
                                      linewidth = 0.9, na.rm = TRUE)

  if (grouped)
    p <- p + ggplot2::scale_fill_manual(values = viz_pal(th, viz_ngroups(dd[[g]]))) +
      ggplot2::scale_colour_manual(values = viz_pal(th, viz_ngroups(dd[[g]])))

  p <- p +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = c(0, 0.06)))

  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("density", s$f),
                         ylab = "Density",
                         sub = "Smoothed distribution; the area under each curve is one",
                         legend_title = g),
       chips = list(viz_chip("Rows", format(nrow(dd), big.mark = ",")),
                    viz_chip("Groups", if (grouped)
                      as.character(viz_ngroups(dd[[g]])) else "1")),
       tip = NULL,
       note = c(viz_dropped_note(tk$dropped), viz_sampled_note(cap)))
}

# ---- Q-Q plot -------------------------------------------------------------

viz_plot_qq <- function(s, th) {
  x <- s$f$x
  g <- nz(s$f$color)
  tk <- viz_take(s$data, c(x, g))
  cap <- viz_cap(tk$data, 20000)
  dd <- cap$data
  if (!nrow(dd)) return(viz_fail("No rows are available for this variable."))

  grouped <- !is.null(g)
  dd$.grp <- if (grouped) as.character(dd[[g]]) else "All rows"

  sp <- split(dd[[x]], dd$.grp)
  pts <- do.call(rbind, lapply(names(sp), function(k) {
    v <- sort(sp[[k]][is.finite(sp[[k]])])
    n <- length(v)
    if (n < 3) return(NULL)
    data.frame(grp = k, theo = stats::qnorm(stats::ppoints(n)), sample = v,
               stringsAsFactors = FALSE)
  }))
  if (is.null(pts) || !nrow(pts))
    return(viz_fail("At least three values are needed for a Q-Q plot."))

  pts$tip <- paste0("Theoretical: ", round(pts$theo, 2),
                    "<br>Sample: ", viz_comma(pts$sample))

  lines <- do.call(rbind, lapply(split(pts, pts$grp), function(z) {
    q <- stats::quantile(z$sample, c(0.25, 0.75), names = FALSE)
    tq <- stats::qnorm(c(0.25, 0.75))
    sl <- if (diff(tq) != 0) diff(q) / diff(tq) else 0
    data.frame(grp = z$grp[[1]], slope = sl, intercept = q[[1]] - sl * tq[[1]],
               stringsAsFactors = FALSE)
  }))

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = theo, y = sample))
  p <- p + if (grouped)
    ggplot2::geom_point(ggplot2::aes(colour = grp, text = tip), size = 1.8, alpha = 0.75)
  else
    ggplot2::geom_point(ggplot2::aes(text = tip), size = 1.8, alpha = 0.75,
                        colour = th$pal[1])
  p <- p + if (grouped)
    ggplot2::geom_abline(data = lines,
                         ggplot2::aes(slope = slope, intercept = intercept, colour = grp),
                         linetype = "dashed", linewidth = 0.6, show.legend = FALSE)
  else
    ggplot2::geom_abline(slope = lines$slope[[1]],
                         intercept = lines$intercept[[1]],
                         colour = th$ink, linetype = "dashed", linewidth = 0.6)

  if (grouped)
    p <- p + ggplot2::scale_colour_manual(values = viz_pal(th, length(unique(pts$grp))))

  p <- p +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.04)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.04))

  list(plot = viz_finish(p, s, th, xlab = "Theoretical quantiles",
                         ylab = viz_auto_ylab("qq", s$f),
                         sub = "Points close to the dashed line suggest a normal distribution",
                         legend_title = g),
       chips = list(viz_chip("Rows", format(nrow(pts), big.mark = ",")),
                    viz_chip("Skewness", viz_num(viz_skewness(dd[[x]]), 2))),
       tip = "text",
       note = c(viz_dropped_note(tk$dropped), viz_sampled_note(cap)))
}
