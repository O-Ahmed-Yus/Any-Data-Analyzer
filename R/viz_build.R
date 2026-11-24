# Visualisation studio - chart builders --------------------------------------
# A builder takes one spec (type, data, mapped fields, options, theme) and
# returns a list with the ggplot object, the summary values shown above the
# chart, and any note the user should read (sampling, dropped rows, merged
# categories). Nothing here reads Shiny inputs directly.

viz_chip <- function(label, value) list(label = label, value = value)

viz_fail <- function(message) list(error = message)

# Deterministic sampling that leaves the session RNG untouched.
viz_with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv())
  old <- if (had) get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  force(expr)
}

viz_cap <- function(d, max = 8000) {
  n <- nrow(d)
  if (n <= max) return(list(data = d, sampled = FALSE, n = n))
  idx <- viz_with_seed(90421, sample.int(n, max))
  list(data = d[idx, , drop = FALSE], sampled = TRUE, n = n)
}

# Rows used by a chart: selected columns, complete cases only.
viz_take <- function(d, vars) {
  vars <- unique(vars[nzchar(vars)])
  vars <- vars[vars %in% names(d)]
  dd <- d[, vars, drop = FALSE]
  ok <- if (length(vars)) stats::complete.cases(dd) else rep(TRUE, nrow(dd))
  list(data = dd[ok, , drop = FALSE], dropped = sum(!ok))
}

viz_dropped_note <- function(n) {
  if (!n) return(character())
  paste0(n, if (n == 1) " row" else " rows", " with missing values were left out.")
}

viz_sampled_note <- function(cap) {
  if (!isTRUE(cap$sampled)) return(character())
  paste0("Showing a random sample of ", format(nrow(cap$data), big.mark = ","),
         " points out of ", format(cap$n, big.mark = ","), ".")
}

viz_agg_fun <- function(key) {
  switch(key,
    mean = function(z) mean(z, na.rm = TRUE),
    median = function(z) stats::median(z, na.rm = TRUE),
    sum = function(z) sum(z, na.rm = TRUE),
    function(z) mean(z, na.rm = TRUE))
}

viz_agg_word <- function(key) {
  switch(key, mean = "Mean", median = "Median", sum = "Total", "Value")
}

viz_aggregate <- function(dd, grp, value, fn) {
  if (is.null(value) || !nzchar(value)) {
    dplyr::count(dd, dplyr::across(dplyr::all_of(grp)), name = "value")
  } else {
    dd |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::summarise(value = fn(.data[[value]]), .groups = "drop")
  }
}

# Order of a discrete axis. `totals` maps category to its total value.
viz_levels <- function(totals, how, first_seen = NULL) {
  lv <- names(totals)
  switch(how,
    desc = lv[order(-totals)],
    asc = lv[order(totals)],
    name = sort(lv),
    if (is.null(first_seen)) lv else first_seen[first_seen %in% lv])
}

viz_wrap_discrete <- function(width = 14) {
  ggplot2::scale_x_discrete(labels = function(x) {
    vapply(as.character(x), function(v) {
      if (nchar(v) <= width) v
      else paste(strwrap(v, width = width), collapse = "\n")
    }, character(1))
  })
}

# ---- bar ------------------------------------------------------------------

viz_plot_bar <- function(s, th) {
  x <- s$f$x
  y <- nz(s$f$y)
  g <- nz(s$f$color)
  dd <- viz_take(s$data, c(x, y, g))$data
  if (!nrow(dd)) return(viz_fail("No rows are available for these variables."))

  grp <- c(x, g)
  fn <- viz_agg_fun(s$opt$agg)
  agg <- viz_aggregate(dd, grp, y, fn)
  agg <- agg[!is.na(agg[[x]]), , drop = FALSE]
  if (!nrow(agg)) return(viz_fail("No rows are available for these variables."))

  totals <- tapply(agg$value, as.character(agg[[x]]), sum)
  first_seen <- unique(as.character(dd[[x]]))
  lv <- viz_levels(totals, s$opt$sort, first_seen)
  agg[[x]] <- factor(as.character(agg[[x]]), levels = lv)

  horiz <- identical(s$opt$orient, "h")
  ylab <- if (is.null(y)) "Number of rows" else paste(viz_agg_word(s$opt$agg), y)
  lab <- if (is.null(y)) paste0(x, ": ", agg[[x]], "<br>Rows: ", viz_comma(agg$value))
         else paste0(x, ": ", agg[[x]], "<br>", ylab, ": ", viz_comma(agg$value))
  if (!is.null(g))
    lab <- paste0(lab, "<br>", g, ": ", as.character(agg[[g]]))
  agg$tip <- lab

  grouped <- !is.null(g)
  pos <- if (!grouped) "identity"
         else if (identical(s$opt$pos, "stack")) "stack" else "dodge"
  position <- if (pos == "dodge") ggplot2::position_dodge2(preserve = "single", padding = 0.06)
              else if (pos == "stack") "stack" else "identity"

  if (horiz) {
    agg[[x]] <- factor(agg[[x]], levels = rev(lv))
    a <- if (grouped) ggplot2::aes(y = .data[[x]], x = value, fill = .data[[g]], text = tip)
         else ggplot2::aes(y = .data[[x]], x = value, text = tip)
    lay <- if (grouped) ggplot2::geom_col(a, width = 0.72, position = position)
           else ggplot2::geom_col(a, width = 0.72, fill = th$pal[1])
    p <- ggplot2::ggplot(agg, a) + lay
    if (isTRUE(s$opt$labels))
      p <- p + ggplot2::geom_text(ggplot2::aes(label = viz_comma(value)),
                                  hjust = -0.18, size = 3.3, colour = th$muted)
    p <- p + ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, if (isTRUE(s$opt$labels)) 0.16 else 0.05)))
    sub <- if (is.null(y)) "Count of rows in each category" else
      paste0(viz_agg_word(s$opt$agg), " of ", y, " per category")
  } else {
    a <- if (grouped) ggplot2::aes(x = .data[[x]], y = value, fill = .data[[g]], text = tip)
         else ggplot2::aes(x = .data[[x]], y = value, text = tip)
    lay <- if (grouped) ggplot2::geom_col(a, width = 0.72, position = position)
           else ggplot2::geom_col(a, width = 0.72, fill = th$pal[1])
    p <- ggplot2::ggplot(agg, a) + lay
    if (isTRUE(s$opt$labels))
      p <- p + ggplot2::geom_text(ggplot2::aes(label = viz_comma(value)),
                                  vjust = -0.45, size = 3.3, colour = th$muted)
    p <- p + viz_wrap_discrete() +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(big.mark = ","),
        expand = ggplot2::expansion(mult = c(0, if (isTRUE(s$opt$labels)) 0.12 else 0.05)))
    sub <- if (is.null(y)) "Count of rows in each category" else
      paste0(viz_agg_word(s$opt$agg), " of ", y, " per category")
  }

  if (grouped)
    p <- p + ggplot2::scale_fill_manual(values = viz_pal(th, viz_ngroups(agg[[g]])))

  chips <- list(viz_chip("Categories", format(length(lv), big.mark = ",")))
  chips <- c(chips, list(viz_chip(if (is.null(y)) "Rows" else viz_agg_word(s$opt$agg),
                                  viz_num(sum(agg$value), 4))))
  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("bar", s$f),
                         ylab = viz_auto_ylab("bar", s$f), sub = sub,
                         legend_title = g), chips = chips, tip = "text",
       note = character())
}

# ---- line -----------------------------------------------------------------

viz_date_labels <- function(x) {
  rng <- suppressWarnings(diff(range(x, na.rm = TRUE)))
  if (!is.finite(rng)) return("%d %b")
  if (rng <= 62) "%d %b" else if (rng <= 1100) "%b %Y" else "%Y"
}

viz_plot_line <- function(s, th) {
  x <- s$f$x
  y <- s$f$y
  g <- nz(s$f$color)
  tk <- viz_take(s$data, c(x, y, g))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows are available for these variables."))

  is_date <- inherits(dd[[x]], "Date") || inherits(dd[[x]], "POSIXt")
  if (is_date) dd[[x]] <- as.Date(dd[[x]])

  grp <- c(x, g)
  fn <- viz_agg_fun(s$opt$line_agg)
  agg <- viz_aggregate(dd, grp, y, fn)
  if (is_date || is.numeric(agg[[x]])) agg <- agg[order(agg[[x]]), , drop = FALSE]
  if (is.factor(agg[[x]])) agg <- agg[order(agg[[x]]), , drop = FALSE]

  agg$tip <- paste0(
    if (is_date) format(agg[[x]], "%d %b %Y") else as.character(agg[[x]]),
    "<br>", y, ": ", viz_comma(agg$value),
    if (!is.null(g)) paste0("<br>", g, ": ", as.character(agg[[g]])) else "")

  grouped <- !is.null(g)
  a <- if (grouped) ggplot2::aes(x = .data[[x]], y = value, colour = .data[[g]],
                                 group = .data[[g]], text = tip)
       else ggplot2::aes(x = .data[[x]], y = value, text = tip, group = 1)
  p <- ggplot2::ggplot(agg, a)
  p <- p + if (grouped) ggplot2::geom_line(linewidth = 1, na.rm = TRUE)
           else ggplot2::geom_line(linewidth = 1, colour = th$pal[1], na.rm = TRUE)

  if (isTRUE(s$opt$points) && nrow(agg) <= 80)
    p <- p + ggplot2::geom_point(size = 1.7, na.rm = TRUE)

  if (grouped)
    p <- p + ggplot2::scale_colour_manual(values = viz_pal(th, viz_ngroups(agg[[g]])))

  if (is_date) p <- p + ggplot2::scale_x_date(labels = scales::label_date(viz_date_labels(agg[[x]])),
                                              expand = ggplot2::expansion(mult = 0.02))
  else if (is.numeric(agg[[x]])) p <- p + ggplot2::scale_x_continuous(
    labels = scales::label_number(big.mark = ","),
    expand = ggplot2::expansion(mult = 0.02))
  else p <- p + viz_wrap_discrete(12)

  p <- p + ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","))

  reps <- nrow(dd) - nrow(agg)
  sub <- if (reps > 0)
    paste0(viz_agg_word(s$opt$line_agg), " where several rows share the same x value")
  else NULL
  notes <- c(viz_dropped_note(tk$dropped),
             if (reps > 0) paste0(format(reps, big.mark = ","),
                                  " repeated x values were summarised.") else character())

  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("line", s$f),
                         ylab = viz_auto_ylab("line", s$f), sub = sub,
                         legend_title = g),
       chips = list(viz_chip("Points", format(nrow(agg), big.mark = ",")),
                    viz_chip("Series", if (grouped) as.character(viz_ngroups(agg[[g]])) else "1")),
       tip = "text", note = notes)
}

# ---- scatter --------------------------------------------------------------

viz_plot_scatter <- function(s, th) {
  x <- s$f$x
  y <- s$f$y
  g <- nz(s$f$color)
  tk <- viz_take(s$data, c(x, y, g))
  cap <- viz_cap(tk$data)
  dd <- cap$data
  if (!nrow(dd)) return(viz_fail("No rows are available for these variables."))

  dd$tip <- paste0(
    x, ": ", viz_comma(dd[[x]]), "<br>", y, ": ", viz_comma(dd[[y]]),
    if (!is.null(g)) paste0("<br>", g, ": ", as.character(dd[[g]])) else "")

  grouped <- !is.null(g)
  a <- if (grouped) ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[g]], text = tip)
       else ggplot2::aes(x = .data[[x]], y = .data[[y]], text = tip)
  p <- ggplot2::ggplot(dd, a)
  p <- p + if (grouped) ggplot2::geom_point(size = 2.1, alpha = 0.7, stroke = 0, na.rm = TRUE)
           else ggplot2::geom_point(size = 2.1, alpha = 0.7, stroke = 0, colour = th$pal[1],
                                    na.rm = TRUE)

  trend <- s$opt$trend
  if (!identical(trend, "none")) {
    method <- if (identical(trend, "loess")) "loess" else "lm"
    if (method == "loess" && nrow(dd) < 20) {
      method <- "lm"
      trend <- "lm"
    }
    sm <- if (grouped) ggplot2::aes(x = .data[[x]], y = .data[[y]], group = .data[[g]])
          else ggplot2::aes(x = .data[[x]], y = .data[[y]], group = 1)
    p <- p + ggplot2::geom_smooth(
      mapping = sm, data = dd, inherit.aes = FALSE,
      method = method, formula = y ~ x, se = TRUE,
      colour = th$ink, fill = th$pal[1], alpha = 0.14, linewidth = 0.8,
      show.legend = FALSE, na.rm = TRUE)
  }

  if (grouped)
    p <- p + ggplot2::scale_colour_manual(values = viz_pal(th, viz_ngroups(dd[[g]])))

  p <- p +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.03))

  r <- suppressWarnings(stats::cor(dd[[x]], dd[[y]], use = "complete.obs"))
  chips <- list(viz_chip("Rows", format(nrow(dd), big.mark = ",")),
                viz_chip("Pearson r", viz_num(r, 3)))
  sub <- switch(trend,
    lm = "Straight trend line with a 95% confidence band",
    loess = "Smoothed (LOESS) trend with a 95% confidence band",
    NULL)
  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("scatter", s$f),
                         ylab = viz_auto_ylab("scatter", s$f), sub = sub,
                         legend_title = g),
       chips = chips, tip = "text",
       note = c(viz_dropped_note(tk$dropped), viz_sampled_note(cap)))
}


