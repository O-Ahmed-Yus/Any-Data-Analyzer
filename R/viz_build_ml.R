# Visualisation studio - model evaluation charts -----------------------------
# Feature importance, actual vs predicted and residuals, plus the dispatcher
# used by the server.

# ---- feature importance ---------------------------------------------------

viz_plot_importance <- function(s, th) {
  feature <- s$f$feature
  importance <- s$f$importance
  if (identical(feature, importance))
    return(viz_fail("Choose the column of feature names and a separate numeric column of importance values."))
  tk <- viz_take(s$data, c(feature, importance))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No complete rows are available for these two columns."))

  # One row per feature: repeated feature names are averaged.
  n_before <- nrow(dd)
  dd <- dd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(feature))) |>
    dplyr::summarise(imp = mean(.data[[importance]], na.rm = TRUE), .groups = "drop")
  merged <- n_before - nrow(dd)

  dd <- dd[!is.na(dd$imp), , drop = FALSE]
  dd <- dd[order(-abs(dd$imp)), , drop = FALSE]
  n_all <- nrow(dd)
  dd <- utils::head(dd, 15)
  dd[[feature]] <- factor(as.character(dd[[feature]]),
                          levels = rev(as.character(dd[[feature]])))
  dd$tip <- paste0(dd[[feature]], "<br>Importance: ", viz_num(dd$imp, 4))

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = imp, y = .data[[feature]])) +
    ggplot2::geom_col(ggplot2::aes(text = tip), fill = th$pal[1], width = 0.68) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Importance", y = NULL)

  if (isTRUE(s$opt$labels))
    p <- p + ggplot2::geom_text(ggplot2::aes(label = viz_num(imp, 3)),
                                hjust = -0.2, size = 3.2, colour = th$muted)

  notes <- c(viz_dropped_note(tk$dropped),
             if (merged > 0) paste0(merged, " repeated feature names were averaged.") else
               character(),
             if (n_all > 15) paste0("Showing the 15 most important features out of ", n_all, ".") else
               character())

  top <- dd[[feature]][[1]]
  list(plot = viz_finish(p, s, th, xlab = "Importance", ylab = NULL,
                         sub = "Longer bars indicate a larger contribution to the model"),
       chips = list(viz_chip("Features", as.character(n_all)),
                    viz_chip("Top feature", as.character(top))),
       tip = "text", note = notes)
}

# ---- actual vs predicted --------------------------------------------------

viz_reg_metrics <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  a <- actual[ok]; p <- predicted[ok]
  if (!length(a)) return(NULL)
  resid <- a - p
  ss_res <- sum(resid^2)
  ss_tot <- sum((a - mean(a))^2)
  list(n = length(a), rmse = sqrt(mean(resid^2)), mae = mean(abs(resid)),
       bias = mean(resid), r2 = if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_)
}

viz_plot_actualpred <- function(s, th) {
  actual <- s$f$actual
  predicted <- s$f$predicted
  if (identical(actual, predicted))
    return(viz_fail("Choose two different columns: the observed values and the model predictions."))
  tk <- viz_take(s$data, c(actual, predicted))
  cap <- viz_cap(tk$data)
  dd <- cap$data
  if (!nrow(dd)) return(viz_fail("No rows have both an actual and a predicted value."))

  met <- viz_reg_metrics(dd[[actual]], dd[[predicted]])
  dd$tip <- paste0("Actual: ", viz_comma(dd[[actual]]),
                   "<br>Predicted: ", viz_comma(dd[[predicted]]))

  lim <- range(c(dd[[actual]], dd[[predicted]]), na.rm = TRUE)
  lim <- lim + c(-0.03, 0.03) * diff(lim)

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[[actual]], y = .data[[predicted]])) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = th$muted, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(text = tip), size = 2, alpha = 0.7, stroke = 0,
                        colour = th$pal[1]) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                limits = lim, expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                limits = lim, expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::coord_equal()

  list(plot = viz_finish(p, s, th, xlab = actual, ylab = predicted,
                         sub = "Dashed line = perfect prediction"),
       chips = list(viz_chip("R squared", viz_num(met$r2, 3)),
                    viz_chip("RMSE", viz_num(met$rmse, 3)),
                    viz_chip("MAE", viz_num(met$mae, 3)),
                    viz_chip("Rows", format(met$n, big.mark = ","))),
       tip = "text", note = c(viz_dropped_note(tk$dropped), viz_sampled_note(cap)))
}

# ---- residual plot --------------------------------------------------------

viz_plot_residual <- function(s, th) {
  actual <- s$f$actual
  predicted <- s$f$predicted
  if (identical(actual, predicted))
    return(viz_fail("Choose two different columns: the observed values and the model predictions."))
  tk <- viz_take(s$data, c(actual, predicted))
  cap <- viz_cap(tk$data)
  dd <- cap$data
  if (!nrow(dd)) return(viz_fail("No rows have both an actual and a predicted value."))

  dd$resid <- dd[[actual]] - dd[[predicted]]
  met <- viz_reg_metrics(dd[[actual]], dd[[predicted]])
  dd$tip <- paste0("Predicted: ", viz_comma(dd[[predicted]]),
                   "<br>Residual: ", viz_comma(dd$resid))

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = .data[[predicted]], y = resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = th$muted,
                        linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(text = tip), size = 2, alpha = 0.7, stroke = 0,
                        colour = th$pal[1]) +
    ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                expand = ggplot2::expansion(mult = 0.06))

  list(plot = viz_finish(p, s, th, xlab = predicted, ylab = "Residual (actual - predicted)",
                         sub = "Points scattered evenly around zero indicate a well-behaved model"),
       chips = list(viz_chip("Mean residual", viz_num(met$bias, 3)),
                    viz_chip("RMSE", viz_num(met$rmse, 3)),
                    viz_chip("Rows", format(met$n, big.mark = ","))),
       tip = "text", note = c(viz_dropped_note(tk$dropped), viz_sampled_note(cap)))
}

# ---- dispatcher -----------------------------------------------------------

viz_builders <- function() {
  list(bar = viz_plot_bar, line = viz_plot_line, scatter = viz_plot_scatter,
       hist = viz_plot_hist, box = viz_plot_box, pie = viz_plot_pie,
       density = viz_plot_density, corr = viz_plot_corr, qq = viz_plot_qq,
       errorbar = viz_plot_errorbar, confmat = viz_plot_confmat,
       roc = viz_plot_roc, importance = viz_plot_importance,
       actualpred = viz_plot_actualpred, residual = viz_plot_residual)
}

viz_build_chart <- function(s) {
  th <- viz_theme_get(s$theme)
  builders <- viz_builders()
  fn <- builders[[s$type]]
  if (is.null(fn)) return(viz_fail("This chart type is not available."))

  out <- tryCatch(fn(s, th), error = function(e) {
    viz_fail(paste0("This chart could not be drawn: ", conditionMessage(e),
                    ". Try different variables."))
  })
  if (is.null(out$plot) && is.null(out$error)) out$error <- "This chart could not be drawn."
  out
}
