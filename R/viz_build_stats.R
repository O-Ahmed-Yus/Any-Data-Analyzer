# Visualisation studio - statistical and evaluation charts -------------------
# Correlation heatmap, mean with error bars, confusion matrix and ROC curve.
# All statistics are computed from the rows actually plotted.

# ---- correlation heatmap --------------------------------------------------

viz_plot_corr <- function(s, th) {
  vars <- s$f$vars
  if (length(vars) < 2) return(viz_fail("Select at least two numeric variables."))
  vars <- utils::head(vars, 10)
  tk <- viz_take(s$data, vars)
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No complete rows are available for these variables."))
  if (length(vars) < 2) return(viz_fail("Select at least two numeric variables."))

  method <- if (identical(s$opt$method, "spearman")) "spearman" else "pearson"
  m <- stats::cor(dd, method = method)
  if (anyNA(m)) {
    keep <- vars[stats::complete.cases(t(m))]
    if (length(keep) < 2) return(viz_fail("These variables vary too little to correlate."))
    vars <- keep
    m <- stats::cor(dd[, vars, drop = FALSE], method = method)
  }

  df <- expand.grid(Var1 = vars, Var2 = vars, stringsAsFactors = FALSE)
  df$value <- as.vector(m)
  df$Var1 <- factor(df$Var1, levels = rev(vars))
  df$Var2 <- factor(df$Var2, levels = vars)
  df$label <- ifelse(is.na(df$value), "–", format(round(df$value, 2), nsmall = 2))
  df$label_col <- ifelse(abs(df$value) >= 0.6, "#FFFFFF", th$ink)

  nv <- length(vars)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Var2, y = Var1, fill = value)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 1.1) +
    ggplot2::geom_text(ggplot2::aes(label = label), colour = df$label_col,
                       size = if (nv <= 5) 3.9 else if (nv <= 8) 3.3 else 2.8,
                       fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = th$div_low, mid = th$div_mid, high = th$div_high,
                                  midpoint = 0, limits = c(-1, 1), na.value = "#F1F5F9",
                                  name = if (identical(method, "spearman")) "rho" else "r") +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 0),
                   panel.grid.major = ggplot2::element_blank())

  upper <- m[upper.tri(m)]
  best_i <- if (length(upper)) which.max(abs(upper)) else NA_integer_
  pair <- if (!is.na(best_i)) {
    ij <- which(upper.tri(m), arr.ind = TRUE)[best_i, ]
    paste0(rownames(m)[ij[1]], " / ", colnames(m)[ij[2]])
  } else NULL

  chips <- list(viz_chip("Variables", as.character(nv)),
                viz_chip("Rows", format(nrow(dd), big.mark = ",")),
                viz_chip("Method", tools::toTitleCase(method)))
  if (!is.null(pair)) chips <- c(chips, list(viz_chip("Strongest pair", pair)))

  list(plot = viz_finish(p, s, th, xlab = NULL, ylab = NULL,
                         sub = paste0(tools::toTitleCase(method),
                                      " correlation of complete observations")),
       chips = chips, tip = NULL,
       note = viz_dropped_note(tk$dropped))
}

# ---- mean with error bars -------------------------------------------------

viz_plot_errorbar <- function(s, th) {
  x <- s$f$x
  y <- s$f$y
  tk <- viz_take(s$data, c(x, y))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows are available for these variables."))

  res <- dd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(x))) |>
    dplyr::summarise(n = dplyr::n(),
                     avg = mean(.data[[y]], na.rm = TRUE),
                     sd = stats::sd(.data[[y]], na.rm = TRUE),
                     .groups = "drop")
  res <- res[!is.na(res[[x]]), , drop = FALSE]
  few <- sum(res$n < 2 | is.na(res$sd))
  res <- res[res$n >= 2 & !is.na(res$sd), , drop = FALSE]
  if (!nrow(res)) return(viz_fail("Each group needs at least two values to show error bars."))

  err <- if (identical(s$opt$err, "se")) "se" else if (identical(s$opt$err, "sd")) "sd" else "ci"
  res$half <- switch(err,
    se = res$sd / sqrt(res$n),
    sd = res$sd,
    stats::qt(0.975, res$n - 1) * res$sd / sqrt(res$n))
  res$lo <- res$avg - res$half
  res$hi <- res$avg + res$half
  lv <- sort(unique(as.character(res[[x]])))
  res[[x]] <- factor(as.character(res[[x]]), levels = lv)
  res$tip <- paste0(x, ": ", res[[x]], "<br>Mean: ", viz_num(res$avg),
                    "<br>n: ", res$n)

  horiz <- identical(s$opt$orient, "h")
  if (horiz) {
    p <- ggplot2::ggplot(res, ggplot2::aes(y = .data[[x]], x = avg)) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y",
                             width = 0.16, colour = th$muted, linewidth = 0.7)
  } else {
    p <- ggplot2::ggplot(res, ggplot2::aes(x = .data[[x]], y = avg)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi),
                             width = 0.16, colour = th$muted, linewidth = 0.7)
  }
  p <- p + ggplot2::geom_point(size = 3.1, colour = th$pal[1], shape = 21,
                               fill = th$pal[1], stroke = 0)

  if (isTRUE(s$opt$labels)) {
    p <- p + if (horiz)
      ggplot2::geom_text(ggplot2::aes(x = hi, label = viz_num(avg)), hjust = -0.35,
                         size = 3.3, colour = th$muted)
    else
      ggplot2::geom_text(ggplot2::aes(y = hi, label = viz_num(avg)), vjust = -0.7,
                         size = 3.3, colour = th$muted)
  }

  if (horiz) {
    p <- p + ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = ","),
                                         expand = ggplot2::expansion(mult = c(0.02, 0.1)))
  } else {
    p <- p + viz_wrap_discrete(12) +
      ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ","),
                                  expand = ggplot2::expansion(mult = c(0.02, 0.12)))
  }

  sub <- switch(err,
    se = "Points are group means; bars are one standard error",
    sd = "Points are group means; bars are one standard deviation",
    "Points are group means; bars are 95% confidence intervals (t distribution)")

  notes <- c(viz_dropped_note(tk$dropped),
             if (few > 0) paste0(few, if (few == 1) " group was" else " groups were",
                                 " left out: fewer than two values or no variation.") else
               character())

  list(plot = viz_finish(p, s, th, xlab = viz_auto_xlab("errorbar", s$f),
                         ylab = viz_auto_ylab("errorbar", s$f), sub = sub),
       chips = list(viz_chip("Groups", as.character(nrow(res))),
                    viz_chip("Rows", format(sum(res$n), big.mark = ","))),
       tip = NULL, note = notes)
}

# ---- confusion matrix -----------------------------------------------------

viz_class_metrics <- function(m) {
  k <- nrow(m)
  tot <- sum(m)
  diagv <- diag(m)
  precision <- ifelse(colSums(m) > 0, diagv / colSums(m), NA_real_)
  recall <- ifelse(rowSums(m) > 0, diagv / rowSums(m), NA_real_)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA_real_)
  list(accuracy = sum(diagv) / tot,
       precision = mean(precision, na.rm = TRUE),
       recall = mean(recall, na.rm = TRUE),
       f1 = mean(f1, na.rm = TRUE),
       k = k)
}

viz_plot_confmat <- function(s, th) {
  actual <- s$f$actual
  predicted <- s$f$predicted
  if (identical(actual, predicted))
    return(viz_fail("Choose two different columns: the actual labels and the predicted labels."))
  tk <- viz_take(s$data, c(actual, predicted))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows have both an actual and a predicted label."))

  act <- as.character(dd[[actual]])
  pred <- as.character(dd[[predicted]])
  lv <- unique(c(sort(unique(act)), sort(unique(pred))))
  if (length(lv) < 2) return(viz_fail("The labels need at least two categories."))
  if (length(lv) > 10)
    return(viz_fail(paste0("These columns have ", length(lv),
                           " categories. Use columns with 10 or fewer labels.")))

  m <- table(actual = factor(act, levels = lv), predicted = factor(pred, levels = lv))
  met <- viz_class_metrics(m)

  norm <- s$opt$normalize
  shown <- switch(norm,
    row = prop.table(m, 1),
    col = prop.table(m, 2),
    all = m / sum(m),
    m)
  df <- expand.grid(actual = lv, predicted = lv, stringsAsFactors = FALSE)
  df$count <- as.vector(m)
  df$value <- as.vector(shown)
  df$actual <- factor(df$actual, levels = rev(lv))
  df$predicted <- factor(df$predicted, levels = lv)
  df$label <- if (identical(norm, "none")) format(df$count, big.mark = ",")
              else viz_pct(df$value, if (max(df$value, na.rm = TRUE) < 0.1) 1 else 0)
  df$label_col <- ifelse(df$value >= 0.6 * max(df$value, na.rm = TRUE), "#FFFFFF", th$ink)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = actual, fill = value)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = label), colour = df$label_col,
                       size = if (length(lv) <= 4) 4.4 else 3.2, fontface = "bold") +
    ggplot2::scale_fill_gradient(low = th$seq[1], high = th$seq[4], name = NULL,
                                 labels = if (identical(norm, "none")) scales::label_number(big.mark = ",")
                                          else scales::label_percent()) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "Predicted", y = "Actual") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank())

  sub <- if (identical(norm, "row")) "Each row is scaled to 100% of its actual class"
         else if (identical(norm, "col")) "Each column is scaled to 100% of its predicted class"
         else if (identical(norm, "all")) "All cells are scaled to 100% of the total"
         else "Cells show the number of rows; rows are actual classes"

  chips <- list(viz_chip("Accuracy", viz_pct(met$accuracy, 1)),
                viz_chip("Precision", viz_pct(met$precision, 1)),
                viz_chip("Recall", viz_pct(met$recall, 1)),
                viz_chip("F1 score", viz_num(met$f1, 3)))

  pos_note <- if (length(lv) == 2)
    paste0("Positive class: ", lv[2], ".")
  else paste0("Precision, recall and F1 are averaged over the ", length(lv), " classes.")

  list(plot = viz_finish(p, s, th, xlab = "Predicted", ylab = "Actual", sub = sub),
       chips = chips, tip = "text", note = c(pos_note, viz_dropped_note(tk$dropped)))
}

# ---- ROC curve ------------------------------------------------------------

# Sensitivity and 1 - specificity at every distinct score. Ties are handled by
# computing each threshold on the full set of tied values.
viz_roc_points <- function(score, pos) {
  uc <- sort(unique(score))
  cnt_pos <- tabulate(match(score[pos], uc), nbins = length(uc))
  cnt_neg <- tabulate(match(score[!pos], uc), nbins = length(uc))
  tp <- rev(cumsum(rev(cnt_pos)))
  fp <- rev(cumsum(rev(cnt_neg)))
  fpr <- c(0, fp / sum(cnt_neg))
  tpr <- c(0, tp / sum(cnt_pos))
  if (tail(fpr, 1) < 1 || tail(tpr, 1) < 1) {
    fpr <- c(fpr, 1)
    tpr <- c(tpr, 1)
  }
  data.frame(fpr = fpr, tpr = tpr)
}

viz_auc <- function(fpr, tpr) {
  sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2)
}

viz_plot_roc <- function(s, th) {
  actual <- s$f$actual
  score <- s$f$score
  if (identical(actual, score))
    return(viz_fail("Choose the column of true labels and a numeric score column."))
  tk <- viz_take(s$data, c(actual, score))
  dd <- tk$data
  if (!nrow(dd)) return(viz_fail("No rows have both a label and a score."))

  lab <- as.character(dd[[actual]])
  lv <- sort(unique(lab))
  if (length(lv) != 2)
    return(viz_fail(paste0("The actual label column has ", length(lv),
                           " categories. A ROC curve needs exactly two.")))
  if (!is.numeric(dd[[score]]))
    return(viz_fail("The score column must be numeric."))

  pos <- lab == lv[2]
  pts <- viz_roc_points(dd[[score]], pos)
  auc <- viz_auc(pts$fpr, pts$tpr)
  pts$tip <- paste0("1 - specificity: ", viz_pct(pts$fpr, 1),
                    "<br>Sensitivity: ", viz_pct(pts$tpr, 1))

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = th$muted, linewidth = 0.5) +
    ggplot2::geom_step(ggplot2::aes(text = tip), direction = "hv", group = 1,
                       colour = th$pal[1], linewidth = 1) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(),
                                limits = c(0, 1), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(),
                                limits = c(0, 1), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::coord_equal()

  notes <- c(viz_dropped_note(tk$dropped))
  if (any(dd[[score]] < 0 | dd[[score]] > 1))
    notes <- c(notes, "The scores are outside 0-1; they are used as a ranking only.")

  grade <- if (auc >= 0.9) "excellent" else if (auc >= 0.8) "good" else
    if (auc >= 0.7) "fair" else "poor"
  list(plot = viz_finish(p, s, th, xlab = "1 - specificity (false positive rate)",
                         ylab = "Sensitivity (true positive rate)",
                         sub = paste0("Positive class: ", lv[2], ". Diagonal = no discrimination.")),
       chips = list(viz_chip("AUC", viz_num(auc, 3)),
                    viz_chip("Rows", format(nrow(dd), big.mark = ",")),
                    viz_chip("Discrimination", grade)),
       tip = "text", note = notes)
}
