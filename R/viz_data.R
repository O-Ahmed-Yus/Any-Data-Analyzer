# Visualisation studio - data profiling and chart registry -------------------
# One dataset is profiled once per session revision. The registry says what
# each chart needs; everything else (which controls appear, which columns are
# offered, which defaults are chosen) is derived from it.

# ---- column types ---------------------------------------------------------

viz_type_of <- function(x) {
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.logical(x)) return("logical")
  if (is.factor(x) || is.character(x)) return("factor")
  if (is.numeric(x)) return("numeric")
  "other"
}

viz_profile <- function(d) {
  d <- as.data.frame(d)
  nh <- attr(d, "nh")
  ty <- vapply(d, viz_type_of, character(1))
  lv <- vapply(d, function(x) length(unique(x[!is.na(x)])), integer(1))
  names(d) <- make.unique(names(d))
  labels <- names(d)[ty %in% c("factor", "logical") |
                       (ty == "numeric" & lv <= 10)]
  list(
    data = d, n = nrow(d), p = ncol(d), types = ty, levels = lv,
    num = names(d)[ty == "numeric"],
    cat = names(d)[ty %in% c("factor", "logical")],
    date = names(d)[ty %in% c("date", "datetime")],
    label = labels
  )
}

# Candidate columns for a field, in dataset order, with the readable type.
viz_candidates <- function(p, types) {
  keep <- vapply(seq_along(p$types), function(i) {
    t <- p$types[[i]]
    ("num" %in% types && t == "numeric") ||
      ("cat" %in% types && t %in% c("factor", "logical")) ||
      ("date" %in% types && t %in% c("date", "datetime")) ||
      ("any" %in% types) ||
      ("label" %in% types && names(p$data)[i] %in% p$label)
  }, logical(1))
  names(p$data)[keep]
}

# ---- chart registry -------------------------------------------------------

viz_f <- function(label, types, required = TRUE, multiple = FALSE,
                  none = NULL, hint = NULL) {
  list(label = label, types = types, required = required, multiple = multiple,
       none = none, hint = hint)
}

# groups: Basic / Statistical / ML. `options` lists the chart-specific controls
# shown inside Customize chart (and only for that chart).
viz_charts <- function() {
  list(
    bar = list(
      label = "Bar chart", group = "Basic", icon = "bar",
      desc = "Compare categories",
      fields = list(
        x = viz_f("Category", "cat"),
        y = viz_f("Value", "num", required = FALSE, none = "Count of rows"),
        color = viz_f("Group by", "cat", required = FALSE, none = "None")
      ),
      options = c("orient", "sort", "agg", "pos", "labels")
    ),
    line = list(
      label = "Line chart", group = "Basic", icon = "line",
      desc = "Trend over time",
      fields = list(
        x = viz_f("X axis (time or number)", c("date", "num", "cat")),
        y = viz_f("Value", "num"),
        color = viz_f("Group by", "cat", required = FALSE, none = "None")
      ),
      options = c("line_agg", "points")
    ),
    scatter = list(
      label = "Scatterplot", group = "Basic", icon = "scatter",
      desc = "Relationship between two variables",
      fields = list(
        x = viz_f("Horizontal axis", "num"),
        y = viz_f("Vertical axis", "num"),
        color = viz_f("Colour by", "cat", required = FALSE, none = "None")
      ),
      options = c("trend")
    ),
    hist = list(
      label = "Histogram", group = "Basic", icon = "hist",
      desc = "Distribution of one variable",
      fields = list(
        x = viz_f("Numeric variable", "num")
      ),
      options = c("bins")
    ),
    box = list(
      label = "Boxplot", group = "Basic", icon = "box",
      desc = "Spread and outliers",
      fields = list(
        y = viz_f("Numeric variable", "num"),
        x = viz_f("Compare groups", "cat", required = FALSE, none = "None")
      ),
      options = c("labels")
    ),
    pie = list(
      label = "Pie / Donut", group = "Basic", icon = "pie",
      desc = "Simple proportions",
      fields = list(
        x = viz_f("Category", "cat"),
        y = viz_f("Value", "num", required = FALSE, none = "Count of rows")
      ),
      options = c("donut", "agg", "labels")
    ),

    density = list(
      label = "Density plot", group = "Statistical", icon = "density",
      desc = "Smoothed distribution",
      fields = list(
        x = viz_f("Numeric variable", "num"),
        color = viz_f("Compare groups", "cat", required = FALSE, none = "None")
      ),
      options = character()
    ),
    corr = list(
      label = "Correlation", group = "Statistical", icon = "corr",
      desc = "Correlation heatmap",
      fields = list(
        vars = viz_f("Numeric variables (2 to 10)", "num", multiple = TRUE)
      ),
      options = c("method")
    ),
    qq = list(
      label = "Q-Q plot", group = "Statistical", icon = "qq",
      desc = "Check normality",
      fields = list(
        x = viz_f("Numeric variable", "num"),
        color = viz_f("Compare groups", "cat", required = FALSE, none = "None")
      ),
      options = character()
    ),
    errorbar = list(
      label = "Error bars", group = "Statistical", icon = "error",
      desc = "Means with uncertainty",
      fields = list(
        x = viz_f("Group", "cat"),
        y = viz_f("Numeric variable", "num")
      ),
      options = c("err", "labels", "orient")
    ),

    confmat = list(
      label = "Confusion matrix", group = "ML", icon = "confmat",
      desc = "Classification accuracy",
      fields = list(
        actual = viz_f("Actual label", "label"),
        predicted = viz_f("Predicted label", "label")
      ),
      options = c("normalize")
    ),
    roc = list(
      label = "ROC curve", group = "ML", icon = "roc",
      desc = "Binary classification",
      fields = list(
        actual = viz_f("Actual label (2 categories)", "label"),
        score = viz_f("Predicted probability", "num")
      ),
      options = character()
    ),
    importance = list(
      label = "Feature importance", group = "ML", icon = "importance",
      desc = "Rank predictors",
      fields = list(
        feature = viz_f("Feature", "cat"),
        importance = viz_f("Importance", "num")
      ),
      options = c("labels")
    ),
    actualpred = list(
      label = "Actual vs predicted", group = "ML", icon = "actualpred",
      desc = "Regression fit",
      fields = list(
        actual = viz_f("Actual value", "num"),
        predicted = viz_f("Predicted value", "num")
      ),
      options = character()
    ),
    residual = list(
      label = "Residuals", group = "ML", icon = "residual",
      desc = "Model diagnostics",
      fields = list(
        actual = viz_f("Actual value", "num"),
        predicted = viz_f("Predicted value", "num")
      ),
      options = character()
    )
  )
}

viz_chart <- function(type) {
  ch <- viz_charts()
  if (is.null(type) || !type %in% names(ch)) ch[[viz_default_type()]] else ch[[type]]
}

viz_chart_types <- function() names(viz_charts())

viz_default_type <- function() "hist"

viz_group_types <- function(group) {
  ch <- viz_charts()
  names(ch)[vapply(ch, function(x) identical(x$group, group), logical(1))]
}

# ---- defaults -------------------------------------------------------------

# A categorical column that is pleasant to chart: at least two, at most `max`
# distinct values.
viz_pick_cat <- function(p, exclude = character(), max = 20) {
  ok <- setdiff(p$cat, exclude)
  ok <- ok[p$levels[ok] >= 2 & p$levels[ok] <= max]
  if (!length(ok)) return(NULL)
  ok[[which.min(p$levels[ok])]]
}

viz_pick_num <- function(p, exclude = character()) {
  ok <- setdiff(p$num, exclude)
  if (!length(ok)) return(NULL)
  ok[[1]]
}

viz_pick_date <- function(p) if (length(p$date)) p$date[[1]] else NULL

viz_pick_label <- function(p, exclude = character(), max = 12) {
  ok <- setdiff(p$label, exclude)
  ok <- ok[p$levels[ok] >= 2 & p$levels[ok] <= max]
  if (!length(ok)) return(NULL)
  ok[[which.min(p$levels[ok])]]
}

# Sensible starting values for every field of a chart.
viz_default_fields <- function(p, type) {
  ch <- viz_chart(type)
  n2 <- viz_pick_num(p)
  n3 <- viz_pick_num(p, if (!is.null(n2)) n2 else character())

  switch(type,
    bar = list(x = viz_pick_cat(p), y = "", color = ""),
    line = {
      xd <- viz_pick_date(p)
      list(x = xd %||% n2, y = if (is.null(xd)) n3 else n2, color = "")
    },
    scatter = list(x = n2, y = n3, color = ""),
    hist = list(x = n2),
    box = list(y = n2, x = ""),
    pie = list(x = viz_pick_cat(p, max = 8), y = ""),
    density = list(x = n2, color = ""),
    corr = list(vars = utils::head(p$num, 5)),
    qq = list(x = n2, color = ""),
    errorbar = list(x = viz_pick_cat(p), y = n2),
    confmat = {
      a <- viz_pick_label(p)
      list(actual = a, predicted = viz_pick_label(p, a))
    },
    roc = {
      a <- viz_pick_label(p, max = 2)
      list(actual = a, score = n2)
    },
    importance = list(feature = viz_pick_cat(p), importance = n2),
    actualpred = list(actual = n2, predicted = n3),
    residual = list(actual = n2, predicted = n3),
    NULL
  )
}

# Charts that need two different columns of the same kind.
viz_distinct_need <- function(type) {
  switch(type, scatter = "num", confmat = "label", actualpred = "num",
         residual = "num", NULL)
}

viz_need_word <- function(types) {
  words <- c(num = "numeric", cat = "categorical", date = "date",
             label = "categorical", any = "")
  out <- unique(unname(words[types]))
  out <- out[nzchar(out)]
  if (!length(out)) return("usable")
  paste(out, collapse = " or ")
}

# Does this dataset have what the chart needs?
viz_chart_ready <- function(p, type) {
  ch <- viz_chart(type)
  for (role in names(ch$fields)) {
    f <- ch$fields[[role]]
    cand <- viz_candidates(p, f$types)
    if (f$required && !length(cand)) return(FALSE)
    if (identical(role, "vars") && length(cand) < 2) return(FALSE)
  }
  need <- viz_distinct_need(type)
  if (!is.null(need) && length(viz_candidates(p, need)) < 2) return(FALSE)
  if (identical(type, "roc") && !length(p$label[p$levels[p$label] == 2])) return(FALSE)
  TRUE
}

# What to tell the user when the chart cannot be drawn.
viz_chart_requirement <- function(p, type) {
  ch <- viz_chart(type)
  alts <- viz_alternatives(p)

  need <- viz_distinct_need(type)
  if (!is.null(need) && length(viz_candidates(p, need)) < 2) {
    return(list(
      title = paste0("This chart needs two ", viz_need_word(need), " columns"),
      text = "Choose a chart that uses one column, or load a dataset with more columns of this type.",
      alternatives = alts))
  }
  if (identical(type, "roc") && !length(p$label[p$levels[p$label] == 2])) {
    return(list(
      title = "This chart needs a label column with two categories",
      text = "Your data needs a column such as Yes / No (or 0 / 1) holding the true class, plus a numeric score column.",
      alternatives = alts))
  }
  for (role in names(ch$fields)) {
    f <- ch$fields[[role]]
    cand <- viz_candidates(p, f$types)
    if (f$required && !length(cand)) {
      return(list(
        title = paste0("This chart needs a ", viz_need_word(f$types), " column"),
        text = paste0("No column is suitable for '", f$label,
                      "'. Try another chart, or check that the sidebar filters kept enough columns."),
        alternatives = alts))
    }
    if (identical(role, "vars") && length(cand) < 2) {
      return(list(title = "Select at least two numeric variables",
                  text = "The correlation heatmap needs two or more numeric columns.",
                  alternatives = alts))
    }
  }
  list(title = "This chart cannot be drawn with the current columns",
       text = "Try another chart type or different variables.",
       alternatives = alts)
}

# Chart types this dataset can actually support, best first.
viz_alternatives <- function(p, max = 3) {
  ok <- Filter(function(t) viz_chart_ready(p, t), viz_chart_types())
  utils::head(ok, max)
}

# ---- suggestions ----------------------------------------------------------

# Up to three charts that fit the shape of the data, with a one-line reason.
viz_suggestions <- function(p) {
  out <- list()
  add <- function(type, reason) {
    if (length(out) < 3 && !any(vapply(out, function(o) o$type == type, logical(1))))
      out[[length(out) + 1]] <<- list(type = type, reason = reason)
  }
  num <- p$num
  cat <- p$cat[p$levels[p$cat] >= 2 & p$levels[p$cat] <= 12]

  if (length(p$date) && length(num))
    add("line", paste0("Track ", viz_short(num[[1]]), " over ", viz_short(p$date[[1]])))
  if (length(num) >= 2)
    add("scatter", paste0("Compare ", viz_short(num[[1]]), " and ", viz_short(num[[2]])))
  if (length(cat) && length(num))
    add("bar", paste0("Summarise ", viz_short(num[[1]]), " by ", viz_short(cat[[1]])))
  if (length(num))
    add("hist", paste0("See how ", viz_short(num[[1]]), " is distributed"))
  if (length(num) >= 3)
    add("corr", "Correlations across the numeric columns")
  if (length(cat))
    add("pie", paste0("Proportions of ", viz_short(cat[[1]])))
  out
}

viz_short <- function(x, width = 22) {
  x <- as.character(x)
  ifelse(nchar(x) > width, paste0(substr(x, 1, width - 1), "…"), x)
}

# ---- automatic text -------------------------------------------------------

# First usable name in `x`, or NULL. Length-safe so an empty mapped field
# (character(0)) behaves like an unmapped one instead of returning NA.
nz <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NULL else x
}

viz_auto_title <- function(type, f, p) {
  nm <- function(v, fallback) if (is.null(nz(v))) fallback else v
  switch(type,
    bar = if (is.null(nz(f$y))) paste0("Count by ", nm(f$x, "category"))
          else paste0(f$y, " by ", nm(f$x, "category")),
    line = paste0(nm(f$y, "Value"), " over ", nm(f$x, "time")),
    scatter = paste0(nm(f$y, "Y"), " vs ", nm(f$x, "X")),
    hist = paste0("Distribution of ", nm(f$x, "variable")),
    box = if (is.null(nz(f$x))) paste0("Distribution of ", nm(f$y, "variable"))
          else paste0(nm(f$y, "Value"), " by ", f$x),
    pie = if (is.null(nz(f$y))) paste0("Share by ", nm(f$x, "category"))
          else paste0("Total ", f$y, " by ", nm(f$x, "category")),
    density = paste0("Density of ", nm(f$x, "variable")),
    corr = "Correlation matrix",
    qq = paste0("Normal Q-Q plot: ", nm(f$x, "variable")),
    errorbar = paste0("Mean ", nm(f$y, "value"), " by ", nm(f$x, "group")),
    confmat = "Confusion matrix",
    roc = "ROC curve",
    importance = "Feature importance",
    actualpred = "Actual vs predicted",
    residual = "Residuals vs predicted",
    "Chart"
  )
}

viz_auto_xlab <- function(type, f) {
  switch(type,
    bar = if (is.null(nz(f$y))) f$x else NULL,
    line = f$x, scatter = f$x, hist = f$x, box = nz(f$x),
    density = f$x, qq = "Theoretical quantiles", errorbar = f$x,
    confmat = "Predicted", roc = "1 - specificity",
    actualpred = "Actual", residual = "Predicted", NULL)
}

viz_auto_ylab <- function(type, f) {
  switch(type,
    bar = if (is.null(nz(f$y))) "Number of rows" else f$y,
    line = f$y, scatter = f$y, hist = "Count", box = f$y,
    density = "Density", qq = "Sample quantiles", errorbar = f$y,
    confmat = "Actual", roc = "Sensitivity", importance = f$feature,
    actualpred = "Predicted", residual = "Residual", NULL)
}
