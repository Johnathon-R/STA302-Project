# Fill in missing asset allocations when the reported shares determine them
impute_allocation <- function(data, proportion_cols) {

  missing_count <- rowSums(is.na(data[proportion_cols]))
  reported_sum <- rowSums(data[proportion_cols], na.rm = TRUE)

  # Allow a small rounding difference when the reported shares add up to 100%
  sum_is_one <- abs(reported_sum - 1) <= 1e-3

  # If the reported shares cover the whole fund, the missing shares are zero
  complete_missing_rows <- missing_count > 0 & sum_is_one

  for (col in proportion_cols) {
    target_rows <- complete_missing_rows & is.na(data[[col]])
    data[target_rows, col] <- 0
  }

  # With one missing share, give it whatever is left over
  single_missing_rows <- missing_count == 1

  for (col in proportion_cols) {
    other_cols <- setdiff(proportion_cols, col)

    target_rows <- single_missing_rows &
      is.na(data[[col]]) &
      !sum_is_one

    if (any(target_rows)) {
      data[target_rows, col] <- 1 - rowSums(
        data[target_rows, other_cols, drop = FALSE],
        na.rm = TRUE
      )
    }
  }

  return(data)
}

# Calculate the course screening cutoffs for this fit
model_cutoffs <- function(model, cooks_cutoff = NULL) {
  n <- nobs(model)
  p <- model$rank # Counts the intercept when the model includes one

  c(
    cooks = if (is.null(cooks_cutoff)) qf(0.5, p, n - p) else unname(cooks_cutoff),
    dffits = 2 * sqrt(p / n),
    dfbetas = 2 / sqrt(n),
    leverage = 2 * p / n
  )
}

# Build one row per fitted observation with its influence measures and flags
influence_table <- function(model, cutoffs, db = dfbetas(model)) {
  diagnostics <- data.frame(
    observation = seq_len(nobs(model)),
    row_id = rownames(model.frame(model)),
    cooks = cooks.distance(model),
    dffits = dffits(model),
    dfbetas = apply(abs(db), 1, max),
    residuals = rstudent(model),
    leverage = hatvalues(model),
    row.names = NULL
  )
  diagnostics$cooks_flag <- diagnostics$cooks > cutoffs["cooks"]
  diagnostics$dffits_flag <- abs(diagnostics$dffits) > cutoffs["dffits"]
  diagnostics$dfbetas_flag <- diagnostics$dfbetas > cutoffs["dfbetas"]
  # Count the three influence flags and track high leverage separately
  diagnostics$flags <- rowSums(diagnostics[, c(
    "cooks_flag", "dffits_flag", "dfbetas_flag"
  )])
  diagnostics$high_leverage <- diagnostics$leverage > cutoffs["leverage"]

  diagnostics
}

# Print the diagnostic report and return its tables for review or export
check_model <- function(model, plots = TRUE, pairs_plot = TRUE,
                        cooks_cutoff = NULL, max_rows = 20, point_cex = 0.8) {
  # Keep the output aligned with the rows used to fit the model
  model$na.action <- NULL
  cutoffs <- model_cutoffs(model, cooks_cutoff)
  db <- dfbetas(model)
  diagnostics <- influence_table(model, cutoffs, db)
  # Keep both the observation and coefficient for each DFBETAS flag
  db_indices <- which(abs(db) > cutoffs[["dfbetas"]], arr.ind = TRUE)
  flagged <- subset(diagnostics, flags > 0 | high_leverage)

  checks <- list(
    summary = summary(model),
    confint = confint(model),
    partial_tests = drop1(model, test = "F"),
    vif = if (length(attr(terms(model), "term.labels")) > 1L &&
              !anyNA(coef(model)) && requireNamespace("car", quietly = TRUE)) {
      car::vif(model)
    },
    cutoffs = cutoffs,
    diagnostics = diagnostics,
    flagged = flagged[order(-flagged$flags, -flagged$cooks), , drop = FALSE],
    dfbetas = db,
    dfbetas_flagged = data.frame(
      observation = db_indices[, "row"],
      row_id = diagnostics$row_id[db_indices[, "row"]],
      coefficient = colnames(db)[db_indices[, "col"]],
      dfbeta = db[db_indices],
      row.names = NULL
    )
  )

  cat("\nModel summary\n")
  print(checks$summary)
  cat("\n95% confidence intervals\n")
  print(checks$confint)
  cat("\nPartial F-tests\n")
  print(checks$partial_tests)
  if (!is.null(checks$vif)) {
    cat("\nVIF / GVIF\n")
    print(checks$vif)
  }
  cat("\nScreening cutoffs\n")
  print(checks$cutoffs)
  cat("\nObservations by influence flag count\n")
  print(table(factor(diagnostics$flags, levels = 0:3)))
  cat("\nHigh-leverage observations:", sum(diagnostics$high_leverage), "\n")
  cat("\nFlagged observations (showing", min(max_rows, nrow(flagged)),
      "of", nrow(flagged), ")\n")
  print(head(checks$flagged, max_rows), row.names = FALSE)

  if (plots) {
    plotter <- model_plots(model, point_cex = point_cex,
                           diagnostics = diagnostics, cutoffs = cutoffs)
    if (pairs_plot) {
      plotter$show()
    } else {
      plotter$show(setdiff(names(plotter), c("pairs", "show")))
    }
  }

  invisible(checks)
}

# Build a set of plots for a fitted model
# Call plots$qq() for one plot, or plots$show() for the full set
model_plots <- function(model, point_cex = 0.8,
                        diagnostics = influence_table(model, cutoffs),
                        cutoffs = model_cutoffs(model)) {
  model$na.action <- NULL
  model_frame <- model.frame(model)
  response <- model.response(model_frame)
  errors <- residuals(model)
  predictors <- model.matrix(model)
  predictors <- predictors[, colnames(predictors) != "(Intercept)", drop = FALSE]
  group <- model_frame[["etf_indicator"]]

  # Edit these labels to shorten the predictor names on the plots
  short_labels <- c(
    total_net_assets = "Assets", fund_age_years = "Age",
    asset_stocks = "Stocks", asset_bonds = "Bonds",
    annual_holdings_turnover = "Turnover",
    fund_price_earning_ratio = "P/E", fund_sector_technology = "Technology",
    pe_reported = "P/E reported", pe_centered_when_reported = "Centered P/E",
    fund_annual_report_net_expense_ratio = "Expense", etf_indicator = "ETF",
    "fund_annual_report_net_expense_ratio:etf_indicator" = "Expense x ETF",
    "etf_indicator:fund_annual_report_net_expense_ratio" = "Expense x ETF"
  )

  if (!is.null(group)) {
    group <- factor(group, levels = c(0, 1), labels = c("Mutual fund", "ETF"))
  }

  # Give each plot its own panel and restore the graphics settings afterward
  draw <- function(code) {
    # Setting mfrow resets spacing and text size, so restore it first
    old_par <- par(c("mfrow", "mar", "mex", "cex", "cex.lab", "cex.axis", "cex.main"))
    on.exit(par(old_par))
    par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1),
        cex = 1, cex.lab = 1, cex.axis = 0.9, cex.main = 1.05)
    force(code)
  }

  # Keep the original name for any column without a short label
  plot_labels <- function(columns) {
    labels <- unname(short_labels[columns])
    labels[is.na(labels)] <- columns[is.na(labels)]
    labels
  }

  # Draw an influence measure and highlight observations beyond its cutoff
  influence_plot <- function(measure, label, two_sided = FALSE) {
    values <- diagnostics[[measure]]
    cutoff <- cutoffs[[measure]]
    limits <- if (two_sided) c(-cutoff, cutoff) else cutoff
    colours <- ifelse(abs(values) > cutoff, "red", "black")

    draw({
      plot(diagnostics$observation, values, type = "h", col = colours,
           ylim = range(c(values, limits, 0), finite = TRUE),
           xlab = "Observation in fitted model", ylab = label, main = label)
      points(diagnostics$observation, values, pch = 1, cex = point_cex,
             col = colours)
      abline(h = limits, col = "red", lty = 2)
    })
  }

  # Show the distribution of the response used in the fit
  histogram <- function() draw({
    hist(response, xlab = names(model_frame)[1], main = "Response distribution")
  })

  # Compare the response for ETFs and mutual funds when fund type is available
  response_boxplot <- function() draw({
    if (is.null(group)) {
      boxplot(response, ylab = names(model_frame)[1], main = "Response boxplot")
    } else {
      boxplot(response ~ group, xlab = "Fund type", ylab = names(model_frame)[1],
              main = "Response by fund type")
    }
  })

  # Look for curvature or a change in spread across the fitted values
  residuals_vs_fitted <- function() draw({
    plot(fitted(model), errors, xlab = "Fitted values", ylab = "Residual",
         main = "Residuals versus fitted", pch = 1, cex = point_cex)
    abline(h = 0, col = "red")
  })

  # Compare the residual quantiles with normal quantiles
  qq <- function() draw({
    qqnorm(errors, main = "Normal Q-Q plot of residuals", pch = 1, cex = point_cex)
    qqline(errors, col = "red", lwd = 2)
  })

  # Compare the centre and spread of the residuals by fund type when available
  residual_boxplot <- function() draw({
    if (is.null(group)) {
      boxplot(errors, ylab = "Residual", main = "Residual boxplot")
    } else {
      boxplot(errors ~ group, xlab = "Fund type", ylab = "Residual",
              main = "Residuals by fund type")
    }
    abline(h = 0, col = "red")
  })

  # Find observations with unusual combinations of predictor values
  leverage <- function() influence_plot("leverage", "Leverage")

  # Show which observations have the largest overall effect on the fit
  cooks <- function() influence_plot("cooks", "Cook's distance")

  # Show how much each observation changes its own fitted value
  dffits <- function() influence_plot("dffits", "DFFITS", two_sided = TRUE)

  # Show the largest standardized coefficient change when each row is omitted
  dfbetas <- function() influence_plot("dfbetas", "Maximum absolute DFBETAS")

  # Look for observations that combine high leverage with large residuals
  residuals_vs_leverage <- function() draw({
    plot(diagnostics$leverage, diagnostics$residuals,
         col = ifelse(diagnostics$flags > 0, "red", "black"),
         pch = 1, cex = point_cex, xlab = "Leverage",
         ylab = "Studentized residual", main = "Residuals versus leverage")
    abline(h = 0, col = "red")
    abline(v = cutoffs[["leverage"]], col = "red", lty = 2)
  })

  # Draw one residual plot per selected model-matrix column
  residuals_vs_predictors <- function(predictor = colnames(predictors)) {
    for (column in predictor) {
      draw({
        plot(predictors[, column], errors, xlab = plot_labels(column),
             ylab = "Residual", pch = 1, cex = point_cex,
             main = "Residuals versus predictor")
        abline(h = 0, col = "red")
      })
    }
  }

  # Compare predictors and residuals side by side in a scatterplot matrix
  pairs_plot <- function() {
    if (ncol(predictors) == 0L) return(invisible(NULL))

    draw({
      graphics::pairs(cbind(predictors, Residuals = errors),
                      labels = c(plot_labels(colnames(predictors)), "Residuals"),
                      pch = 1, cex = 0.3 * point_cex,
                      col = adjustcolor("black", alpha.f = 0.3),
                      main = "Predictors and residuals",
                      panel = function(x, y, ...) {
                        points(x, y, ...)
                        # Add zero lines only on axes that show residuals
                        if (par("mfg")[1] == ncol(predictors) + 1L) {
                          abline(h = 0, col = "red", lwd = 2)
                        }
                        if (par("mfg")[2] == ncol(predictors) + 1L) {
                          abline(v = 0, col = "red", lwd = 2)
                        }
                      })
    })
  }

  methods <- list(
    histogram = histogram,
    response_boxplot = response_boxplot,
    residuals_vs_fitted = residuals_vs_fitted,
    qq = qq,
    residual_boxplot = residual_boxplot,
    leverage = leverage,
    cooks = cooks,
    dffits = dffits,
    dfbetas = dfbetas,
    residuals_vs_leverage = residuals_vs_leverage,
    residuals_vs_predictors = residuals_vs_predictors,
    pairs = pairs_plot
  )

  # Draw the named plots in order. With no names supplied, draw them all
  show <- function(which = names(methods)) {
    for (name in match.arg(which, names(methods), several.ok = TRUE)) {
      methods[[name]]()
    }
    invisible(NULL)
  }

  c(methods, list(show = show))
}
