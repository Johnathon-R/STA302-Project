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

  data
}

# Calculate the course cutoffs
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
    residuals = rstandard(model),
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

# Print the diagnostic report and return its tables
check_model <- function(model, plots = TRUE,
                        pairs_plot = TRUE,
                        cooks_cutoff = NULL,
                        max_rows = 20,
                        point_cex = 0.8) {
  # Keep the output aligned with the rows used to fit the model
  model$na.action <- NULL
  cutoffs <- model_cutoffs(model, cooks_cutoff)
  db <- dfbetas(model)
  diagnostics <- influence_table(model, cutoffs, db)

  # Keep both the observation and coefficient for each DFBETAS flag
  db_indices <- which(abs(db) > cutoffs[["dfbetas"]], arr.ind = TRUE)
  flagged <- diagnostics[
    which(diagnostics$flags > 0 | diagnostics$high_leverage), , drop = FALSE
  ]

  checks <- list(
    summary = summary(model),
    confint = confint(model),
    partial_tests = drop1(model, test = "F"),
    vif = if (length(attr(terms(model), "term.labels")) > 1L &&
                !anyNA(coef(model)) &&
                requireNamespace("car", quietly = TRUE)) {
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

# Build lecture-style plots for a fitted model.
# Call plots$qq() for one plot or plots$show() for all plots.
# plots$presentation_plot() makes a separate four-panel figure.
model_plots <- function(model, point_cex = 0.8,
                        diagnostics = influence_table(model, cutoffs),
                        cutoffs = model_cutoffs(model)) {
  model$na.action <- NULL
  data <- model.frame(model)
  e <- resid(model)
  x <- model.matrix(model)
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]

  # Use the same plot settings for direct calls and plots$show().
  draw <- function(plot) {
    old <- par(mfrow = c(1, 1))
    on.exit(par(old))
    plot()
  }

  # The horizontal axis is the fitted observation number, as in the tables.
  influence_plot <- function(y, cutoff, title, signed = FALSE) {
    plot(seq_along(y), y, xlab = "Observation in fitted model",
         ylab = title, main = title, pch = 1, cex = point_cex,
         ylim = range(c(y, cutoff, if (signed) -cutoff, 0), finite = TRUE))
    abline(h = if (signed) c(-cutoff, cutoff) else cutoff,
           col = "red", lty = 2)
  }

  histogram <- function() draw(function() {
    hist(model.response(data), xlab = names(data)[1],
         main = "Response distribution")
  })

  response_boxplot <- function() draw(function() {
    if ("etf_indicator" %in% names(data)) {
      boxplot(model.response(data) ~ factor(data$etf_indicator,
              levels = c(0, 1), labels = c("Mutual fund", "ETF")),
              xlab = "Fund type", ylab = names(data)[1],
              main = "Response by fund type")
    } else {
      boxplot(model.response(data), ylab = names(data)[1],
              main = "Response boxplot")
    }
  })

  residuals_vs_fitted <- function() draw(function() {
    plot(fitted(model), e, xlab = "Fitted values", ylab = "Residuals",
         main = "Residuals vs fitted", pch = 1, cex = point_cex)
    abline(h = 0, col = "red")
  })

  qq <- function() draw(function() {
    qqnorm(e, main = "Normal Q-Q plot of residuals", pch = 1,
           cex = point_cex)
    qqline(e, col = "red")
  })

  residual_boxplot <- function() draw(function() {
    if ("etf_indicator" %in% names(data)) {
      boxplot(e ~ factor(data$etf_indicator, levels = c(0, 1),
              labels = c("Mutual fund", "ETF")),
              xlab = "Fund type", ylab = "Residuals",
              main = "Residuals by fund type")
    } else {
      boxplot(e, ylab = "Residuals", main = "Residual boxplot")
    }
    abline(h = 0, col = "red")
  })

  leverage <- function() draw(function() {
    influence_plot(diagnostics$leverage, cutoffs[["leverage"]], "Leverage")
  })

  standardized_residuals <- function() draw(function() {
    influence_plot(diagnostics$residuals, 2, "Standardized residuals", TRUE)
  })

  cooks <- function() draw(function() {
    influence_plot(diagnostics$cooks, cutoffs[["cooks"]],
                   "Cook's distance")
  })

  dffits <- function() draw(function() {
    influence_plot(diagnostics$dffits, cutoffs[["dffits"]],
                   "DFFITS", TRUE)
  })

  # Each coefficient has its own DFBETAS, so plot each column separately.
  dfbetas <- function() {
    db <- stats::dfbetas(model)
    for (i in seq_len(ncol(db))) {
      draw(function() {
        influence_plot(db[, i], cutoffs[["dfbetas"]],
                       paste("DFBETAS:", colnames(db)[i]), TRUE)
      })
    }
  }

  residuals_vs_leverage <- function() draw(function() {
    plot(diagnostics$leverage, diagnostics$residuals,
         xlab = "Leverage", ylab = "Standardized residuals",
         main = "Residuals vs leverage", pch = 1, cex = point_cex)
    abline(h = c(-2, 0, 2), col = "red", lty = c(2, 1, 2))
    abline(v = cutoffs[["leverage"]], col = "red", lty = 2)
  })

  residuals_vs_predictors <- function(predictor = colnames(x)) {
    for (name in predictor) {
      draw(function() {
        plot(x[, name], e, xlab = name, ylab = "Residuals",
             main = "Residuals vs predictor", pch = 1, cex = point_cex)
        abline(h = 0, col = "red")
      })
    }
  }

  pairs_plot <- function() {
    if (ncol(x) == 0L) return(invisible(NULL))
    draw(function() {
      pairs(cbind(x, Residuals = e), pch = 1, cex = 0.3 * point_cex,
            main = "Predictors and residuals")
    })
  }

  # A single figure for slides or a report; this does not change plots$show().
  presentation_plot <- function(top = 10, title = "Model diagnostics") {
    old <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1),
               oma = c(0, 0, 2, 0))
    on.exit(par(old))

    rank_plot <- function(y, cutoff, label) {
      i <- head(order(y, decreasing = TRUE), top)
      barplot(y[i], names.arg = i,
              col = ifelse(y[i] > cutoff, "#C94F42", "#37678A"),
              border = NA, ylim = c(0, 1.1 * max(y[i], cutoff)),
              xlab = "Observation in fitted model", ylab = label,
              main = label)
      abline(h = cutoff, col = "#C94F42", lty = 2, lwd = 2)
      mtext(paste("Course cutoff:", signif(cutoff, 3)),
            side = 3, line = 0.2, adj = 1, cex = 0.75)
    }

    plot(fitted(model), e, xlab = "Fitted values", ylab = "Residuals",
         main = "Residuals vs fitted", pch = 16, cex = point_cex,
         col = adjustcolor("#37678A", alpha.f = 0.35))
    abline(h = 0, col = "#C94F42", lwd = 2)

    qqnorm(e, main = "Normal Q-Q plot", pch = 16, cex = point_cex,
           col = adjustcolor("#37678A", alpha.f = 0.35))
    qqline(e, col = "#C94F42", lwd = 2)

    rank_plot(diagnostics$leverage, cutoffs[["leverage"]], "Leverage")
    rank_plot(diagnostics$cooks, cutoffs[["cooks"]], "Cook's distance")
    mtext(title, outer = TRUE, cex = 1.2, font = 2)
    invisible(NULL)
  }

  methods <- list(
    histogram = histogram,
    response_boxplot = response_boxplot,
    residuals_vs_fitted = residuals_vs_fitted,
    qq = qq,
    residual_boxplot = residual_boxplot,
    leverage = leverage,
    standardized_residuals = standardized_residuals,
    cooks = cooks,
    dffits = dffits,
    dfbetas = dfbetas,
    residuals_vs_leverage = residuals_vs_leverage,
    residuals_vs_predictors = residuals_vs_predictors,
    pairs = pairs_plot
  )

  show <- function(which = names(methods)) {
    for (name in match.arg(which, names(methods), several.ok = TRUE)) {
      methods[[name]]()
    }
    invisible(NULL)
  }

  c(methods, list(show = show, presentation_plot = presentation_plot))
}
