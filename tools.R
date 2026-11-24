impute_allocation <- function(data, proportion_cols) {
  
  # Count missing proportions in each row
  missing_count <- rowSums(is.na(data[proportion_cols]))
  
  # Calculate the sum of reported proportions
  reported_sum <- rowSums(data[proportion_cols], na.rm = TRUE)
  
  # Identify rows where reported proportions sum to 1
  sum_is_one <- abs(reported_sum - 1) <= 1e-3
  
  # If reported proportions sum to 1, set remaining missing values to 0
  complete_missing_rows <- missing_count > 0 & sum_is_one
  
  for (col in proportion_cols) {
    target_rows <- complete_missing_rows & is.na(data[[col]])
    data[target_rows, col] <- 0
  }
  
  # If exactly one proportion is missing, calculate the remainder
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

# Recheck a fitted model without changing it or removing observations.
# Usage: checks <- check_model(model)
# After refitting: checks_revised <- check_model(model_revised)
# Each diagnostic uses its own figure instead of a multi-panel layout.
# In R Markdown, use fig.width=10, fig.height=6 for wide, full-size figures.
check_model <- function(model, plots = TRUE, pairs_plot = TRUE,
                        cooks_cutoff = NULL, max_rows = 20, point_cex = 0.8) {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    stop("Supply a fitted linear model from lm().")
  }
  if (df.residual(model) <= 1) {
    stop("At least two residual degrees of freedom are needed.")
  }
  if (length(point_cex) != 1L || !is.numeric(point_cex) ||
      !is.finite(point_cex) || point_cex <= 0) {
    stop("point_cex must be one positive, finite number.")
  }

  # Suppress NA padding in this local copy so every output uses fitted rows.
  # This also keeps rows aligned for models fitted with na.exclude.
  model$na.action <- NULL
  model_frame <- model.frame(model)
  response <- model.response(model_frame)
  errors <- residuals(model)
  predictors <- model.matrix(model)
  predictors <- predictors[, colnames(predictors) != "(Intercept)", drop = FALSE]

  n <- nobs(model)
  p <- model$rank # Includes the intercept, as in the existing diagnostics table.
  if (is.null(cooks_cutoff)) cooks_cutoff <- qf(0.5, p, n - p)
  cutoffs <- c(
    cooks = cooks_cutoff,
    dffits = 2 * sqrt(p / n),
    dfbetas = 2 / sqrt(n),
    leverage = 2 * p / n # Screening rule; leverage alone does not imply influence.
  )

  db <- dfbetas(model)
  diagnostics <- data.frame(
    observation = seq_len(n),
    row_id = rownames(model_frame),
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
  diagnostics$flags <- rowSums(diagnostics[, c(
    "cooks_flag", "dffits_flag", "dfbetas_flag"
  )])
  diagnostics$high_leverage <- diagnostics$leverage > cutoffs["leverage"]

  # Keep the coefficient names: which(abs(db) > cutoff) alone gives matrix
  # element indices, which are not observation numbers.
  db_indices <- which(abs(db) > cutoffs["dfbetas"], arr.ind = TRUE)
  dfbetas_flagged <- data.frame(
    observation = db_indices[, "row"],
    row_id = rownames(model_frame)[db_indices[, "row"]],
    coefficient = colnames(db)[db_indices[, "col"]],
    dfbeta = db[db_indices],
    row.names = NULL
  )
  flagged <- diagnostics[
    which(diagnostics$flags > 0 | diagnostics$high_leverage), , drop = FALSE
  ]
  flagged <- flagged[order(-flagged$flags, -flagged$cooks), , drop = FALSE]

  model_summary <- summary(model)
  intervals <- confint(model)
  partial_tests <- drop1(model, test = "F")
  vif_result <- NULL
  if (requireNamespace("car", quietly = TRUE)) {
    if (anyNA(coef(model))) {
      warning("VIF skipped: the model has aliased coefficients.")
    } else if (length(attr(terms(model), "term.labels")) > 1) {
      vif_result <- car::vif(model)
    }
  } else {
    warning("VIF skipped: package 'car' is not installed.")
  }

  cat("\nModel summary\n")
  print(model_summary)
  cat("\n95% confidence intervals\n")
  print(intervals)
  cat("\nPartial F-tests (drop1)\n")
  print(partial_tests)
  cat("\nVIF / GVIF\n")
  print(vif_result)
  cat("\nScreening cutoffs\n")
  print(cutoffs)
  cat("\nNumber of observations with 0, 1, 2, or 3 influence flags\n")
  print(table(factor(diagnostics$flags, levels = 0:3)))
  cat("\nHigh-leverage observations:", sum(diagnostics$high_leverage), "\n")
  cat("\nFlagged observations (highest flag count, then Cook's distance first)\n")
  print(head(flagged, max_rows), row.names = FALSE)
  if (nrow(flagged) > max_rows) {
    cat("Showing", max_rows, "of", nrow(flagged),
        "rows; all are available in the returned $flagged table.\n")
  }

  if (plots) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    point_col <- "black"
    short_labels <- c(
      total_net_assets = "Assets", fund_age_years = "Age",
      asset_stocks = "Stocks", asset_bonds = "Bonds",
      annual_holdings_turnover = "Turnover",
      fund_price_earning_ratio = "P/E", fund_sector_technology = "Technology",
      pe_reported = "P/E reported", pe_centered_when_reported = "Centered P/E",
      fund_annual_report_net_expense_ratio = "Expense", etf_indicator = "ETF",
      "fund_annual_report_net_expense_ratio:etf_indicator" = "Expense x ETF",
      "etf_indicator:fund_annual_report_net_expense_ratio" = "Expense x ETF",
      Residuals = "Residuals"
    )
    group <- model_frame$etf_indicator
    if (!is.null(group)) {
      group <- factor(group, levels = c(0, 1), labels = c("Mutual fund", "ETF"))
    }

    # Give every plot the entire device, including in short R Markdown outputs.
    # Enlarging the plotting area avoids squeezing four plots into one figure.
    par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3, 1),
        cex = 1, cex.lab = 1, cex.axis = 0.9, cex.main = 1.05)
    hist(response, xlab = names(model_frame)[1], main = "Distribution of returns")
    if (!is.null(group)) {
      boxplot(response ~ group, xlab = "Fund type", ylab = "Return",
              main = "Returns by fund type")
    } else {
      boxplot(response, ylab = names(model_frame)[1], main = "Response boxplot")
    }
    plot(fitted(model), errors, xlab = "Fitted return", ylab = "Residual",
         main = "Residuals versus fitted", pch = 1,
         cex = point_cex, col = point_col)
    abline(h = 0, col = "red")
    qqnorm(errors, main = "Normal Q-Q plot of residuals", pch = 1,
           cex = point_cex, col = point_col)
    qqline(errors, col = "red", lwd = 2)

    # Show the influence measures alongside their screening cutoffs.
    if (!is.null(group)) {
      boxplot(errors ~ group, xlab = "Fund type", ylab = "Residual",
              main = "Residuals by fund type")
    } else {
      boxplot(errors, ylab = "Residual", main = "Residual boxplot")
    }
    abline(h = 0, col = "red")
    for (measure in c("leverage", "cooks", "dffits", "dfbetas")) {
      values <- diagnostics[[measure]]
      cutoff <- cutoffs[measure]
      plot(diagnostics$observation, values, type = "h",
           col = ifelse(abs(values) > cutoff, "red", "black"),
           xlab = "Observation in fitted model", ylab = measure,
           main = paste("Diagnostic:", measure))
      points(diagnostics$observation, values, pch = 1, cex = point_cex,
             col = ifelse(abs(values) > cutoff, "red", point_col))
      abline(h = cutoff, col = "red", lty = 2)
      if (measure == "dffits") abline(h = -cutoff, col = "red", lty = 2)
    }
    plot(diagnostics$leverage, diagnostics$residuals,
         col = ifelse(diagnostics$flags > 0, "red", point_col),
         pch = 1, cex = point_cex,
         xlab = "Leverage", ylab = "Studentized residual",
         main = "Residuals versus leverage")
    abline(h = 0, col = "red")
    abline(v = cutoffs["leverage"], col = "red", lty = 2)

    # Each predictor, including the interaction, also gets a separate figure.
    for (predictor in colnames(predictors)) {
      label <- if (predictor %in% names(short_labels)) short_labels[[predictor]] else predictor
      plot(predictors[, predictor], errors, xlab = label,
           ylab = "Residual", pch = 1, cex = point_cex, col = point_col,
           main = "Residuals versus predictor")
      abline(h = 0, col = "red")
    }

    if (pairs_plot && ncol(predictors) > 0) {
      par(mfrow = c(1, 1), cex = 1)
      plot_data <- as.data.frame(predictors)
      plot_data$Residuals <- errors
      labels <- names(plot_data)
      matched <- labels %in% names(short_labels)
      labels[matched] <- unname(short_labels[labels[matched]])
      pairs(plot_data, labels = labels, pch = 1, cex = 0.3 * point_cex,
            col = adjustcolor("black", alpha.f = 0.3),
            main = "Predictors, interaction, and residuals",
            panel = function(x, y, ...) {
              points(x, y, ...)
              if (par("mfg")[1] == ncol(plot_data)) {
                abline(h = 0, col = "red", lwd = 2)
              }
              if (par("mfg")[2] == ncol(plot_data)) {
                abline(v = 0, col = "red", lwd = 2)
              }
            })
    }
  }

  invisible(list(
    summary = model_summary, confint = intervals, partial_tests = partial_tests,
    vif = vif_result, cutoffs = cutoffs, diagnostics = diagnostics,
    flagged = flagged, dfbetas = db, dfbetas_flagged = dfbetas_flagged
  ))
}
