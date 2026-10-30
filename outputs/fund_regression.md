---
title: "STA302: 2018 Fund Returns and Multiple Linear Regression"
output:
  html_document:
    toc: true
    toc_depth: 2
    code_folding: show
---



## Load the two datasets

Read the `ETFs.csv` and `MutualFunds.csv` files in `data/`.
it does not download or load price files. Blank entries are treated as
missing values (`NA`).


``` r
etfs <- read.csv("data/ETFs.csv", na.strings = c("", "NA"))
mutualfunds <- read.csv("data/MutualFunds.csv", na.strings = c("", "NA"))
```

## Keep the relevant columns

Keep a fund identifier, the eight source variables needed for the predictors,
and only the 2018 calendar-year fund return. Category returns are benchmark
returns, so they are not our response. Trailing and quarterly returns are also
excluded. Returns remain in their original decimal units: 0.05 means 5%.


``` r
response <- "fund_return_2018"

columns <- c(
  "fund_symbol", "fund_annual_report_net_expense_ratio",
  "total_net_assets", "inception_date", "asset_stocks", "asset_bonds",
  "annual_holdings_turnover", "fund_price_earning_ratio",
  "fund_sector_technology", response
)

etfs <- etfs[, columns]
mutualfunds <- mutualfunds[, columns]

# Identify fund type before combining the datasets.
etfs$etf_indicator <- 1
mutualfunds$etf_indicator <- 0
funds <- rbind(etfs, mutualfunds)
```

## Prepare the nine predictors

Use the annual-report net expense ratio because it is available in both files.
Convert ratios and weights from proportions to percentages (0.01 becomes 1%).
Use natural logs for assets and P/E; zero or negative values cannot be logged
and are marked missing. Age is measured in fractional years to January 1, 2018.
Funds launched after that date have no applicable age at the start of 2018.


``` r
funds$inception_date <- as.Date(funds$inception_date)
funds$fund_age_years <- as.numeric(
  as.Date("2018-01-01") - funds$inception_date
) / 365.25
funds$fund_age_years[funds$fund_age_years < 0] <- NA

# Replace nonpositive inputs with NA before taking logs.
assets <- funds$total_net_assets
assets[assets <= 0] <- NA
pe <- funds$fund_price_earning_ratio
pe[pe <= 0] <- NA

funds$log_total_net_assets <- log(assets)
funds$log_pe_ratio <- log(pe)
funds$net_expense_ratio_pct <- 100 * funds$fund_annual_report_net_expense_ratio
funds$stock_weight_pct <- 100 * funds$asset_stocks
funds$bond_weight_pct <- 100 * funds$asset_bonds
funds$annual_turnover_pct <- 100 * funds$annual_holdings_turnover
funds$technology_weight_pct <- 100 * funds$fund_sector_technology

predictors <- c(
  "etf_indicator", "net_expense_ratio_pct", "log_total_net_assets",
  "fund_age_years", "stock_weight_pct", "bond_weight_pct",
  "annual_turnover_pct", "log_pe_ratio", "technology_weight_pct"
)

# Retain only the identifier, prepared predictors, and the 2018 response.
funds <- funds[, c("fund_symbol", predictors, response)]
```

Stock and bond weights are included without cash and other holdings, avoiding
an automatic sum-to-100% constraint. Missing holdings are left missing, not
replaced with zero. Technology weight retains the source's sector-weight
meaning; it is not recalculated as a share of total fund assets.

## Check missing values and prepare the 2018 sample

`funds` retains every row and only the 2018 response. `model_data_2018` keeps
only funds with all nine predictors and a 2018 return.


``` r
# Missing predictor counts, shown separately for each fund type.
rbind(
  ETF = colSums(is.na(funds[funds$etf_indicator == 1, predictors])),
  Mutual_fund = colSums(is.na(funds[funds$etf_indicator == 0, predictors]))
)
```

```
##             etf_indicator net_expense_ratio_pct log_total_net_assets fund_age_years
## ETF                     0                    15                   30            900
## Mutual_fund             0                   210                   34           3288
##             stock_weight_pct bond_weight_pct annual_turnover_pct log_pe_ratio technology_weight_pct
## ETF                     1036            1036                 950          814                   694
## Mutual_fund             2440            2440                1808         6811                  5516
```

``` r
complete_2018 <- complete.cases(funds[, c(predictors, "fund_return_2018")])
model_data_2018 <- funds[complete_2018, ]

# Compare available rows before and after complete-case selection.
rbind(
  All_funds = table(factor(funds$etf_indicator, levels = c(0, 1))),
  Complete_2018 = table(factor(model_data_2018$etf_indicator, levels = c(0, 1)))
)
```

```
##                   0    1
## All_funds     23783 2310
## Complete_2018 13201  564
```

``` r
# Initial check of the relationship between fund type and expense ratio.
cor(model_data_2018$etf_indicator, model_data_2018$net_expense_ratio_pct)
```

```
## [1] -0.2336357
```

In the count table, 0 means mutual fund and 1 means ETF. Complete-case selection
can change the mix of funds; missingness is not assumed to be random.

## Scope and additional predictors

The source files contain 2021 reporting dates. The predictor measurements are
not established as January 2018 observations, so this is an exploratory
association model, not a historical forecasting or causal model. Obtain
pre-2018 characteristics before making such claims.

**Fund category** is the strongest candidate to investigate next: it can
separate investment strategies (for example, domestic equity, international
equity, and bond funds). `fund_category` exists in these files, but it is not
added automatically because categories can be sparse and overlap strongly with
stock/bond weights. Check ETF and mutual-fund representation within categories
before a category-adjusted comparison. Other useful candidates are verified
pre-2018 geographic exposure and market capitalization/style. The available
trailing beta fields must not be treated as pre-2018 measures without checking
their estimation dates. Avoid adding later returns as explanatory variables.

Different share classes may represent the same underlying portfolio, and funds
also share market exposures. Row independence cannot be established from these
plots. Fund-family/portfolio identifiers would help assess clustering; HC3
standard errors below do not correct dependence between rows. The surviving
funds in this later snapshot may also be unrepresentative of all funds in 2018.

## Descriptive tables and box plots

For easier interpretation, the model uses return in **percentage points**:
-0.05 becomes -5. Predictor percentages already use the same convention.


``` r
model_data_2018$return_2018_pct <- 100 * model_data_2018$fund_return_2018
fund_type <- factor(model_data_2018$etf_indicator,
                    levels = c(0, 1), labels = c("Mutual fund", "ETF"))
variables <- c("return_2018_pct", predictors)
descriptive <- t(sapply(model_data_2018[, variables], function(x) {
  c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x),
    Q1 = unname(quantile(x, 0.25)), Median = median(x),
    Q3 = unname(quantile(x, 0.75)), Max = max(x))
}))
knitr::kable(descriptive, digits = 3)
```



|                      |     N|   Mean|      SD|     Min|      Q1| Median|     Q3|      Max|
|:---------------------|-----:|------:|-------:|-------:|-------:|------:|------:|--------:|
|return_2018_pct       | 13765| -8.705|   6.226| -47.814| -12.796| -7.897| -4.620|   22.578|
|etf_indicator         | 13765|  0.041|   0.198|   0.000|   0.000|  0.000|  0.000|    1.000|
|net_expense_ratio_pct | 13765|  1.083|   0.576|   0.010|   0.710|  1.000|  1.370|   11.420|
|log_total_net_assets  | 13765| 20.378|   2.133|   9.513|  18.970| 20.429| 21.782|   27.348|
|fund_age_years        | 13765| 10.564|   9.045|   0.008|   3.715|  9.027| 15.535|   93.465|
|stock_weight_pct      | 13765| 85.827|  23.135|   0.000|  83.390| 97.160| 99.050|  100.000|
|bond_weight_pct       | 13765|  8.656|  18.986|   0.000|   0.000|  0.000|  4.350|   98.430|
|annual_turnover_pct   | 13765| 72.133| 148.456|   1.000|  26.000| 47.000| 82.000| 6627.000|
|log_pe_ratio          | 13765|  3.154|   0.325|  -1.109|   2.995|  3.143|  3.326|    4.094|
|technology_weight_pct | 13765| 17.569|  11.089|   0.000|  10.600| 17.710| 22.870|  100.000|

``` r
group_summary <- t(sapply(split(model_data_2018$return_2018_pct, fund_type),
                           function(x) c(N = length(x), Mean = mean(x), SD = sd(x))))
knitr::kable(group_summary, digits = 3)
```



|            |     N|    Mean|    SD|
|:-----------|-----:|-------:|-----:|
|Mutual fund | 13201|  -8.640| 6.143|
|ETF         |   564| -10.222| 7.788|


``` r
par(mfrow = c(1, 2))
boxplot(return_2018_pct ~ fund_type, data = model_data_2018,
        col = c("lightblue", "peachpuff"), ylab = "2018 return (%)",
        main = "Unadjusted returns by fund type")
hist(model_data_2018$return_2018_pct, breaks = 60, col = "lightblue",
     main = "Distribution of 2018 returns", xlab = "2018 return (%)")
```

![plot of chunk return-boxplots](outputs/figures/return-boxplots-1.png)

``` r
par(mfrow = c(1, 1))
```


``` r
par(mfrow = c(3, 3), mar = c(3, 4, 3, 1))
for (v in predictors[-1]) {
  boxplot(model_data_2018[[v]] ~ fund_type, main = v,
          col = c("lightblue", "peachpuff"), cex.main = 0.8, ylab = "Value")
}
par(mfrow = c(1, 1))
```

![plot of chunk predictor-boxplots](outputs/figures/predictor-boxplots-1.png)

## Fit the prespecified multiple linear regression

Each slope describes the association with 2018 return while holding the other
predictors fixed. The ETF coefficient is the adjusted ETF-minus-mutual-fund
difference in percentage points. Log-variable slopes correspond to a one-unit
increase in the natural log, not a one-dollar change in assets.


``` r
model <- lm(return_2018_pct ~ etf_indicator + net_expense_ratio_pct +
              log_total_net_assets + fund_age_years + stock_weight_pct +
              bond_weight_pct + annual_turnover_pct + log_pe_ratio +
              technology_weight_pct, data = model_data_2018)
summary(model)
```

```
## 
## Call:
## lm(formula = return_2018_pct ~ etf_indicator + net_expense_ratio_pct + 
##     log_total_net_assets + fund_age_years + stock_weight_pct + 
##     bond_weight_pct + annual_turnover_pct + log_pe_ratio + technology_weight_pct, 
##     data = model_data_2018)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -46.421  -2.641   0.151   2.955  35.321 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           -2.848e+01  8.140e-01 -34.983  < 2e-16 ***
## etf_indicator          4.143e-01  2.381e-01   1.740   0.0819 .  
## net_expense_ratio_pct -1.154e+00  8.753e-02 -13.188  < 2e-16 ***
## log_total_net_assets   2.690e-01  2.284e-02  11.778  < 2e-16 ***
## fund_age_years         8.656e-03  5.149e-03   1.681   0.0927 .  
## stock_weight_pct      -7.116e-02  5.051e-03 -14.089  < 2e-16 ***
## bond_weight_pct        4.215e-02  6.170e-03   6.831 8.77e-12 ***
## annual_turnover_pct    1.538e-03  3.145e-04   4.891 1.02e-06 ***
## log_pe_ratio           6.238e+00  1.609e-01  38.761  < 2e-16 ***
## technology_weight_pct  7.913e-02  4.614e-03  17.152  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.251 on 13755 degrees of freedom
## Multiple R-squared:  0.2892,	Adjusted R-squared:  0.2888 
## F-statistic: 621.9 on 9 and 13755 DF,  p-value: < 2.2e-16
```

``` r
coefficient_table <- cbind(summary(model)$coefficients, confint(model))
knitr::kable(coefficient_table, digits = 5,
             caption = "OLS coefficients, t tests and 95% confidence intervals")
```



Table: OLS coefficients, t tests and 95% confidence intervals

|                      |  Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|     2.5 %|    97.5 %|
|:---------------------|---------:|----------:|---------:|------------------:|---------:|---------:|
|(Intercept)           | -28.47620|    0.81399| -34.98338|            0.00000| -30.07173| -26.88066|
|etf_indicator         |   0.41429|    0.23810|   1.74002|            0.08188|  -0.05241|   0.88099|
|net_expense_ratio_pct |  -1.15436|    0.08753| -13.18753|            0.00000|  -1.32593|  -0.98278|
|log_total_net_assets  |   0.26903|    0.02284|  11.77770|            0.00000|   0.22425|   0.31380|
|fund_age_years        |   0.00866|    0.00515|   1.68121|            0.09275|  -0.00144|   0.01875|
|stock_weight_pct      |  -0.07116|    0.00505| -14.08940|            0.00000|  -0.08106|  -0.06126|
|bond_weight_pct       |   0.04215|    0.00617|   6.83122|            0.00000|   0.03006|   0.05424|
|annual_turnover_pct   |   0.00154|    0.00031|   4.89078|            0.00000|   0.00092|   0.00215|
|log_pe_ratio          |   6.23750|    0.16092|  38.76093|            0.00000|   5.92207|   6.55293|
|technology_weight_pct |   0.07913|    0.00461|  17.15152|            0.00000|   0.07009|   0.08818|

``` r
fit_table <- data.frame(N = nobs(model), R_squared = summary(model)$r.squared,
                        Adjusted_R_squared = summary(model)$adj.r.squared,
                        Residual_SD_pp = sigma(model),
                        Training_RMSE_pp = sqrt(mean(residuals(model)^2)))
knitr::kable(fit_table, digits = 4)
```



|     N| R_squared| Adjusted_R_squared| Residual_SD_pp| Training_RMSE_pp|
|-----:|---------:|------------------:|--------------:|----------------:|
| 13765|    0.2892|             0.2888|         5.2508|           5.2489|

## ANOVA and hypothesis tests

The overall F test tests that all nine slopes are zero. The partial F tests
below test each term after adjusting for **all** other predictors. For a
single slope, the partial F statistic equals the squared ordinary t statistic.
The final comparison explicitly tests H0: the adjusted ETF coefficient is zero.
All these classical tests assume independent, constant-variance errors and an
adequate mean model; small p-values alone do not establish model adequacy.


``` r
null_model <- lm(return_2018_pct ~ 1, data = model_data_2018)
knitr::kable(anova(null_model, model), digits = 5, caption = "Overall F test")
```



Table: Overall F test

| Res.Df|      RSS| Df| Sum of Sq|       F| Pr(>F)|
|------:|--------:|--:|---------:|-------:|------:|
|  13764| 533564.5| NA|        NA|      NA|     NA|
|  13755| 379237.0|  9|  154327.5| 621.943|      0|

``` r
knitr::kable(drop1(model, test = "F"), digits = 5,
             caption = "Partial F tests: each term adjusted for all others")
```



Table: Partial F tests: each term adjusted for all others

|                      | Df|   Sum of Sq|      RSS|      AIC|    F value|  Pr(>F)|
|:---------------------|--:|-----------:|--------:|--------:|----------:|-------:|
|<none>                | NA|          NA| 379237.0| 45665.18|         NA|      NA|
|etf_indicator         |  1|    83.47500| 379320.5| 45666.21|    3.02765| 0.08188|
|net_expense_ratio_pct |  1|  4794.87376| 384031.9| 45836.13|  173.91100| 0.00000|
|log_total_net_assets  |  1|  3824.47014| 383061.5| 45801.30|  138.71428| 0.00000|
|fund_age_years        |  1|    77.92793| 379314.9| 45666.01|    2.82646| 0.09275|
|stock_weight_pct      |  1|  5473.12212| 384710.1| 45860.42|  198.51120| 0.00000|
|bond_weight_pct       |  1|  1286.60820| 380523.6| 45709.80|   46.66553| 0.00000|
|annual_turnover_pct   |  1|   659.48816| 379896.5| 45687.10|   23.91976| 0.00000|
|log_pe_ratio          |  1| 41422.69811| 420659.7| 47090.10| 1502.40932| 0.00000|
|technology_weight_pct |  1|  8110.64108| 387347.6| 45954.47|  294.17453| 0.00000|

``` r
without_etf <- update(model, . ~ . - etf_indicator)
knitr::kable(anova(without_etf, model), digits = 5,
             caption = "Adjusted ETF indicator hypothesis test")
```



Table: Adjusted ETF indicator hypothesis test

| Res.Df|      RSS| Df| Sum of Sq|       F|  Pr(>F)|
|------:|--------:|--:|---------:|-------:|-------:|
|  13756| 379320.5| NA|        NA|      NA|      NA|
|  13755| 379237.0|  1|    83.475| 3.02765| 0.08188|

## Residual and normal Q-Q plots

Look for a horizontal residual smooth near zero, roughly constant vertical
spread, and Q-Q points near the line. The scale-location plot highlights
unequal variance. Tail departures and influential observations need attention;
normality concerns the model errors, not the predictors or raw returns.


``` r
par(mfrow = c(2, 2))
plot(model, which = c(1, 2, 3, 5), id.n = 3)
```

![plot of chunk standard-diagnostics](outputs/figures/standard-diagnostics-1.png)

``` r
par(mfrow = c(1, 1))
```


``` r
par(mfrow = c(1, 2))
plot(fitted(model), model_data_2018$return_2018_pct,
     pch = 16, col = adjustcolor("steelblue", 0.2),
     xlab = "Fitted return (%)", ylab = "Observed return (%)",
     main = "Observed versus fitted")
abline(0, 1, col = "red", lwd = 2)
boxplot(residuals(model) ~ fund_type, col = c("lightblue", "peachpuff"),
        main = "Residuals by fund type", ylab = "Residual (percentage points)")
abline(h = 0, col = "red", lty = 2)
```

![plot of chunk observed-fitted-and-residual-boxplot](outputs/figures/observed-fitted-and-residual-boxplot-1.png)

``` r
par(mfrow = c(1, 1))
```


``` r
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (v in predictors[-1]) {
  x <- model_data_2018[[v]]
  plot(x, residuals(model), pch = 16, col = adjustcolor("steelblue", 0.15),
       xlab = v, ylab = "Residual", cex.lab = 0.8)
  abline(h = 0, lty = 2)
  lines(lowess(x, residuals(model)), col = "red", lwd = 2)
}
par(mfrow = c(1, 1))
```

![plot of chunk residuals-by-predictor](outputs/figures/residuals-by-predictor-1.png)

## Collinearity

The correlation plot shows pairwise relationships. VIF checks each predictor
against all other predictors together. Values above 5 merit investigation;
this is a guide, not an automatic deletion rule.


``` r
correlations <- cor(model_data_2018[, predictors])
par(mar = c(12, 12, 3, 2))
image(seq_along(predictors), seq_along(predictors), correlations,
      zlim = c(-1, 1), col = colorRampPalette(c("navy", "white", "firebrick"))(100),
      axes = FALSE, xlab = "", ylab = "", main = "Predictor correlations (blue -1; red +1)")
axis(1, at = seq_along(predictors), labels = predictors, las = 2, cex.axis = 0.7)
axis(2, at = seq_along(predictors), labels = predictors, las = 2, cex.axis = 0.7)
for (i in seq_along(predictors)) for (j in seq_along(predictors)) {
  text(i, j, round(correlations[i, j], 2), cex = 0.65,
       col = ifelse(abs(correlations[i, j]) > 0.6, "white", "black"))
}
```

![plot of chunk collinearity](outputs/figures/collinearity-1.png)

``` r
par(mar = c(5, 4, 4, 2) + 0.1)
vif <- sapply(predictors, function(v) {
  auxiliary <- lm(reformulate(setdiff(predictors, v), response = v),
                  data = model_data_2018)
  1 / (1 - summary(auxiliary)$r.squared)
})
knitr::kable(data.frame(Predictor = predictors, VIF = vif), row.names = FALSE, digits = 3)
```



|Predictor             |   VIF|
|:---------------------|-----:|
|etf_indicator         | 1.112|
|net_expense_ratio_pct | 1.271|
|log_total_net_assets  | 1.185|
|fund_age_years        | 1.083|
|stock_weight_pct      | 6.816|
|bond_weight_pct       | 6.851|
|annual_turnover_pct   | 1.088|
|log_pe_ratio          | 1.363|
|technology_weight_pct | 1.307|

## Unequal variance and robust inference

The studentized Breusch–Pagan (Koenker) test regresses squared residuals on the
predictors; H0 is constant conditional variance. With this large sample,
interpret its p-value together with the plots. HC3 standard errors allow
unequal variance but still assume independent observations and a suitable mean
model. Their large-sample z tests and confidence intervals are a sensitivity
check, not a cure for dependence, missing confounders or incorrect timing.


``` r
squared_residuals <- residuals(model)^2
variance_model <- lm(reformulate(predictors, response = "squared_residuals"),
                     data = model_data_2018)
bp_stat <- nobs(model) * summary(variance_model)$r.squared
knitr::kable(data.frame(Test = "Studentized Breusch-Pagan", Statistic = bp_stat,
                        df = length(predictors),
                        p_value = pchisq(bp_stat, length(predictors), lower.tail = FALSE)))
```



|Test                      | Statistic| df| p_value|
|:-------------------------|---------:|--:|-------:|
|Studentized Breusch-Pagan |   698.071|  9|       0|

``` r
# HC3 covariance: inflate each squared residual by its leverage adjustment.
X <- model.matrix(model)
bread <- solve(crossprod(X))
hc3_weights <- (residuals(model) / (1 - hatvalues(model)))^2
hc3_cov <- bread %*% crossprod(X, X * hc3_weights) %*% bread
hc3_se <- sqrt(diag(hc3_cov))
robust_table <- data.frame(Estimate = coef(model), HC3_SE = hc3_se,
                           z = coef(model) / hc3_se,
                           p_value = 2 * pnorm(-abs(coef(model) / hc3_se)),
                           CI_lower = coef(model) - qnorm(0.975) * hc3_se,
                           CI_upper = coef(model) + qnorm(0.975) * hc3_se)
knitr::kable(robust_table, digits = 5)
```



|                      |  Estimate|  HC3_SE|         z| p_value|  CI_lower|  CI_upper|
|:---------------------|---------:|-------:|---------:|-------:|---------:|---------:|
|(Intercept)           | -28.47620| 1.17492| -24.23672| 0.00000| -30.77900| -26.17340|
|etf_indicator         |   0.41429| 0.33173|   1.24888| 0.21171|  -0.23589|   1.06447|
|net_expense_ratio_pct |  -1.15436| 0.14739|  -7.83196| 0.00000|  -1.44324|  -0.86548|
|log_total_net_assets  |   0.26903| 0.02365|  11.37496| 0.00000|   0.22267|   0.31538|
|fund_age_years        |   0.00866| 0.00519|   1.66846| 0.09522|  -0.00151|   0.01882|
|stock_weight_pct      |  -0.07116| 0.00776|  -9.17247| 0.00000|  -0.08637|  -0.05596|
|bond_weight_pct       |   0.04215| 0.00888|   4.74499| 0.00000|   0.02474|   0.05956|
|annual_turnover_pct   |   0.00154| 0.00042|   3.64324| 0.00027|   0.00071|   0.00237|
|log_pe_ratio          |   6.23750| 0.28028|  22.25492| 0.00000|   5.68817|   6.78683|
|technology_weight_pct |   0.07913| 0.00635|  12.45451| 0.00000|   0.06668|   0.09159|

## Influential observations

Cook's distance combines residual size and leverage. The 4/n and 2p/n lines
are screening guides, not reasons to delete observations. Identify and check
funds before deciding whether their records are incorrect or represent a
population outside the research question.


``` r
cooks <- cooks.distance(model)
leverage <- hatvalues(model)
n <- nobs(model)
p <- length(coef(model))
par(mfrow = c(1, 2))
plot(cooks, type = "h", main = "Cook's distance", ylab = "Cook's D", xlab = "Row")
abline(h = 4 / n, col = "red", lty = 2)
plot(leverage, rstandard(model), pch = 16, col = adjustcolor("steelblue", 0.2),
     main = "Leverage and standardized residuals", xlab = "Leverage", ylab = "Standardized residual")
abline(v = 2 * p / n, h = c(-3, 3), col = "red", lty = 2)
```

![plot of chunk influence](outputs/figures/influence-1.png)

``` r
par(mfrow = c(1, 1))
influence_table <- data.frame(Fund = model_data_2018$fund_symbol,
                              ETF = model_data_2018$etf_indicator,
                              Cooks_D = cooks, Leverage = leverage,
                              Standardized_residual = rstandard(model))
knitr::kable(head(influence_table[order(-cooks), ], 10), digits = 5, row.names = FALSE)
```



|Fund  | ETF| Cooks_D| Leverage| Standardized_residual|
|:-----|---:|-------:|--------:|---------------------:|
|BIZD  |   1| 0.07444|  0.02971|               4.93081|
|LOANX |   0| 0.05764|  0.02840|               4.44104|
|LOAIX |   0| 0.05491|  0.02703|               4.44592|
|FXSAX |   0| 0.04752|  0.04384|              -3.21935|
|FRICX |   0| 0.04433|  0.01545|               5.31581|
|FFRLX |   0| 0.04302|  0.01523|               5.27414|
|FFRSX |   0| 0.04280|  0.01521|               5.26430|
|ABRJX |   0| 0.03448|  0.00441|              -8.82188|
|ABRSX |   0| 0.03415|  0.00433|              -8.86004|
|FXTAX |   0| 0.02747|  0.15104|              -1.24247|

## Box–Cox table and profile plot

Ordinary Box–Cox requires a positive response. Use **gross return = 1 + decimal
return**, the ending value of one unit invested. This has an economic meaning
and is positive when loss is less than 100%; no arbitrary minimum-based shift
is used. Lambda = 1 is equivalent to the original untransformed regression up
to scale and intercept; lambda = 0 models log gross return. The grid-based 95%
profile interval is approximate. Transformation does not fix predictor timing,
clustering, or omitted variables, and is explored rather than automatically
adopted. The selected transformed model has different coefficient units.


``` r
model_data_2018$gross_return <- 1 + model_data_2018$fund_return_2018
stopifnot(all(model_data_2018$gross_return > 0))
gross_model <- update(model, gross_return ~ .)
bc <- MASS::boxcox(gross_model, lambda = seq(-5, 5, by = 0.1),
                   plotit = FALSE)
best_lambda <- bc$x[which.max(bc$y)]
cutoff <- max(bc$y) - qchisq(0.95, df = 1) / 2
boxcox_table <- data.frame(Lambda = bc$x, Profile_log_likelihood = bc$y,
                           Inside_95pct_profile_interval = bc$y >= cutoff)
write.csv(boxcox_table, "outputs/boxcox_full_table.csv", row.names = FALSE)
# Display conventional transformations and the optimum; export the full grid.
show_rows <- abs(bc$x - best_lambda) < 1e-8 |
             sapply(bc$x, function(x) any(abs(x - c(-2, -1, -0.5, 0, 0.5, 1, 2)) < 1e-8))
knitr::kable(boxcox_table[show_rows, ], digits = 4, row.names = FALSE)
```



| Lambda| Profile_log_likelihood|Inside_95pct_profile_interval |
|------:|----------------------:|:-----------------------------|
|   -2.0|              -28143.15|FALSE                         |
|   -1.0|              -27254.74|FALSE                         |
|   -0.5|              -26927.92|FALSE                         |
|    0.0|              -26665.93|FALSE                         |
|    0.5|              -26461.35|FALSE                         |
|    1.0|              -26308.47|FALSE                         |
|    2.0|              -26141.53|FALSE                         |
|    2.5|              -26121.77|TRUE                          |

``` r
profile_interval <- range(bc$x[bc$y >= cutoff])
knitr::kable(data.frame(Best_lambda = best_lambda,
                        Lower_grid_95 = profile_interval[1], Upper_grid_95 = profile_interval[2]))
```



| Best_lambda| Lower_grid_95| Upper_grid_95|
|-----------:|-------------:|-------------:|
|         2.5|           2.4|           2.6|

``` r
if (any(profile_interval %in% range(bc$x))) {
  cat("The profile interval touches the grid boundary; expand the grid before interpreting it.")
}
plot(bc$x, bc$y, type = "l", xlab = expression(lambda),
     ylab = "Profile log likelihood", main = "Box-Cox profile: gross return")
abline(h = cutoff, lty = 2, col = "red")
abline(v = c(best_lambda, 1), col = c("blue", "darkgreen"), lty = c(1, 2))
legend("bottomright", c("Best grid lambda", "Untransformed (lambda = 1)", "95% cutoff"),
       col = c("blue", "darkgreen", "red"), lty = c(1, 2, 2), cex = 0.8)
```

![plot of chunk box-cox](outputs/figures/box-cox-1.png)

``` r
model_data_2018$transformed_return <- if (abs(best_lambda) < 1e-8) {
  log(model_data_2018$gross_return)
} else {
  (model_data_2018$gross_return^best_lambda - 1) / best_lambda
}
transformed_model <- update(model, transformed_return ~ .)
```


``` r
par(mfrow = c(2, 2))
plot(transformed_model, which = c(1, 2, 3, 5), id.n = 3)
```

![plot of chunk transformed-diagnostics](outputs/figures/transformed-diagnostics-1.png)

``` r
par(mfrow = c(1, 1))
```

Do not compare R-squared across the two response scales as evidence of improved
prediction. Inspect whether the transformed diagnostic patterns improve; the
lambda was selected on this same sample.

## Save tables and interpret the findings


``` r
write.csv(coefficient_table, "outputs/ols_coefficients.csv")
write.csv(robust_table, "outputs/hc3_coefficients.csv")
write.csv(fit_table, "outputs/model_fit.csv", row.names = FALSE)
write.csv(descriptive, "outputs/descriptive_statistics.csv")
write.csv(drop1(model, test = "F"), "outputs/partial_F_tests.csv")
write.csv(influence_table, "outputs/influence_diagnostics.csv", row.names = FALSE)
write.csv(data.frame(Predictor = predictors, VIF = vif), "outputs/vif.csv", row.names = FALSE)
```

The complete-case sample contains 13765 funds, including
564 ETFs. The adjusted R-squared is
0.289. The adjusted ETF difference is
0.414 percentage points, with HC3 95%
interval [-0.236,
1.064]. These are associations
in this selected sample, conditional on the measured predictors.

A successful analysis reports where assumptions fail; it does not require all
diagnostics to pass. Review residual curvature, variance, tail behavior,
influence and VIF before proposing revisions. If prediction becomes the goal,
add held-out evaluation, ideally splitting by underlying fund rather than
share class. No out-of-sample accuracy is claimed here.

### Interpretation of this run

The residual plots show unequal spread and pronounced tail departures from
normality. The variance test rejects constant variance. Emphasize the HC3
interval for the ETF comparison: it includes zero, so this model does not
provide clear evidence of an adjusted ETF advantage at the 5% level.
Stock and bond VIFs are about 6.8; the ETF VIF is about 1.1. Thus the main
collinearity concern is asset allocation, rather than the ETF indicator.

The Box–Cox optimum is approximately 2.5 on gross returns. This is an
exploratory transformation, not a standard log-return recommendation. Check
the transformed Q-Q and variance plots before adopting it. Both models remain
limited by predictor timing, possible shared portfolios, and selection into
the complete-case sample. Investigate influential funds and consider category
controls and nonlinear relationships as next steps; do not remove observations
or add terms solely to obtain smaller p-values.

Method references: [R's ANOVA documentation](https://www.stat.math.ethz.ch/R-manual/R-patched/RHOME/library/stats/html/anova.lm.html)
and [MASS Box–Cox documentation](https://www.stat.math.ethz.ch/R-manual/R-devel/library/MASS/html/boxcox.html).
