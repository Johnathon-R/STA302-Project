---
title: "STA302: Final Project"
author: "Alexandros, Nikolas, Johnathon"
output: html_document
---

## 1. Load and clean the data

Read only the two fund files. Keep the 2018 return and the columns needed
for the nine predictors. Blank entries are treated as missing (`NA`).


``` r
# Read in the datasets
etfs <- read.csv("data/ETFs.csv", na.strings = c("", "NA"))
mutualfunds <- read.csv("data/MutualFunds.csv", na.strings = c("", "NA"))

# Columns to keep
columns <- c(
    "fund_return_2018",
    "fund_annual_report_net_expense_ratio",
    "total_net_assets",
    "inception_date",
    "asset_stocks",
    "asset_bonds",
    "annual_holdings_turnover",
    "fund_price_earning_ratio",
    "fund_sector_technology"
)

# Filter the columns but keep all rows
etfs <- etfs[, columns]
mutualfunds <- mutualfunds[, columns]

# Create indicator variable for etf and mutual fund
etfs$etf_indicator <- 1
mutualfunds$etf_indicator <- 0
funds <- rbind(etfs, mutualfunds)

# Calculate age in years at January 1, 2018
# SERIOUS: FIX THIS YEAR CALCULATION MAY NOT BE CORRECT.
funds$inception_date <- as.Date(funds$inception_date)
funds$fund_age_years <- as.numeric(
    as.Date("2019-01-01") - funds$inception_date
) / 365.25

# Funds launched after that date have no applicable age for this analysis.
funds$fund_age_years[funds$fund_age_years < 0] <- NA

# The model uses age, so the original date column is no longer needed.
funds$inception_date <- NULL

# Check missing values before excluding incomplete rows.
colSums(is.na(funds))
```

```
##                     fund_return_2018 fund_annual_report_net_expense_ratio 
##                                 4181                                  225 
##                     total_net_assets                         asset_stocks 
##                                   64                                 3476 
##                          asset_bonds             annual_holdings_turnover 
##                                 3476                                 2758 
##             fund_price_earning_ratio               fund_sector_technology 
##                                 7625                                 6210 
##                        etf_indicator                       fund_age_years 
##                                    0                                 2819
```

``` r
# Keep a separate cleaned table with every model value available.
model_data <- funds[complete.cases(funds), ]

# Compare counts before and after cleaning: 0 = mutual fund, 1 = ETF.
table(funds$etf_indicator)
```

```
## 
##     0     1 
## 23783  2310
```

``` r
table(model_data$etf_indicator)
```

```
## 
##     0     1 
## 13202   564
```

Missing stock or bond weights are **unknown, not zero**. A row missing any
required value is excluded from `model_data`, but remains in `funds`.
Recorded zeros and other nonmissing values are kept. No outliers are removed
and no values are filled in. The indicator and age are the only derived
predictors; assets, P/E, weights, expenses, turnover and returns are unchanged.

## 2. Fit the model


``` r
model <- lm(
    fund_return_2018 ~ etf_indicator +
        fund_annual_report_net_expense_ratio +
        total_net_assets + fund_age_years +
        asset_stocks + asset_bonds +
        annual_holdings_turnover + fund_price_earning_ratio +
        fund_sector_technology,
    data = model_data
)
```


``` r
summary(model)
```

```
## 
## Call:
## lm(formula = fund_return_2018 ~ etf_indicator + fund_annual_report_net_expense_ratio + 
##     total_net_assets + fund_age_years + asset_stocks + asset_bonds + 
##     annual_holdings_turnover + fund_price_earning_ratio + fund_sector_technology, 
##     data = model_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.46066 -0.02554  0.00300  0.02955  0.35213 
## 
## Coefficients:
##                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          -1.002e-01  5.268e-03 -19.022  < 2e-16 ***
## etf_indicator                         6.090e-04  2.339e-03   0.260 0.794618    
## fund_annual_report_net_expense_ratio -1.456e+00  8.463e-02 -17.201  < 2e-16 ***
## total_net_assets                      1.112e-13  1.698e-14   6.547 6.08e-11 ***
## fund_age_years                        1.726e-04  4.987e-05   3.462 0.000538 ***
## asset_stocks                         -7.506e-02  4.988e-03 -15.049  < 2e-16 ***
## asset_bonds                           3.190e-02  6.077e-03   5.248 1.56e-07 ***
## annual_holdings_turnover              1.206e-03  3.091e-04   3.900 9.66e-05 ***
## fund_price_earning_ratio              3.048e-03  6.754e-05  45.132  < 2e-16 ***
## fund_sector_technology                6.867e-02  4.575e-03  15.009  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.05191 on 13756 degrees of freedom
## Multiple R-squared:  0.3054,	Adjusted R-squared:  0.305 
## F-statistic: 672.1 on 9 and 13756 DF,  p-value: < 2.2e-16
```

``` r
confint(model) # 95% confidence intervals
```

```
##                                              2.5 %        97.5 %
## (Intercept)                          -1.105408e-01 -8.988790e-02
## etf_indicator                        -3.976547e-03  5.194550e-03
## fund_annual_report_net_expense_ratio -1.621702e+00 -1.289911e+00
## total_net_assets                      7.789573e-14  1.444711e-13
## fund_age_years                        7.488742e-05  2.703937e-04
## asset_stocks                         -8.484155e-02 -6.528751e-02
## asset_bonds                           1.998411e-02  4.380869e-02
## annual_holdings_turnover              5.996679e-04  1.811532e-03
## fund_price_earning_ratio              2.915714e-03  3.180478e-03
## fund_sector_technology                5.970041e-02  7.763640e-02
```

The ETF coefficient compares ETFs with mutual funds while holding the other
predictors fixed. No transformations or corrections are applied to this model.

## 3. Summary tables and tests


``` r
summary(model_data)
```

```
##  fund_return_2018   fund_annual_report_net_expense_ratio total_net_assets   
##  Min.   :-0.47814   Min.   :0.00010                      Min.   :1.354e+04  
##  1st Qu.:-0.12803   1st Qu.:0.00710                      1st Qu.:1.732e+08  
##  Median :-0.07897   Median :0.01000                      Median :7.447e+08  
##  Mean   :-0.08706   Mean   :0.01083                      Mean   :6.280e+09  
##  3rd Qu.:-0.04621   3rd Qu.:0.01370                      3rd Qu.:2.884e+09  
##  Max.   : 0.22578   Max.   :0.11420                      Max.   :7.534e+11  
##   asset_stocks     asset_bonds      annual_holdings_turnover
##  Min.   :0.0000   Min.   :0.00000   Min.   : 0.0100         
##  1st Qu.:0.8339   1st Qu.:0.00000   1st Qu.: 0.2600         
##  Median :0.9716   Median :0.00000   Median : 0.4700         
##  Mean   :0.8583   Mean   :0.08656   Mean   : 0.7214         
##  3rd Qu.:0.9905   3rd Qu.:0.04345   3rd Qu.: 0.8200         
##  Max.   :1.0000   Max.   :0.98430   Max.   :66.2700         
##  fund_price_earning_ratio fund_sector_technology etf_indicator    
##  Min.   : 0.33            Min.   :0.0000         Min.   :0.00000  
##  1st Qu.:19.99            1st Qu.:0.1060         1st Qu.:0.00000  
##  Median :23.17            Median :0.1772         Median :0.00000  
##  Mean   :24.58            Mean   :0.1757         Mean   :0.04097  
##  3rd Qu.:27.82            3rd Qu.:0.2287         3rd Qu.:0.00000  
##  Max.   :59.99            Max.   :1.0000         Max.   :1.00000  
##  fund_age_years   
##  Min.   : 0.2601  
##  1st Qu.: 4.7146  
##  Median :10.0260  
##  Mean   :11.5620  
##  3rd Qu.:16.5332  
##  Max.   :94.4641
```

``` r
# Overall test: are all nine predictor coefficients zero?
intercept_only <- lm(fund_return_2018 ~ 1, data = model_data)
anova(intercept_only, model)
```

```
## Analysis of Variance Table
## 
## Model 1: fund_return_2018 ~ 1
## Model 2: fund_return_2018 ~ etf_indicator + fund_annual_report_net_expense_ratio + 
##     total_net_assets + fund_age_years + asset_stocks + asset_bonds + 
##     annual_holdings_turnover + fund_price_earning_ratio + fund_sector_technology
##   Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
## 1  13765 53.37                                  
## 2  13756 37.07  9      16.3 672.09 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# Test each predictor while accounting for all the others.
drop1(model, test = "F")
```

```
## Single term deletions
## 
## Model:
## fund_return_2018 ~ etf_indicator + fund_annual_report_net_expense_ratio + 
##     total_net_assets + fund_age_years + asset_stocks + asset_bonds + 
##     annual_holdings_turnover + fund_price_earning_ratio + fund_sector_technology
##                                      Df Sum of Sq    RSS    AIC   F value
## <none>                                            37.070 -81436          
## etf_indicator                         1    0.0002 37.070 -81437    0.0678
## fund_annual_report_net_expense_ratio  1    0.7973 37.867 -81145  295.8756
## total_net_assets                      1    0.1155 37.185 -81395   42.8633
## fund_age_years                        1    0.0323 37.102 -81426   11.9839
## asset_stocks                          1    0.6103 37.680 -81213  226.4795
## asset_bonds                           1    0.0742 37.144 -81410   27.5464
## annual_holdings_turnover              1    0.0410 37.111 -81422   15.2101
## fund_price_earning_ratio              1    5.4891 42.559 -79537 2036.9180
## fund_sector_technology                1    0.6070 37.677 -81214  225.2663
##                                         Pr(>F)    
## <none>                                            
## etf_indicator                        0.7946182    
## fund_annual_report_net_expense_ratio < 2.2e-16 ***
## total_net_assets                     6.078e-11 ***
## fund_age_years                       0.0005383 ***
## asset_stocks                         < 2.2e-16 ***
## asset_bonds                          1.556e-07 ***
## annual_holdings_turnover             9.665e-05 ***
## fund_price_earning_ratio             < 2.2e-16 ***
## fund_sector_technology               < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# The first column is the response; exclude it from predictor covariance.
covariance_matrix <- cov(model_data[, -1])
knitr::kable(signif(covariance_matrix, 4))
```



|                                     | fund_annual_report_net_expense_ratio| total_net_assets| asset_stocks| asset_bonds| annual_holdings_turnover| fund_price_earning_ratio| fund_sector_technology| etf_indicator| fund_age_years|
|:------------------------------------|------------------------------------:|----------------:|------------:|-----------:|------------------------:|------------------------:|----------------------:|-------------:|--------------:|
|fund_annual_report_net_expense_ratio |                            3.320e-05|       -2.010e+07|    6.000e-06|  -1.331e-04|                1.623e-03|                4.166e-04|             -1.070e-05|    -2.669e-04|      4.748e-03|
|total_net_assets                     |                           -2.010e+07|        7.004e+20|    1.505e+08|  -7.443e+07|               -1.796e+09|                9.683e+09|              1.107e+08|    -1.709e+06|      2.112e+10|
|asset_stocks                         |                            6.000e-06|        1.505e+08|    5.352e-02|  -4.015e-02|               -3.892e-02|                2.631e-01|              3.521e-03|     5.537e-03|      1.601e-01|
|asset_bonds                          |                           -1.331e-04|       -7.443e+07|   -4.015e-02|   3.604e-02|                8.392e-03|               -2.105e-01|             -2.855e-03|    -3.376e-03|     -1.068e-01|
|annual_holdings_turnover             |                            1.623e-03|       -1.796e+09|   -3.892e-02|   8.392e-03|                2.204e+00|               -3.548e-01|             -1.741e-03|    -3.663e-03|     -4.445e-01|
|fund_price_earning_ratio             |                            4.166e-04|        9.683e+09|    2.631e-01|  -2.105e-01|               -3.548e-01|                5.706e+01|              4.043e-01|    -1.303e-01|      5.385e+00|
|fund_sector_technology               |                           -1.070e-05|        1.107e+08|    3.521e-03|  -2.855e-03|               -1.741e-03|                4.043e-01|              1.230e-02|    -1.142e-03|      6.173e-02|
|etf_indicator                        |                           -2.669e-04|       -1.709e+06|    5.537e-03|  -3.376e-03|               -3.663e-03|               -1.303e-01|             -1.142e-03|     3.929e-02|     -1.755e-01|
|fund_age_years                       |                            4.748e-03|        2.112e+10|    1.601e-01|  -1.068e-01|               -4.445e-01|                5.385e+00|              6.173e-02|    -1.755e-01|      8.182e+01|

The covariance diagonal runs from top left to bottom right and contains
variances. Covariances depend on units, so their sizes alone do not measure
collinearity. Four significant digits are used only to display the table.

### Covariance heatmap

The numbers are covariances, not correlations. Asset variance is much larger
than the other entries, so it dominates the colour scale. Scientific notation
keeps large and small numbers readable (for example, `1e+06` means one million).


``` r
# Use the predictors in the same order as the fitted model.
predictor_data <- model$model[, -1]
covariance_matrix <- cov(predictor_data)
labels <- c("ETF", "Expense", "Assets", "Age", "Stocks",
            "Bonds", "Turnover", "P/E", "Technology")

# Use blue for negative values, white for zero, and red for positive values.
colour_limit <- max(abs(covariance_matrix))
colours <- colorRampPalette(c("navy", "white", "firebrick"))(100)

par(mar = c(7, 7, 4, 2))
image(1:9, 1:9, covariance_matrix[, 9:1],
      col = colours, zlim = c(-colour_limit, colour_limit),
      axes = FALSE, xlab = "", ylab = "",
      main = "Predictor covariance (blue: negative; red: positive)")
axis(1, at = 1:9, labels = labels, las = 2)
axis(2, at = 1:9, labels = rev(labels), las = 2)

# Draw a number in every cell, with the diagonal from top left to bottom right.
for (row in 1:9) {
  for (column in 1:9) {
    value <- covariance_matrix[row, column]
    text(column, 10 - row, format(value, digits = 2, scientific = TRUE),
         cex = 0.8,
         col = ifelse(abs(value) > colour_limit / 2, "white", "black"))
  }
}
```

![plot of chunk covariance-heatmap](outputs/figures/covariance-heatmap-1.png)

``` r
par(mar = c(5, 4, 4, 2) + 0.1)
```

## 4. Graphs

These plots use the original returns and ordinary residuals. A residual is
observed return minus fitted return; calculating it does not transform the model.

**Boxplot:** compare returns between ETFs and mutual funds.


``` r
boxplot(fund_return_2018 ~ etf_indicator,
    data = model_data,
    names = c("Mutual fund", "ETF"),
    ylab = "2018 return (decimal)", main = "2018 returns by fund type"
)
```

![plot of chunk return-boxplot](outputs/figures/return-boxplot-1.png)

**Histogram:** examine the distribution of returns.


``` r
hist(model_data$fund_return_2018,
    xlab = "2018 return (decimal)", main = "Distribution of 2018 returns"
)
```

![plot of chunk return-histogram](outputs/figures/return-histogram-1.png)

**Residual plot:** look for scatter around zero with no curve or funnel shape.


``` r
plot(fitted(model), residuals(model),
    xlab = "Fitted return", ylab = "Residual", main = "Residuals versus fitted"
)
abline(h = 0, col = "red")
```

![plot of chunk residual-plot](outputs/figures/residual-plot-1.png)

**Normal Q-Q plot:** points close to the line support approximately normal
errors; departures at the ends suggest unusual tails.


``` r
qqnorm(residuals(model), main = "Normal Q-Q plot of residuals")
qqline(residuals(model), col = "red")
```

![plot of chunk qq-plot](outputs/figures/qq-plot-1.png)

**Residual boxplot:** compare the spread of errors between fund types.


``` r
boxplot(residuals(model) ~ model_data$etf_indicator,
    names = c("Mutual fund", "ETF"),
    ylab = "Residual", main = "Residuals by fund type"
)
abline(h = 0, col = "red")
```

![plot of chunk residual-boxplot](outputs/figures/residual-boxplot-1.png)

## Notes for later

This is the baseline model, with problems left for later investigation.
Ordinary tests assume independent errors and constant variance. Excluding
missing values can change which funds are represented. Also, the files contain
2021 reporting dates, so predictor measurements are not confirmed as pre-2018.

[Group proposal document](https://utoronto-my.sharepoint.com/:w:/r/personal/alexandros_phardis_mail_utoronto_ca/_layouts/15/Doc.aspx?sourcedoc=%7B829F4894-DB17-4CA6-B650-DFCDD9866136%7D&file=STA302%20Final%20Project%20Proposal.docx&action=default&mobileredirect=true)
