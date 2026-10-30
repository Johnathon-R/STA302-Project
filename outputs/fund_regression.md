---
title: "STA302: 2018 Fund Returns"
output: html_document
---

## Before you start

Run the code blocks from top to bottom. Each block uses results from the ones
above it. `<-` saves a result under a name, `$` selects a column, and `#` starts
a comment explaining the code.

We use only the 2018 return and the nine chosen predictors. Values stay in
their original units: a return of `0.05` means 5%. No logs, rescaling,
transformations, or model corrections are applied.



## 1. Read the two files

`read.csv()` reads a CSV file into a table. `NA` means a missing value.


``` r
etfs <- read.csv("data/ETFs.csv", na.strings = c("", "NA"))
mutualfunds <- read.csv("data/MutualFunds.csv", na.strings = c("", "NA"))
```

## 2. Keep the columns we need

`c()` makes a list of column names. In `[rows, columns]`, leaving the row
position blank keeps every row.


``` r
columns <- c(
  "fund_symbol",
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

etfs <- etfs[, columns]
mutualfunds <- mutualfunds[, columns]

# Label each fund: 1 means ETF, 0 means mutual fund.
etfs$etf_indicator <- 1
mutualfunds$etf_indicator <- 0

# Stack the two tables into one table.
funds <- rbind(etfs, mutualfunds)
```

## 3. Calculate fund age

Convert inception dates into dates R understands. Subtracting dates gives the
number of days between them; dividing by 365.25 gives approximate years.
This and the ETF indicator are the only derived predictors.


``` r
funds$inception_date <- as.Date(funds$inception_date)

funds$fund_age_years <- as.numeric(
  as.Date("2018-01-01") - funds$inception_date
) / 365.25

# A fund launched after January 1, 2018 has no applicable age at that date.
funds$fund_age_years[funds$fund_age_years < 0] <- NA
```

## 4. Check missing values

Keep `funds` as the full table. Make a separate table called `model_data` for
fitting the regression. Only rows with every model variable available are
used; no missing values are filled in and no outliers are removed.


``` r
model_columns <- c(
  "fund_return_2018",
  "etf_indicator",
  "fund_annual_report_net_expense_ratio",
  "total_net_assets",
  "fund_age_years",
  "asset_stocks",
  "asset_bonds",
  "annual_holdings_turnover",
  "fund_price_earning_ratio",
  "fund_sector_technology"
)

# Count missing values in each model column.
colSums(is.na(funds[, model_columns]))
```

```
##                     fund_return_2018                        etf_indicator 
##                                 4181                                    0 
## fund_annual_report_net_expense_ratio                     total_net_assets 
##                                  225                                   64 
##                       fund_age_years                         asset_stocks 
##                                 4188                                 3476 
##                          asset_bonds             annual_holdings_turnover 
##                                 3476                                 2758 
##             fund_price_earning_ratio               fund_sector_technology 
##                                 7625                                 6210
```

``` r
# Keep rows with no missing model values.
complete_rows <- complete.cases(funds[, model_columns])
model_data <- funds[complete_rows, ]

# Compare fund counts before and after removing incomplete rows.
# In these tables: 0 = mutual fund, 1 = ETF.
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
## 13201   564
```

## 5. Summarize and plot the data

`summary()` shows the minimum, quartiles, median, mean, and maximum.
The boxplot compares the two fund types before adjusting for other predictors.


``` r
summary(model_data[, model_columns])
```

```
##  fund_return_2018   etf_indicator     fund_annual_report_net_expense_ratio
##  Min.   :-0.47814   Min.   :0.00000   Min.   :0.00010                     
##  1st Qu.:-0.12796   1st Qu.:0.00000   1st Qu.:0.00710                     
##  Median :-0.07897   Median :0.00000   Median :0.01000                     
##  Mean   :-0.08705   Mean   :0.04097   Mean   :0.01083                     
##  3rd Qu.:-0.04620   3rd Qu.:0.00000   3rd Qu.:0.01370                     
##  Max.   : 0.22578   Max.   :1.00000   Max.   :0.11420                     
##  total_net_assets    fund_age_years       asset_stocks     asset_bonds     
##  Min.   :1.354e+04   Min.   : 0.008214   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:1.732e+08   1st Qu.: 3.715264   1st Qu.:0.8339   1st Qu.:0.00000  
##  Median :7.447e+08   Median : 9.026694   Median :0.9716   Median :0.00000  
##  Mean   :6.280e+09   Mean   :10.563531   Mean   :0.8583   Mean   :0.08656  
##  3rd Qu.:2.884e+09   3rd Qu.:15.534565   3rd Qu.:0.9905   3rd Qu.:0.04350  
##  Max.   :7.534e+11   Max.   :93.464750   Max.   :1.0000   Max.   :0.98430  
##  annual_holdings_turnover fund_price_earning_ratio fund_sector_technology
##  Min.   : 0.0100          Min.   : 0.33            Min.   :0.0000        
##  1st Qu.: 0.2600          1st Qu.:19.99            1st Qu.:0.1060        
##  Median : 0.4700          Median :23.17            Median :0.1771        
##  Mean   : 0.7213          Mean   :24.58            Mean   :0.1757        
##  3rd Qu.: 0.8200          3rd Qu.:27.82            3rd Qu.:0.2287        
##  Max.   :66.2700          Max.   :59.99            Max.   :1.0000
```


``` r
boxplot(fund_return_2018 ~ etf_indicator, data = model_data,
        names = c("Mutual fund", "ETF"),
        main = "2018 returns by fund type",
        ylab = "2018 return (decimal)")
```

![plot of chunk return-boxplot](outputs/figures/return-boxplot-1.png)


``` r
hist(model_data$fund_return_2018,
     main = "Distribution of 2018 returns",
     xlab = "2018 return (decimal)")
```

![plot of chunk return-histogram](outputs/figures/return-histogram-1.png)

## 6. Fit the multiple linear regression

`lm()` fits a linear regression. The response goes to the left of `~`, and
the predictors go to the right. `+` adds another predictor to the model.


``` r
model <- lm(
  fund_return_2018 ~ etf_indicator +
    fund_annual_report_net_expense_ratio +
    total_net_assets +
    fund_age_years +
    asset_stocks +
    asset_bonds +
    annual_holdings_turnover +
    fund_price_earning_ratio +
    fund_sector_technology,
  data = model_data
)

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
## -0.46067 -0.02553  0.00301  0.02954  0.35213 
## 
## Coefficients:
##                                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          -1.000e-01  5.267e-03 -18.994  < 2e-16 ***
## etf_indicator                         5.881e-04  2.339e-03   0.251  0.80151    
## fund_annual_report_net_expense_ratio -1.456e+00  8.463e-02 -17.209  < 2e-16 ***
## total_net_assets                      1.111e-13  1.698e-14   6.545 6.17e-11 ***
## fund_age_years                        1.716e-04  4.987e-05   3.442  0.00058 ***
## asset_stocks                         -7.502e-02  4.988e-03 -15.043  < 2e-16 ***
## asset_bonds                           3.192e-02  6.077e-03   5.252 1.53e-07 ***
## annual_holdings_turnover              1.208e-03  3.091e-04   3.908 9.37e-05 ***
## fund_price_earning_ratio              3.047e-03  6.753e-05  45.121  < 2e-16 ***
## fund_sector_technology                6.873e-02  4.575e-03  15.023  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.05191 on 13755 degrees of freedom
## Multiple R-squared:  0.3054,	Adjusted R-squared:  0.305 
## F-statistic:   672 on 9 and 13755 DF,  p-value: < 2.2e-16
```

In the output, **Estimate** is each coefficient, **Pr(>|t|)** is its p-value,
and **Adjusted R-squared** summarizes the model's fit while accounting for the
number of predictors. The ETF coefficient compares ETFs with mutual funds
while holding the other predictors fixed. Its units are decimal return.


``` r
# Ordinary 95% confidence intervals for the coefficients.
confint(model)
```

```
##                                              2.5 %        97.5 %
## (Intercept)                          -1.103660e-01 -8.971748e-02
## etf_indicator                        -3.997090e-03  5.173250e-03
## fund_annual_report_net_expense_ratio -1.622251e+00 -1.290488e+00
## total_net_assets                      7.785200e-14  1.444212e-13
## fund_age_years                        7.388051e-05  2.693797e-04
## asset_stocks                         -8.480091e-02 -6.524852e-02
## asset_bonds                           2.000406e-02  4.382644e-02
## annual_holdings_turnover              6.019687e-04  1.813728e-03
## fund_price_earning_ratio              2.914790e-03  3.179535e-03
## fund_sector_technology                5.976081e-02  7.769554e-02
```

## 7. ANOVA and hypothesis tests

First compare the full model with an intercept-only model. This tests whether
all nine predictor coefficients are zero together.


``` r
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
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1  13764 53.356                                  
## 2  13755 37.060  9    16.296 672.05 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Next test each predictor after accounting for all the others. `drop1()` checks
what happens if each predictor is removed, one at a time. The ETF row tests
whether its adjusted coefficient is zero. These tests do not change `model`.


``` r
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
## <none>                                            37.060 -81432          
## etf_indicator                         1    0.0002 37.060 -81434    0.0632
## fund_annual_report_net_expense_ratio  1    0.7979 37.858 -81141  296.1564
## total_net_assets                      1    0.1154 37.176 -81391   42.8352
## fund_age_years                        1    0.0319 37.092 -81422   11.8449
## asset_stocks                          1    0.6097 37.670 -81210  226.2777
## asset_bonds                           1    0.0743 37.134 -81407   27.5841
## annual_holdings_turnover              1    0.0411 37.101 -81419   15.2695
## fund_price_earning_ratio              1    5.4854 42.546 -79534 2035.9438
## fund_sector_technology                1    0.6081 37.668 -81210  225.6902
##                                         Pr(>F)    
## <none>                                            
## etf_indicator                        0.8015077    
## fund_annual_report_net_expense_ratio < 2.2e-16 ***
## total_net_assets                     6.166e-11 ***
## fund_age_years                       0.0005799 ***
## asset_stocks                         < 2.2e-16 ***
## asset_bonds                          1.527e-07 ***
## annual_holdings_turnover             9.366e-05 ***
## fund_price_earning_ratio             < 2.2e-16 ***
## fund_sector_technology               < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## 8. Check the regression assumptions

R already provides the main diagnostic plots, so we can draw them directly.
These plots help identify problems to fix later; they do not fix the model.

**Residuals versus fitted:** look for residuals scattered around zero, without
curvature or a funnel shape.


``` r
plot(model, which = 1)
```

![plot of chunk residual-plot](outputs/figures/residual-plot-1.png)

**Normal Q-Q:** points close to the line support approximately normal errors.
Large departures, especially at the ends, suggest non-normal tails.


``` r
plot(model, which = 2)
```

![plot of chunk qq-plot](outputs/figures/qq-plot-1.png)

**Scale-location:** a roughly horizontal trend and even spread support
constant error variance. The plotted residuals are standardized only for the
diagnostic; the regression still uses the original return.


``` r
plot(model, which = 3)
```

![plot of chunk variance-plot](outputs/figures/variance-plot-1.png)

**Residuals versus leverage:** observations far to the right have unusual
predictor combinations. Large residuals and high leverage together merit review.


``` r
plot(model, which = 5, cook.levels = numeric(0))
```

![plot of chunk leverage-plot](outputs/figures/leverage-plot-1.png)

## 9. Check relationships between predictors

The correlation table checks whether pairs of predictors are closely related.
Values near -1 or 1 indicate a strong linear relationship. Pay particular
attention to ETF versus expense ratio and stock versus bond weights.
This is an initial check; pairwise correlations do not detect every form of
collinearity among several predictors.


``` r
# Remove the response from the list before calculating predictor correlations.
predictor_columns <- model_columns[-1]
round(cor(model_data[, predictor_columns]), 2)
```

```
##                                      etf_indicator
## etf_indicator                                 1.00
## fund_annual_report_net_expense_ratio         -0.23
## total_net_assets                              0.00
## fund_age_years                               -0.10
## asset_stocks                                  0.12
## asset_bonds                                  -0.09
## annual_holdings_turnover                     -0.01
## fund_price_earning_ratio                     -0.09
## fund_sector_technology                       -0.05
##                                      fund_annual_report_net_expense_ratio
## etf_indicator                                                       -0.23
## fund_annual_report_net_expense_ratio                                 1.00
## total_net_assets                                                    -0.13
## fund_age_years                                                       0.09
## asset_stocks                                                         0.00
## asset_bonds                                                         -0.12
## annual_holdings_turnover                                             0.19
## fund_price_earning_ratio                                             0.01
## fund_sector_technology                                              -0.02
##                                      total_net_assets fund_age_years
## etf_indicator                                    0.00          -0.10
## fund_annual_report_net_expense_ratio            -0.13           0.09
## total_net_assets                                 1.00           0.09
## fund_age_years                                   0.09           1.00
## asset_stocks                                     0.02           0.08
## asset_bonds                                     -0.01          -0.06
## annual_holdings_turnover                        -0.05          -0.03
## fund_price_earning_ratio                         0.05           0.08
## fund_sector_technology                           0.04           0.06
##                                      asset_stocks asset_bonds
## etf_indicator                                0.12       -0.09
## fund_annual_report_net_expense_ratio         0.00       -0.12
## total_net_assets                             0.02       -0.01
## fund_age_years                               0.08       -0.06
## asset_stocks                                 1.00       -0.91
## asset_bonds                                 -0.91        1.00
## annual_holdings_turnover                    -0.11        0.03
## fund_price_earning_ratio                     0.15       -0.15
## fund_sector_technology                       0.14       -0.14
##                                      annual_holdings_turnover
## etf_indicator                                           -0.01
## fund_annual_report_net_expense_ratio                     0.19
## total_net_assets                                        -0.05
## fund_age_years                                          -0.03
## asset_stocks                                            -0.11
## asset_bonds                                              0.03
## annual_holdings_turnover                                 1.00
## fund_price_earning_ratio                                -0.03
## fund_sector_technology                                  -0.01
##                                      fund_price_earning_ratio
## etf_indicator                                           -0.09
## fund_annual_report_net_expense_ratio                     0.01
## total_net_assets                                         0.05
## fund_age_years                                           0.08
## asset_stocks                                             0.15
## asset_bonds                                             -0.15
## annual_holdings_turnover                                -0.03
## fund_price_earning_ratio                                 1.00
## fund_sector_technology                                   0.48
##                                      fund_sector_technology
## etf_indicator                                         -0.05
## fund_annual_report_net_expense_ratio                  -0.02
## total_net_assets                                       0.04
## fund_age_years                                         0.06
## asset_stocks                                           0.14
## asset_bonds                                           -0.14
## annual_holdings_turnover                              -0.01
## fund_price_earning_ratio                               0.48
## fund_sector_technology                                 1.00
```

## Keep in mind

This is the starting model. Use the diagnostics to decide what needs work
later. Ordinary confidence intervals and hypothesis tests rely on model
assumptions, including independent errors and constant variance.

The source files contain reporting dates from 2021, so predictor values are
not confirmed to describe funds before 2018. Missing data and multiple share
classes of the same fund can also affect conclusions. These limitations are
not resolved by fitting the regression.
