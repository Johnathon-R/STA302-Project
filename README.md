# STA302 Project

## Objective

We are studying ETF performance over a fixed period. Our aim is to use multiple linear regression to examine which fund characteristics are associated with performance.

- **Research question:** [Add the final research question.]
- **Performance measure and time period:** [Specify how performance will be measured and the period studied.]
- **Predictors:** [List the fund characteristics we will include.]

## Data

We are using [US Funds dataset from Yahoo Finance](https://www.kaggle.com/datasets/stefanoleone992/mutual-funds-and-etfs), published on Kaggle by Stefano Leone. The data comes from Yahoo Finance and includes ETF and mutual fund information. Our project focuses on ETFs.

**Files used:** [List the files used in the final analysis, such as `ETFs.csv` and, if needed, `ETF prices.csv`.]

### Downloading the data

Open R with the project folder as your working directory, then run:

```r
source("download_data.R")
etfs <- read.csv("data/ETFs.csv")
```

The script uses base R, so no extra packages or Python setup are needed. It downloads only `ETFs.csv` from version 4 of the dataset, unzips it, and saves it in `data/`. If a nonempty local copy already exists, it leaves it unchanged. The data files are excluded from GitHub.

If the download is unavailable, download `ETFs.csv` from the Kaggle link above and place it in `data/` yourself.

## Analysis

The analysis will be done in R.

[Add a short description of the data cleaning, model, and diagnostic checks once these are decided.]

## Results

[Add the main findings and limitations once the analysis is complete.]

## Group members

[Add group members' names.]
