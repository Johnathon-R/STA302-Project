# Reproducible calculations for fund_model_reasoning.pdf.
# Run from the project root: Rscript workflows/fit_fund_models.R
suppressPackageStartupMessages(library(jsonlite))

figure_dir <- "/private/tmp/fund_model_figures"
dir.create(figure_dir, recursive=TRUE, showWarnings=FALSE)
out_json <- "/private/tmp/fund_model_results.json"

etf <- read.csv("data/ETFs.csv", na.strings=c("", "NA"))
mf <- read.csv("data/MutualFunds.csv", na.strings=c("", "NA"))
sector <- setdiff(grep("^fund_sector_", names(etf), value=TRUE),
                  "fund_sector_utilities")
valuation <- c("fund_price_book_ratio", "fund_price_cashflow_ratio",
               "fund_price_earning_ratio", "fund_price_sales_ratio")
base_vars <- c("fund_return_ytd", "fee_pct", "log_assets", "age_years",
               "log_turnover", "asset_stocks", sector, valuation,
               "top10_holdings_total_assets", "investment_type", "size_type")

prepare <- function(raw, type, endpoint) {
  date_ok <- !is.na(raw$returns_as_of_date) & raw$returns_as_of_date == endpoint
  d <- raw[date_ok,,drop=FALSE]
  d$inception_date <- as.Date(d$inception_date)
  d <- d[!is.na(d$inception_date) & d$inception_date < as.Date("2021-01-01"),,drop=FALSE]
  d$fee_pct <- 100*d$fund_annual_report_net_expense_ratio
  d$log_assets <- ifelse(is.finite(d$total_net_assets) & d$total_net_assets > 0,
                         log(d$total_net_assets), NA_real_)
  d$age_years <- as.numeric(as.Date("2021-01-01")-d$inception_date)/365.25
  d$log_turnover <- ifelse(is.finite(d$annual_holdings_turnover) &
                           d$annual_holdings_turnover >= 0,
                           log1p(d$annual_holdings_turnover), NA_real_)
  bounded <- c("asset_stocks", sector, "top10_holdings_total_assets")
  invalid <- sapply(bounded, function(v)
    sum(!is.na(d[[v]]) & (d[[v]] < 0 | d[[v]] > 1)))
  for (v in bounded) d[[v]][!is.na(d[[v]]) &
                            (d[[v]] < 0 | d[[v]] > 1)] <- NA_real_
  d$investment_type[d$investment_type == ""] <- NA
  d$size_type[d$size_type == ""] <- NA
  d$investment_type <- factor(d$investment_type)
  d$size_type <- factor(d$size_type)
  complete <- complete.cases(d[,base_vars,drop=FALSE])
  for (v in base_vars) if (is.numeric(d[[v]])) complete <- complete & is.finite(d[[v]])
  used <- droplevels(d[complete,,drop=FALSE])
  list(type=type,endpoint=endpoint, raw=nrow(raw), date_rows=sum(date_ok),
       eligible=nrow(d), used=nrow(used), invalid=invalid,
       before=d, data=used)
}

samples <- list(
  ETF=prepare(etf,"ETF","2021-06-30"),
  Mutual_fund=prepare(mf,"Mutual fund","2021-07-30")
)

linear_terms <- c("fee_pct", "log_assets", "age_years", "log_turnover",
                  "asset_stocks", sector, valuation,
                  "top10_holdings_total_assets", "investment_type", "size_type")
quadratic_terms <- c("I(log_assets^2)", "I(age_years^2)",
                     "I(log_turnover^2)", "I(asset_stocks^2)")
linear_formula <- reformulate(linear_terms, response="fund_return_ytd")
quadratic_formula <- reformulate(c(linear_terms,quadratic_terms),
                                response="fund_return_ytd")

fit_checked <- function(formula, data) {
  fit <- lm(formula, data=data, na.action=na.fail)
  if (fit$rank != ncol(model.matrix(fit))) stop("Rank-deficient fit")
  fit
}
hc3 <- function(fit) {
  X <- model.matrix(fit)
  h <- hatvalues(fit)
  if (any(h >= 1-1e-8)) stop("HC3 leverage too close to one")
  bread <- chol2inv(chol(crossprod(X)))
  meat <- crossprod(X, X*as.vector((residuals(fit)/(1-h))^2))
  bread %*% meat %*% bread
}
wald_f <- function(fit, terms) {
  labs <- attr(terms(fit),"term.labels")
  assign <- attr(model.matrix(fit),"assign")
  ids <- match(terms,labs)
  if (anyNA(ids)) stop("Term not found: ",paste(terms,collapse=","))
  cols <- which(assign %in% ids)
  b <- coef(fit)[cols]
  V <- hc3(fit)[cols,cols,drop=FALSE]
  q <- length(cols)
  stat <- as.numeric(crossprod(b,solve(V,b))/q)
  c(F=stat,df1=q,df2=df.residual(fit),p=pf(stat,q,df.residual(fit),lower.tail=FALSE))
}

groups <- list(
  Fee="fee_pct",
  Assets=c("log_assets","I(log_assets^2)"),
  Age=c("age_years","I(age_years^2)"),
  Turnover=c("log_turnover","I(log_turnover^2)"),
  Stock_weight=c("asset_stocks","I(asset_stocks^2)"),
  Sectors=sector,
  Valuation=valuation,
  Concentration="top10_holdings_total_assets",
  Investment_style="investment_type",
  Company_size_style="size_type"
)

block_tests <- function(fit,data,group_terms) {
  labels <- attr(terms(fit),"term.labels")
  out <- lapply(names(group_terms),function(name) {
    term <- group_terms[[name]]
    reduced <- fit_checked(reformulate(setdiff(labels,term),response="fund_return_ytd"),data)
    ordinary <- anova(reduced,fit)
    robust <- wald_f(fit,term)
    data.frame(group=name,df=robust["df1"],classical_F=ordinary$F[2],
               classical_p=ordinary$`Pr(>F)`[2],HC3_F=robust["F"],
               HC3_p=robust["p"],row.names=NULL)
  })
  ans <- do.call(rbind,out)
  ans$Holm_HC3_p <- p.adjust(ans$HC3_p,"holm")
  ans
}

results <- list()
for (name in names(samples)) {
  d <- samples[[name]]$data
  linear <- fit_checked(linear_formula,d)
  quadratic <- fit_checked(quadratic_formula,d)
  comparison <- anova(linear,quadratic)
  nonlinear <- wald_f(quadratic,quadratic_terms)
  # One decision rule fixed before seeing either cohort: adopt the four
  # extra curvature terms only if the HC3 joint test is below 0.05.
  use_quadratic <- nonlinear["p"] < .05
  final <- if (use_quadratic) quadratic else linear
  chosen_groups <- if (use_quadratic) groups else
    lapply(groups,function(x) setdiff(x,quadratic_terms))
  tests <- block_tests(final,d,chosen_groups)
  V <- hc3(final)
  fee <- coef(final)["fee_pct"]
  fee_se <- sqrt(V[which(names(coef(final))=="fee_pct"),
                   which(names(coef(final))=="fee_pct")])
  fee_ci <- fee+c(-1,1)*qt(.975,df.residual(final))*fee_se
  cooks <- cooks.distance(final)
  keep <- cooks <= 4/nobs(final)
  sensitivity <- fit_checked(formula(final),d[keep,,drop=FALSE])
  # Descriptive variance trend diagnostic, not a proof of homoskedasticity.
  variance_aux <- lm(I(residuals(final)^2) ~ fitted(final))
  variance_p <- coef(summary(variance_aux))[2,4]
  scaled_design <- scale(model.matrix(final)[,-1,drop=FALSE])
  condition_number <- kappa(scaled_design,exact=TRUE)
  results[[name]] <- list(
    sample=samples[[name]][c("type","endpoint","raw","date_rows","eligible","used")],
    invalid=as.list(samples[[name]]$invalid),
    linear_adj_r2=summary(linear)$adj.r.squared,
    quadratic_adj_r2=summary(quadratic)$adj.r.squared,
    nonlinear_classical_F=comparison$F[2],
    nonlinear_classical_p=comparison$`Pr(>F)`[2],
    nonlinear_HC3_F=unname(nonlinear["F"]),
    nonlinear_HC3_p=unname(nonlinear["p"]),
    final_form=if (use_quadratic) "quadratic" else "linear",
    n=nobs(final),parameters=final$rank,
    adj_r2=summary(final)$adj.r.squared,rmse=summary(final)$sigma,
    fee_estimate=unname(fee),fee_se=unname(fee_se),
    fee_ci=unname(fee_ci),
    variance_trend_p=unname(variance_p),
    scaled_design_condition=unname(condition_number),
    max_leverage=max(hatvalues(final)),
    cooks_flagged=sum(!keep),
    sensitivity_n=nobs(sensitivity),
    sensitivity_fee=unname(coef(sensitivity)["fee_pct"]),
    tests=tests,
    formula=paste(deparse(formula(final)),collapse=" ")
  )
  results[[name]]$fit <- final
}
nonlinear_adjusted <- p.adjust(sapply(results,function(x)x$nonlinear_HC3_p),
                               method="holm")
for(name in names(results))
  results[[name]]$nonlinear_Holm_p <- unname(nonlinear_adjusted[name])

png(file.path(figure_dir,"01_return_dates.png"),width=1600,height=700,res=150)
op <- par(mfrow=c(1,2),mar=c(8,4,3,1))
for (i in 1:2) {
  raw <- if (i==1) etf else mf
  tab <- sort(table(raw$returns_as_of_date),decreasing=TRUE)
  tab <- head(tab,8)
  barplot(tab,las=2,cex.names=.65,col="#4D7198",main=if(i==1) "ETFs" else "Mutual funds",
          ylab="Funds",border=NA)
}
par(op);dev.off()

png(file.path(figure_dir,"02_sample_attrition.png"),width=1600,height=700,res=150)
mat <- sapply(samples,function(s)c(raw=s$raw,date=s$date_rows,
                                   eligible=s$eligible,model=s$used))
retained <- sweep(mat,2,mat["raw",],"/")*100
barplot(retained,beside=TRUE,col=c("#CAD7E4","#9EB8D2","#6E94B7","#325F88"),
        legend.text=rownames(mat),args.legend=list(x="topright",bty="n",cex=.8),
        ylab="Percent of raw rows",main="Sample retention at each step")
dev.off()

png(file.path(figure_dir,"03_missingness.png"),width=1600,height=800,res=150)
op<-par(mfrow=c(1,2),mar=c(8,5,3,1))
for(name in names(samples)) {
  d<-samples[[name]]$before
  miss<-sapply(base_vars,function(v) mean(is.na(d[[v]])))
  miss<-sort(miss,decreasing=TRUE)
  miss<-head(miss,6)
  short<-c(asset_stocks="Stock weight",log_turnover="Turnover",
           fund_price_earning_ratio="P/E",fund_price_cashflow_ratio="P/CF",
           fund_price_book_ratio="P/B")
  labels<-ifelse(names(miss)%in%names(short),short[names(miss)],
                 sub("^fund_sector_","Sector: ",names(miss)))
  barplot(100*miss,names.arg=labels,las=2,cex.names=.78,col="#9EB8D2",
          ylab="Missing percent",main=paste(name,"largest missing fields"),border=NA)
}
par(op);dev.off()

png(file.path(figure_dir,"04_fee_scatter.png"),width=1600,height=750,res=150)
op<-par(mfrow=c(1,2),mar=c(5,5,3,1))
for(name in names(samples)) {
  d<-samples[[name]]$data
  plot(d$fee_pct,d$fund_return_ytd,pch=16,cex=.35,
       col=adjustcolor("#284F78",alpha.f=.18),
       xlab="Expense ratio (percentage points)",ylab="Recorded YTD return",
       main=paste(name,"same-date complete cases"))
  lines(lowess(d$fee_pct,d$fund_return_ytd),col="#C23B36",lwd=2)
}
par(op);dev.off()

for(name in names(samples)) {
  fit<-results[[name]]$fit
  png(file.path(figure_dir,paste0("05_diagnostics_",name,".png")),
      width=1600,height=1250,res=150)
  op<-par(mfrow=c(2,2),mar=c(5,5,3,1))
  plot(fit,which=c(1,2,3,5),pch=20,cex=.4)
  par(op);dev.off()
}

png(file.path(figure_dir,"06_fee_sensitivity.png"),width=1300,height=700,res=150)
op<-par(mar=c(5,8,3,2))
plot(NA,xlim=c(-.08,.012),ylim=c(.55,2.55),yaxt="n",ylab="",
     xlab="Return change per 1 percentage point higher fee",
     main="Fee estimates and influence sensitivity")
axis(2,at=c(2,1),labels=c("ETF","Mutual fund"),las=1)
abline(v=0,lty=2,col="gray50")
for (i in seq_along(results)) {
  x<-results[[i]]; y<-3-i
  segments(x$fee_ci[1],y,x$fee_ci[2],y,col="#325F88",lwd=3)
  points(x$fee_estimate,y,pch=19,col="#325F88",cex=1.4)
  points(x$sensitivity_fee,y-.17,pch=1,col="#B14E3F",cex=1.5,lwd=2)
}
legend("topright",legend=c("Main estimate with HC3 interval","After Cook's screen"),
       pch=c(19,1),col=c("#325F88","#B14E3F"),bty="n",cex=.75)
par(op)
dev.off()

png(file.path(figure_dir,"07_model_comparison.png"),width=1300,height=650,res=150)
gain<-sapply(results,function(x)x$quadratic_adj_r2-x$linear_adj_r2)
barplot(gain,col="#6E94B7",ylim=c(0,max(gain)*1.3),
        ylab="Adjusted R-squared gain",main="Four quadratic terms add little fit")
dev.off()

png(file.path(figure_dir,"08_block_tests.png"),width=1450,height=900,res=150)
block_names<-results[[1]]$tests$group
p_matrix<-sapply(results,function(x)x$tests$Holm_HC3_p)
score<- -log10(pmax(p_matrix,1e-12))
score[score>12]<-12
op<-par(mar=c(8,10,3,5))
image(1:ncol(score),1:nrow(score),t(score[nrow(score):1,,drop=FALSE]),
      col=colorRampPalette(c("#EDF2F7","#8EB0D0","#2D5F8B"))(100),
      axes=FALSE,xlab="",ylab="",main="HC3 block tests after Holm correction")
axis(1,at=1:ncol(score),labels=c("ETF","Mutual fund"),tick=FALSE)
axis(2,at=1:nrow(score),labels=rev(gsub("_"," ",block_names)),
     las=1,tick=FALSE,cex.axis=.85)
for(i in seq_len(nrow(score))) for(j in seq_len(ncol(score)))
  text(j,nrow(score)+1-i,ifelse(p_matrix[i,j]<.05,"p < .05","n.s."),
       col=if(score[i,j]>5)"white" else "black",cex=.75)
par(op);dev.off()

for(name in names(results)) results[[name]]$fit<-NULL
write_json(results,out_json,auto_unbox=TRUE,pretty=TRUE,digits=12)
cat(out_json,"\n")
for(name in names(results)) {
  x<-results[[name]]
  cat(name,"n",x$n,"form",x$final_form,"adjR2",x$adj_r2,
      "fee",x$fee_estimate,"nonlinear p",x$nonlinear_HC3_p,"\n")
}
