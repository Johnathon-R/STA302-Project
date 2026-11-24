import json
from pathlib import Path
from xml.sax.saxutils import escape

from PIL import Image as PILImage
from reportlab.lib import colors
from reportlab.lib.enums import TA_CENTER, TA_LEFT
from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Image, Table, TableStyle,
    PageBreak, KeepTogether,
)

ROOT = Path(__file__).resolve().parents[1]
FIG = Path("/private/tmp/fund_model_figures")
DATA = json.loads(Path("/private/tmp/fund_model_results.json").read_text())
OUT = ROOT / "workflows/fund_model_reasoning.pdf"
ETF = DATA["ETF"]
MF = DATA["Mutual_fund"]

PAGE_W, PAGE_H = letter
styles = getSampleStyleSheet()
styles.add(ParagraphStyle(name="ReportTitle", fontName="Helvetica-Bold", fontSize=19,
                          leading=23, textColor=colors.HexColor("#1F2E3B"), spaceAfter=10))
styles.add(ParagraphStyle(name="ReportSub", fontName="Helvetica", fontSize=9,
                          leading=13, textColor=colors.HexColor("#586574"), spaceAfter=14))
styles.add(ParagraphStyle(name="Stage", fontName="Helvetica-Bold", fontSize=14,
                          leading=18, textColor=colors.HexColor("#1F2E3B"),
                          spaceBefore=0, spaceAfter=9, keepWithNext=True))
styles.add(ParagraphStyle(name="Substage", fontName="Helvetica-Bold", fontSize=10.5,
                          leading=14, textColor=colors.HexColor("#263B4D"),
                          spaceBefore=10, spaceAfter=5, keepWithNext=True))
styles.add(ParagraphStyle(name="ReportBody", fontName="Helvetica", fontSize=9.2,
                          leading=13.5, textColor=colors.HexColor("#20262B"),
                          spaceAfter=8))
styles.add(ParagraphStyle(name="ReportSmall", fontName="Helvetica", fontSize=8.2,
                          leading=11.2, textColor=colors.HexColor("#38424C"),
                          spaceAfter=6))
styles.add(ParagraphStyle(name="Caption", fontName="Helvetica-Oblique", fontSize=8,
                          leading=11, textColor=colors.HexColor("#59636E"),
                          spaceBefore=3, spaceAfter=10))
styles.add(ParagraphStyle(name="Cell", fontName="Helvetica", fontSize=7.8,
                          leading=10.3, textColor=colors.HexColor("#20262B")))
styles.add(ParagraphStyle(name="CellHead", fontName="Helvetica-Bold", fontSize=7.8,
                          leading=10.3, textColor=colors.white))

story = []

def p(text, style="ReportBody"):
    story.append(Paragraph(text, styles[style]))

def heading(text):
    p(text,"Stage")

def subhead(text):
    p(text,"Substage")

def fig(filename, caption, width=514, max_height=410):
    path = FIG / filename
    with PILImage.open(path) as im:
        w,h = im.size
    height = min(width*h/w,max_height)
    use_width = height*w/h
    story.append(KeepTogether([
        Image(str(path), width=use_width, height=height),
        Paragraph(caption,styles["Caption"]),
    ]))

def fmt_p(x):
    if x is None:
        return "NA"
    if x == 0 or x < 1e-12:
        return "&lt;1e-12"
    if x < .001:
        return f"{x:.2e}"
    return f"{x:.3f}"

def table(rows,widths,header=True,small=False):
    cells = []
    for i,row in enumerate(rows):
        st=styles["CellHead"] if i==0 and header else styles["Cell"]
        cells.append([Paragraph(escape(str(v)),st) for v in row])
    t=Table(cells,colWidths=widths,repeatRows=1 if header else 0,hAlign="LEFT")
    commands=[
        ("VALIGN",(0,0),(-1,-1),"MIDDLE"),
        ("LEFTPADDING",(0,0),(-1,-1),6),
        ("RIGHTPADDING",(0,0),(-1,-1),6),
        ("TOPPADDING",(0,0),(-1,-1),5 if not small else 3),
        ("BOTTOMPADDING",(0,0),(-1,-1),5 if not small else 3),
        ("GRID",(0,0),(-1,-1),.35,colors.HexColor("#D5DDE5")),
    ]
    if header:
        commands.append(("BACKGROUND",(0,0),(-1,0),colors.HexColor("#365F82")))
    for i in range(1,len(rows)):
        if i%2==0: commands.append(("BACKGROUND",(0,i),(-1,i),colors.HexColor("#F4F7FA")))
    t.setStyle(TableStyle(commands))
    story.append(t)
    story.append(Spacer(1,8))

def page():
    story.append(PageBreak())

def footer(canvas, doc):
    canvas.saveState()
    canvas.setStrokeColor(colors.HexColor("#D5DDE5"))
    canvas.line(47,39,PAGE_W-47,39)
    canvas.setFont("Helvetica",8)
    canvas.setFillColor(colors.HexColor("#617080"))
    canvas.drawString(47,26,"STA302  |  Fund model reasoning  |  17 Nov 2026")
    canvas.drawRightString(PAGE_W-47,26,str(doc.page))
    canvas.restoreState()

# Page 1
p("Fund Regression Model and Decision Record","ReportTitle")
p("A fitted analysis of the recorded 2021 year-to-date fund returns. Each step explains what was tested, what happened, and the decision that followed.","ReportSub")
heading("Stage 1  Define a comparable response")
p("<b>Question.</b> Which recorded fund characteristics are associated with year-to-date return after adjustment for other measured characteristics? The project CSVs do not provide one usable common return endpoint for ETFs and mutual funds. Treating their different windows as one response would mix market-period effects with fund type.")
fig("01_return_dates.png","Figure 1. The dominant ETF endpoint is 30 June 2021; the dominant mutual-fund endpoint is 30 July 2021. The 1970 ETF value is a source sentinel, not a usable 2021 endpoint.",width=510,max_height=235)
p("<b>Decision.</b> Fit two separate cross-sectional models: ETFs at 30 June and mutual funds at 30 July. Each model asks about within-type association. Neither model estimates an ETF-minus-mutual-fund effect or their fee-slope difference. The date field is assumed to describe the YTD return endpoint; its source definition still needs confirmation.")
p("<b>Data source.</b> Local <i>data/ETFs.csv</i> and <i>data/MutualFunds.csv</i>, the files used in the earlier project Rmds. No raw data file was edited.","ReportSmall")
page()

# Page 2
heading("Stage 2  Build a traceable sample")
p("I combined the same two CSVs and used the pre-2021 inception rule from the project post-cleaning Rmd. I retained funds whose recorded return endpoint equals the chosen date, then required the response and every model term to be observed. I did not fill missing values with zero, annualize a shorter return, or delete an unusual return merely because it is extreme.")
table([
    ["Cohort","Raw","Date matched","Inception eligible","Model complete"],
    ["ETF",f"{ETF['sample']['raw']:,}",f"{ETF['sample']['date_rows']:,}",f"{ETF['sample']['eligible']:,}",f"{ETF['sample']['used']:,}"],
    ["Mutual fund",f"{MF['sample']['raw']:,}",f"{MF['sample']['date_rows']:,}",f"{MF['sample']['eligible']:,}",f"{MF['sample']['used']:,}"],
], [92,70,105,120,105])
fig("02_sample_attrition.png","Figure 2. Retention is measured within each raw source, so the two cohorts can be compared despite their different starting sizes.",width=510,max_height=230)
p(f"<b>What happened.</b> The final model contains {ETF['n']:,} ETFs ({ETF['n']/ETF['sample']['eligible']:.1%} of eligible ETFs) and {MF['n']:,} mutual funds ({MF['n']/MF['sample']['eligible']:.1%} of eligible mutual funds). This is substantial, unequal attrition. Results apply to complete cases and may not generalize to all funds.")
p(f"<b>Range check.</b> Stock, sector, and top-ten concentration fields were treated as proportions. Recorded top-ten values above 1 were set to missing: {ETF['invalid']['top10_holdings_total_assets']} ETFs and {MF['invalid']['top10_holdings_total_assets']} mutual funds before complete-case selection. This assumes that the field is meant to be on a 0-to-1 scale; the original units need independent verification.","ReportSmall")
page()

# Page 3
heading("Stage 3  Choose predictors for an adjusted model")
p("The earlier all-column audit tested fields one at a time. That is not an adjusted model. Here I kept variables with a plausible role in a single-period return comparison and enough common coverage to estimate a model in both cohorts. The same predictor groups are used for both cohorts, so changes in p-values do not decide which controls to retain.")
fig("03_missingness.png","Figure 3. The largest missing fields among date- and inception-eligible funds. Several missing sector values overlap, so the bars cannot be added.",width=500,max_height=245)
subhead("Included variables")
p("Net expense ratio; log total assets; age at 1 January 2021; log(1 + turnover); stock weight; ten sector weights; four valuation ratios (price/book, price/cash flow, P/E, price/sales); top-ten concentration; investment style; and company-size style. Logs compress very large assets and turnover while retaining zero turnover. Utilities is the omitted sector reference, and Blend and Large are the style reference levels. Stock and bond weights were highly redundant in an initial fit, so only stock weight is in this common model.","ReportSmall")
subhead("Excluded variables")
p("Other returns, return rankings, moving averages, alpha, beta, Sharpe ratios, and category returns reuse the outcome period or represent outcomes rather than prior characteristics. Names, tickers, manager biographies, and holding text are identifiers or unreliable free text. Bond duration and credit grades apply to a different bond-focused population. Yield is excluded because its measurement window is unclear and it could overlap the response.","ReportSmall")
p("<b>Limit.</b> The CSVs do not date the included characteristics. This is an association analysis, not a valid forecast using known pre-period inputs.","ReportSmall")
page()

# Page 4
heading("Stage 4  State and inspect the candidate model")
p("The common linear model is below. It is a multiple linear regression: all listed terms are fitted together. Each F-test later asks whether one predictor group contributes after the others, on the same complete-case rows.")
table([
    ["Part of model","Exact terms"],
    ["Response","fund_return_ytd"],
    ["Core numeric","fee_pct + log_assets + age_years + log_turnover + asset_stocks"],
    ["Sectors","fund_sector_basic_materials, fund_sector_communication_services, fund_sector_consumer_cyclical, fund_sector_consumer_defensive, fund_sector_energy, fund_sector_financial_services, fund_sector_healthcare, fund_sector_industrials, fund_sector_real_estate, fund_sector_technology"],
    ["Valuation","fund_price_book_ratio + fund_price_cashflow_ratio + fund_price_earning_ratio + fund_price_sales_ratio"],
    ["Other","top10_holdings_total_assets + investment_type + size_type"],
], [100,402],small=True)
p("In formula notation: <b>recorded return = intercept + fee + size + age + turnover + stock weight + ten sector effects + four valuation effects + concentration + style effects + error.</b> The mutual-fund model may add squared size, age, turnover, and stock-weight terms if a pre-specified joint curvature test supports them. The response and expense ratio are stored as decimals; <i>fee_pct</i> multiplies expense ratio by 100, so one unit is one percentage point.","ReportSmall")
fig("04_fee_scatter.png","Figure 4. Raw fee-return relationships among complete cases, before adjustment. Red lines are descriptive smooths; the fitted multiple-regression fee effect can differ.",width=510,max_height=240)
page()

# Page 5
heading("Stage 5  Test whether a curved mean improves fit")
p("I compared the linear model with a nested model adding four squared terms: log assets, age, log turnover, and stock weight. Size and turnover span wide ranges, while stock weight is bounded, so straight-line effects may miss curvature. The candidate variables and observations are identical in the two fits. A partial F-test checks whether all four extra coefficients are zero. Because the variance plots suggest unequal spread, the decision uses an HC3-based approximate F test. Holm adjustment across the two cohorts leaves the decision unchanged.")
table([
    ["Cohort","Linear adjusted R2","Curved adjusted R2","HC3 F (4 df)","Holm p","Chosen"],
    ["ETF",f"{ETF['linear_adj_r2']:.4f}",f"{ETF['quadratic_adj_r2']:.4f}",f"{ETF['nonlinear_HC3_F']:.2f}",fmt_p(ETF['nonlinear_Holm_p']).replace("&lt;","<"),"Linear"],
    ["Mutual fund",f"{MF['linear_adj_r2']:.4f}",f"{MF['quadratic_adj_r2']:.4f}",f"{MF['nonlinear_HC3_F']:.2f}",fmt_p(MF['nonlinear_Holm_p']).replace("&lt;","<"),"Curved"],
], [82,104,108,75,62,71])
fig("07_model_comparison.png","Figure 5. The added terms improve adjusted R-squared by less than 0.002 in either cohort. Statistical evidence for curvature is strongest in the much larger mutual-fund sample.",width=495,max_height=250)
p(f"<b>Decision.</b> Keep the simpler ETF model (Holm p = {ETF['nonlinear_Holm_p']:.3f}). Use the quadratic mutual-fund model (Holm p = {MF['nonlinear_Holm_p']:.6f}), but describe its small practical fit gain. The final models have {ETF['parameters']} and {MF['parameters']} fitted parameters, including intercepts, respectively.")
page()

# Page 6
heading("Stage 6  Test the fitted predictor groups")
p("For each final model, I removed one entire predictor group and compared it with the full fit on exactly the same rows. The classical partial F is computed, but the table uses an HC3 Wald F approximation because residual variance changes. The group test includes both linear and squared terms where the mutual-fund model has curvature. Holm p-values adjust the ten group tests within each cohort. A large p-value is lack of evidence here, not proof of no effect.","ReportSmall")
fig("08_block_tests.png","Figure 6. Darker cells have smaller adjusted p-values. This is a map of conditional association within the fitted sample, not a ranking of future investment predictors.",width=500,max_height=300)
rows=[["Group","ETF HC3 F","ETF Holm p","MF HC3 F","MF Holm p"]]
for i, e in enumerate(ETF["tests"]):
    m=MF["tests"][i]
    rows.append([e["group"].replace("_"," "),f"{e['HC3_F']:.2f}",
                 fmt_p(e["Holm_HC3_p"]).replace("&lt;","<"),
                 f"{m['HC3_F']:.2f}",fmt_p(m["Holm_HC3_p"]).replace("&lt;","<")])
table(rows,[150,81,88,81,88],small=True)
page()

# Page 7
heading("Stage 7  Diagnose the ETF fit")
p(f"The chosen ETF model uses {ETF['n']:,} rows. Adjusted R-squared is {ETF['adj_r2']:.3f}; residual standard deviation is {ETF['rmse']:.3f} in return units. These summarize fit in the selected sample, not predictive accuracy on new funds.")
fig("05_diagnostics_ETF.png","Figure 7. Residuals versus fitted, Q-Q, scale-location, and residuals versus leverage for the ETF model.",width=510,max_height=420)
p(f"<b>What happened.</b> The Q-Q tails depart from the normal line, and the scale-location plot shows changing spread. The maximum leverage is {ETF['max_leverage']:.3f}. The auxiliary variance-trend p-value is {fmt_p(ETF['variance_trend_p'])}; this is a screen, not proof of a variance model. These observations motivated HC3 standard errors and an influence sensitivity check.","ReportSmall")
page()

# Page 8
heading("Stage 8  Diagnose the mutual-fund fit")
p(f"The chosen mutual-fund model uses {MF['n']:,} rows. Adjusted R-squared is {MF['adj_r2']:.3f}; residual standard deviation is {MF['rmse']:.3f}. The extra squared terms improve the mean fit only slightly.")
fig("05_diagnostics_Mutual_fund.png","Figure 8. The same four diagnostics for the mutual-fund model, using the curved mean specification.",width=510,max_height=420)
p(f"<b>What happened.</b> Tail departures and changing spread remain visible. The variance-trend p-value is {fmt_p(MF['variance_trend_p'])}. HC3 addresses unequal variance in the reported uncertainty, but not dependence among related share classes or all possible mean-model errors.","ReportSmall")
page()

# Page 9
heading("Stage 9  Check whether influential rows drive the fee result")
p("Cook's distance greater than 4/n marks rows for review; it does not prove they are wrong. I retained all such rows in the main fits, then refit without them only as a sensitivity check. This compares coefficients, not two formal p-value decisions.")
fig("06_fee_sensitivity.png","Figure 9. Main fee estimates and approximate 95% HC3 intervals. The intervals are unadjusted; the table p-values are Holm-adjusted across ten tests. Open circles are sensitivity refits, not replacements.",width=500,max_height=265)
table([
    ["Cohort","Main fee coefficient","95% HC3 interval","Holm p","Flagged / n","Screened fee"],
    ["ETF",f"{ETF['fee_estimate']:.4f}",
     f"[{ETF['fee_ci'][0]:.4f}, {ETF['fee_ci'][1]:.4f}]",
     fmt_p(ETF['tests'][0]['Holm_HC3_p']).replace("&lt;","<"),
     f"{ETF['cooks_flagged']} / {ETF['n']}",f"{ETF['sensitivity_fee']:.4f}"],
    ["Mutual fund",f"{MF['fee_estimate']:.4f}",
     f"[{MF['fee_ci'][0]:.4f}, {MF['fee_ci'][1]:.4f}]",
     fmt_p(MF['tests'][0]['Holm_HC3_p']).replace("&lt;","<"),
     f"{MF['cooks_flagged']} / {MF['n']}",f"{MF['sensitivity_fee']:.4f}"],
], [85,91,125,58,75,68])
p("<b>Interpretation.</b> A one-percentage-point higher expense ratio corresponds to the coefficient shown in decimal return units, conditional on the other model terms. For example, the mutual-fund estimate of -0.0101 is about -1.01 percentage points of recorded return. The ETF fee block misses the Holm 0.05 threshold, and its estimate changes materially after the influence screen. Neither estimate is a causal fee effect or an ETF-versus-mutual-fund comparison.","ReportSmall")
page()

# Page 10
heading("What this model can and cannot conclude")
p("<b>Observed fit.</b> The separate models explain some within-sample variation in the recorded 2021 returns. Sector exposure, valuation, and investment style have strong conditional group tests in both cohorts. The mutual-fund fee group is below the Holm threshold; the ETF fee group is just above it. The fit, tests, and uncertainty are specific to the selected dates and complete cases.")
p("<b>Unresolved source timing.</b> The CSVs do not record when fees, assets, allocations, valuation ratios, and other predictors were measured. Some may reflect the return period or later information. This blocks honest prospective prediction claims. The 2021 YTD endpoint field also needs source confirmation.")
p("<b>Unresolved identities and dependence.</b> Earlier project checks found incorrect fund names and families. Numeric measurements have not been independently validated. Related share classes may have correlated errors; the data lack trustworthy portfolio identifiers for a defensible cluster correction. HC3 handles unequal variance only and does not solve these problems.")
p("<b>Selection and model shape.</b> Complete-case retention is much lower for ETFs. P/E and sector data tend to be missing for some fund types, so the fitted population is selected. The residual plots show heavy tails, changing variance, and some remaining curvature; the influence screen changes the fee estimates. No defensible analysis can claim this is an issue-free final model from the current files.")
subhead("Next evidence needed for a final comparison")
p("Obtain a verified common-period total-return outcome for both fund types, dated pre-period characteristics, and trustworthy portfolio or share-class identifiers. Rebuild the sample, then rerun the model and its diagnostics. Until then, this PDF is a reproducible exploratory association analysis, not a result about the fee-return difference between ETFs and mutual funds.")
subhead("Reproduction")
p("Run <i>Rscript workflows/fit_fund_models.R</i> from the project root, then run <i>python3 workflows/_build_fund_model_pdf.py</i> with ReportLab and Pillow available. The R script fits and tests both models and writes the figures used here. All reported values are taken from that same run.","ReportSmall")

doc=SimpleDocTemplate(str(OUT),pagesize=letter,rightMargin=49,leftMargin=49,
                      topMargin=45,bottomMargin=53,title="Fund Regression Model and Decision Record",
                      author="STA302 project")
doc.build(story,onFirstPage=footer,onLaterPages=footer)
print(OUT)
