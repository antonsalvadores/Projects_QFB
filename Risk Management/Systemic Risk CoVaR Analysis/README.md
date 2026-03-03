# Risk Management: Systemic Risk and CoVaR Analysis

## Financial & Business Objective
Quantify and analyze the systemic risk contribution of individual institutions to the broader European financial market (Euro Stoxx 600) and a custom portfolio. The objective is to identify which sectors—Banking (BBVA), Insurance (AXA), Utilities (Iberdrola), or Technology (Infineon)—generate the highest contagion effect during periods of financial distress (e.g., the COVID-19 crash). This analysis is critical for macroprudential stress testing, systemic risk buffering, and robust portfolio risk allocation.

## Mathematical Methodology
This project implements the ΔCoVaR methodology (Adrian & Brunnermeier) alongside dynamic risk modelling techniques:
* **Dynamic Value at Risk (VaR):** Estimation of time-varying unconditional VaR at the 1% tail level using Conditional Autoregressive Value at Risk (CaViaR) models to capture volatility clustering and heteroskedasticity.
* **Quantile Regression:** Application of quantile regression ($\tau = 0.01$) to estimate the conditional distribution of the market index (and portfolio) given that a specific institution is strictly at its VaR.
* **Systemic Risk Contribution (ΔCoVaR):** Calculation of the marginal systemic risk contribution of each firm by isolating the difference between the CoVaR conditional on the firm being in distress (at its VaR) versus its median state. 
* **Descriptive Econometrics:** Stationarity testing, autocorrelation evaluation, and distribution tail analysis (Q-Q plots, Boxplots) of historical returns.

## Tech Stack
* **Language:** R
* **Core Libraries:** `quantreg` (Quantile Regression), `rugarch`, `PerformanceAnalytics`, `xts`, `fBasics`

## Key Results
* Successfully calculated and plotted the dynamic CoVaR and ΔCoVaR for four major European institutions, isolating their contagion effect on the Euro Stoxx 600.
* Demonstrated empirically how systemic risk contributions vary drastically across sectors and market regimes (Pre-COVID vs. Post-COVID).
* Built a robust econometric pipeline to extract risk metrics that go beyond traditional modern portfolio theory, addressing tail dependencies and market spillovers.

## Repository Structure
* `Práctica_2_Antón.R`: Core R script executing the data ingestion, econometric testing, CaViaR modelling, and quantile regressions required to compute dynamic ΔCoVaR.
* `Data_P2.xlsx`: Historical pricing dataset containing daily close prices for the selected equities and the Euro Stoxx 600 index from 2015 to 2025.
* `Final Assignment.pdf`: Comprehensive PDF report detailing the descriptive analysis, mathematical formulations, and interpretation of the systemic risk metrics.
* `Statement.docx`: Academic requirement document outlining the parameters and theoretical framework of the systemic risk assignment.
* `/plots`: Directory containing all generated graphical outputs (~30 `.png` files), including dynamic VaR overlaps, conditional CoVaR series, and econometric diagnostic plots utilized in the final report.