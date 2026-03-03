# Volatility Modeling and GARCH Processes

## Financial Objective
Model and forecast the time-varying volatility (conditional variance) of financial asset returns. The primary goal is to capture market anomalies such as volatility clustering and asymmetric leverage effects, which are critical for robust risk management, Value at Risk (VaR) calculation, and accurate derivative pricing.

## Mathematical Methodology
The project applies advanced econometric techniques to model financial heteroskedasticity:
* **Heteroskedasticity Detection:** Implementation of Engle's ARCH LM test to confirm the presence of autoregressive conditional heteroskedasticity in the residuals of the return series.
* **Symmetric Volatility Modeling:** Estimation of Generalized Autoregressive Conditional Heteroskedasticity (GARCH(p,q)) models to capture volatility clustering (the tendency for large variations to be followed by large variations).
* **Asymmetric Volatility Modeling:** Estimation of Exponential GARCH (EGARCH) and Threshold GARCH (TGARCH/GJR-GARCH) models to quantify the "leverage effect" (where negative market shocks increase volatility more significantly than positive shocks of the same magnitude).
* **Estimation and Diagnostics:** Parameters are estimated using Maximum Likelihood Estimation (MLE). Model selection and goodness-of-fit are evaluated using Information Criteria (AIC, BIC) and standardized residual diagnostics.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `arch`, `statsmodels`, `pandas`, `numpy`, `scipy`, `matplotlib`

## Key Results
* Successfully detected and modeled the volatility dynamics of historical financial time series, proving the inadequacy of constant-variance assumptions in financial markets.
* Quantified asymmetric responses to market shocks using EGARCH and TGARCH models, providing a more accurate representation of downside risk.
* Generated conditional variance forecasts to serve as a quantitative foundation for dynamic risk assessment and portfolio optimization.

## Repository Structure
* `HW_02_Econometría.ipynb`: Core Jupyter notebook containing the data processing, statistical testing, and estimation of the GARCH family models.
* `HW_02_Econometría.html`: HTML export of the notebook for easy visualization of code, mathematical outputs, and volatility plots.
* `HW02_Econometría_functions.py`: Auxiliary Python module containing custom functions for automating statistical tests and volatility model evaluations.
* `Statement.pdf`: Technical assignment document detailing the theoretical framework and mathematical requirements.
* `Datos_historicos.csv` & `datos_ej2p2.csv`: Historical financial datasets used for empirical volatility modeling.