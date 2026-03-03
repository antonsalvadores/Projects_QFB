# Time Series Analysis and Macroeconomic Forecasting

## Financial Objective
Analyze historical financial time series (gold prices, stock indices such as the IBEX 35, and macroeconomic interest rates) to identify their underlying stochastic processes. The goal is to model historical behavior, project future returns, and detect long-term equilibrium relationships (cointegration) across different financial markets.

## Mathematical Methodology
The analysis is based on advanced time series econometrics:
* **Univariate Analysis:** Stationarity tests (Augmented Dickey-Fuller), structural identification via ACF/PACF, and estimation of Autoregressive Integrated Moving Average (ARIMA) models.
* **Volatility Modeling:** Detection of heteroskedastic effects and conditional variance modeling in residuals.
* **Multivariate Analysis:** Granger causality tests, estimation of Vector Autoregression (VAR) and Vector Error Correction Models (VECM), supported by cointegration tests (Engle-Granger).
* **Model Diagnostics:** Hyperparameter optimization using Information Criteria (AIC, BIC) and statistical validation of residuals (Ljung-Box test for autocorrelation, Jarque-Bera test for normality).

## Tech Stack
* **Language:** Python
* **Core Libraries:** `statsmodels`, `pandas`, `numpy`, `scipy`, `matplotlib`

## Key Results
* Identified and statistically fitted the optimal ARIMA model to forecast logarithmic returns of the analyzed assets, ensuring non-correlated residuals.
* Empirically demonstrated cointegration relationships between equity indices and macroeconomic variables, confirming the existence of long-term mean-reversion dynamics.
* Validated predictive directionality (Granger Causality) between interest rates and exchange rates in historical scenarios.

## Repository Structure
* `HW01_Econometría.ipynb`: Main notebook containing the data import pipeline, cleaning, statistical testing, and mathematical modeling.
* `HW01_Econometría_functions.py`: Auxiliary Python module with custom functions to automate hypothesis testing and metric calculations.
* `Statement.pdf`: Technical document outlining the mathematical requirements and theoretical framework of the solved problems.
* `.csv` / `.xlsx` files: Financial datasets used in the analysis (Gold prices, IBEX 35, and macroeconomic indicators).