# MSc in Banking and Quantitative Finance - Portfolio

Centralized repository containing academic and practical projects developed during the **MSc in Banking and Quantitative Finance (QFB)**.

## 🛠 Tech Stack
* **Languages:** Python (NumPy, Pandas, SciPy, Statsmodels), R, MATLAB.
* **Tools:** Gurobi Optimizer, Monte Carlo Simulation, Stochastic Calculus, Time Series Analysis, LaTeX/Markdown.

## 📂 Project Structure

### 1. Econometrics & Time Series Analysis
* **[Factor Investing (Fama-French & Carhart)](./Econometrics/Factor%20Investing%20Fama%20Carhart)**: 
  * Implementation in **R** of Fama-French 3-Factor and Carhart 4-Factor models.
  * Analysis of risk premiums and market anomalies using Fama-MacBeth regressions on 25 European portfolios.
* **[Time Series & Simulation](./Econometrics/Time%20Series%20and%20Simulation)**: 
  * Characterization of the **Bera-Jarque** statistic properties through Monte Carlo simulation.
  * Price and return forecasting using **ARIMA** models.
* **[Volatility Modeling (GARCH)](./Econometrics/Volatility%20Modeling%20and%20GARCH)**: 
  * Modeling conditional volatility using **GARCH, EGARCH, and TGARCH** specifications.
  * Multivariate analysis and connectivity measuring using **DCC-GARCH** and Diebold-Yilmaz methodology.

### 2. Numerical Methods & Derivatives Pricing
* **[Exotic Options Pricing](./Numerical%20Methods/Exotic%20Options%20Pricing)**: 
  * Monte Carlo pricing engine for path-dependent options (Asian, Barrier) with variance reduction techniques (Antithetic Variables, Control Variates).
* **[Calibration of Models](./Numerical%20Methods/Calibration%20of%20Models)**: 
  * Calibration of local and stochastic volatility parameters to market data.
* **[Pricing of European Options](./Numerical%20Methods/Pricing%20of%20European%20Options)**: 
  * Implementation of Black-Scholes analytical formulas and numerical approximations for standard European options.

### 3. Risk Management
* **[Factor Risk Models (PCA & BARRA)](./Risk%20Management/Factor%20Risk%20Models)**: 
  * Construction of risk models using **Principal Component Analysis (PCA)** to decompose systematic and idiosyncratic risk.
  * Comparison with fundamental factor models (BARRA type) for portfolio risk attribution.
* **[Systemic Risk (CoVaR)](./Risk%20Management/Systemic%20Risk%20CoVaR%20Analysis)**: 
  * Measurement of systemic risk contribution using **CoVaR** (Conditional Value at Risk) and $\Delta$CoVaR methodology (Adrian & Brunnermeier).
  * Implementation using **Quantile Regression** and dynamic CaViaR models.

### 4. Financial Optimization
* **[Cash Flow Optimization](./Cash%20Flow%20Optimization)**: 
  * Linear Programming (LP) model using **Python (Gurobi)** to optimize corporate cash flows and Asset Liability Management (ALM).

---
*Note: This repository is continuously updated as the academic year progresses.*
