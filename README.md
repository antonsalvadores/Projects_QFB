# MSc in Banking and Quantitative Finance - Portfolio

Centralized repository containing the quantitative, econometric, and software engineering projects developed during the **M.Sc. in Banking and Quantitative Finance**. 

The projects demonstrate the application of advanced mathematics, stochastic calculus, and machine learning to financial modelling, derivative pricing, and risk management.

## 🛠 Tech Stack
* **Languages:** Python (NumPy, pandas, SciPy, scikit-learn, statsmodels), R.
* **Core Competencies:** Stochastic Calculus, Monte Carlo Simulations, Linear Programming (Gurobi), Time Series Econometrics, Natural Language Processing (NLP), Object-Oriented Programming (OOP) for pricing libraries.

---

## 📂 Repository Structure

### 1. Numerical Methods & Derivatives Pricing
* **[Pricing Interest Rate Derivatives](./Numerical%20Methods/Pricing%20IR%20Derivatives)**
  * Developed an industry-standard, object-oriented Python library to price fixed-income instruments and non-linear IR options (Caps/Caplets).
  * Implemented Euribor yield curve bootstrapping and Normal/Shifted Lognormal volatility surface modelling.
* **[Exotic Options Pricing](./Numerical%20Methods/Exotic%20Options%20Pricing)**
  * Built a Monte Carlo pricing engine for path-dependent derivatives (Asian, Barrier options).
  * Implemented stochastic processes (Geometric Brownian Motion) and variance reduction techniques (Antithetic Variables, Control Variates).
* **[Calibration of Models](./Numerical%20Methods/Calibration%20of%20Models)**
  * Calibrated stochastic pricing models to empirical market data (IBEX 35) using numerical optimization routines (Nelder-Mead, SLSQP) to minimize pricing errors.
* **[Pricing of European Options](./Numerical%20Methods/Pricing%20of%20European%20Options)**
  * Implemented Black-Scholes-Merton (BSM) analytical formulas and engineered a modular Python pricing engine for standard European options.
* **[Numerical Approximation and Estimation Errors](./Numerical%20Methods/Numerical%20Approximation%20and%20Estimation%20Errors)**
  * Quantified computational stability and floating-point errors in financial algorithms using Taylor series expansions and custom numerical limiters.

### 2. Risk Management
* **[Systemic Risk CoVaR Analysis](./Risk%20Management/Systemic%20Risk%20CoVaR%20Analysis)**
  * Measured systemic risk contribution of major European institutions using dynamic ΔCoVaR methodology (Adrian & Brunnermeier).
  * Implemented Quantile Regression and Conditional Autoregressive Value at Risk (CaViaR) models in R.
* **[Factor Risk Models](./Risk%20Management/Factor%20Risk%20Models)**
  * Constructed structural multi-factor risk models for the Euro Stoxx 600 universe to decompose portfolio volatility into systematic and idiosyncratic risk components.

### 3. Machine Learning
* **[Model Selection and Credit Risk Scoring](./Machine%20Learning/Model%20Selection)**
  * Developed a robust credit default probability (PD) scoring model using the German Credit dataset. Implemented hyperparameter tuning and cross-validation to minimize asymmetric misclassification costs.
* **[Text Classification and NLP](./Machine%20Learning/Text%20Classification)**
  * Built an end-to-end NLP pipeline (TF-IDF, Tokenization) to extract quantitative signals from unstructured text data using high-dimensional classification algorithms (LinearSVC).
* **[Classification Methods](./Machine%20Learning/Classification%20Methods)**
  * Engineered and evaluated multiple supervised learning algorithms (Decision Trees, k-NN, Neural Networks) to establish foundational predictive modelling frameworks.

### 4. Econometrics & Time Series Analysis
* **[Volatility Modelling and GARCH](./Econometrics/Volatility%20Modeling%20and%20GARCH)**
  * Modelled conditional volatility and asymmetric market shocks (leverage effects) using GARCH, EGARCH, and TGARCH specifications.
* **[Time Series and Simulation](./Econometrics/Time%20Series%20and%20Simulation)**
  * Conducted macroeconomic forecasting and cointegration analysis using ARIMA and VECM models. Evaluated statistical properties via Monte Carlo simulations.
* **[Factor Investing (Fama-French & Carhart)](./Econometrics/Factor%20Investing%20Fama%20Carhart)**
  * Conducted empirical analysis of the Fama-French 3-Factor and Carhart 4-Factor models. 
  * Implemented Fama-MacBeth two-stage regressions in R to estimate risk premiums on 25 European portfolios.

### 5. Financial Optimization
* **[Cash Flow Optimization](./Cash%20Flow%20Optimization)**
  * Formulated Linear Programming (LP) models using Python (Gurobi) to resolve corporate ALM (Asset Liability Management) problems.
  * Optimized short/long-term borrowing for cash flow management, bond portfolio returns subject to credit constraints, and pension fund liability immunization. Conducted extensive shadow price and reduced cost sensitivity analyses.
