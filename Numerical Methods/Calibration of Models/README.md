# Numerical Methods: Financial Model Calibration

## Financial & Business Objective
Calibrate stochastic pricing models to empirical market data to accurately reflect current market conditions. The objective is to optimize model parameters so that the theoretical prices of financial instruments (such as options) closely match their market-observed counterparts. This process is essential for robust derivative pricing, hedging strategies, and constructing accurate implied volatility surfaces in trading and risk management.

## Mathematical Methodology
This project implements the mathematical optimization required to fit theoretical models to real-world data:
* **Stochastic Processes:** Implementation of numerical simulations for underlying asset dynamics using advanced stochastic differential equations (SDEs).
* **Option Pricing Engines:** Development of pricing algorithms (e.g., analytical formulas, numerical integration, or Monte Carlo methods) to generate theoretical derivative prices based on arbitrary model parameters.
* **Optimization and Calibration:** Formulation of an inverse problem to find the optimal parameter set. This involves minimizing a loss function (such as the Sum of Squared Errors) between model-implied prices and market prices using numerical optimization routines (e.g., gradient-based or heuristic solvers).
* **Time Series Analysis:** Empirical estimation of historical parameters (drift, volatility) from financial time series to serve as initial guesses for the calibration algorithms.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `scipy.optimize`, `numpy`, `pandas`, `matplotlib`

## Key Results
* Engineered a modular, object-oriented pipeline for option pricing and model calibration.
* Successfully calibrated theoretical stochastic models to empirical market data from the IBEX 35 index, minimizing pricing errors.
* Demonstrated the ability to translate complex numerical methods into scalable Python code, ensuring theoretical models are practically applicable to real financial markets.

## Repository Structure
* `HW_04_Gorka_Miguel_Anton.ipynb`: The main Jupyter notebook executing the calibration workflow, numerical optimization, and visualizing the fit against market data.
* `model_calibration.py`: Core Python module containing the numerical optimization algorithms and loss functions used to calibrate the models.
* `Anton_Salvadores_Muniz_my_option_pricing.py`: Custom module implementing the numerical pricing engines for financial derivatives.
* `stochastic_processes.py`: Module defining the dynamics and simulation of the underlying stochastic processes.
* `my_time_series_MIGUEL.py` & `tools_qfb.py`: Auxiliary quantitative tools for time series analysis and general data handling.
* `ibex35raw.csv`: Raw market dataset containing the historical data for the IBEX 35 index used during the calibration process.
* `HW_04_Gorka_Miguel_Anton.pdf` & `HW_04_Gorka_Miguel_Anton.html`: Exported reports detailing the mathematical formulations, methodology, and numerical results.