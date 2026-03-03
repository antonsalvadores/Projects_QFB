# Numerical Methods: Pricing of European Options

## Financial & Business Objective
Develop a computational pricing engine for European-style financial derivatives. The primary objective is to accurately calculate the fair value (premium) of plain vanilla Call and Put options. Building a proprietary pricing model from scratch is a fundamental exercise for Quantitative Developers, forming the basis for market making, derivative structuring, hedging strategies, and risk management systems in any trading desk or asset management firm.

## Mathematical Methodology
This project focuses on the programmatic implementation of core quantitative finance models:
* **Analytical Pricing:** Implementation of closed-form mathematical solutions, such as the Black-Scholes-Merton (BSM) framework, to calculate the theoretical price of European options.
* **Parameter Integration:** The custom pricing engine ingests critical market variables—including the underlying asset price, strike price, time to maturity, risk-free interest rate, and volatility—to compute the option's Net Present Value (NPV).
* **Algorithm Validation:** Testing the mathematical robustness and numerical accuracy of the pricing functions across various market scenarios and moneyness states (In-the-Money, At-the-Money, Out-of-the-Money).

## Tech Stack
* **Language:** Python
* **Core Libraries:** `numpy`, `scipy`

## Key Results
* Engineered a reusable and modular Python library (`my_option_pricing`) specifically designed for the rapid valuation of European derivatives.
* Validated the accuracy of the pricing algorithms through systematic testing within a structured Jupyter Notebook environment.
* Demonstrated strong programming fundamentals by abstracting complex mathematical formulas into clean, executable, and scalable code.

## Repository Structure
* `Anton_Salvadores_Muniz_H008_pricing_european_options.ipynb`: The main Jupyter notebook used to execute, test, and validate the European option pricing models against theoretical expectations.
* `Anton_Salvadores_Muniz_my_option_pricing.py`: The core Python module containing the mathematical functions and algorithms for derivative valuation.
* `tools_qfb.py`: An auxiliary Python module containing general quantitative tools, data structures, and helper functions used to support the pricing environment.
* `Anton_Salvadores_Muniz_H008_pricing_european_options.html`: An HTML render of the main notebook, allowing for easy visualization of the pricing outputs, test cases, and execution flow without requiring a local Python environment.
* `tools_qfb.cpython-311.pyc`: Compiled Python bytecode ensuring faster execution of the tools module.