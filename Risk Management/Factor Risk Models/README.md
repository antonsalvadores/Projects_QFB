# Risk Management: Factor Risk Models

## Financial & Business Objective
Develop a multi-factor risk model to decompose and quantify the systematic and idiosyncratic risk of an equity portfolio. By mapping individual asset returns to common risk factors (such as market, sector, or style characteristics like size and value), the model enables portfolio managers to identify hidden concentration risks, optimize asset allocation, and ensure that the portfolio's active risk is strictly aligned with its investment mandate.

## Mathematical Methodology
This project implements the mathematical framework of structural risk modelling:
* **Factor Exposure Mapping:** Ingesting and processing cross-sectional asset characteristics to define the fundamental exposures of each security within the Euro Stoxx 600 universe.
* **Factor Returns Estimation:** Executing cross-sectional regressions at each time step to estimate the unobservable returns of pure factor portfolios based on historical price data.
* **Risk Decomposition & Covariance Construction:** Breaking down total asset variance into systematic (factor-driven) and specific (idiosyncratic) risk components. This includes the computation of the factor covariance matrix and the specific risk diagonal matrix to reconstruct the total asset covariance matrix.
* **Portfolio Risk Attribution:** Utilizing matrix algebra to calculate the portfolio's overall volatility and attributing the risk contributions to specific style or industry factors.

## Tech Stack
* **Language:** R
* **Core Concepts:** Linear Algebra, Cross-Sectional Regression, Financial Data Manipulation

## Key Results
* Engineered a robust structural factor risk model for a broad European equity universe (Euro Stoxx 600), successfully isolating systematic risk drivers from stock-specific noise.
* Mathematically derived the factor covariance matrix and asset-specific risk, providing the necessary quantitative inputs for advanced mean-variance portfolio optimization.
* Demonstrated strong proficiency in R for high-dimensional matrix operations and risk analytics, proving the ability to build proprietary risk management tools.

## Repository Structure
* `Practica_1_Angel_Anton.R`: Core R script containing the data ingestion pipeline, cross-sectional regressions, and matrix algebra required to construct the multi-factor risk model.
* `Data.xlsx` & `Datos eurostoxx 600 (1) (1).xlsx` (Cotizaciones / Caracteristicas): Historical datasets containing the daily pricing data and cross-sectional style/sector characteristics for the analyzed equity universe.
* `Statement.pdf`: Technical assignment document detailing the mathematical requirements and theoretical framework of the risk model.
* `Práctica_1_Riesgos_Ángel_Antón.pdf`: Comprehensive PDF report documenting the methodology, empirical results, and financial interpretations of the risk decomposition.