# Numerical Methods: Pricing Interest Rate Derivatives

## Financial & Business Objective
Develop a robust, object-oriented quantitative library to price Interest Rate (IR) derivatives. The primary objective is to accurately value fixed-income instruments and non-linear IR options (such as Caps and Caplets) using empirical market data. This architecture mimics industry-standard pricing engines used by trading desks, ALM (Asset Liability Management) teams, and risk management departments to manage interest rate exposure.

## Mathematical Methodology
This project implements advanced quantitative finance frameworks translated into modular, production-ready code:
* **Yield Curve Construction:** Modelling and interpolating the term structure of interest rates (Euribor curves) for accurate discounting and forward rate projection.
* **Volatility Surfaces:** Implementation of implied volatility surface modelling, handling Normal and Shifted Lognormal dynamics to accommodate different market environments, including negative interest rates.
* **Derivative Pricing Models:** Valuation of linear products (Fixed and Floating Rate Bonds) and non-linear options (Caps, Caplets) using Bachelier (Normal) and Black (Shifted Lognormal) analytical frameworks.
* **Financial Conventions & Calendars:** Strict algorithmic adherence to market conventions, including day count fractions, schedule generation, and TARGET calendar holiday adjustments for precise cash flow timing.

## Tech Stack
* **Language:** Python (Advanced Object-Oriented Programming)
* **Core Libraries:** `pandas`, `numpy`, `scipy`

## Key Results
* Engineered a modular, highly scalable pricing library from scratch, demonstrating the ability to build quantitative software infrastructure rather than relying on black-box third-party modules.
* Successfully calculated the Net Present Value (NPV) of complex bond structures and interest rate options using real-world Euribor and volatility data.
* Automated the ingestion of raw financial data into structured pricing objects.

## Repository Structure
The project is built with a highly structured Object-Oriented design:
* **Execution Scripts (`/main`):** * `main_a.py` & `main_b_c.py`: Entry points executing the pricing workflows and outputting final valuations.
* **Market Data (`/data`):** * `import_data.py`: Pipeline to ingest raw market inputs. 
  * `Datos_Ejercicio_1.xlsx`: Historical Euribor and volatility datasets.
* **Financial Basics (`/basics`):** * Modules managing date math: `day_counter.py`, `schedule_generator.py`, and `target_calendar.py`.
* **Underlying Assets (`/underlyings`):** * Core risk factor models: `interest_rate_curve.py` and `normal_volatility_surface.py`.
* **Financial Products (`/products`):** * *Interest Rate:* Bond and coupon classes.
  * *Volatility:* Non-linear derivative classes.