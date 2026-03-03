# Exotic Option Pricing Engine (Monte Carlo & Python)

This project, developed for my M.Sc. in Quantitative Banking and Finance, builds a Python-based Monte Carlo engine to price complex, path-dependent exotic options.

---
### 📄 Key Project Documents
* **[View the Jupyter Notebook (Main Code)](HW_03_exercises.ipynb)**
* **[View the Final Assignment (PDF)](docs/HW_03_exercises_pdf.pdf)**
---

## Project Objectives & Features

The pricing engine is built to value complex derivatives not solvable with closed-form solutions:

1.  **Basket Call Option:**
    * Prices a European call option whose payoff depends on a weighted basket of **three correlated assets**.
    * Implements **Cholesky decomposition** to accurately model the correlated random walks of the underlying assets.

2.  **Path-Dependent Barrier Options:**
    * **Up-and-Out Call:** Prices a standard barrier option where the payoff is contingent on the asset's price never crossing an upper barrier (`B_up`) during its lifetime.
    * **Complex Dual-Barrier Option:** Prices a custom-defined option with two distinct, time-dependent barriers:
        * Must stay **above** `B_down` for the first half of its life ($t_0$ to $T/2$).
        * Must stay **below** `B_up` for the second half of its life ($T/2$ to $T$).

## Methodology & Tools

* **Valuation Model:** Monte Carlo Simulation
* **Stochastic Process:** Geometric Brownian Motion (GBM) 
* **Core Tools:** Python, NumPy, SciPy, Matplotlib
* **Mathematical Core:** Cholesky Decomposition (for correlation), Statistical Analysis (Sampling vs. Discretization Error).
