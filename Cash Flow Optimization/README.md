# Financial Optimization with Linear Programming

This project, developed during my first year of the M.Sc. in Quantitative Banking and Finance, applies Linear Programming (LP) optimization techniques to solve complex problems in financial planning and portfolio management

---
### 📄 Key Project Documents
* **[View Final Assignment (PDF)](Docs/Assignment.pdf)**
* **[View Problem Statement (PDF)](Docs/Statement.pdf)**
---

## Project Objectives

The repository includes models for:

1.  **Financial Planning (Cash Flow Management):** Maximizing a company's wealth at the end of a 2-year (8-quarter) horizon. The model optimizes borrowing decisions (2-year, 6-month, and quarterly loans) and the investment of surplus funds to meet cash flow requirements in each quarter.
2.  **Bond Portfolio Management (Manager):** Maximizing the return of a bond portfolio, subject to constraints on average credit rating (risk) and portfolio maturity.
3.  **Liability Immunization (Pension Fund):** Minimizing the cost of a bond portfolio structured to fund a pension fund's liabilities over the next 9 years.

## Methodology & Tools

* **Language:** Python
* **Optimization Solver:** `gurobipy` (Gurobi)
* **Data Analysis:** `pandas`

## Key Analyses Performed

In addition to optimization, a full **Sensitivity Analysis** (post-optimality) was conducted:

* **Shadow Prices:** Interpreted to understand how the final objective (wealth) would be impacted by relaxing certain constraints (e.g., a reduced capital requirement in a given quarter).
* **Reduced Costs:** Analyzed to determine how much the price of an out-of-portfolio bond would need to change to be included in the optimal solution.

*Problems adapted from the textbook "Optimization Methods in Finance" by Gerard Cornuejols and Reha Tütüncü.*
