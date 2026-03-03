# Numerical Methods: Numerical Approximation and Estimation Errors

## Financial & Business Objective
Establish a rigorous computational framework to quantify and manage approximation errors in numerical algorithms. In quantitative finance, where closed-form solutions for complex derivatives often do not exist, models rely heavily on numerical approximations. The core objective is to understand the trade-offs between truncation errors and floating-point round-off errors, ensuring that pricing algorithms and risk metric calculations remain numerically stable and do not lead to financial mispricing.

## Mathematical Methodology
This project focuses on the mathematical foundations of computational accuracy:
* **Taylor Series Expansions:** Implementation and visual analysis of Taylor polynomial approximations for non-linear functions, assessing convergence rates and intervals.
* **Error Analysis:** Rigorous quantification of absolute and relative errors inherent in computational mathematics.
* **Algorithmic Stability:** Development of custom numerical functions to evaluate mathematical operations while mitigating the risk of catastrophic cancellation and precision loss in floating-point arithmetic.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `numpy`, `math`, `matplotlib`
* **Visualization Tools:** GeoGebra

## Key Results
* Developed and validated custom Python algorithms to perform numerical approximations with strictly controlled error bounds.
* Mathematically proved and visualized the limitations and convergence behavior of polynomial approximations across different domains.
* Demonstrated a deep understanding of computational stability, a prerequisite for developing robust, high-performance financial pricing engines.

## Repository Structure
* `QFB_2024_2025_P01_G05_crespo_cuesta_salvadores.ipynb`: Main Jupyter notebook executing the numerical approximations, error calculations, and theoretical validations.
* `Anton_Gorka_Miguel_HW_02_functions.py`: Auxiliary Python module containing the custom-built numerical functions and algorithms used in the main notebook.
* `tools_qfb.py`: Python module containing general quantitative tools and helper functions.
* `graficas_taylor_ej2.ggb`: GeoGebra file containing the dynamic graphical representation of the Taylor series expansions and approximation intervals.
* `QFB_2024_2025_P01_G05_crespo_cuesta_salvadores.pdf`: PDF report detailing the formal mathematical proofs, error analysis, and conclusions of the project.
* `QFB_2024_2025_P01_G05_crespo_cuesta_salvadores.html`: HTML render of the main notebook for easy visualization of code execution, outputs, and formatting.