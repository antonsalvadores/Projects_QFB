# Machine Learning: Model Selection and Credit Risk Scoring

## Financial & Business Objective
Develop a robust credit scoring model to classify applicants into 'good' or 'bad' credit risks using historical credit data. The primary objective is to implement rigorous model selection techniques to minimize the expected loss from default, specifically addressing the asymmetric costs of misclassification (false positives vs. false negatives) inherent in credit risk management.

## Mathematical Methodology
The project focuses on the statistical validation, feature engineering, and optimization of predictive models:
* **Data Preprocessing:** Handling mixed data types (categorical and numerical), feature scaling, and transformation to prepare the dataset for algorithmic ingestion.
* **Resampling Methods:** Implementation of K-Fold Cross-Validation and stratified sampling to obtain unbiased estimates of out-of-sample performance and strictly prevent overfitting.
* **Hyperparameter Tuning:** Algorithmic search across parameter spaces (e.g., Grid Search) to optimize the bias-variance tradeoff for the chosen classification algorithms.
* **Model Selection Criteria:** Evaluation of models using metrics beyond standard accuracy, utilizing ROC-AUC, Precision-Recall curves, and cost-sensitive evaluation tailored to financial risk.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `scikit-learn`, `pandas`, `numpy`, `matplotlib`, `seaborn`

## Key Results
* Constructed a mathematically rigorous pipeline for hyperparameter optimization and algorithm selection.
* Identified the optimal predictive model for credit default probability, balancing model complexity with generalization capability.
* Quantified the impact of parameter tuning on the final classification metrics, establishing a scalable framework for default probability (PD) scoring.

## Repository Structure
* `p2_martinez_perez_salvadores.ipynb`: Main Jupyter notebook executing the model selection workflow, cross-validation, and performance evaluation.
* `tools_assignment_2.py`: Python module containing custom helper functions for data processing, grid search execution, and metric visualization.
* `german_credit_data.csv` & `german_credit_data_numeric.csv`: Raw and numerically encoded datasets containing the historical credit records used for training and testing.
* `p2_smc_martinez_perez_salvadores.pdf`: PDF export summarizing the project report and mathematical findings.
* `p2_smc_martinez_perez_salvadores.html`: HTML render of the notebook for visualization of the execution pipeline and output graphs.