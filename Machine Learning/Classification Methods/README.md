# Machine Learning: Classification Methods

## Financial & Business Objective
Develop and evaluate predictive classification models using supervised machine learning algorithms. The core objective is to accurately predict categorical target variables (e.g., risk scoring, default probability, or market regime classification) based on historical feature sets. This provides a quantitative foundation for automated decision-making and risk mitigation strategies.

## Mathematical Methodology
The project implements a comprehensive end-to-end machine learning pipeline:
* **Data Preprocessing & EDA:** Feature scaling (standardization/normalization), handling missing data, and encoding categorical variables to prepare the dataset for algorithmic ingestion.
* **Supervised Learning Algorithms:** Training and deployment of foundational and advanced classification models, which may include Logistic Regression, k-Nearest Neighbors (k-NN), Decision Trees, Support Vector Machines (SVM), or ensemble methods.
* **Model Validation:** Implementation of k-fold cross-validation to ensure out-of-sample robustness and prevent overfitting.
* **Performance Metrics:** Rigorous evaluation of model performance using confusion matrices, Accuracy, Precision, Recall, F1-Score, and ROC-AUC curves.
* **Hyperparameter Tuning:** Optimization of model parameters (e.g., via Grid Search or Randomized Search) to maximize predictive power.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `scikit-learn`, `pandas`, `numpy`, `matplotlib`, `seaborn`

## Key Results
* Successfully engineered and trained multiple classification models, establishing a robust framework for categorical prediction.
* Conducted rigorous statistical comparisons across algorithms to identify the optimal model based on a balance of precision, recall, and ROC-AUC.
* Extracted quantitative insights regarding feature importance, identifying which variables have the highest predictive power for the target classification.

## Repository Structure
* `p01_smc_martinez_perez_salvadores.ipynb`: Main Jupyter notebook detailing the complete machine learning workflow, from exploratory data analysis to final model evaluation.
* `tools_assignment_1.py`: Auxiliary Python module containing custom functions for data preprocessing, visualization, and metric computation.
* `p01_smc_martinez_perez_salvadores.pdf`: PDF export of the notebook/report summarizing the methodology and findings.
* `p01_smc_martinez_perez_salvadores.html`: HTML render of the notebook for easy visualization of code execution and output plots.