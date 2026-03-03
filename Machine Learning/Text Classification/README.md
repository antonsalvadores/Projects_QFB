# Machine Learning: Text Classification and NLP

## Financial & Business Objective
Extract actionable quantitative signals from unstructured text data using Natural Language Processing (NLP) and machine learning classification algorithms. The objective is to establish a pipeline that can automatically ingest, process, and categorize textual information (such as sentiment analysis or document classification), bridging the gap between qualitative information and systematic, data-driven decision making.

## Mathematical Methodology
This project implements a complete NLP pipeline to transform raw text into mathematical vectors for algorithmic prediction:
* **Text Preprocessing:** Cleaning and standardizing unstructured text data, including tokenization, stop-word removal, and stemming/lemmatization to reduce the dimensionality of the text corpus.
* **Feature Extraction (Vectorization):** Transformation of text into numerical sparse matrices using techniques such as Term Frequency-Inverse Document Frequency (TF-IDF) and Bag-of-Words (CountVectorizer).
* **Predictive Modelling:** Application of classification algorithms suited for high-dimensional sparse data (such as Multinomial Naive Bayes, Support Vector Machines, or Logistic Regression) to map text vectors to discrete categories.
* **Model Evaluation:** Validation of the classifier's performance on test data using Accuracy, F1-Score, and confusion matrices to evaluate the precision and recall across different text categories.

## Tech Stack
* **Language:** Python
* **Core Libraries:** `scikit-learn`, `pandas`, `numpy`, Natural Language Toolkit (`nltk` or equivalent)

## Key Results
* Engineered an end-to-end NLP pipeline capable of cleaning and vectorizing raw text data.
* Successfully trained and evaluated text classification models, identifying the optimal algorithm for processing high-dimensional textual features.
* Demonstrated the ability to extract predictive power from unstructured datasets, a critical skill for modern quantitative and systematic strategies.

## Repository Structure
* `p3_smc_martinez_perez_salvadores.ipynb`: The core Jupyter notebook detailing the text preprocessing steps, vectorization, and the training of the classification models.
* `tools_assignment_3.py`: Auxiliary Python module containing custom functions to automate the text cleaning and feature extraction processes.
* `train.tsv`: The raw tabular dataset containing the labeled text corpus used to train and validate the predictive models.
* `p3_smc_martinez_perez_salvadores.pdf`: PDF document summarizing the methodology, the mathematical transformations, and the final classification results.
* `p3_smc_martinez_perez_salvadores.html`: HTML render of the main notebook, allowing for easy visualization of the NLP workflow and output metrics.