# Sentiment Based Predictive Modeling for PPG Sales Data

Final project for INFSCI 2595 Machine Learning, University of Pittsburgh, Spring 2022.

This repo contains the data, Rmarkdown files and R scripts used to perform predictive modeling using sentiment-derived features.

## Introduction

### Data and Setup

- The raw data was sponsored by PPG Inc sales team for education purposes only. 
  - Data used for modeling was already cleaned, integrated, and anonymized.
- The sentiment-derived features were extracted and provided to us.
  - Five different sets of sentiment derived features correspond to differnet lexicon or method used.
  - Bing lexicon derived features have names starting with `xb_`
  - NRC lexicon derived features have names starting with `xn_`
  - AFINN lexicon derived features have names starting with `xa_`
  - Word count derived features have names starting with `xw_`
  - `sentimentr` derived features have names starting with `xs_`

- The data was already split to training and hold-out test set
  - For the training set, we get all input variables and response variables.
  - For the hold-out set, we get the input variables only; The response variables were held by the professor to evaluate the final model.

### Project Goals

- The project has two main prediction tasks using the provided training set:
  1. Predict if a product could sell (classification task);
  2. Predict the time a sales representative spends on a customer (regression task).

- Additionally, the professor is also curious about:
  1. If these sentiment based features are useful in these kind of prediction tasks. If so which ones are more useful.
  2. Which customers are the hardest to predict using the models trained.


## Project Structure

### 1. Data EDA

The exploratory data analysis (EDA) part involves
- `1A`: examine data distribution 
- `1B`: investigate correlations among the variables

### 2. Regression

The regression part follow a specific order from simple to advanced.

- `2A` to `2C` work through manually building baseline linear regrssion models and some analysis.
- `2D1` to `2D3` involve using resampling to train the selected LMs, regularized LMs, and advanced models.
- `2D4` discusses selecting the best regression model from the above model sets.

### 3. Classification

Project structure follows that of regrssion section.

- `3A` to `3C` work on manually crafting baseline logistic regression models and some analysis.
- `3D1` to `3D3` are about using resampling to train the selected GLMs, regularized GLMs, and advanced models.
- `3D4` discusses selecting the best classification model from the above model sets.

### 4. Interpretation

Notebooks starting with `4` are all related to interpretation.

- `4A` is about identifying the key input features that affect the prediction.
- `4B` is about finding out the hardest to predict customer.
- `4C` involves making prediction using my best models for the two tasks on the hold-out test set input. 
  - The predictions from my best models were combined and saved as `my_holdout_pred.csv`.

#### Final Model Evaluation

The `my_holdout_pred.csv` prediction file was submitted via a website and tested against the hold-out responses (actual observed).

The performance metrics were reported as the `metrics.csv`.

- Regression
  | .metric     | .estimator | .estimate         |
  | ----------- | ---------- | ----------------- |
  | rmse        | standard   | 0.237799038383658 |
  | rsq         | standard   | 0.811094346377037 |
  | mae         | standard   | 0.19211529372743  |

- Classification
  | .metric     | .estimator | .estimate         |
  | ----------- | ---------- | ----------------- |
  | accuracy    | binary     | 0.818181818181818 |
  | mn_log_loss | binary     | 0.465447310356421 |
  | roc_auc     | binary     | 0.841739130434783 |

### Report

I also made a powerpoint to report some of the key findings through this project. This can be found in `report/Report to PPG.pptx`.
