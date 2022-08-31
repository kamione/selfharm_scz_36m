<div align="center">
<!-- Title -->

# Dynamic Patterns of Symptoms and Functioning in Predicting Deliberate Self-harm in Patients with First-Episode Schizophrenia-Spectrum Disorders Over 3 Years.

<div align="left">
<!-- Badges -->

![](https://img.shields.io/tokei/lines/github/kamione/selfharm_scz_36m)

Last update: 31 Aug 2022

<!-- Description -->

## Description

The code repository for the [paper](https://academic.oup.com/schizophreniabulletin/advance-article-abstract/doi/10.1093/schbul/sbac057/6605811?redirectedFrom=fulltext&login=false).

<!-- Folder Structure -->

## Folder Structure

```
├── README.md
├── data
│   ├── processed (upon request)
│   └── raw
├── longitudinal_selfharm.Rproj
├── outputs
│   ├── figs
│   │   ├── boxplot_data-imputation_prediction_results.pdf
│   │   ├── boxplot_mod-rf_data-cf_desc-prediction_results.pdf
│   │   ├── boxplot_model-rf_data-cf_desc-36m_classification_comparisons.pdf
│   │   ├── boxplot_model-rf_data-cf_desc-prediction_results_12mto12m.pdf
│   │   ├── boxplot_model-rf_data-cf_desc-prediction_results_12mto6m.pdf
│   │   ├── boxplot_model-rf_data-cf_desc-prediction_results_6mto12m.pdf
│   │   ├── boxplot_model-rf_data-cf_desc-prediction_results_6mto6m.pdf
│   │   ├── data-cf_desc-longitudial_change.pdf
│   │   ├── data-cf_desc-tvmvar_lag1.pdf
│   └── tables
│       ├── data-cf_desc-dsh_group_comparison.docx
│       ├── data-cf_desc-dsh_group_comparison.html
│       ├── data-cf_desc-models_comparison.docx
│       ├── data-cf_desc-models_comparison.html
│       ├── data-cf_desc-self_category_comparison.docx
│       ├── data-cf_desc-self_category_comparison.html
│       ├── data-cf_model-rf_desc-all_perfromances_selfharm_events.docx
├── scripts
│   ├── 01_a_clean_cf_dataset.R
│   ├── 01_b_clean_imp_dataset.R
│   ├── 02_a_regression.R
│   ├── 03_a_tvmvar_cf.R
│   ├── 04_a_classification_selfharm36m.R
│   ├── 05_a_prediction_6mto6m.R
│   ├── 06_a_prediction_6mto12m.R
│   ├── 07_a_prediction_12mto6m.R
│   ├── 08_a_prediction_12mto12m.R
│   ├── 09_a_prediction_performance.R
└── src
    └── R
        ├── tables.R
        └── visualization.R
```

## We have _**two sets**_ of scripts.

```
The first set of scripts using carry forward data.
1. 01_a_clean_cf_dataset.R
2. 02_a_regression.R
3. 03_a_tvmvar_cf.R
4. 04_a_classification_selfharm36m.R
5. 05_a_prediction_6mto6m.R
6. 06_a_prediction_6mto12m.R
7. 07_a_prediction_12mto6m.R
8. 08_a_prediction_12mto12m.R
9. 09_a_prediction_performance.R

The second set of scripts using imputed data.
1. 01_b_clean_cf_dataset.R
2. 02_b_regression.R
3. 03_b_tvmvar_cf.R
4. 04_b_classification_selfharm36m.R
5. 05_b_prediction_6mto6m.R
6. 06_b_prediction_6mto12m.R
7. 07_b_prediction_12mto6m.R
8. 08_b_prediction_12mto12m.R
9. 09_b_prediction_performance.R
```

<!-- Setup -->

## Setup

Start the RStudio via [longitudinal_selfharm.Rpoj](https://github.com/kamione/selfharm_scz_36m/blob/master/longitudinal_selfharm.Rproj).

<!-- Usage -->

## Usage

### How to run the scripts

```
Step 1. Data Cleaning and Preprocessing
Step 2. Regression Models
Step 3. Time Varying Vector Autoregressive Model
Step 4. Classifying Indiviudals with and without Self-harm
Step 5. Future Self-harm Events Prediction (with 6m information to 6m prediction window)
Step 6. Future Self-harm Events Prediction (with 6m information to 12m prediction window)
Step 7. Future Self-harm Events Prediction (with 12m information to 6m prediction window)
Step 8. Future Self-harm Events Prediction (with 12m information to 12m prediction window)
Step 9. Visualization and Table for Prediction Models
```

<!-- Citation -->

## Citation

```{bibtex}
@article{10.1093/schbul/sbac057,
    author = {Wong, Ting Yat and Chan, Sherry Kit Wa and Cheung, Charlton and Lai Ming Hui, Christy and Suen, Yi Nam and Chang, Wing Chung and Lee, Edwin Ho Ming and Chen, Eric Yu Hai},
    title = "{Dynamic Patterns of Symptoms and Functioning in Predicting Deliberate Self-harm in Patients with First-Episode Schizophrenia-Spectrum Disorders Over 3 Years}",
    journal = {Schizophrenia Bulletin},
    year = {2022},
    doi = {10.1093/schbul/sbac057},
}

```
