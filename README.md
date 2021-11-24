# A dynamic interaction model of clinical symptoms, social functioning and self-harm in patients with first-episode schizophrenia-spectrum disorders over three years.

Last update: 22 Nov 2021

## Folder Structure

```
├── README.md
├── data
│   ├── processed (upon request)
│   └── raw (upon request)
├── longitudinal_selfharm.Rproj
├── outputs
│   ├── figs
│   │   ├── boxplot_data-cf_desc-prediction_results.pdf
│   │   ├── boxplot_data-imputation_prediction_results.pdf
│   │   ├── data-cf_desc-longitudial_change.pdf
│   │   ├── data-cf_desc-tvmvar_lag1.pdf
│   │   ├── data-imputation_tvmvar_lag1.pdf
│   │   └── selfharm_tvmvar.pdf
│   └── tables
│       ├── cf_dsh_ss_comparison.html
│       ├── cf_models_comparison.html
│       ├── data-cf_desc-models_comparison.html
│       ├── dsh_ss_comparison.html
│       ├── models_comparison.html
│       ├── table_data-imputation_comparison-dsh.html
│       └── table_data-imputation_models_comparison.html
└── scripts
    ├── 01_a_clean_cf_dataset.R
    ├── 01_b_clean_dataset.R
    ├── 02_a_regression.R
    ├── 02_b_regression_imp.R
    ├── 03_a_tvmvar_cf.R
    ├── 03_b_tvmvar_imputation.R
    ├── 04_a_prediction.R
    ├── 04_b_prediction_imputation.R
```

## We have _**two sets**_ of scripts.

```
The first set of scripts using carry forward data.
1. 01_a_clean_cf_dataset.R
2. 02_a_regression.R
3. 03_a_tvmvar_cf.R
4. 04_a_prediction.R

The second set of scripts using imputed data.
1. 01_b_clean_dataset.R
2. 02_b_regression_imp.R
3. 03_b_tvmvar_imputation.R
4. 04_b_prediction_imputation.R
```

## How to run the scripts

### Step 1. Data Cleaning

### Step 2. Regression Models

### Step 3. Time Varying Vector Autoregressive Model

### Step 4. Out-of-Sample Prediction
