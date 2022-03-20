# Environment ------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(glue)
library(here)
library(haven)
library(rlang)
library(themis) # recipe for SMOTE
library(stringr)
library(ggthemes)
library(viridis)
library(doParallel)

mssd <- psych::mssd


# Internal Functions -----------------------------------------------------------
# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}


# Data I/O ---------------------------------------------------------------------
preprocessed_df <- here("data", "processed", "cf_selfharm_longitudinal.rds") %>% 
    read_rds() %>%  
    select(
        sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
        DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
        EP1_hosp, dur_adm1, mean_36m_pos:mssd_compliance
    ) %>% 
    mutate(Sex = as.numeric(Sex)) %>%
    mutate(Filter = as.numeric(Filter)) %>%
    mutate(Schiz = as.numeric(Schiz))


# create empty data frame for writing in results
repeat_var <- paste0("iter_", str_pad(1:100, 4, pad = "0"))
results_mat <- matrix(nrow = 2, ncol = 100) %>%
    as_tibble(.name_repair = ~repeat_var) %>% 
    mutate(metrics = c("accurary", "roauc"), .before = "iter_0001")


# Random Forest with Baseline --------------------------------------------------
results_mat_rf_baseline <- results_mat

# run Random Forest
f <- glue("sh_case ~ .") %>% as.formula()

mycluster <- makeForkCluster(nnodes = 4)
registerDoParallel(mycluster)

iter_results <- foreach (iter = 1:100) %dopar% {
    
    
    tmp_df <- preprocessed_df %>% 
        select(-c(mean_36m_pos:mssd_compliance))
    
    set.seed(iter)
    splits <- initial_split(tmp_df, prop = 2/3, stata = sh_case)
    data_tr <- training(splits)
    data_te <- testing(splits)
    
    data_tr_folds <- vfold_cv(data_tr, v = 3, repeats = 1, strata = sh_case)
    
    roc_res <- metric_set(roc_auc)
    
    # recipe
    master_recipe <- recipe(f, data = tmp_df) %>% 
        step_impute_knn(all_predictors(), neighbors = 10) %>% 
        step_smote(all_outcomes())
    
    # model specification
    mod_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("classification")
    
    # workflow
    mod_workflow <- workflow() %>% 
        add_model(mod_spec) %>% 
        add_recipe(master_recipe)
    
    tune_res <- mod_workflow %>% 
        tune_grid(data_tr_folds,
                  grid = 16,
                  control = control_grid(save_pred = TRUE),
                  metrics = metric_set(roc_auc, accuracy))
    
    best_auc <- select_best(tune_res, "roc_auc")
    
    final_mod <- finalize_model(
        mod_spec,
        best_auc
    )
    
    
    final_wf <- workflow() %>%
        add_recipe(master_recipe) %>%
        add_model(final_mod)
    
    final_res <- final_wf %>%
        last_fit(splits)
    
    return(final_res %>% collect_metrics() %>% pull(.estimate))
}

stopCluster(mycluster)
unregister()


results_mat_rf_baseline[1:2, repeat_var] <- iter_results %>% 
    unlist() %>% 
    matrix(ncol = length(iter_results), byrow = FALSE) %>% 
    as_tibble(.name_repair = ~repeat_var)

# save cache
write_rds(results_mat_rf_baseline, here("cache", "data-cf_model-rf_features-baseline_desc-prediction_baselineto36m.rds"))

results_mat_rf_baseline %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!metrics, values_to = "value") %>%
    summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE)
    )

# Random Forest with Baseline --------------------------------------------------
results_mat_rf_baseline_mean <- results_mat

# run random forest
f <- glue("sh_case ~ .") %>% as.formula()

mycluster <- makeForkCluster(nnodes = 4)
registerDoParallel(mycluster)

iter_results <- foreach (iter = 1:100) %dopar% {
    
    
    tmp_df <- preprocessed_df %>% 
        select(-c(mssd_pos:mssd_compliance))
    
    set.seed(iter)
    splits <- initial_split(tmp_df, prop = 2/3, stata = sh_case)
    data_tr <- training(splits)
    data_te <- testing(splits)
    
    data_tr_folds <- vfold_cv(data_tr, v = 3, repeats = 1, strata = sh_case)
    
    roc_res <- metric_set(roc_auc)
    
    # recipe
    master_recipe <- recipe(f, data = tmp_df) %>% 
        step_impute_knn(all_predictors(), neighbors = 10) %>% 
        step_smote(all_outcomes())
    
    # model specification
    mod_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("classification")
    
    # workflow
    mod_workflow <- workflow() %>% 
        add_model(mod_spec) %>% 
        add_recipe(master_recipe)
    
    tune_res <- mod_workflow %>% 
        tune_grid(data_tr_folds,
                  grid = 16,
                  control = control_grid(save_pred = TRUE),
                  metrics = metric_set(roc_auc, accuracy))
    
    best_auc <- select_best(tune_res, "roc_auc")
    
    final_mod <- finalize_model(
        mod_spec,
        best_auc
    )
    
    
    final_wf <- workflow() %>%
        add_recipe(master_recipe) %>%
        add_model(final_mod)
    
    final_res <- final_wf %>%
        last_fit(splits)
    
    return(final_res %>% collect_metrics() %>% pull(.estimate))
}

stopCluster(mycluster)
unregister()


results_mat_rf_baseline_mean[1:2, repeat_var] <- iter_results %>% 
    unlist() %>% 
    matrix(ncol = length(iter_results), byrow = FALSE) %>% 
    as_tibble(.name_repair = ~repeat_var)

# save cache
write_rds(results_mat_rf_baseline_mean, here("cache", "data-cf_model-rf_features-baseline_meansymptoms_desc-prediction_baselineto36m.rds"))

results_mat_rf_baseline_mean %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!metrics, values_to = "value") %>%
    summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE)
    )


# Random Forest with Baseline, Mean and Symptoms -------------------------------
results_mat_rf_baseline_both <- results_mat

# run random forest
f <- glue("sh_case ~ .") %>% as.formula()
mycluster <- makeForkCluster(nnodes = 4)
registerDoParallel(mycluster)

iter_results <- foreach (iter = 1:100) %dopar% {
    
    set.seed(iter)
    splits <- initial_split(preprocessed_df, prop = 2/3, stata = sh_case)
    data_tr <- training(splits)
    data_te <- testing(splits)
    
    data_tr_folds <- vfold_cv(data_tr, v = 3, repeats = 1, strata = sh_case)
    
    roc_res <- metric_set(roc_auc)
    
    # recipe
    master_recipe <- recipe(f, data = preprocessed_df) %>% 
        step_impute_knn(all_predictors(), neighbors = 10) %>% 
        step_smote(all_outcomes())
    
    # model specification
    mod_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
        set_engine("ranger") %>% 
        set_mode("classification")
    
    # workflow
    mod_workflow <- workflow() %>% 
        add_model(mod_spec) %>% 
        add_recipe(master_recipe)
    
    tune_res <- mod_workflow %>% 
        tune_grid(data_tr_folds,
                  grid = 16,
                  control = control_grid(save_pred = TRUE),
                  metrics = metric_set(roc_auc, accuracy))
    
    best_auc <- select_best(tune_res, "roc_auc")
    
    final_mod <- finalize_model(
        mod_spec,
        best_auc
    )
    
    
    final_wf <- workflow() %>%
        add_recipe(master_recipe) %>%
        add_model(final_mod)
    
    final_res <- final_wf %>%
        last_fit(splits)
    
    return(final_res %>% collect_metrics() %>% pull(.estimate))
}

stopCluster(mycluster)
unregister()

results_mat_rf_baseline_both[1:2, repeat_var] <- iter_results %>% 
    unlist() %>% 
    matrix(ncol = length(iter_results), byrow = FALSE) %>% 
    as_tibble(.name_repair = ~repeat_var)

# save cache
write_rds(results_mat_rf_baseline_both, here("cache", "data-cf_model-rf_features-baseline_meanmssdsymptoms_desc-prediction_baselineto36m.rds"))

results_mat_rf_baseline_both %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!metrics, values_to = "value") %>%
    summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE)
    )



# Comparison Between Models ----------------------------------------------------
roauc_models_df <- results_mat_rf_baseline %>% 
    bind_rows(results_mat_rf_baseline_mean) %>% 
    bind_rows(results_mat_rf_baseline_both) %>% 
    filter(metrics == "roauc") %>% 
    mutate(model = c("Baseline", "Baseline + Mean", "Baseline + Mean + MSSD"), .before = "metrics")

figure_comparison_modelclassfication <- roauc_models_df %>% 
    pivot_longer(!c(model, metrics), values_to = "value") %>% 
    ggplot(aes(x = model, y = value)) +
        geom_boxplot(width = 0.6, fill = "grey70", alpha = 0.25) +
        labs(x = "", y = "Area under the ROC Curve") +
        ggthemes::theme_pander() +
        theme(plot.margin = margin(2, 2, 2, 2, "mm")) +
        stat_compare_means(
            comparisons = list(
                c("Baseline", "Baseline + Mean"),
                c("Baseline + Mean", "Baseline + Mean + MSSD"),
                c("Baseline", "Baseline + Mean + MSSD")
            ),
            label = "p.signif"
        ) 
ggsave(plot = figure_comparison_modelclassfication,
       file = here("outputs", "figs", "boxplot_model-rf_data-cf_desc-36m_classification_comparisons.pdf"),
       width = 5, 
       height = 4)

