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
library(ranger)
mssd <- psych::mssd

source(here("src", "R", "visualization.R"))


# Data I/O ---------------------------------------------------------------------
preprocessed_df <- here("data", "processed", "cf_selfharm_longitudinal.rds") %>% 
    read_rds() %>% 
    select(
        sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
        DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
        EP1_hosp, dur_adm1,
        M1_Poscf:M36_Poscf,
        M1_Negcf:M36_Negcf,
        M1_Aff:M36_Aff,
        M1_SOFAScf:M36_SOFAScf,
        M1_compliance:M36_compliance,
        M1_case:M36_case
    ) %>% 
    mutate(Sex = as.numeric(Sex)) %>%
    mutate(Filter = as.numeric(Filter)) %>%
    mutate(Schiz = as.numeric(Schiz)) %>%
    rowwise() %>% 
    mutate(
        m6M1_case = ifelse(sum(c_across(M1_case:M6_case), na.rm = TRUE), 1, 0),
        m6M2_case = ifelse(sum(c_across(M2_case:M7_case), na.rm = TRUE), 1, 0),
        m6M3_case = ifelse(sum(c_across(M3_case:M8_case), na.rm = TRUE), 1, 0),
        m6M4_case = ifelse(sum(c_across(M4_case:M9_case), na.rm = TRUE), 1, 0),
        m6M5_case = ifelse(sum(c_across(M5_case:M10_case), na.rm = TRUE), 1, 0),
        m6M6_case = ifelse(sum(c_across(M6_case:M11_case), na.rm = TRUE), 1, 0),
        m6M7_case = ifelse(sum(c_across(M7_case:M12_case), na.rm = TRUE), 1, 0),
        m6M8_case = ifelse(sum(c_across(M8_case:M13_case), na.rm = TRUE), 1, 0),
        m6M9_case = ifelse(sum(c_across(M9_case:M14_case), na.rm = TRUE), 1, 0),
        m6M10_case = ifelse(sum(c_across(M10_case:M15_case), na.rm = TRUE), 1, 0),
        m6M11_case = ifelse(sum(c_across(M11_case:M16_case), na.rm = TRUE), 1, 0),
        m6M12_case = ifelse(sum(c_across(M12_case:M17_case), na.rm = TRUE), 1, 0),
        m6M13_case = ifelse(sum(c_across(M13_case:M18_case), na.rm = TRUE), 1, 0),
        m6M14_case = ifelse(sum(c_across(M14_case:M19_case), na.rm = TRUE), 1, 0),
        m6M15_case = ifelse(sum(c_across(M15_case:M20_case), na.rm = TRUE), 1, 0),
        m6M16_case = ifelse(sum(c_across(M16_case:M21_case), na.rm = TRUE), 1, 0),
        m6M17_case = ifelse(sum(c_across(M17_case:M22_case), na.rm = TRUE), 1, 0),
        m6M18_case = ifelse(sum(c_across(M18_case:M23_case), na.rm = TRUE), 1, 0),
        m6M19_case = ifelse(sum(c_across(M19_case:M24_case), na.rm = TRUE), 1, 0),
        m6M20_case = ifelse(sum(c_across(M20_case:M25_case), na.rm = TRUE), 1, 0),
        m6M21_case = ifelse(sum(c_across(M21_case:M26_case), na.rm = TRUE), 1, 0),
        m6M22_case = ifelse(sum(c_across(M22_case:M27_case), na.rm = TRUE), 1, 0),
        m6M23_case = ifelse(sum(c_across(M23_case:M28_case), na.rm = TRUE), 1, 0),
        m6M24_case = ifelse(sum(c_across(M24_case:M29_case), na.rm = TRUE), 1, 0),
        m6M25_case = ifelse(sum(c_across(M25_case:M30_case), na.rm = TRUE), 1, 0),
        m6M26_case = ifelse(sum(c_across(M26_case:M31_case), na.rm = TRUE), 1, 0),
        m6M27_case = ifelse(sum(c_across(M27_case:M32_case), na.rm = TRUE), 1, 0),
        m6M28_case = ifelse(sum(c_across(M28_case:M33_case), na.rm = TRUE), 1, 0),
        m6M29_case = ifelse(sum(c_across(M29_case:M34_case), na.rm = TRUE), 1, 0),
        m6M30_case = ifelse(sum(c_across(M30_case:M35_case), na.rm = TRUE), 1, 0),
        m6M31_case = ifelse(sum(c_across(M31_case:M36_case), na.rm = TRUE), 1, 0),
    ) %>% 
    ungroup()
    

# Internal Functions -----------------------------------------------------------
# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}


# Display ----------------------------------------------------------------------
month_case <- NULL
# All information from past events in 6 months
for (i in 7:31) {
    label = paste0("m6M", i, "_case")
    tmp_case <- preprocessed_df %>% 
        select(all_of(label)) %>% 
        sum()
    month_case <- c(month_case, tmp_case)
}

# create empty data frame for writing in results
month_case_length <- length(month_case)
repeat_var <- paste0("iter_", str_pad(1:100, 4, pad = "0"))
results_mat <- matrix(nrow = month_case_length * 2, ncol = 100) %>%
    as_tibble(.name_repair = ~repeat_var) %>% 
    mutate(month = rep(7:31, each = 2), .before = "iter_0001") %>% 
    mutate(metrics = rep(c("accurary", "roauc"), times = month_case_length), .before = "iter_0001")


# Logistic Regression ----------------------------------------------------------
results_mat_lr <- results_mat



# Random Forest ----------------------------------------------------------------

# run the 6-month sliding windows with random forest models
results_mat_rf <- results_mat

for (ith_month in 7:31) {
    
    print(paste0("month: ", ith_month))
    
    outcome_var <- paste0("m6M", ith_month, "_case")
    f <- glue("{outcome_var} ~ .") %>% as.formula()
        
        
    if (ith_month < 10) {
        
        pattern <- glue("^M[1-{ith_month - 1}]_")
        pattern_remove <- glue("^M[1-{ith_month - 2}]_")
        
    } else if (ith_month >= 11) {
        
        month_start <- ith_month - 6
        month_end <- ith_month - 1
        
        month_start_1 <- str_sub(str_pad(month_start, 2, pad = "0"), 1, 1)
        month_start_2 <- str_sub(str_pad(month_start, 2, pad = "0"), 2, 2)
        
        month_end_1 <- str_sub(str_pad(month_end, 2, pad = "0"), 1, 1)
        month_end_2 <- str_sub(str_pad(month_end, 2, pad = "0"), 2, 2)
        
        pattern <- case_when(
            
            month_start_1 == month_end_1 ~
                glue("^M({month_start_1}[{month_start_2}-{month_end_2}])_"),
            month_start_1 == 0 ~
                glue("^M([{month_start_2}-9]|{month_end_1}[0-{month_end_2}])_"),
            month_start_1 != month_end_1 ~
                glue("^M({month_start_1}[{month_start_2}-9]|{month_end_1}[0-{month_end_2}])_")
            
        )
        
        month_end_remove_1 <- str_sub(str_pad(month_end - 1, 2, pad = "0"), 1, 1)
        month_end_remove_2 <- str_sub(str_pad(month_end - 1, 2, pad = "0"), 2, 2)
        
        pattern_remove <- case_when(
            
            month_start_1 == 0 & month_end_remove_1 == 0 ~
                glue("^M([{month_start_2}-{month_end_remove_2}])_"),
            month_start_1 == month_end_remove_1 ~
                glue("^M({month_start_1}[{month_start_2}-{month_end_remove_2}])_"),
            month_start_1 != month_end_remove_1 ~
                glue("^M({month_start_1}[{month_start_2}-9]|{month_end_remove_1}[0-{month_end_remove_2}])_")
        )
    
    }
        
    data_tmp <- preprocessed_df %>% 
        select(Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
               DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
               EP1_hosp, dur_adm1,
               matches(pattern), all_of(outcome_var)) %>% 
        mutate_at(outcome_var, .funs = factor) %>%
        rowwise() %>% 
        mutate(
            mssd_pos = mssd(c_across(matches(glue("{pattern}Poscf"))), na.rm = TRUE),
            mssd_neg = mssd(c_across(matches(glue("{pattern}Negcf"))), na.rm = TRUE),
            mssd_aff = mssd(c_across(matches(glue("{pattern}Aff"))), na.rm = TRUE),
            mssd_sofas = mssd(c_across(matches(glue("{pattern}SOFAScf"))), na.rm = TRUE),
            mssd_comp =  mssd(c_across(matches(glue("{pattern}compliance"))), na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        select(-matches(pattern_remove))   
    
    mycluster <- makeForkCluster(nnodes = 4)
    registerDoParallel(mycluster)
    
    iter_results <- foreach (iter = 1:100) %dopar% {
        
        set.seed(iter)
        splits <- initial_split(data_tmp, prop = 2/3, stata = vars(outcome_var))
        data_tr <- training(splits)
        data_te <- testing(splits)
        
        data_tr_folds <- vfold_cv(data_tr, v = 3, repeats = 1, strata = !!outcome_var)
        
        # recipe
        master_recipe <- recipe(f, data = data_tmp) %>% 
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
    
    col_num <- ith_month - 6
    
    results_mat_rf[(col_num * 2 - 1):(col_num * 2), repeat_var] <- iter_results %>% 
        unlist() %>% 
        matrix(ncol = length(iter_results), byrow = FALSE) %>% 
        as_tibble(.name_repair = ~repeat_var)

}

write_rds(results_mat_rf, file = here("cache", "data-cf_model-rf_desc-prediction_6mto6m.rds"))
# results_mat_rf <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_6mto6m.rds"))
# check out the results
results_mat_rf %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE)
    ) %>% 
    filter(month %in% 11:23) %>% 
    summarize(
        mean = mean(mean),
        sd = mean(sd)
    )

# plot the results using area under the ROC curve
plot_timeseries_boxplots(
    results_mat_rf,
    saveplot = TRUE,
    filename = "boxplot_model-rf_data-cf_desc-prediction_results_6mto6m.pdf"
)



