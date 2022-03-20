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

source(here("src", "R", "visualization.R"))


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
        m12M13_case = ifelse(sum(c_across(M13_case:M18_case), na.rm = TRUE), 1, 0),
        m12M14_case = ifelse(sum(c_across(M14_case:M19_case), na.rm = TRUE), 1, 0),
        m12M15_case = ifelse(sum(c_across(M15_case:M20_case), na.rm = TRUE), 1, 0),
        m12M16_case = ifelse(sum(c_across(M16_case:M21_case), na.rm = TRUE), 1, 0),
        m12M17_case = ifelse(sum(c_across(M17_case:M22_case), na.rm = TRUE), 1, 0),
        m12M18_case = ifelse(sum(c_across(M18_case:M23_case), na.rm = TRUE), 1, 0),
        m12M19_case = ifelse(sum(c_across(M19_case:M24_case), na.rm = TRUE), 1, 0),
        m12M20_case = ifelse(sum(c_across(M20_case:M25_case), na.rm = TRUE), 1, 0),
        m12M21_case = ifelse(sum(c_across(M21_case:M26_case), na.rm = TRUE), 1, 0),
        m12M22_case = ifelse(sum(c_across(M22_case:M27_case), na.rm = TRUE), 1, 0),
        m12M23_case = ifelse(sum(c_across(M23_case:M28_case), na.rm = TRUE), 1, 0),
        m12M24_case = ifelse(sum(c_across(M24_case:M29_case), na.rm = TRUE), 1, 0),
        m12M25_case = ifelse(sum(c_across(M25_case:M30_case), na.rm = TRUE), 1, 0),
        m12M26_case = ifelse(sum(c_across(M26_case:M31_case), na.rm = TRUE), 1, 0),
        m12M27_case = ifelse(sum(c_across(M27_case:M32_case), na.rm = TRUE), 1, 0),
        m12M28_case = ifelse(sum(c_across(M28_case:M33_case), na.rm = TRUE), 1, 0),
        m12M29_case = ifelse(sum(c_across(M29_case:M34_case), na.rm = TRUE), 1, 0),
        m12M30_case = ifelse(sum(c_across(M30_case:M35_case), na.rm = TRUE), 1, 0),
        m12M31_case = ifelse(sum(c_across(M31_case:M36_case), na.rm = TRUE), 1, 0),
    ) %>% 
    ungroup()


# Display ----------------------------------------------------------------------
month_case <- NULL
# All information from past events in 6 months
for (i in 13:31) {
    label = paste0("m12M", i, "_case")
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
    mutate(month = rep(13:31, each = 2), .before = "iter_0001") %>% 
    mutate(metrics = rep(c("accurary", "roauc"), times = month_case_length), .before = "iter_0001")


# Random Forest ----------------------------------------------------------------
results_mat_rf <- results_mat

for (ith_month in 13:31) {
    
    print(paste0("month: ", ith_month))
    
    outcome_var <- paste0("m12M", ith_month, "_case")
    f <- glue("{outcome_var} ~ .") %>% as.formula()
    
    
    # get the pattern and pattern remove for sliding windows
    month_start <- ith_month - 12
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
        
        month_start_1 == 0 & month_start_1 != month_end_remove_1 ~
            glue("^M([{month_start_2}-9]|{month_end_remove_1}[0-{month_end_remove_2}])_"),
        TRUE ~
            glue("^M({month_start_1}[{month_start_2}-9]|{month_end_remove_1}[0-{month_end_remove_2}])_")
    )

    # curate a temporary data frame for analysis
    tmp_data <- preprocessed_df %>% 
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

    # run Random Forest
    mycluster <- makeForkCluster(nnodes = 4)
    registerDoParallel(mycluster)
    
    iter_results <- foreach (iter = 1:100) %dopar% {
        
        set.seed(iter)
        splits <- initial_split(tmp_data, prop = 2/3, stata = vars(outcome_var))
        data_tr <- training(splits)
        data_te <- testing(splits)
        
        data_tr_folds <- vfold_cv(data_tr, v = 3, repeats = 1, strata = !!outcome_var)
        
        roc_res <- metric_set(roc_auc)
        
        # recipe
        master_recipe <- recipe(f, data = tmp_data) %>% 
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
    
    col_num <- ith_month - 12
    
    results_mat_rf[(col_num * 2 - 1):(col_num * 2), repeat_var] <- iter_results %>% 
        unlist() %>% 
        matrix(ncol = length(iter_results), byrow = FALSE) %>% 
        as_tibble(.name_repair = ~repeat_var)
}

# save cache
write_rds(results_mat_rf, here("cache", "data-cf_model-rf_desc-prediction_12mto6m.rds"))
# results_mat_rf <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_12mto6m.rds"))

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
    filename = "boxplot_model-rf_data-cf_desc-prediction_results_12mto6m.pdf"
)












    