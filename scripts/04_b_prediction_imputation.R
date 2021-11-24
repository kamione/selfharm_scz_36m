library(tidyverse)
library(tidymodels)
library(modeldata)
library(glue)
library(here)
library(haven)
library(rlang)
library(themis)
library(psych)
library(stringr)
library(snow)
library(ggthemes)


data <- here("data", "processed", "data-imputation.csv")  %>% 
    read_csv(col_types = cols()) %>% 
    select(Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse,
           M1_Pos:M36_Pos,
           M1_Neg:M36_Neg,
           M1_Aff:M36_Aff,
           M1_SOFAS:M36_SOFAS,
           M1_compliance:M36_compliance,
           M1_case:M36_case
    ) %>% 
    mutate(Sex = recode(Sex, "Male" = 1, "Female" = 2)) %>%
    mutate(Filter = recode(Filter, "EI" = 1, "SC" = 2)) %>%
    #mutate(Sex = factor(Sex, level = c(1, 2))) %>% 
    zap_labels() %>% 
    rowwise() %>% 
    mutate(
        m6M1_case = ifelse(sum(c_across(M1_case:M6_case)), 1, 0),
        m6M2_case = ifelse(sum(c_across(M2_case:M7_case)), 1, 0),
        m6M3_case = ifelse(sum(c_across(M3_case:M8_case)), 1, 0),
        m6M4_case = ifelse(sum(c_across(M4_case:M9_case)), 1, 0),
        m6M5_case = ifelse(sum(c_across(M5_case:M10_case)), 1, 0),
        m6M6_case = ifelse(sum(c_across(M6_case:M11_case)), 1, 0),
        m6M7_case = ifelse(sum(c_across(M7_case:M12_case)), 1, 0),
        m6M8_case = ifelse(sum(c_across(M8_case:M13_case)), 1, 0),
        m6M9_case = ifelse(sum(c_across(M9_case:M14_case)), 1, 0),
        m6M10_case = ifelse(sum(c_across(M10_case:M15_case)), 1, 0),
        m6M11_case = ifelse(sum(c_across(M11_case:M16_case)), 1, 0),
        m6M12_case = ifelse(sum(c_across(M12_case:M17_case)), 1, 0),
        m6M13_case = ifelse(sum(c_across(M13_case:M18_case)), 1, 0),
        m6M14_case = ifelse(sum(c_across(M14_case:M19_case)), 1, 0),
        m6M15_case = ifelse(sum(c_across(M15_case:M20_case)), 1, 0),
        m6M16_case = ifelse(sum(c_across(M16_case:M21_case)), 1, 0),
        m6M17_case = ifelse(sum(c_across(M17_case:M22_case)), 1, 0),
        m6M18_case = ifelse(sum(c_across(M18_case:M23_case)), 1, 0),
        m6M19_case = ifelse(sum(c_across(M19_case:M24_case)), 1, 0),
        m6M20_case = ifelse(sum(c_across(M20_case:M25_case)), 1, 0),
        m6M21_case = ifelse(sum(c_across(M21_case:M26_case)), 1, 0),
        m6M22_case = ifelse(sum(c_across(M22_case:M27_case)), 1, 0),
        m6M23_case = ifelse(sum(c_across(M23_case:M28_case)), 1, 0),
        m6M24_case = ifelse(sum(c_across(M24_case:M29_case)), 1, 0),
        m6M25_case = ifelse(sum(c_across(M25_case:M30_case)), 1, 0),
        m6M26_case = ifelse(sum(c_across(M26_case:M31_case)), 1, 0),
        m6M27_case = ifelse(sum(c_across(M27_case:M32_case)), 1, 0),
        m6M28_case = ifelse(sum(c_across(M28_case:M33_case)), 1, 0),
        m6M29_case = ifelse(sum(c_across(M29_case:M34_case)), 1, 0),
        m6M30_case = ifelse(sum(c_across(M30_case:M35_case)), 1, 0),
        m6M31_case = ifelse(sum(c_across(M31_case:M36_case)), 1, 0),
    ) %>% 
    ungroup()



for (i in 1:36) {
    label = paste0("M", i, "_case")
    data %>% 
        select(all_of(label)) %>% 
        sum() %>% 
        print()
}




# All information from past events

for (i in 1:31) {
    label = paste0("m6M", i, "_case")
    data %>% 
        select(all_of(label)) %>% 
        sum() %>% 
        print()
}


results_mat <- matrix(nrow = 31 * 2, ncol = 1000) %>%
    as_tibble() %>% 
    mutate(month = rep(1:31, each = 2), .before = V1) %>% 
    mutate(metrics = rep(c("accurary", "roauc"), times = 31), .before = V1)

for (ith_month in 1:31) {
    
    print(paste0("month: ", ith_month))
    
    outcome_var <- paste0("m6M", ith_month, "_case")
    f <- glue("{outcome_var} ~ .") %>% as.formula()
    
    if (ith_month == 1) {
        
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse, m6M1_case) %>% 
            mutate_at(all_of(outcome_var), factor)
        
    } else if (ith_month == 2) {
        
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse, starts_with("M1_"),
                   m6M2_case) %>% 
            mutate(m6M2_case = factor(m6M2_case, levels = c(1, 0), labels = c("Yes", "No")))
        
    } else if (ith_month >= 3 & ith_month <= 6) {
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse,
                   matches(glue("^M[1-{ith_month - 1}]_")),
                   all_of(outcome_var)) %>% 
            mutate_at(outcome_var, .funs = factor) %>%
            rowwise() %>% 
            mutate(
                mssd_pos = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Pos"))), na.rm=TRUE),
                mssd_neg = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Neg"))), na.rm=TRUE),
                mssd_aff = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Aff"))), na.rm=TRUE),
                mssd_sofas = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_SOFAS"))), na.rm=TRUE),
                mssd_comp =  mssd(c_across(matches(glue("M[1-{ith_month - 1}]_compliance"))), na.rm=TRUE)
            ) %>% 
            ungroup() %>% 
            select(-matches(glue("M[1-{ith_month - 2}]_")))
        
    } else if (ith_month >= 7) {
        
        
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
                
                month_start_1 == 0 & month_end_remove_1 == 0~
                    glue("^M([{month_start_2}-{month_end_remove_2}])_"),
                month_start_1 == month_end_remove_1 ~
                    glue("^M({month_start_1}[{month_start_2}-{month_end_remove_2}])_"),
                month_start_1 != month_end_remove_1 ~
                    glue("^M({month_start_1}[{month_start_2}-9]|{month_end_remove_1}[0-{month_end_remove_2}])_")
            )
        }
        
        data_tmp <- data %>% 
            select(Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse,
                   matches(pattern), all_of(outcome_var)) %>% 
            mutate_at(outcome_var, .funs = factor) %>%
            rowwise() %>% 
            mutate(
                mssd_pos = mssd(c_across(matches(glue("{pattern}Pos"))), na.rm = TRUE),
                mssd_neg = mssd(c_across(matches(glue("{pattern}Neg"))), na.rm = TRUE),
                mssd_aff = mssd(c_across(matches(glue("{pattern}Aff"))), na.rm = TRUE),
                mssd_sofas = mssd(c_across(matches(glue("{pattern}SOFAS"))), na.rm = TRUE),
                mssd_comp =  mssd(c_across(matches(glue("{pattern}compliance"))), na.rm = TRUE)
            ) %>% 
            ungroup() %>% 
            select(-matches(pattern_remove))
        
    }
    
    for (iter in 1:1000) {
        
        set.seed(iter)
        splits <- initial_split(data_tmp, prop = 2/3, stata = vars(outcome_var))
        data_tr <- training(splits)
        data_te <- testing(splits)
        
        master_recipe <- recipe(f, data = data_tr) %>% 
            #step_zv(all_numeric(), -all_outcomes()) %>% 
            #step_center(all_numeric(), -all_outcomes()) %>% 
            #step_scale(all_numeric(), -all_outcomes()) %>% 
            step_impute_knn(all_predictors(), neighbors = 10) %>% 
            step_smote(all_outcomes())
        
        logistic_mod <- logistic_reg() %>%
            # Set the engine
            set_engine("glm") %>%
            # Set the mode
            set_mode("classification")
        
        logistic_workflow <- 
            workflow() %>% 
            add_recipe(master_recipe) %>% 
            add_model(logistic_mod) 
        
        log_fit_tr <- logistic_workflow %>% 
            fit(data_tr)
        
        log_fit_te <- logistic_workflow %>%
            last_fit(splits)
        
        repeat_var <- paste0("V", iter)
        
        results_mat[(ith_month * 2 - 1):(ith_month * 2), repeat_var] <- 
            log_fit_te %>% collect_metrics() %>% pull(.estimate)
    }
}


fig <- results_mat %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>% 
    rowwise() %>% 
    mutate(mean = mean(V1:V10, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = month, y = value)) +
    geom_smooth(color = "tomato3", fill = "lightgray") + 
    geom_hline(yintercept = 0.5, color = "black", lty = "dashed") +
    labs(x = "Months of Prediction", y = "Area under the ROC Curve") +
    scale_x_continuous(breaks = seq(0, 32, 4)) +
    theme_pander() +
    theme(plot.margin = unit(rep(2, 4), "mm"))

ggsave(fig, width = 6, height = 4,
       filename = here("outputs", "figs", "data-imputation_prediction_results.pdf")
       )


boxplot_predictions <- results_mat %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>% 
    ggplot(aes(x = factor(month), y = value)) +
        geom_boxplot(fill = viridis(31), outlier.alpha = 0.25) +
        geom_smooth(method = "lm", aes(group=1), color = "tomato3") +
        geom_hline(yintercept = 0.5, color = "black", lty = "dashed") +
        labs(x = "Month of Prediction", y = "Area under the ROC Curve") +
        theme_pander() +
        theme(plot.margin = unit(rep(2, 4), "mm"))
ggsave(
    boxplot_predictions, width = 8, height = 4,
    filename = here("outputs", "figs", "boxplot_data-imputation_prediction_results.pdf")
)















# Information from past 6 months

# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}


master_lambda_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 100))

factor_binary <- function(x) {
    factor_vec <- factor(x) %>% relevel(ref = 1)
}


for (ith_month in 1:36) {
    print(paste0("month: ", ith_month))
    
    outcome_var <- paste0("m6M", ith_month, "_case")
    f <- glue("{outcome_var} ~ .") %>% as.formula()
    
    
    
    if (ith_month == 1) {
        
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse, m6M1_case) %>% 
            mutate_at(outcome_var, .funs = factor_binary)
        
    } else if (ith_month == 2) {
        
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse, starts_with("M1_"),
                   m6M2_case) %>% 
            mutate_at(outcome_var, .funs = factor)
        
    } else if (ith_month >= 3 & ith_month <= 10) {
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse,
                   matches(glue("^M[1-{ith_month - 1}]_")),
                   all_of(outcome_var)) %>% 
            mutate_at(outcome_var, .funs = factor) %>%
            rowwise() %>% 
            mutate(
                mssd_pos = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Poscf"))), na.rm=TRUE),
                mssd_neg = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Negcf"))), na.rm=TRUE),
                mssd_aff = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Aff"))), na.rm=TRUE),
                mssd_sofas = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_SOFAScf"))), na.rm=TRUE),
                mssd_comp =  mssd(c_across(matches(glue("M[1-{ith_month - 1}]_compliance"))), na.rm=TRUE)
            ) %>% 
            ungroup() %>% 
            select(-matches(glue("M[1-{ith_month - 2}]_")))
    } else if (ith_month >= 11) {
        
        if (ith ) {
            first_digit = 0
            second_digit = ith_month
        } else {
            first_digit = as.numeric(substr(ith_month - 1, 1, 1))
            second_digit = as.numeric(substr(ith_month - 1, 2, 2))
        }
        
        
        
        pattern = case_when(
            first_digit == 1 ~ glue("M([1-9]|1[0-{second_digit}])_"),
            first_digit == 2 ~ glue("M([1-9]|1[0-9]|2[0-{second_digit}])_"),
            first_digit == 3 ~ glue("M([1-9]|1[0-9]|2[0-9]]|3[0-{second_digit}])_")
        )
        
        first_digi_lag = as.numeric(substr(ith_month - 2, 1, 1))
        second_digit_lag = as.numeric(substr(ith_month - 2, 2, 2))
        
        pattern_removed = case_when(
            first_digit == 1 ~ glue("M([1-9]|1[0-{second_digit_lag}])_"),
            first_digit == 2 ~ glue("M([1-9]|1[0-9]|2[0-{second_digit_lag}])_"),
            first_digit == 3 ~ glue("M([1-9]|1[0-9]|2[0-9]]|3[0-{second_digit_lag}])_")
        )
        
        data_tmp <- data %>% 
            select(Sex, Ageat1stpre, Yrs_edu, Age_onset, DUP_days,
                   DUP_DSH_His, DUP_SS_His, sub_abuse,
                   matches(pattern),
                   all_of(outcome_var)) %>% 
            mutate_at(outcome_var, .funs = factor) %>%
            rowwise() %>% 
            mutate(
                mssd_pos = mssd(c_across(matches(glue("{pattern}Poscf"))), na.rm=TRUE),
                mssd_neg = mssd(c_across(matches(glue("{pattern}Negcf"))), na.rm=TRUE),
                mssd_aff = mssd(c_across(matches(glue("{pattern}Aff"))), na.rm=TRUE),
                mssd_sofas = mssd(c_across(matches(glue("{pattern}SOFAScf"))), na.rm=TRUE),
                mssd_comp =  mssd(c_across(matches(glue("{pattern}compliance"))), na.rm=TRUE)
            ) %>% 
            ungroup() %>% 
            select(-matches(pattern_removed))
        
    }
    
    set.seed(1234)
    splits <- initial_split(data_tmp, prop = 3/4, stata = vars(outcome_var))
    data_tr <- training(splits)
    data_te <- testing(splits)
    
    set.seed(1234)
    data_tr_cv <- vfold_cv(data_tr, v = 3, repeats = 10, strata = all_of(outcome_var))
    
    master_recipe <- recipe(f, data = data_tr) %>% 
        #step_zv(all_numeric(), -all_outcomes()) %>% 
        #step_center(all_numeric(), -all_outcomes()) %>% 
        #step_scale(all_numeric(), -all_outcomes()) %>% 
        step_impute_knn(all_predictors(), neighbors = 5) %>% 
        step_smote(all_outcomes())
    
    ridge_mod <- logistic_reg(penalty = tune(), mixture = 0) %>% 
        set_engine("glmnet") %>% 
        set_mode("classification")
    
    ridge_workflow <- workflow() %>% 
        add_model(ridge_mod) %>% 
        add_recipe(master_recipe)
    
    # hyperparameter tuning
    #cluster <- parallel::makeCluster(8)
    #registerDoParallel(cluster)
    
    tic()
    #cl <- makeCluster(4) 
    ridge_fit_tr <- ridge_workflow %>% 
        tune_grid(grid = master_lambda_grid,
                  resamples = data_tr_cv,
                  control = control_grid(save_pred = TRUE),
                  metrics = metric_set(roc_auc, pr_auc, accuracy))
    #topCluster(cluster)
    #unregister()
    #stopCluster(cl)
    toc()
    
    ridge_best_lambda <- ridge_fit_tr %>% select_best("roc_auc")
    
    ridge_tuning_plot <- ridge_fit_tr %>% 
        collect_metrics() %>% 
        filter(.metric == "roc_auc") %>% 
        ggplot(aes(x = penalty, y = mean)) + 
        geom_point() + 
        geom_line() + 
        geom_vline(xintercept = ridge_best_lambda$penalty, color = "tomato3") +
        labs(x = "Penalty", y = "Area under the ROC Curve") +
        scale_x_log10(labels = scales::label_number()) +
        theme_pander()
    
    # model testing
    ridge_workflow_sel_param <- ridge_workflow %>%
        finalize_workflow(ridge_best_lambda)
    
    ridge_fit_te <- ridge_workflow_sel_param %>%
        last_fit(splits)
    
    ridge_y_te_pred <- ridge_fit_te %>% 
        collect_predictions
    
    conf_mat(ridge_y_te_pred, truth = all_of(outcome_var), estimate = .pred_class)
    ridge_roc_auc <- roc_auc(ridge_y_te_pred, truth = all_of(outcome_var), estimate = .pred_class)
    print(ridge_roc_auc)
    sens(ridge_y_te_pred, all_of(outcome_var), .pred_class) %>% print()
    spec(ridge_y_te_pred, all_of(outcome_var), .pred_class) %>% print()
}


