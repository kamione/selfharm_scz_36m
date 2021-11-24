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
library(viridis)

data <- here("data", "processed", "cf_selfharm_longitudinal.csv")  %>% 
    read_csv(col_types = cols()) %>% 
    select(Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse,
           M1_Poscf:M36_Poscf,
           M1_Negcf:M36_Negcf,
           M1_Aff:M36_Aff,
           M1_SOFAScf:M36_SOFAScf,
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
                mssd_pos = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Poscf"))), na.rm=TRUE),
                mssd_neg = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Negcf"))), na.rm=TRUE),
                mssd_aff = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_Aff"))), na.rm=TRUE),
                mssd_sofas = mssd(c_across(matches(glue("M[1-{ith_month - 1}]_SOFAScf"))), na.rm=TRUE),
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
                mssd_pos = mssd(c_across(matches(glue("{pattern}Poscf"))), na.rm = TRUE),
                mssd_neg = mssd(c_across(matches(glue("{pattern}Negcf"))), na.rm = TRUE),
                mssd_aff = mssd(c_across(matches(glue("{pattern}Aff"))), na.rm = TRUE),
                mssd_sofas = mssd(c_across(matches(glue("{pattern}SOFAScf"))), na.rm = TRUE),
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


write_rds(results_mat, path = here("cache", "data-cf_desc-prediction.rds"))

# results_mat <- read_rds(here("cache", "data-cf_desc-prediction.rds"))

# check out the results
results_mat %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>% 
    View()

results_mat %>% 
    filter(metrics == "accurary") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE)) %>% 
    View()

# plot the results using area under the ROC curve
boxplot_predictions <- results_mat %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>% 
    ggplot(aes(x = factor(month), y = value)) +
    geom_boxplot(fill = viridis(31), outlier.alpha = 0.25) +
    geom_smooth(method = "lm", aes(group=1), color = "tomato3") +
    geom_hline(yintercept = 0.5, color = "black", lty = "dashed") +
    labs(x = "Month of Prediction", y = "Area under the ROC Curve") +
    theme_pander() +
    theme(plot.margin = unit(rep(2, 5), "mm"))
ggsave(
    boxplot_predictions, width = 6, height = 4,
    filename = here("outputs", "figs", "boxplot_data-cf_desc-prediction_results.pdf")
)
