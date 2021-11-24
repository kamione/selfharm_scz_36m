# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(ggpubr)
library(gtsummary)
library(glue)
library(skimr)
library(psych)
library(mice)

options(skimr_strip_metadata = FALSE)

# Data IO  ---------------------------------------------------------------------
data <- here("data", "raw", "baseline_Mortality_new.sav") %>%
    read_spss()

mnortalityinfo_df <- here("data", "raw", "Mortality SPSS_Matched1234.sav") %>% 
    read_spss() %>% 
    zap_labels() %>%
    zap_label() %>% 
    zap_formats() %>%
    zap_widths() %>% 
    rename("HCS_code" = "HCScode")

data_filtered <- data %>% 
    zap_labels() %>%
    zap_label() %>% 
    zap_formats() %>%
    zap_widths() %>% 
    mutate(decease_mo = na_if(decease_mo, 999)) %>% 
    filter(is.na(decease_mo)) %>%  # remove participants who decease
    select(Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse,
           M1_Pos:M36_Pos, M1_Neg:M36_Neg, M1_Aff:M36_Aff, M1_SOFAS:M36_SOFAS,
           M1_compliance:M36_compliance, M1_dsh:M36_dsh, M1_SS:M36_SS)

data_imputated <- mice(data_filtered, m = 5, seed = 1234) %>% complete()
    
data_final <- data_imputated %>% 
    rowwise() %>% 
    mutate(n_total_dsh = sum(c_across(c(M1_dsh:M36_dsh, M1_SS:M36_SS)), na.rm = TRUE), .after = "Sex") %>%
    mutate(
        mean_36m_pos = mean(c_across(M1_Pos:M36_Pos), na.rm = TRUE),
        mean_36m_neg = mean(c_across(M1_Neg:M36_Neg), na.rm = TRUE),
        mean_36m_aff = mean(c_across(M1_Aff:M36_Aff), na.rm = TRUE),
        mean_36m_sofas = mean(c_across(M1_SOFAS:M36_SOFAS), na.rm = TRUE),
        mean_36m_compliance = mean(c_across(M1_compliance:M36_compliance), na.rm = TRUE),
        mssd_pos = mssd(c_across(M1_Pos:M36_Pos)),
        mssd_neg = mssd(c_across(M1_Neg:M36_Neg)),
        mssd_aff = mssd(c_across(M1_Aff:M36_Aff)),
        mssd_sofas = mssd(c_across(M1_SOFAS:M36_SOFAS)),
        mssd_compliance = mssd(c_across(M1_compliance:M36_compliance))) %>% 
    ungroup() %>% 
    mutate(sh_case = if_else(n_total_dsh > 0, 1, 0)) %>% 
    mutate(sh_case = factor(sh_case, levels = c(0, 1), labels = c("Without DSH/SS", "With DSH/SS"))) %>% 
    mutate(Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female"))) %>% 
    mutate(Filter = factor(Filter, levels = c(0, 1), labels = c("SC", "EI"))) %>% 
    select(-n_total_dsh)
    


# generate monthly status of dsh/SS, 1 = YES, 0 = NO
data_final_copy <- data_final
 
for (n in 1:36) {
    variable_name <- glue("M{n}_case")
    data_final_copy <- data_final_copy %>%
        mutate("M{n}_case_no" :=
            rowSums(
                select(., glue("M{n}_dsh"), glue("M{n}_SS")), na.rm = TRUE)
            ) %>% 
        mutate("M{n}_case" := if_else(across(glue("M{n}_case_no")) > 0, 1, 0)) %>% 
        select(-glue("M{n}_case_no"))
} 

data_for_analysis <- data_final %>% 
    select(sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days, DUP_DSH_His,
           DUP_SS_His, sub_abuse, mean_36m_pos:mssd_compliance)

data_for_analysis %>% 
    tbl_summary(
        by = sh_case,
        missing = "no",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "DSH in DUP",
            DUP_SS_His ~ "SS in DUP",
            sub_abuse ~ "Substance Abuse",
            mean_36m_pos ~ "Pos. Symptom (36m Mean)",
            mean_36m_neg ~ "Neg. Symptoms (36m Mean)",
            mean_36m_aff ~ "Aff. Symptoms (36m Mean)",
            mean_36m_sofas ~ "SOFAS (36m Mean)",
            mean_36m_compliance ~ "Compliance (36m Mean)",
            mssd_pos ~ "Pos. Symptom (MSSD)",
            mssd_neg ~ "Neg. Symptoms (MSSD)",
            mssd_aff ~ "Aff. Symptoms (MSSD)",
            mssd_sofas ~ "SOFAS (MSSD)",
            mssd_compliance ~ "Compliance (MSSD)"
        ) 
    ) %>% 
    add_p() %>% 
    add_q(method = "fdr") %>% 
    bold_p(t = 0.05, q = TRUE) %>% 
    bold_labels() %>% 
    as_gt() %>% 
    gt::gtsave(filename = here("outputs", "tables", "table_data-imputation_comparison-dsh.html"))

# write clean_data
write_csv(data_final_copy, here("data", "processed", "data-imputation.csv"))
