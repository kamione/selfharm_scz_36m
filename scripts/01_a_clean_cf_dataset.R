# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(ggpubr)
library(gtsummary)
library(glue)
library(skimr)
library(officer)
mssd <- psych::mssd

# page setup for table to docx
sect_properties <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 9, height = 12),
    type = "continuous",
    page_margins = page_mar(gutter = 0)
)

source(here("src", "R", "tables.R"))


# Data IO  ---------------------------------------------------------------------
main_df <- here("data", "raw", "baseline_Mortality_new.sav") %>%
    read_spss() %>% 
    zap_labels() %>%
    zap_label() %>% 
    zap_formats() %>%
    zap_widths()

supp_df <- here("data", "raw", "Mortality SPSS_Matched1234.sav") %>% 
    read_spss() %>% 
    zap_labels() %>%
    zap_label() %>% 
    zap_formats() %>%
    zap_widths() %>% 
    rename("HCS_code" = "HCScode") %>% 
    select(HCS_code, Mortality, EP1_hosp, dur_adm1)


# Preprocessing ----------------------------------------------------------------
sh_case_df <- main_df %>% 
    mutate(decease_mo = na_if(decease_mo, 999)) %>% 
    # remove participants who decease
    filter(is.na(decease_mo)) %>%  
    rename("baseline_sa" = "M1_sa") %>% 
    mutate(baseline_sa = if_else(baseline_sa == 888, 0, 1)) %>% 
    left_join(supp_df, by = "HCS_code") %>% 
    mutate(
        Sex = factor(Sex, 
                     levels = c(1, 2), 
                     labels = c("Male", "Female"))
    ) %>%
    mutate(
        Filter = factor(Filter, 
                        levels = c(0, 1), 
                        labels = c("Standard Care", "Early Intervention"))
    ) %>% 
    mutate(
        Schiz = factor(Schiz, 
                       levels = c(0, 1), 
                       labels = c("Other Schizophrenia Spectrum", "Schizophrenia"))
    ) %>% 
    rowwise() %>%
    mutate(is_dsh = ifelse(sum(c_across(M1_dsh:M36_dsh), na.rm = TRUE), 1, 0)) %>% 
    mutate(is_ss = ifelse(sum(c_across(M1_SS:M36_SS), na.rm = TRUE), 1, 0)) %>% 
    mutate(sh_category = is_dsh + is_ss * 2) %>% 
    mutate(
        n_total_dsh = sum(c_across(c(M1_dsh:M36_dsh, M1_SS:M36_SS)), na.rm = TRUE), 
        .after = "Sex"
    ) %>%
    mutate(
        mean_36m_pos = mean(c_across(M1_Poscf:M36_Poscf), na.rm = TRUE),
        mean_36m_neg = mean(c_across(M1_Negcf:M36_Negcf), na.rm = TRUE),
        mean_36m_aff = mean(c_across(M1_Aff:M36_Aff), na.rm = TRUE),
        mean_36m_sofas = mean(c_across(M1_SOFAScf:M36_SOFAScf), na.rm = TRUE),
        mean_36m_compliance = mean(c_across(M1_compliance:M36_compliance), na.rm = TRUE),
        mssd_pos = mssd(c_across(M1_Poscf:M36_Poscf), na.rm = TRUE),
        mssd_neg = mssd(c_across(M1_Negcf:M36_Negcf), na.rm = TRUE),
        mssd_aff = mssd(c_across(M1_Aff:M36_Aff), na.rm = TRUE),
        mssd_sofas = mssd(c_across(M1_SOFAScf:M36_SOFAScf), na.rm = TRUE),
        mssd_compliance = mssd(c_across(M1_compliance:M36_compliance), na.rm = TRUE),
    ) %>% 
    ungroup() %>% 
    mutate(sh_case = if_else(n_total_dsh > 0, 1, 0)) %>% 
    mutate(
        sh_case = factor(sh_case,
                         levels = c(0, 1),
                         labels = c("Without DSH", "With DSH"))
    ) %>% 
    # drop subjects with NA in baseline variables
    drop_na(sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
            DUP_DSH_His, DUP_SS_His, sub_abuse)

# write the table
table_dsh_group_comparison <- sh_case_df %>% 
    select(sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
           EP1_hosp, dur_adm1, mean_36m_pos:mssd_compliance) %>% 
    tbl_summary(
        by = sh_case,
        missing = "no",
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "NSSI in DUP",
            DUP_SS_His ~ "SA in DUP",
            Smoker4 ~ "Current Smoker",
            Schiz ~ "Diagnosis",
            sub_abuse ~ "Illicit Substance Use (Lifetime)",
            baseline_sa ~ "Substance Abuse (Baseline)",
            EP1_hosp ~ "Hospitalization at Onset",
            dur_adm1 ~ "Days of 1st Hospitalization Admission",
            mean_36m_pos ~ "Pos. Symptom (36m Mean)",
            mean_36m_neg ~ "Neg. Symptom (36m Mean)",
            mean_36m_aff ~ "Dep. Symptom (36m Mean)",
            mean_36m_sofas ~ "SOFAS (36m Mean)",
            mean_36m_compliance ~ "Medication Adherence (36m Mean)",
            mssd_pos ~ "Pos. Symptom (36m MSSD)",
            mssd_neg ~ "Neg. Symptom (36m MSSD)",
            mssd_aff ~ "Dep. Symptom (36m MSSD)",
            mssd_sofas ~ "SOFAS (36m MSSD)",
            mssd_compliance ~ "Medication Adherence (36m MSSD)"
        )
    ) %>% 
    add_p() %>% 
    add_q(method = "fdr") %>% 
    bold_p(t = 0.05, q = TRUE) %>% 
    bold_labels() %>% 
    modify_footnote(
        label ~ paste(
            "Abbreviations:",
            "DUP = Duration of Untreated Psychosis;",
            "NSSI = Non-Suicidal Self-Injury;",
            "SA = Suicidal Attempts;",
            "Pos = Positive;",
            "Neg = Negative;",
            "Dep = Depressive;",
            "SOFAS = Social and Occupational Functioning Assessment Scale;",
            "MSSD = Mean of the Squared Successive Differences",
            sep = " "
        )
    )


# save table
table_dsh_group_comparison %>% 
    as_gt() %>% 
    gt::gtsave(filename = here("outputs", "tables", "data-cf_desc-dsh_group_comparison.html"))

table_dsh_group_comparison %>% 
    as_flex_table() %>% 
    flextable::bold(bold = TRUE, part = "header") %>% 
    flextable::fontsize(size = 10, part = "all") %>% 
    flextable::save_as_docx(
        path = here("outputs", "tables", "data-cf_desc-dsh_group_comparison.docx"),
        pr_section = sect_properties
    )

# comparing the self harm type
table_harm_category_comparison <- sh_case_df %>% 
    filter(sh_category != 0) %>% 
    mutate(
        sh_category = factor(sh_category, 
                             levels = 1:3, 
                             labels = c("NSSI", "SA", "Both"))
    ) %>% 
    select(sh_category, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
           EP1_hosp, dur_adm1, mean_36m_pos:mssd_compliance) %>% 
    tbl_summary(
        by = sh_category,
        missing = "no",
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "NSSI in DUP",
            DUP_SS_His ~ "SA in DUP",
            Smoker4 ~ "Current Smoker",
            Schiz ~ "Diagnosis",
            sub_abuse ~ "Illicit Substance Use (Lifetime)",
            baseline_sa ~ "Substance Abuse (Baseline)",
            EP1_hosp ~ "Hospitalization at Onset",
            dur_adm1 ~ "Days of 1st Hospitalization Admission",
            mean_36m_pos ~ "Pos. Symptom (36m Mean)",
            mean_36m_neg ~ "Neg. Symptom (36m Mean)",
            mean_36m_aff ~ "Dep. Symptom (36m Mean)",
            mean_36m_sofas ~ "SOFAS (36m Mean)",
            mean_36m_compliance ~ "Medication Adherence (36m Mean)",
            mssd_pos ~ "Pos. Symptom (36m MSSD)",
            mssd_neg ~ "Neg. Symptom (36m MSSD)",
            mssd_aff ~ "Dep. Symptom (36m MSSD)",
            mssd_sofas ~ "SOFAS (36m MSSD)",
            mssd_compliance ~ "Medication Adherence (36m MSSD)"
        )
    ) %>% 
    add_p() %>% 
    add_q(method = "fdr") %>% 
    bold_p(t = 0.05, q = TRUE) %>% 
    bold_labels() %>% 
    add_stat(everything() ~ add_stat_pairwise) %>% 
    modify_footnote(
        label ~ paste(
            "Abbreviations:",
            "DUP = Duration of Untreated Psychosis;",
            "NSSI = Non-Suicidal Self-Injury;",
            "SA = Suicidal Attempts;",
            "Pos = Positive;",
            "Neg = Negative;",
            "Dep = Depressive;",
            "SOFAS = Social and Occupational Functioning Assessment Scale;",
            "MSSD = Mean of the Squared Successive Differences",
             sep = " "
        )
    )

table_harm_category_comparison %>% 
    as_gt() %>% 
    gt::gtsave(filename = here("outputs", "tables", "data-cf_desc-self_category_comparison.html"))

table_harm_category_comparison %>% 
    as_flex_table() %>% 
    flextable::bold(bold = TRUE, part = "header") %>% 
    flextable::fontsize(size = 10, part = "all") %>% 
    flextable::save_as_docx(
        path = here("outputs", "tables", "data-cf_desc-self_category_comparison.docx"),
        pr_section = sect_properties
    )


# generate monthly status of dsh/SS, 1 = YES, 0 = NO
preprocessed_df <- sh_case_df
for (n in 1:36) {
    variable_name <- glue("M{n}_case")
    preprocessed_df <- preprocessed_df %>%
        mutate("M{n}_case_no" :=
                   rowSums(
                       select(., glue("M{n}_dsh"), glue("M{n}_SS")), na.rm = TRUE)
        ) %>% 
        mutate("M{n}_case" := if_else(across(glue("M{n}_case_no")) > 0, 1, 0)) %>% 
        select(-glue("M{n}_case_no"))
}


# write clean_data in csv and rds formats
write_csv(preprocessed_df, here("data", "processed", "cf_selfharm_longitudinal.csv"))
write_rds(preprocessed_df, here("data", "processed", "cf_selfharm_longitudinal.rds"))

