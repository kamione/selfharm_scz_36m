# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(jtools)

# page setup for table to docx
sect_properties <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 7, height = 12),
    type = "continuous",
    page_margins = page_mar(gutter = 0)
)


# Data IO  ---------------------------------------------------------------------
preprocessed_df <- here("data", "processed", "data-imp_decs-selfharm_longitudinal.rds") %>% 
    read_rds() %>% 
    select(sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days,
           DUP_DSH_His, DUP_SS_His, sub_abuse, baseline_sa, Smoker4, Schiz,
           EP1_hosp, dur_adm1, mean_36m_pos:mssd_compliance)

# Regression -------------------------------------------------------------------
m0_fit <- preprocessed_df %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter + Yrs_edu + Age_onset +
            DUP_days + DUP_DSH_His + DUP_SS_His + sub_abuse + 
            baseline_sa + Smoker4 + Schiz + EP1_hosp + dur_adm1,
        family = binomial(link="logit"))

m1_fit <- preprocessed_df %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter + Yrs_edu + Age_onset +
            DUP_days + DUP_DSH_His + DUP_SS_His + sub_abuse + 
            baseline_sa + Smoker4 + Schiz + EP1_hosp + dur_adm1 + 
            mean_36m_pos + mean_36m_neg + mean_36m_aff + mean_36m_sofas + 
            mean_36m_compliance,
        family = binomial(link="logit"))

m2_fit <- preprocessed_df %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter + Yrs_edu + Age_onset +
            DUP_days + DUP_DSH_His + DUP_SS_His + sub_abuse + 
            baseline_sa + Smoker4 + Schiz + EP1_hosp + dur_adm1 +
            mssd_pos + mssd_neg + mssd_aff + mssd_sofas + mssd_compliance,
        family = binomial(link="logit"))

m0_fit_table <- m0_fit %>% 
    tbl_regression(
        add_estimate_to_reference_rows = TRUE,
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "DSH in DUP",
            DUP_SS_His ~ "SS in DUP",
            sub_abuse ~ "Substance Abuse (Lifetime)",
            baseline_sa ~ "Substance Abuse (Baseline)",
            Smoker4 ~ "Current Smoker",
            Schiz ~ "Diagnosis",
            EP1_hosp ~ "Hospitalization at Onset",
            dur_adm1 ~ "Days of 1st Hospitalization Admission"
        )
    ) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE) %>% 
    bold_p(q = TRUE) %>% 
    modify_footnote(
        label ~ paste(
            "Abbreviations:",
            "DUP = Duration of Untreated Psychosis;",
            "DSH = Deliberated Self-harm;",
            "SA = Suicidal Attempts;",
            "Pos = Positive;",
            "Neg = Negative;",
            "Dep = Depressive;",
            "SOFAS = Social and Occupational Functioning Assessment Scale;",
            "MSSD = Mean of the Squared Successive Differences",
            sep = " "
        )
    )

m1_fit_table <- m1_fit %>% 
    tbl_regression(
        add_estimate_to_reference_rows = TRUE,
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "DSH in DUP",
            DUP_SS_His ~ "SS in DUP",
            sub_abuse ~ "Substance Abuse (Lifetime)",
            baseline_sa ~ "Substance Abuse (Baseline)",
            Smoker4 ~ "Current Smoker",
            Schiz ~ "Diagnosis",
            EP1_hosp ~ "Hospitalization at Onset",
            dur_adm1 ~ "Days of 1st Hospitalization Admission",
            mean_36m_pos ~ "Pos. Symptom (36m Mean)",
            mean_36m_neg ~ "Neg. Symptom (36m Mean)",
            mean_36m_aff ~ "Dep. Symptom (36m Mean)",
            mean_36m_sofas ~ "SOFAS (36m Mean)",
            mean_36m_compliance ~ "Compliance (36m Mean)"
        )
    ) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE) %>% 
    bold_p(q = TRUE) %>% 
    modify_footnote(
        label ~ paste(
            "Abbreviations:",
            "DUP = Duration of Untreated Psychosis;",
            "DSH = Deliberated Self-harm;",
            "SA = Suicidal Attempts;",
            "Pos = Positive;",
            "Neg = Negative;",
            "Dep = Depressive;",
            "SOFAS = Social and Occupational Functioning Assessment Scale;",
            "MSSD = Mean of the Squared Successive Differences",
            sep = " "
        )
    )

m2_fit_table <- m2_fit %>% 
    tbl_regression(
        add_estimate_to_reference_rows = TRUE,
        label = list(
            Ageat1stpre ~ "Age (Baseline)",
            Yrs_edu ~ "Education Years", 
            Age_onset ~ "Age of Onset",
            Filter ~ "Treatment",
            DUP_days ~ "DUP (Days)",
            DUP_DSH_His ~ "DSH in DUP",
            DUP_SS_His ~ "SS in DUP",
            sub_abuse ~ "Substance Abuse (Lifetime)",
            baseline_sa ~ "Substance Abuse (Baseline)",
            Smoker4 ~ "Current Smoker",
            Schiz ~ "Diagnosis",
            EP1_hosp ~ "Hospitalization at Onset",
            dur_adm1 ~ "Days of 1st Hospitalization Admission",
            mssd_pos ~ "Pos. Symptom (MSSD)",
            mssd_neg ~ "Neg. Symptom (MSSD)",
            mssd_aff ~ "Dep. Symptom (MSSD)",
            mssd_sofas ~ "SOFAS (MSSD)",
            mssd_compliance ~ "Compliance (MSSD)"
        )
    ) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE) %>% 
    bold_p(q = TRUE) %>% 
    modify_footnote(
        label ~ paste(
            "Abbreviations:",
            "DUP = Duration of Untreated Psychosis;",
            "DSH = Deliberated Self-harm;",
            "SA = Suicidal Attempts;",
            "Pos = Positive;",
            "Neg = Negative;",
            "Dep = Depressive;",
            "SOFAS = Social and Occupational Functioning Assessment Scale;",
            "MSSD = Mean of the Squared Successive Differences",
            sep = " "
        )
    )

table_regresson_all_models <- tbl_merge(
    tbls = list(m0_fit_table, m1_fit_table, m2_fit_table),
    tab_spanner = c("**M0**", "**M1**", "**M2**")
) 

table_regresson_all_models %>%
    as_gt() %>% 
    gt::gtsave(filename = here("outputs", "tables", "data-imp_desc-models_comparison.html"))

table_regresson_all_models %>%
    as_flex_table() %>% 
    flextable::bold(bold = TRUE, part = "header") %>% 
    flextable::fontsize(size = 10, part = "all") %>% 
    flextable::autofit() %>% 
    flextable::save_as_docx(
        path = here("outputs", "tables", "data-imp_desc-models_comparison.docx"),
        pr_section = sect_properties
    )

# check model fitness
summ(m0_fit)
summ(m1_fit)
summ(m2_fit)

anova(m0_fit, m1_fit, test = "Chisq")
anova(m0_fit, m2_fit, test = "Chisq")

# check variance inflation factor
car::vif(m0_fit)
car::vif(m1_fit)
car::vif(m2_fit)

