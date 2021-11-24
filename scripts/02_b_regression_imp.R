# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(gtsummary)
library(jtools)


# Data IO  ---------------------------------------------------------------------
data <- here("data", "processed", "data-imputation.csv") %>% 
    read_csv(col_types = cols()) %>% 
    select(sh_case, Ageat1stpre, Sex, Filter, Yrs_edu, Age_onset, DUP_days, DUP_DSH_His,
           DUP_SS_His, sub_abuse, mean_36m_pos:mssd_compliance) %>% 
    mutate(sh_case = factor(sh_case),
           Sex = factor(Sex),
           Filter = factor(Filter))


# Regression -------------------------------------------------------------------
m0_fit <- data %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter + Yrs_edu + Age_onset +
            DUP_days + DUP_DSH_His + DUP_SS_His + sub_abuse,
        family = binomial(link="logit"))
summ(m0_fit)

m1_fit <- data %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter +Yrs_edu + Age_onset +
            DUP_days + DUP_DSH_His + DUP_SS_His + sub_abuse + mean_36m_pos +
            mean_36m_neg + mean_36m_aff + mean_36m_sofas + mean_36m_compliance,
        family = binomial(link="logit"))
summ(m1_fit)

m2_fit <- data %>% 
    glm(formula = sh_case ~ Ageat1stpre + Sex + Filter + Yrs_edu + Age_onset + DUP_days +
            DUP_DSH_His + DUP_SS_His + sub_abuse + mssd_pos + mssd_neg +
            mssd_aff + mssd_sofas + mssd_compliance,
        family = binomial(link="logit"))

anova(m0_fit, m1_fit, test = "Chisq")


m0_fit_table <- m0_fit %>% 
    tbl_regression(add_estimate_to_reference_rows = TRUE) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE)

m1_fit_table <- m1_fit %>% 
    tbl_regression(add_estimate_to_reference_rows = TRUE) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE) %>% 
    bold_p(q = TRUE)

m2_fit_table <- m2_fit %>% 
    tbl_regression(add_estimate_to_reference_rows = TRUE) %>% 
    add_q(method = "fdr",
          pvalue_fun = function(x) style_pvalue(x, digits = 2),
          quiet = TRUE) %>% 
    bold_p(q = TRUE)


tbl_merge(
    tbls = list(m0_fit_table, m1_fit_table, m2_fit_table),
    tab_spanner = c("**M0**", "**M1**", "**M2**")
) %>%
    as_gt() %>% 
    gt::gtsave(filename = here("outputs", "tables", "table_data-imputation_models_comparison.html"))


# check model fitness
summ(m0_fit)
summ(m1_fit)
summ(m2_fit)

anova(m0_fit, m1_fit, test = "Chisq")
anova(m0_fit, m2_fit, test = "Chisq")
anova(m1_fit, m2_fit, test = "Chisq")

