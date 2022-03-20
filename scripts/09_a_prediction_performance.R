# Environment ------------------------------------------------------------------
library(ggpubr)

source(here("src", "R", "visualization.R"))


# Visualization ----------------------------------------------------------------
results_mat_rf_6mto6m <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_6mto6m.rds"))
results_mat_rf_6mto12m <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_6mto12m.rds"))
results_mat_rf_12mto6m <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_12mto6m.rds"))
results_mat_rf_12mto12m <- read_rds(here("cache", "data-cf_model-rf_desc-prediction_12mto12m.rds"))

boxplot_rf_6mto6m <- plot_timeseries_boxplots(results_mat_rf_6mto6m)
boxplot_rf_6mto12m <- plot_timeseries_boxplots(results_mat_rf_6mto12m)
boxplot_rf_12mto6m <- plot_timeseries_boxplots(results_mat_rf_12mto6m)
boxplot_rf_12mto12m <- plot_timeseries_boxplots(results_mat_rf_12mto12m)

ggarrange(boxplot_rf_6mto6m, boxplot_rf_6mto12m, boxplot_rf_12mto6m, boxplot_rf_12mto12m,
          ncol = 2, nrow = 2, labels = LETTERS[1:4]) %>% 
    ggexport(filename = here("outputs", "figs", "summary_boxplots_model-rf_desc_prediction.pdf"),
             width = 14, height = 9)

             