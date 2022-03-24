# Environment ------------------------------------------------------------------
library(ggpubr)
library(flextable)
library(officer)

source(here("src", "R", "visualization.R"))

# page setup for table to docx
sect_properties_portrait <- prop_section(
    page_size = page_size(orient = "portrait",
                          width = 21/2.54, height = 29.7/2.54),
    type = "continuous",
    page_margins = page_mar(gutter = 0)
)

sect_properties_landscape <- prop_section(
    page_size = page_size(orient = "landscape",
                          width = 29.7/2.54, height = 21/2.54),
    type = "continuous",
    page_margins = page_mar(gutter = 0)
)



# Data I/O ---------------------------------------------------------------------
results_mat_rf_6mto6m <- read_rds(here("cache", "data-imp_model-rf_desc-prediction_6mto6m.rds"))
results_mat_rf_6mto12m <- read_rds(here("cache", "data-imp_model-rf_desc-prediction_6mto12m.rds"))
results_mat_rf_12mto6m <- read_rds(here("cache", "data-imp_model-rf_desc-prediction_12mto6m.rds"))
results_mat_rf_12mto12m <- read_rds(here("cache", "data-imp_model-rf_desc-prediction_12mto12m.rds"))


# Visualization ----------------------------------------------------------------
boxplot_rf_6mto6m <- plot_timeseries_boxplots(results_mat_rf_6mto6m)
boxplot_rf_6mto12m <- plot_timeseries_boxplots(results_mat_rf_6mto12m)
boxplot_rf_12mto6m <- plot_timeseries_boxplots(results_mat_rf_12mto6m)
boxplot_rf_12mto12m <- plot_timeseries_boxplots(results_mat_rf_12mto12m)

ggarrange(boxplot_rf_6mto6m, boxplot_rf_6mto12m, boxplot_rf_12mto6m, boxplot_rf_12mto12m,
          ncol = 2, nrow = 2, labels = LETTERS[1:4]) %>% 
    ggexport(filename = here("outputs", "figs", "summary_boxplots_data-imp_model-rf_desc_prediction.pdf"),
             width = 14, height = 9)

# Tables -----------------------------------------------------------------------
summary_6mto6m_df <- results_mat_rf_6mto6m %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE))
summary_6mto12m_df <- results_mat_rf_6mto12m %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE))
summary_12mto6m_df <- results_mat_rf_12mto6m %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE))
summary_12mto12m_df <- results_mat_rf_12mto12m %>% 
    filter(metrics == "roauc") %>% 
    pivot_longer(!c(month, metrics), values_to = "value") %>%
    group_by(month) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE))

all_models_df <- summary_6mto6m_df %>% 
    left_join(summary_6mto12m_df, by = "month") %>% 
    left_join(summary_12mto6m_df, by = "month") %>% 
    left_join(summary_12mto12m_df, by = "month") %>% 
    mutate_if(is.numeric, round, 2)



table_all_models <- flextable(all_models_df) %>% 
    set_header_labels(month = "Month", mean.x = "Mean", mean.x.x = "Mean",
                      mean.y = "Mean", mean.y.y = "Mean", sd.x = "SD", 
                      sd.x.x = "SD", sd.y = "SD", sd.y.y = "SD") %>% 
    add_header_row(
        colwidths = c(1, 2, 2, 2, 2),
        values = c("", "6m to 6m", "6m to 12m", "12m to 6m", "12m to 12m")
    ) %>% 
    theme_vanilla() %>% 
    add_footer_lines("") %>% 
    align(align = "center", part = "all")

save_as_docx(
    table_all_models,
    pr_section = sect_properties_portrait,
    path = here("outputs", "tables", "data-imp_model-rf_desc-all_perfromances_selfharm_events.docx")
)
