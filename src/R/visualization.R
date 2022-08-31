# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(glue)
library(ggplot2)
library(ggpubr)
library(viridis)

# Internal Functions -----------------------------------------------------------


# External Functions -----------------------------------------------------------
plot_timeseries_boxplots <- function(dat, saveplot = FALSE, filename = NULL) {
    
    num_of_month <- nrow(dat %>% filter(metrics == "roauc"))
    
    first_month <- dat %>% pull(month) %>% first()
    last_month <- dat %>% pull(month) %>% last()
    
    if (first_month == 7) {
        title_label <- "Using Prior 6 Month Information"
    } else if (first_month == 13) {
        title_label <- "Using Prior 12 Month Information"
    } else {
        stop("The start month is not correct!")
    }
    
    if (last_month == 25) {
        x_axis_label <- "Month of Prediction (12 Month Prediction Window)"
    } else if (last_month == 31) {
        x_axis_label <- "Month of Prediction (6 Month Prediction Window)"
    } else {
        stop("The last month is not correct!")
    }
    
    
    # pivot to long data frame
    dat_long <- dat %>% 
        filter(metrics == "roauc") %>% 
        pivot_longer(!c(month, metrics), values_to = "value")
    
    mean_value <- dat_long %>% pull(value) %>% mean(na.rm = TRUE)
    sd_value <- dat_long %>% pull(value) %>% sd(na.rm = TRUE)
    
    label <- paste(round(mean_value, 3), round(sd_value, 3), sep = "%+-%")
    
    colorpal <- viridis(31)[first_month:last_month]
    
    timeseries_boxplots <- dat_long %>% 
        ggplot(aes(x = factor(month), y = value)) +
            geom_boxplot(fill = colorpal, outlier.alpha = 0.25) +
            geom_hline(aes(yintercept = mean_value), color = "tomato3") +
            annotate("text", x = 1, y = mean_value, label = label, hjust = 0, vjust = -1, parse = TRUE) +
            scale_x_discrete(limits = factor(1:31)) +
            scale_y_continuous(breaks = seq(0.1, 1, 0.1), limits = c(0.35, 0.95)) +
            geom_hline(yintercept = 0.5, color = "black", lty = "dashed") +
            labs(x = x_axis_label, y = "Area under the ROC Curve", title = title_label) +
            ggthemes::theme_pander() +
            theme(plot.margin = margin(2, 2, 2, 2, "mm"))
    
    if (saveplot == TRUE) {
        
        if (is.null(filename)) {
            warning("Please sepcify a filename to save your plot!")
        } else {
            ggsave(plot = timeseries_boxplots,
                   filename = here("outputs", "figs", filename),
                   height = 5,
                   width = 7
            )
        }
    }
    
    return(timeseries_boxplots)
}
