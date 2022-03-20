# Environment ------------------------------------------------------------------
library(tidyverse)
library(here)
library(haven)
library(mgm)
library(qgraph)
library(ggthemes)


# Data IO  ---------------------------------------------------------------------
preprocessed_df <- here("data", "processed", "cf_selfharm_longitudinal.rds") %>% 
    read_rds()


# Visualization Time x Symptom Severity ----------------------------------------
# prepare data frame for analysis
preprocessed_df_long <- preprocessed_df %>%
    mutate(index = 1:n(), .before = sh_case) %>% 
    select(index,
           sh_case,
           M1_Poscf:M36_Poscf,
           M1_Negcf:M36_Negcf,
           M1_Aff:M36_Aff,
           M1_SOFAScf:M36_SOFAScf,
           M1_compliance:M36_compliance,
           M1_case:M36_case
    ) %>% 
    pivot_longer(
        !c(index, sh_case),
        names_to = c("month", "measure"),
        names_pattern = "M(.*)_(.*)",
        values_to = "value"
    ) %>%
    mutate(month = as.numeric(month)) %>% 
    pivot_wider(
        names_from = measure,
        values_from = value
    )

longitudial_change_fig <- preprocessed_df_long %>% 
    rename(
        "Positive Symptom" = "Poscf",
        "Negative Symptom" = "Negcf",
        "Depressive Symptom" = "Aff"
    ) %>% 
    # mutate(case = factor(case, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
    pivot_longer(
        cols = `Positive Symptom`:`Depressive Symptom`,
        names_to = "symptom",
        values_to = "severity"
    ) %>% 
    ggplot(aes(x = month, y = severity, linetype = sh_case, color = symptom, fill = symptom)) +
        geom_smooth() +
        labs(x = "Month",
             y = "Symptom Severity",
             color = "",
             fill = "",
             linetype = "") +
        scale_x_continuous(breaks = c(0, seq(4, 36, 4))) +
        scale_color_manual(values = c("tomato3", "darkgrey", "#E69F00")) +
        scale_fill_manual(values = c("tomato3", "darkgrey", "#E69F00")) +
        ggthemes::theme_pander() +
        theme(
            plot.margin = margin(2, 2, 2, 2, "mm"),
            legend.position = "right",
            legend.key.size = unit(0.8, 'cm'),
            legend.background = element_rect(fill = "transparent", color = NA)
        )
longitudial_change_fig %>% 
    ggsave(filename = here("outputs", "figs", "data-cf_desc-longitudial_change.pdf"),
           height = 3,
           width = 6)


# Time-varying Mixed Vector Autoregressive Model -------------------------------

# create dataframe
tvmvar_data <- NULL
tvmvar_data$data <- preprocessed_df_long %>% select(Poscf:case) %>% drop_na()
tvmvar_data$type <- c("g", "g", "g", "g", "g","c")
tvmvar_data$level <- c(1, 1, 1, 1, 1, 2)
tvmvar_data$timepoint <- preprocessed_df_long %>%
    select(index, month, Poscf:case) %>%
    drop_na() %>%
    select(index, month)

p <- ncol(tvmvar_data$data)
num_row <- nrow(tvmvar_data$data)

set.seed(1111)
bw_object <- bwSelect(data = tvmvar_data$data,
                      type = tvmvar_data$type,
                      level = tvmvar_data$level,
                      bwSeq = seq(0.2, 1, 0.1),
                      bwFolds = 1,
                      bwFoldsize = 10,
                      modeltype = "mvar",
                      lags = 1,
                      scale = TRUE,
                      beepvar = tvmvar_data$timepoint$month,
                      dayvar = rep(1, num_row),
                      pbar = TRUE)

# save cache
write_rds(bw_object, here("cache", "data-cf_decs-bw_object.rds"))

bandwidth <- as.numeric(names(which.min(bw_object$meanError)))

set.seed(1111)
fit_tvmvar <- tvmvar(data = tvmvar_data$data,
                     type = tvmvar_data$type,
                     level = tvmvar_data$level,
                     lambdaSel = "CV",
                     lags = 1,
                     binarySign = TRUE,
                     consec = tvmvar_data$timepoint$month,
                     estpoints = seq(0, 1, length.out = 35),
                     bandwidth = bandwidth,
                     threshold = "none",
                     scale = TRUE)

# save cache
write_rds(fit_tvmvar, here("cache", "data-cf_decs-fit_tvmvar.rds"))

#pred_obj <- predict(object = fit_tvmvar, 
#                    data = tvmvar_data$data, 
#                    tvMethod = "weighted", 
#                    consec = tvmvar_data$timepoint$month)
#pred_obj$errors
#pred_obj$tverrors


wadj_av <- matrix(apply(fit_tvmvar$wadj, 1:3, mean), 6, 6)
Q <- qgraph(t(wadj_av), layout = 'spring')

qgraph(t(fit_tvmvar$wadj[, , 1, 3]), 
       edge.color = t(fit_tvmvar$edgecolor[, , 1, 3]),
       layout = Q$layout, 
       nodeNames = colnames(tvmvar_data$data), 
       legend = F)

# Prepare Edges over time
wadj_w_sign <- fit_tvmvar$wadj * fit_tvmvar$signs
wadj_w_sign[is.na(wadj_w_sign)] <- 0

n_estpoints <- 35

# Define lists of parameters I would like to see over time
l_es <- list()
l_es[[1]] <- fit_tvmvar$wadj[1, 6, 1, ] # Positive Symptom
l_es[[2]] <- fit_tvmvar$wadj[2, 6, 1, ] # Negative Symptom
l_es[[3]] <- fit_tvmvar$wadj[3, 6, 1, ] # Depressive Symptom
l_es[[4]] <- fit_tvmvar$wadj[4, 6, 1, ] # SOFAS
l_es[[5]] <- fit_tvmvar$wadj[5, 6, 1, ] # Compliance


# Visualization ----------------------------------------------------------------
pdf(here("outputs", "figs", "data-cf_desc-tvmvar_lag1.pdf"), width = 12, height = 8)

# ---------- 0) Set up Layout ----------
# (0) set up Layout
lmat <- matrix(c(1, 2, 3, 4,
                 5, 5, 5, 5,
                 6, 6, 6, 6), ncol = 4, byrow = T)
lo <- layout(lmat, heights = c(1, 1.5, 0.1))
# layout.show(lo)

# ---------- b) Three Network Plots ----------
# (1) display selected graphs
E_select <- c(5, 13, 21, 29)
for(tp in E_select) {
    qgraph(fit_tvmvar$wadj[, , 1, tp], 
           edge.color = fit_tvmvar$edgecolor[, , 1, tp],
           border.color = "gray60",
           layout = Q$layout, 
           labels = c("POS", "NEG", "DEP", "SOFAS", "COMP", "DSH"), 
           legend = FALSE,
           vsize = 13, 
           esize = 10,
           edge.width = 1.5,
           asize = 10, 
           mar = c(4, 10, 10, 10),
           fade = FALSE,
           minimum = 0.1, 
           maximum = 1)
    title(glue("Month = {tp + 1}"))
}


# ---------- c) A couple of edges over time ----------
col_es <- RColorBrewer::brewer.pal(5, 'Set1')

plot.new()
par(mar = c(0, 7, 1, 3))
plot.window(xlim = c(1, 35), ylim = c(-0.2, 0.6))

abline(h = 0, lty = 3, col = "grey40")

for(i in 1:5) {
    lines(1:n_estpoints, l_es[[i]], col = col_es[i], lwd = 4, lty = 1)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col='white', pch=20, cex = 1.5)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col=col_es[i], pch=20)
}

# add axis
axis(2, seq(-0.2, 0.45, 0.05), las = 2)
title(ylab = "Parameter Value", line = 3.5, cex.lab = 1.5)

# add legend
legend_labels <- c(expression("POS"["t-1"]  %->%  "DSH"["t"]),
                   expression("NEG"["t-1"]  %->%  "DSH"["t"]),
                   expression("DEP"["t-1"]  %->%  "DSH"["t"]))
legend(22, -0.05, legend_labels, lwd = 2, col = col_es[1:3], bty = "n", cex = 1.5)
legend_labels <- c(expression("SOFAS"["t-1"]  %->%  "DSH"["t"]),
                   expression("COMP"["t-1"]  %->%  "DSH"["t"]))
legend(29, -0.05, legend_labels, lwd = 2, col = col_es[4:6], bty = "n", cex = 1.5)


# add estimation points
x_seq <- 0:n_estpoints
y_dash <- .01
segments(x_seq, 0.45 - y_dash, x_seq, 0.45 + y_dash, col = "tomato3")

# add arrows
g_location <- c(2, 12.5, 22.5, 33)
for (i in 1:4) {
    shape::Arrows(g_location[i], 0.6, E_select[i], 0.47, col = "gray30", 
                  lwd = 2.5, arr.adj = 1, arr.type = "triangle")
}

dev.off()
