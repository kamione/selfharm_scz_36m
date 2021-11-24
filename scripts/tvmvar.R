# Environment ------------------------------------------------------------------
library(mgm)
library(qgraph)


# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ------------------- Symptom Time Series: "Time-varying mixed VAR model' -------------------
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

head(symptom_data$data[,1:5])
p <- ncol(symptom_data$data)
n <- nrow(symptom_data$data)
length(unique(symptom_data$data_time$dayno))

# -------------------- Estimating time-varying mixed VAR model --------------------

set.seed(1)
fit_tvmvar <- tvmvar(data = symptom_data$data, # Takes ~15 min
                     type = symptom_data$type,
                     level = symptom_data$level,
                     lags = 1,
                     estpoints = seq(0, 1, length=20),
                     bandwidth = 0.2,
                     threshold = "none",
                     saveData = TRUE)

# saveRDS(fit_tvmvar, file = paste0(fileDir, "fit_tvmvar.RDS"))
fit_tvmvar <- readRDS(file = paste0(fileDir, "fit_tvmvar.RDS"))

# Check how much daat has been used
fit_tvmvar

# -------------------- Make Predictions time-varying mixed VAR model --------------------

pred_tvmvar <- predict(object = fit_tvmvar, 
                       data = symptom_data$data,
                       tvMethod = "weighted", 
                       beepvar = symptom_data$data_time$beepno,
                       dayvar = symptom_data$data_time$dayno)

pred_tvmvar$errors


# -------------------- Visualizing time-varying mixed VAR Model --------------------


# -------------------- Aux Functions for Viz --------------------


f_timeline <- function(length=.15, 
                       gap=.005, 
                       mar=c(0,0,0,0)) {
    
    
    plot.new()
    par(mar=mar)
    plot.window(xlim=c(0,1), ylim=c(-.1,.1))
    #box()
    
    # arrows
    p_weeks <- c(4,14,4,12)
    bor_end <- c(0,cumsum(p_weeks)/sum(p_weeks))
    for(i in 1:4) {
        arrows(bor_end[i]+gap, 0, bor_end[i+1]-gap, code=3, length=length, lwd=1.5)
    }
    #for(i in 1:3) abline(v=bor_end[i+1], lty=2, col='grey')
    
    # text
    t_lengths <- p_weeks / sum(p_weeks)
    midpoints <- bor_end[-1] - t_lengths/2
    
    text(midpoints, rep(.07,4), c('Baseline', 
                                  'Double-blind period', 
                                  'Postassessment', 
                                  'Additional Postassessment'))
    
    text(midpoints, rep(-.07,4), c('4 Weeks', 
                                   '14 Weeks', 
                                   '4 Weeks', 
                                   '12 Weeks'))
    
    # change of medication
    points(c(42,98) / (sum(p_weeks)*7), rep(0,2), pch=20, cex=1.5)
    
}


# -------------------- Define Some Variable necessary for plotting --------------------


# pick colors
library(scales)
n_groups <- length(unique(symptom_data$groups))
group_cols <- alpha(RColorBrewer::brewer.pal(n_groups, 'Set1'), .5)


# create plot for layout
wadj_av <- matrix(apply(fit_tvmvar$wadj, 1:3, mean), 48, 48)
Q <- qgraph(t(wadj_av), 
            layout = 'spring')

# Prepare Edges over time
wadj_w_sign <- fit_tvmvar$wadj * fit_tvmvar$signs
wadj_w_sign[is.na(wadj_w_sign)] <- 0

n_estpoints <- 20

# Define lists of parameters I would like to see over time
l_es <- list()
l_es[[1]] <- wadj_w_sign[15, 15, 1, ] # Worrying -> Worrying
l_es[[2]] <- wadj_w_sign[31, 31, 1, ] # Tired -> Tired
l_es[[3]] <- wadj_w_sign[2, 17, 1, ] # Selflike -> Down
l_es[[4]] <- wadj_w_sign[2, 16, 1, ] # Concentration -> Down
l_es[[5]] <- wadj_w_sign[18, 17, 1, ] # Selflike -> Ashamed
l_es[[6]] <- wadj_w_sign[19, 12, 1, ] # Strong -> Self doubt



# -------------------- Plotting --------------------

pdf(paste0(figDir, 'Fig_tvmvar_application.pdf'), width = 8, height = 8)


# ---------- 0) Set up Layout ----------

lmat <- matrix(c(1, 2, 3,
                 4, 4, 4,
                 5, 5, 5, 
                 6, 6, 6), ncol=3, byrow = T)
lo <- layout(lmat, heights = c(1,.9, .2, 1))
# layout.show(lo)

# ---------- b) Three Network Plots ----------

E_select <- c(2, 9, 15)

for(i in E_select) {
    qgraph(t(fit_tvmvar$wadj[, , 1, i]), 
           edge.color = t(fit_tvmvar$edgecolor[, , 1, i]),
           layout = Q$layout, 
           nodeNames = symptom_data$colnames, 
           groups = symptom_data$groups,
           color = group_cols, 
           legend = F)
}


# ---------- c) A couple of edges over time ----------

col_es <- RColorBrewer::brewer.pal(6, 'Set1')

plot.new()
par(mar = c(1,7,1,7))
plot.window(xlim=c(0,1), ylim=c(-.2,.4))

abline(h = 0, lty=2, col='grey')

for(i in 1:6) {
    lines(seq(0, 1, length = n_estpoints), l_es[[i]], col=col_es[i], lwd=1.5, lty=1)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col='white', pch=20, cex = 1.5)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col=col_es[i], pch=20)
}

# Add axis
axis(2, round(seq(-.2, .4, length=9), 2), las=2)
title(ylab = 'Parameter value', line =3.5)

# Legend 
legend(.4, .19, c('Worrying -> Worrying', 
                  'Tired -> Tired',
                  'Selflike -> Down'), lwd = 1.5, col = col_es[1:3], bty = 'n')
legend(.7, .19, c('Concentration -> Down',
                  'Selflike -> Ashamed',
                  'Strong -> Self doubt'), lwd = 1.5, col = col_es[4:6], bty = 'n')


# Estimation points
x_seq <- seq(0, 1, length = 20)
y_dash <- .01
segments(x_seq, .4-y_dash, x_seq, .4+y_dash, col='blue')




# ---------- d) Time line ----------

f_timeline(length = .1, mar=c(0, 6.5, 0, 6.5))


# ---------- a) The Big Legend ----------

# Some settings
legend.cex <- 1
yjust <- 1
y_coord <- .87

# plot(1, type='n', xaxt='n', ann=FALSE, yaxt='n', bty='n', ylim=c(0,1), xlim=c(0,1))
plot.new()
par(mar = c(1,1,1,1))
plot.window(xlim=c(0,1), ylim=c(0,1))

start_x <- .02
step_x <- .2

ind_mood <- which(symptom_data$groups == 'Mood')
legend(start_x, y_coord, paste0(ind_mood, ': ', symptom_data$colnames[ind_mood]), pch=20, col = group_cols[2], cex = legend.cex, bty = 'n', yjust=yjust)

ind_symptoms <- which(symptom_data$groups == 'Symptoms')
legend(start_x+step_x*1, y_coord, paste0(ind_symptoms, ': ', symptom_data$colnames[ind_symptoms]), pch=20, col = group_cols[6], cex = legend.cex, bty = 'n', yjust=yjust)
ind_selfesteem <- which(symptom_data$groups == 'Self-esteem')
legend(start_x+step_x*1, y_coord-.4, paste0(ind_selfesteem, ': ', symptom_data$colnames[ind_selfesteem]), pch=20, col = group_cols[4], cex = legend.cex, bty = 'n', yjust=yjust)

ind_social <- which(symptom_data$groups == 'Social')
legend(start_x+step_x*2, y_coord, paste0(ind_social, ': ', symptom_data$colnames[ind_social]), pch=20, col = group_cols[5], cex = legend.cex, bty = 'n', yjust=yjust)

ind_physical <- which(symptom_data$groups == 'Physical')
legend(start_x+step_x*3, y_coord, paste0(ind_physical, ': ', symptom_data$colnames[ind_physical]), pch=20, col = group_cols[3], cex = legend.cex, bty = 'n', yjust=yjust)

ind_events <- which(symptom_data$groups == 'Events')
legend(start_x+step_x*4, y_coord, paste0(ind_events, ': ', symptom_data$colnames[ind_events]), pch=20, col = group_cols[1], cex = legend.cex, bty = 'n', yjust=yjust)

leg_title_sh <- .01
text(.07, y_coord+leg_title_sh, 'Mood', cex = 1.3, font = 2)
text(.07+step_x*1, y_coord+leg_title_sh, 'Self-esteem', cex = 1.3, font = 2)
text(.07+step_x*1, y_coord+leg_title_sh-.4, 'Symptoms', cex = 1.3, font = 2)
text(.07+step_x*2, y_coord+leg_title_sh, 'Social', cex = 1.3, font = 2)
text(.07+step_x*3, y_coord+leg_title_sh, 'Physical', cex = 1.3, font = 2)
text(.07+step_x*4, y_coord+leg_title_sh, 'Events', cex = 1.3, font = 2)



unique(symptom_data$groups)
symptom_data$colnames[symptom_data$groups == 'Events']
which(symptom_data$groups == 'Events')



dev.off()



















# prepare data frame for analysis

tmp <- sh_case_df_copy %>% 
    select(HCS_code,
           M1_Poscf:M36_Poscf,
           M1_Negcf:M36_Negcf,
           M1_Aff:M36_Aff,
           M1_SOFAScf:M36_SOFAScf,
           M1_compliance:M36_compliance,
           M1_case:M36_case
           ) %>% 
    zap_labels() %>% 
    pivot_longer(
        !HCS_code,
        names_to = c("month", "measure"),
        names_pattern = "M(.*)_(.*)",
        values_to = "value"
    ) %>% 
    mutate(month = as.numeric(month)) %>% 
    pivot_wider(
        names_from = measure,
        values_from = value
    ) %>%
    drop_na()

tvmvar_data <- NULL

tvmvar_data$data <- tmp %>% select(Poscf:case)
tvmvar_data$type <- c("g", "g", "g", "g", "g", "c")
tvmvar_data$level <- c(1, 1, 1, 1, 1, 2)
tvmvar_data$timepoint <-  tmp %>% select(HCS_code, month)

p <- ncol(tvmvar_data$data)
n <- nrow(tvmvar_data$data)



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
                      dayvar = rep(1, nrow(tvmvar_data$data)),
                      pbar = TRUE)

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


fit_tvmvar_lage5 <- tvmvar(data = tvmvar_data$data,
                     type = tvmvar_data$type,
                     level = tvmvar_data$level,
                     lambdaSel = "CV",
                     lags = 5,
                     binarySign = TRUE,
                     consec = tvmvar_data$timepoint$month,
                     estpoints = seq(0, 1, length.out = 20),
                     bandwidth = bandwidth,
                     threshold = "none",
                     scale = TRUE)




pred_obj <- predict(object = fit_tvmvar, 
                    data = tvmvar_data$data, 
                    tvMethod = "weighted", 
                    consec = tvmvar_data$timepoint$month)




fit_tvmvar <- fit_tvmvar_lage5

pred_obj$errors
pred_obj$tverrors

fit_tvmvar


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

n_estpoints <- 20

# Define lists of parameters I would like to see over time
l_es <- list()
l_es[[1]] <- fit_tvmvar$wadj[1, 6, 1, ] # Worrying -> Worrying
l_es[[2]] <- fit_tvmvar$wadj[2, 6, 1, ] # Tired -> Tired
l_es[[3]] <- fit_tvmvar$wadj[3, 6, 1, ] # Selflike -> Down
l_es[[4]] <- fit_tvmvar$wadj[4, 6, 1, ] # Concentration -> Down
l_es[[5]] <- fit_tvmvar$wadj[5, 6, 1, ] # Selflike -> Ashamed





# Visualization ----------------------------------------------------------------

pdf('selfharm_tvmvar_lag5.pdf', width = 12, height = 8)

# (0) set up Layout
lmat <- matrix(c(1, 2, 3, 4,
                 5, 5, 5, 5,
                 6, 6, 6, 6), ncol = 4, byrow = T)
lo <- layout(lmat, heights = c(1, 1.1, 0.2))
# layout.show(lo)

# (1) display selected graphs
E_select <- c(3, 8, 12, 17)

for(tp in E_select) {
    qgraph(fit_tvmvar$wadj[, , 1, tp], 
           edge.color = fit_tvmvar$edgecolor[, , 1, tp],
           border.color = "gray60",
           layout = Q$layout, 
           labels = c("POS", "NEG", "AFF", "SOFAS", "COMP", "DSH"), 
           legend = FALSE,
           vsize = 13, 
           esize = 10,
           asize = 10, 
           mar = c(6, 6, 6, 6),
           fade = FALSE,
           minimum = 0.05, 
           maximum = 1)
}


# ---------- c) A couple of edges over time ----------

col_es <- RColorBrewer::brewer.pal(5, 'Set1')

plot.new()
par(mar = c(1,7,1,7))
plot.window(xlim=c(0,1), ylim=c(-.2,.4))

abline(h = 0, lty=2, col='grey')

for(i in 1:5) {
    lines(seq(0, 1, length = n_estpoints), l_es[[i]], col=col_es[i], lwd=1.5, lty=1)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col='white', pch=20, cex = 1.5)
    # points(seq(0, 1, length = n_estpoints), l_es[[i]], col=col_es[i], pch=20)
}

# Add axis
axis(2, seq(-2, 4, 0.05), las=2)
title(ylab = 'Parameter value', line =3.5)

# Legend
legend_labels <- c(expression("POS"["t-1"]  %->%  "DSH/SS"["t"]),
                   expression("NEG"["t-1"]  %->%  "DSH/SS"["t"]),
                   expression("AFF"["t-1"]  %->%  "DSH/SS"["t"]))
legend(0.5, -.05, legend_labels, lwd = 2, col = col_es[1:3], bty = 'n', cex = 1.3)
legend_labels <- c(expression("SOFAS"["t-1"]  %->%  "DSH/SS"["t"]),
                   expression("COMPLIENCE"["t-1"]  %->%  "DSH/SS"["t"]))
legend(0.7, -.05, legend_labels, lwd = 2, col = col_es[4:5], bty = 'n', cex = 1.3)



# Estimation points
x_seq <- seq(0, 1, length = n_estpoints)
y_dash <- .01
segments(x_seq, .4 - y_dash, x_seq, .4 + y_dash, col = "tomato3")

dev.off()



