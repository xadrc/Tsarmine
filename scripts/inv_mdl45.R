#	___[ MODEL SETTINGS ]___

#	response
response		<- df_modelvar $ destabilizations
name_response	<- "destabilizations"

# 	independent variable/s
var_1		 	<- df_modelvar $ temperatures
var_1_name 		<- "air temperatures"
var_1_unit 		<- "°C"
var_1_breaks	<-  seq(-20, 30, 5)

#	model
model_name		<- "Air temperatures effects"
model_formulae	<- response ~ var_1
model_family	<- "binomial"
model 			<- glm(model_formulae, model_family)

#	coefficients
intercept		<- round(as.numeric(coef(model)[1]), 3)		# intercept
var_1_beta 		<- round(as.numeric(coef(model)[2]), 3)		# bêta coeff
var_1_oddrat	<- round(exp(var_1_beta), 3)				# odds ratio
var_1_pval		<- summary(model)$coefficients[2,4]			# p-value

p_class	<- function(pval){
    if (pval >= 0 & pval < 0.001){
        pclass	<- "***"
    } else if (pval >= 0.001 & pval < 0.01){
        pclass	<- "**"
    } else if (pval >= 0.01 & pval < 0.05){
        pclass	<- "*"
    } else if (pval >= 0.05){
        pclass	<- "–"
    }
    return(pclass)
}

var_1_pclass	<- p_class(var_1_pval)
var_1_range 	<- seq(var_1_breaks[1], var_1_breaks[length(var_1_breaks)], .01)
var_1_logodds 	<- intercept + var_1_beta * var_1_range
var_1_prob 		<- exp(var_1_logodds) / (1 + exp(var_1_logodds))

#	___[ PLOT ]___

quartz(
    title = paste(model_name),
    height = 3, width = 3,
    pointsize = 7
)
par(
    mfrow = c(1,1),
    omi = c(rep(1/4,2), rep(0,2))
)

# response / var_1
# set window
par(new = FALSE)
par(mar = rep(0, 4), pty = "m")
plot(
    NA,
    type = "n",
    axes = FALSE,
    xlim = range(var_1_range), ylim = c(0, 1)
)
box(lwd = .1)	;	grid()
axis(
    side = 1,
    mgp = c(3, .3, 0)
)
axis(
    side = 2,
    mgp = c(3, .5, 0),
    at = c(0, .2, .4, .6, .8, 1),
    labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
    las = 0
)

# histograms
hist_p1	<- var_1
hist_p1[response == 0] 	<- 0
hist_p1[hist_p1 == 0]	<- NA

hist_p0	<- var_1
hist_p0[response == 1] 	<- 0
hist_p0[hist_p0 == 0]	<- NA

hist_p1 <- na.omit(hist_p1)
hist_p0 <- na.omit(hist_p0)

hist_p1 <- hist(
    hist_p1,
    breaks = var_1_breaks,
    plot = FALSE
)
hist_p0 <- hist(
    hist_p0,
    breaks = var_1_breaks,
    plot = FALSE
)
scale_max <- ceiling(max(c(hist_p1$counts, hist_p0$counts)))
if (scale_max%%2 == 1){
    scale_max <- scale_max + 1
}
scale_tot <- scale_max / 2*5

par(new = TRUE)
par(mar = rep(0, 4), pty = "m")
plot(
    NA,
    type = "n",
    axes = FALSE,
    xlim = c(0, length(var_1_breaks)-1),
    ylim = c(-scale_tot, 0)
)
barplot(
    - hist_p1$counts,
    space = 0, border = NA,
    col = "grey90",
    horiz = FALSE, add = TRUE, axes = FALSE
)
par(new = TRUE)
par(mar = rep(0, 4), pty = "m")
plot(
    NA,
    type = "n",
    axes = FALSE,
    xlim = c(0, length(var_1_breaks)-1),
    ylim = c(0, scale_tot )
)
barplot(
    hist_p0$counts,
    space = 0, border = NA,
    col = "grey90",
    horiz = FALSE, add = TRUE, axes = FALSE
)

# data points
par(new = TRUE)
par(mar = c(rep(0, 4)), pty = "m")
plot(
    var_1, response,
    pch = 20, cex=.8,
    xlab="Trigger",
    ylab="Probability of destabilization",
    xlim = range(var_1_range), ylim = c(0,1),
    axes = FALSE
)

# prediction curve
par(new = TRUE)
par(mar = c(rep(0, 4)), pty = "m")
plot(
    var_1_range, var_1_prob,
    type = "l", lwd = .8, lty = 3, col = "red3",
    xlab = "", ylab = "",
    ylim = c(0,1),
    xlim = range(var_1_range),
    axes = FALSE
)

# modelled curve
curve <- data.frame(
    x = var_1_range,
    y = var_1_prob
)
curve$x[curve$x <= min(var_1, na.rm = TRUE)]	<- NA
curve$y[curve$x <= min(var_1, na.rm = TRUE)]	<- NA
curve$x[curve$x >= max(var_1, na.rm = TRUE)]	<- NA
curve$y[curve$x >= max(var_1, na.rm = TRUE)]	<- NA

par(new = TRUE)
par(mar = c(rep(0, 4)), pty = "m")
plot(
    curve$x, curve$y,
    type = "l", lwd = 1, col = "red",
    xlab = "", ylab = "",
    ylim = c(0,1),
    xlim = range(var_1_range),
    axes = FALSE
)

# legends
par(new = TRUE)
par(mar = rep(0, 4), pty = "m")
plot(
    1, 1,
    type = "n",
    axes = FALSE,
    xlim = c(0,1), ylim = c(0,1)
)
legend(
    .02, .85,
    legend = c(
        "data",
        "modelling",
        "forecast"
    ),
    pch = c(20, NA, NA),
    lwd = c(NA, 1, .8),
    lty = c(NA, 1, 3),
    col = c(
        "black",
        "red",
        "red3"
    ),
    horiz = FALSE, bty = "n",
    cex = 1
)
legend(
    "center",
    title = paste(toupper(var_1_name)),
    legend = paste0("[", toupper(var_1_unit), "]"),
    pch = NA,
    text.col = "grey40",
    bty = "n"
)
legend(
    .6, .15,
    legend = paste0("β : ", var_1_beta, " ", var_1_pclass),
    text.col = "grey50",
    bty = "n"
)
axis(
    side = 2,
    at = c(1, .8, .6),
    labels = c(0, round(scale_max/2, 2), scale_max),
    pos = 1, lwd = .8, las = 2, cex.axis = .9,
    col = "grey60",
)
axis(
    side = 2,
    at = c(0, .2, .4),
    labels = c(0, round(scale_max/2, 2), scale_max),
    pos = 1, lwd = .8, las = 2, cex.axis = .9,
    col = "grey60"
)

quartz.save(
    file = file.path(dir_plt, "models", "mdl_45.png"),
    type = "png",
    dpi = 300
)

dev.off()