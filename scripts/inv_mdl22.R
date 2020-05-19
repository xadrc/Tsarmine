#	___[ MODEL SETTINGS ]___

#	variables

resp 		<- df_difftilts$w_means
resp_name	<- "blocs tilts"
resp_unit	<- "deg/w"
resp_meas 	<- "weekly cumulative tilts"
resp_range	<- c(-.2, 5.2)

var	 		<- df_evo$wprec
var_name	<- "precipitations"
var_unit 	<- "mm/w"
var_meas	<- "weekly sums"
var_range 	<- c(-2, 80)

d_ <- data.frame(resp, var)	;	rm(resp, var)

d_keep <- d_
d_keep$resp[d_keep$resp >= 1] <- NA
d_keep <- na.omit(d_keep)

d_omit <- d_
d_omit$resp[d_omit$resp < 1] <- NA
d_omit <- na.omit(d_omit)

#	model

model_name	<- "Precipitations - weekly sums"

l_mod		<- lm(resp ~ var, data = d_keep)
q_mod		<- lm(resp ~ var + I(var^2), data = d_keep)

l_slope			<- round(l_mod$coef[[2]], 3)						# slope
l_intercept 	<- round(l_mod$coef[[1]], 3)						# intercept
l_pval	 		<- round(summary(l_mod)$coefficients[2,4], 3)		# p-value
q_pval 			<- round(summary(q_mod)$coefficients[2,4], 3)		# p-value

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

l_pcl <- p_class(l_pval)		# p-value level
q_pcl <- p_class(q_pval)

reglin <- data.frame(
    x = seq(
        extendrange(d_keep$var, f = .04)[1],
        extendrange(d_keep$var, f = .04)[2],
        .01
    ),
    y = NA
)
reglin$y <- l_intercept + reglin$x * l_slope

#	___[ PLOT ]___

quartz(
    title = paste(model_name),
    width = 3, height = 3,
    pointsize = 8
)
par(omi = c(rep(1/4,2), rep(0,2)))

par(mar = rep(0, 4), pty = "m")
plot(
    NA,
    xlim = var_range,
    ylim = resp_range,
    xlab = "", ylab = "",
    #asp = 1,
    axes = F
)
box(lwd = .1) ; grid()
axis(
    side = 1,
    mgp = c(3, .3, 0)
)
axis(
    side = 2,
    mgp = c(3, .5, 0)
)
# data points used
lines(
    d_keep$var, d_keep$resp,
    type = "p",
    pch = 20,
    cex = .6
)
# data points omitted
lines(
    d_omit$var, d_omit$resp,
    type = "p",
    pch = 20,
    cex = .6,
    col = "grey70"
)
# plot regression lines
lines(
    reglin$x, reglin$y,
    type = "l",
    col = "red"
)
lines(
    sort(d_keep$var), fitted(q_mod)[order(d_keep$var)],
    type = "l", lty = 3, lwd = 1.4,
    col = "blue3"
)
# legends
par(new = TRUE)
par(mar = rep(0, 4), pty = "m")
plot(
    NA,
    type = "n",
    xlim = c(0, 1), ylim = c(0,1),
    axes = FALSE
)
legend(
    0, 0.8,
    legend = c(
        "data points",
        "omitted points",
        "linear regression",
        "polynomial regression"
    ),
    pch = c(20, 20, NA, NA),
    pt.cex = c(1, 1, NA, NA),
    lty = c(NA, NA, 1, 3),
    lwd = c(NA, NA, 1, 1.4),
    col = c(
        "black",
        "grey70",
        "red",
        "blue3"
    ),
    horiz = FALSE, bty = "n",
    cex = 1
)
text(
    0, .99,
    labels = paste(toupper(resp_name), " [", toupper(resp_unit), "]"),
    col = "grey40",
    pos = 4,
    cex = .9
)
text(
    .99, 0,
    labels = paste(toupper(var_name), " [", toupper(var_unit), "]"),
    col = "grey40",
    pos = 2,
    cex = .9
)
#text(
#	.5, .5,
#	labels = paste0(" coef. : ", a_ , " ", pcl_),
#	col = "grey40",
#	cex  = 1
#)

#	___[ SUMMARY ]___

mat_mod <- as.data.frame(
    matrix(
        data = c(
            "response : ", 	paste0(resp_name, " (", resp_meas, ")"),
            "var : ", 		paste0(var_name, " (", var_meas, ")")
        ),
        byrow = TRUE,
        ncol = 2
    )
)
colnames(mat_mod) <- NULL

quartz.save(
    file = file.path(dir_plt, "models", "mdl_22.png"),
    type = "png",
    dpi = 300
)

dev.off()