quartz(
	title="summary data cleaning",
	width=10, height=5,
	pointsize=12
)
par(
	mfrow = c( 4, 1),
	omi = c(.3, .3, 0, 0)
)
layout(
	matrix( c(1, 2, 3, 4),
	byrow = FALSE),
	heights = c(rep(1, 4))
)

ticks	<- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by = "month"
)
ticks	<- as.POSIXct(
	ticks,
	format = "%d,%m.%y %H"
)
mar <- rep(0, 4)

#		___[  RAW  ]___

par(mar = mar, pty ="m")
plot(
	df_mod$Date, df_mod$field1,
	type="l", lwd = .4,
	col = "grey20",
	xlim = range(df_mod$Date),
	xlab= "", ylab= "",
	axes = FALSE
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(df_mod$field1),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("RECORDED"),
	cex = 1.2,
	bty = "n"
)

#		___[  OUTLIERS  ]___

par(new = FALSE )
par(mar = mar, pty ="m")
plot(
	df_mod$Date, df_mod$field1,
	type = "l", lwd = .2,
	col="grey20",
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlim = range(df_mod$Date),
	xlab = "", ylab = "",
	axes = FALSE
)
lines(
	df_modout$Date, df_modout$field1,
	type = "p", pch = ".",
	lwd = 1,
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlab = "", ylab = ""
)
lines(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = .4,
	col = "grey20",
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlab = "", ylab = ""
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(df_mod$field1),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("OUTLIERS"),
	cex = 1.2,
	bty = "n"
)
legend(
	"bottomright", 1,
	pch = c(20),
	col = c("black"),
	legend = c("Outliers"),
	bty = "n",
	cex = 1.2
)

#		___[  RECONSTRUCTED  ]___

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = .2,
	col = "grey20",
	axes = FALSE,
	xlim = range(df_mod$Date),
	xlab = "", ylab = ""
)
lines(
	df_modc$Date, df_modc$field1_fx,
	type = "p", pch = "+",
	col  = "grey20",
	cex = .4
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(df_modc$field1),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("RECONSTRUCTED DATA POINTS"),
	cex = 1.2,
	bty = "n"
)
legend(
	"bottomright", 1,
	pch = c("+"), col = c("black"),
	legend = c("Reconstructed points"),
	bty = "n",
	cex = 1.2
)

#		___[  FIN  ]___

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = .4,
	col = "grey20",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(df_modc$field1),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("CLEANED SERIES"),
	cex = 1.2,
	bty = "n"
)

#		___[  attributes  ]___

axis.POSIXct(
	side = 1,
	df_mod$Date,
	format = "%m.%y",
	at = ticks,
	cex.axis = 1.2
)
#	mtext("Date", side= 1, line= 2, las=0, cex = 1.2)

#		___[  save  ]___

rm(ticks, mar)
quartz.save(
    file  = file.path(dir_exp, "plot_15.png"), 
    type = "png", 
    dpi  = 300
)
