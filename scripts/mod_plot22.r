quartz(
	title="diurnal temperature influence",
	width=10, height=5,
	pointsize=12
)
par(
	mfrow = c( 3, 1),
	omi = c(.25, rep(0, 3))
)
layout(
	matrix( c(1, 2, 3),
	byrow = FALSE),
	heights = c(rep(1, 3))
)

ticks <- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by = "month"
)
ticks <- as.POSIXct(
	ticks,
	format = "%d,%m.%y %H"
)

#		___[  input  ]___

par(mar = c(0, 2, 0, 0), pty="m")
plot(
	df_modc$Date, df_modc$tilt,
	type = "l", lwd = .4,
	col = "grey20",
	axes = FALSE,
	xlab = "", ylab = "",
	xlim = range(df_mod$Date)
)
#	attr.
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(extendrange(df_modc$tilt)),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("TILT (observed)  [deg]"),
	cex = 1,
	bty = "n"
)

#			___[   trend   ]___

par(new = FALSE)
par(mar = c(0, 2, 0, 0), pty = "m")
plot(
	df_modc$Date, runmean(df_modc$tilt, 24, endrule = "mean"),
	type = "l", lty = 1, lwd = .4,
	col = "grey50",
	axes = FALSE,
	xlab = "", ylab = "",
	xlim = range(df_mod$Date)
)
#	attr.
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd = .15)
abline(
	v = ticks,
	h = pretty(extendrange(df_modc$tilt)),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("TILT (trend)  [deg]"),
	cex = 1,
	bty = "n"
)

#		___[  residual  ]___

par(new = FALSE)
par(mar = c(0, 2, 0, 0), pty="m")
plot(
	df_modc$Date, df_modc$tilt - runmean(df_modc$tilt, 24, endrule = "mean"),
	type = "l", lwd = .4,
	col = "grey20",
	axes = FALSE,
	xlab = "", ylab = "",
	xlim = range(df_mod$Date)
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
box(lwd=.15)
abline(
	v = ticks,
	h = pretty(extendrange(df_modc$tilt-runmean(df_modc$tilt, 24, endrule = "mean"))),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("Tilt (seasonal)  [deg]"),
	cex = 1,
	bty = "n"
)

#		___[  date axis  ]___

axis.POSIXct(
	side = 1,
	df_mod$Date,
	format = "%m.%y",
	at = ticks,
	cex.axis = 1.2
)

#		___[  save  ]___

rm(ticks)
quartz.save(
    file  = file.path(dir_exp, "plot_22.png"), 
    type = "png", 
    dpi  = 300
)