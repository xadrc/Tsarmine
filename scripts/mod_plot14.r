quartz(
	title = "cleaned data",
	width = 10, height  = 5,
	pointsize = 12
)
par(
	mfrow = c( 3, 1),
	omi = c(.3, .3, 0, 0)
)
layout(
	matrix( c(1, 2, 3),
	byrow = FALSE),
	heights = c(rep(1,3))
)

mar <- rep(0, 4)

#___TICKS____________________________________________________#

ticks <- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by = "month"
)
ticks <- as.POSIXct(
	ticks,
	format = "%d,%m.%y %H"
)

#___PLOT_FIELD_1____________________________________________#

par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = .5,
	col = "green4",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Ry  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field1)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#___PLOT_FIELD_2____________________________________________#

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field2,
	type = "l", lwd = .5,
	col = "red4",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Rx  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field2)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#___PLOT_FIELD_3____________________________________________#

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field3,
	type = "l", lwd = .5,
	col = "blue4",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Rz  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field3)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#___DATE_AXIS________________________________________________#

axis.POSIXct(
	side = 1,
	df_mod$Date,
	format = "%m.%y",
	at =ticks,
	cex.axis = 1.2
)
#	mtext( "Date", side = 1, line = 2.5, las = 0, cex = 1.2)

#legend(
#	"topright", 1,
#	pch = c(15), col = c("lightgrey"),
#	legend = c("Missing time spans"),
#	bty = "n",
#	cex = 1.2
#)

#___SAVE_PLOT________________________________________________#

rm(ticks, mar)
quartz.save(
    file  = file.path(dir_exp, "plot_14.png"), 
    type = "png", 
    dpi  = 300
)
