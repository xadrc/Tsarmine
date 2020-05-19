quartz( title = "Outliers", width = 10, height = 5, pointsize = 12)
par( mfrow = c( 3, 1), omi = c(.3, .3, 0, 0) )
layout( matrix( c( 1, 2, 3), byrow = FALSE ), heights = c( rep(1,3)) )

mar <- rep(0, 4)

#____________________________________________________________#

ticks <- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by = "month"
)
ticks <- as.POSIXct( ticks, format = "%d,%m.%y %H")

#___PLOT_FIELD_1____________________________________________#

par(mar = mar, pty ="m")
plot(
	df_mod$Date, df_mod$field1,
	type = "l", lwd = .5,
	col="grey50",
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
lines(
	df_modout$Date, df_modout$field1,
	type = "p", pch = ".",
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlab = "", ylab = ""
)
lines(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = 1,
	col= "green4",
	ylim = c( min(df_mod$field1, na.rm = TRUE), max(df_mod$field1, na.rm = TRUE)),
	xlab = "", ylab = ""
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
	h= pretty(extendrange(df_mod$field1)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#___PLOT_FIELD_2____________________________________________#

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	df_mod$Date, df_mod$field2,
	type = "l", lwd = .5,
	col="grey50",
	ylim = c( min(df_mod$field2, na.rm = TRUE), max(df_mod$field2, na.rm = TRUE)),
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
lines(
	df_modout$Date, df_modout$field2,
	type = "p", pch = ".",
	ylim = c( min(df_mod$field2, na.rm = TRUE), max(df_mod$field2, na.rm = TRUE)),
	xlab = "", ylab = ""
)
lines(
	df_modc$Date, df_modc$field2,
	type = "l", lwd = 1,
	col= "red4",
	ylim = c( min(df_mod$field2, na.rm = TRUE), max(df_mod$field2, na.rm = TRUE)),
	xlab = "", ylab = ""
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
	h= pretty(extendrange(df_mod$field2)),
	lty = 3,
	col = "lightgrey"
		)
box(lwd = .15)

#___PLOT_FIELD_3____________________________________________#

par(new = FALSE )
par(mar = mar, pty ="m")
plot(
	df_mod$Date, df_mod$field3,
	type = "l", lwd = .5,
	col="grey50",
	ylim = c( min(df_mod$field3, na.rm = TRUE), max(df_mod$field3, na.rm = TRUE)),
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
lines(
	df_modout$Date, df_modout$field3,
	type = "p", pch = ".",
	ylim = c( min(df_mod$field3, na.rm = TRUE), max(df_mod$field3, na.rm = TRUE)),
	xlab = "", ylab = ""
)
lines(
	df_modc$Date, df_modc$field3,
	type = "l", lwd = 1,
	col= "blue4",
	ylim = c( min(df_mod$field3, na.rm = TRUE), max(df_mod$field3, na.rm = TRUE)),
	xlab = "", ylab = ""
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
	h= pretty(extendrange(df_mod$field3)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#___DATE_AXIS________________________________________________#

axis.POSIXct( side = 1, df_mod$Date, format = "%m.%y", at =ticks, cex.axis = 1.2 )
#	mtext("Date", side= 1, adj=0, line= 2.5, las=0, cex = 1.2)

legend(
	"bottomright", 1,
	pch = c(15, 20),
	col = c("lightgrey", "black"),
	legend = c("missing values", "Outliers"),
	bty = "n", cex = 1.2
)

#mtext( paste0( "observations rejected : ", nrow(df_modout)))

#___SAVE_PLOT________________________________________________#

rm(ticks, mar)
quartz.save(
    file  = file.path(dir_exp, "plot_12.png"), 
    type = "png", 
    dpi  = 300
)
