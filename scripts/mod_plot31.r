quartz(
	title="Movements",
	width=10, height=4,
	pointsize=10
)
par(omi=c(.35, .35, 0, 0))

mar 	<- rep(0, 4)

Date	<- df_modc$Date
ticks	<- seq.Date( from=as.Date("2015-05-01"), to=as.Date("2016-05-01"), by = "month")
ticks	<- as.POSIXct( ticks, format = "%d,%m.%y %H")

jumps	<- df_modc$tilt_24
ramps	<- df_modc$tilt_24
jumps [ df_modc$jumps == 0 ]	<- NA
ramps [ df_modc$ramps == 0 ]	<- NA

#			___[   tilt   ]___

par( mar = mar, pty = "m" )
plot(
	Date, df_modc$tilt_24,
	type = "n",
	axes = FALSE,
	xlab = "", ylab = "",
	xlim = range(df_mod$Date)
)
lines(
	Date, jumps,
	type = "l", lwd = .8,
	col = "orange3"
)
lines(
	Date, ramps,
	type = "l", lwd = .4,
	col = "blue4"
)
axis(
	2, at = NULL,
	cex.axis = 1.2
)
abline(
	v = ticks,
	h = pretty(df_modc$tilt_24),
	lty = 3,
	col = "lightgrey"
)
legend(
	"topleft", 1,
	legend = c("POSITION  [DEG]"),
	cex = 1,
	bty = "n"
)
legend(
	"topright", 1,
	legend = c("ramps", "jumps"),
	lwd = c(.5, .5),
	col = c("blue4", "orange3"),
	bty = "n",
	bg = "white",
	cex = 1.2
)
box(lwd = .15)

axis.POSIXct( side = 1, df_mod$Date, format = "%m.%y", at = ticks, cex.axis = 1.2 )

rm(mar, Date, ticks, jumps, ramps)
quartz.save(
    file  = file.path(dir_exp, "plot_31.png"), 
    type = "png", 
    dpi  = 300
)