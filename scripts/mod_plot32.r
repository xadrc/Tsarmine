quartz(
	title="Cumulative tilt",
	width=10, height=4,
	pointsize=10
)
par(omi=c(.35, .35, 0, 0))

Date 	<- df_modc$Date
ticks 	<- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by="month"
)
ticks 	<- as.POSIXct(
	ticks,
	format = "%d,%m.%y %H"
)
mar 	<- rep(0, 4)

#			___[   Window   ]____

par(mar = mar, pty = "m")
plot(
	df_modc$Date, df_modc$wtilt,
	type = "n",
	axes = FALSE
)
abline(
	v = ticks,
	h = pretty(extendrange(df_modc$wtilt)),
	lty = 3,
	col = "lightgrey"
)
lines(
	df_modc$Date, df_modc$wtilt,
	type = "h",
	col = "grey90"
)
axis(
	2, 
	cex.axis = 1
)
axis.POSIXct(
	1, Date,
	format = "%m.%y",
	at=ticks,
	cex.axis = 1
)
box(lwd = .1)


#			___[   legends   ]___

legend(
	"topleft", 1,
	legend = " CUMULATIVE TILT [DEG/WEEK] ",
	horiz = TRUE,
	pch  = 15, pt.cex = 2,
	col = "grey90",
	bty = "n"
)

#			___[   save   ]___

rm(Date, ticks, mar)
quartz.save(
    file  = file.path(dir_exp, "plot_32.png"), 
    type = "png", 
    dpi  = 300
)