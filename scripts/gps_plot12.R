###

quartz(
	title  = "Perm_GPS speed",
	width  = 10,
	height = 4,
	pointsize = 10
)
par(omi = c(.36, .31, 0, .0))

Date	<- df_gps$Date
ticks	<- seq.Date(
	from = as.Date("2015-05-01"),
	to = as.Date("2016-05-01"),
	by = "month"
)
ticks	<- as.POSIXct(ticks, format = "%d,%m.%y %H")

#MS	<- df_gps$speed_month
WS	<- df_gps$speed_week
ylim	<- c(floor(min(WS, na.rm=1)), ceiling(max(WS, na.rm=1)))
ticks_y	<- seq(ylim[1], ylim[2], by =1)

# plot window
par(mar = rep(0, 4), pty = "m")
plot(
	NA,
	type = "n",
	axes = FALSE,
	ylab = "", xlab = "",
	ylim = ylim,
	xlim = range(Date),
)
box(lwd = .1)
axis(
	2,
	las=2,
	cex.axis = 1.2
)
abline(
	v = ticks,
	h = ticks_y,
	lty = 3,
	col = "lightgrey"
)
mtext(
	"Perm_GPS speed [m/yr]",
	side = 2, line = 2, las=0
)
mtext(
	"Date",
	side = 1, line= 2, las=0
)

# plot speed
par(new = FALSE)
#col <- colorRampPalette(c("yellow", "red3"), alpha=TRUE)
#col <- col(diff(ylim)+1)
#ws <- round(WS,0) - 3
#ms <- round(MS, 0) - 3
lines(
	Date, WS,
	type = "h",
	lwd = 1,
	col = "grey90"
)
lines(
	Date, WS,
	type ="l",
	lwd = .15
)
#lines(
#	Date, WS,
#	type = "p",
#	pch = ".",
#	cex = .1,
#	col = col[ws]
#)
#lines(
#	Date, MS,
#	type = "h",
#	lwd = .015
#)
#lines(
#	Date, MS,
#	type = "l",
#	lwd = .8
#)
#lines(
#	Date, MS,
#	type = "p",
#	pch = ".",
#	cex = .1,
#	col = col[ms]
#)
axis.POSIXct(
	1,
	Date,
	format = "%m.%y",
	at = ticks,
	cex.axis = 1.2
)

# legends
legend(
	"topleft", 1,
	lty = 1,
	lwd = .2,
	title = "Perm. GPS",
	legend = "weekly speed [m/yr]",
	horiz = FALSE,
	cex = 1.2,
	xjust = 0,
	bty = "n",
	bg = "white"
)

rm(Date, ticks, ticks_y, WS, ylim)

# save
quartz.save(
	file = file.path(dir_exp, "plot12.png"),
	type = "png",
	dpi = 300
)
