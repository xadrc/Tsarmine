quartz(
	title=paste0("module ", mod_id),
	width=15/2, height=5,
	pointsize=12
)
par(
	mfrow = c(4, 1),
	omi = c( .2, 1/4, 0, 1/4 )
)
layout(
	matrix( c(1, 2, 3, 4),
	byrow = FALSE),
	heights = c(1, 1, 1, 2.8)
)

mar <- rep(0, 4)

#	DATA

onset	<- as.POSIXct( "2015-04-24 05:00:00" )
end		<- as.POSIXct( "2016-04-24 20:00:00" )
t			<- data.frame( Date = seq( onset, end, by = "hours" ))
df_mod <- merge(
	t, df_mod,
	all.x = TRUE,
	by.x = "Date"
)
df_modc <- merge(
	t, df_modc,
	all.x = TRUE,
	by.x = "Date"
)
rm(t, onset, end) 

jumps	<- df_modc$tilt_24
ramps	<- df_modc$tilt_24
jumps [ df_modc$jumps == 0 ]		<- NA
ramps [ df_modc$ramps == 0 ]	<- NA
ylim_tilt			<- c(0, 40)
ylim_cumtilt	<- c(0, ylim_tilt[2]/2)
at_tilt 			<- pretty(ylim_tilt, n = 4)
at_cumtilt	<- pretty(ylim_cumtilt, n = 4)

lwd_raw <- .1

# DATE TICKS

ticks <- seq.Date(
	from = as.Date("2015-05-01"),
	to = as.Date("2016-05-01"),
	by = "month"
)
ticks <- as.POSIXct(
	ticks,
	format = "%d,%m.%y %H"
)

Date <- df_modc$Date

# PLOT 1 :: FIELD 1

par(mar = mar, pty ="m")
plot(
	Date, df_mod$field1,
	type = "n", 
	xlab = "", ylab = "",
	xlim = range(Date),
	ylim = range(df_mod$field1, na.rm = TRUE),
	axes = FALSE
)
axis(
	side = 2,
	las = 2,
	at = NULL,
	mgp = c(3, .6, 0),
	cex.axis = .9
)
abline(
	v = ticks,
	h = pretty((df_mod$field1)),
	lty = 3, lwd = .7,
	col = "lightgrey"
)
lines(
	Date, df_mod$field1,
	type = "l", lwd = lwd_raw,
	xlab = "", ylab = ""
)
lines(
	Date, caTools::runmean(df_modc$field1, 24),
	type="l", lwd = .8,
	col = "green4",
	xlab= "", ylab= ""
)
par(new = TRUE)
par(mar = mar, pty = "m")
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0,1),
	axes = FALSE
)
text(
	-.04, .9,
	labels = "Ry [deg]",
	col = "black",
	pos = 4,
	cex = 1
)
legend(
	"top", 1,
	legend = c(
		"raw signal",
		"cleaned signal"
	),
	hori = TRUE,
	lty = c(1, 1),
	lwd = c( .5, .8),
	col = c("grey70", "black"),
	cex = 1,
	bty = "n"
)
box(lwd = .1)

# PLOT 2 :: FIELD 2

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	Date, df_mod$field2,
	type = "n",
	xlab = "", ylab = "",
	xlim = range(Date),
	ylim = range(df_mod$field2, na.rm = TRUE),
	axes = FALSE
)
axis(
	side = 2,
	las = 2,
	at = NULL,
	mgp = c(3, .6, 0),
	cex.axis = .9
)
abline(
	v = ticks,
	h = pretty((df_mod$field2)),
	lty = 3, lwd = .75,
	col = "lightgrey"
)
lines(
	Date, df_mod$field2,
	type = "l", lwd = lwd_raw,
	xlab = "", ylab = ""
)
lines(
	Date, caTools::runmean(df_modc$field2, 24),
	type="l", lwd = .8,
	col = "red3",
	xlab= "", ylab= ""
)

par(new = TRUE)
par(mar = mar, pty = "m")
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0,1),
	axes = FALSE
)
text(
	-.04, .9,
	labels = "Rx [deg]",
	col = "black",
	pos = 4,
	cex = 1
)
box(lwd = .1)

# PLOT 3 :: FIELD 3

par(new = FALSE)
par(mar = mar, pty ="m")
plot(
	Date, df_mod$field3,
	type = "n",
	xlab = "", ylab = "",
	xlim = range(Date),
	ylim = range(df_mod$field3, na.rm = TRUE),
	axes = FALSE
)
axis(
	side = 2,
	las = 2,
	at = NULL,
	mgp = c(3, .6, 0),
	cex.axis = .9
)
abline(
	v = ticks,
	h = pretty(extendrange(df_mod$field3)),
	lty = 3, lwd = .75,
	col = "lightgrey"
)
lines(
	Date, df_mod$field3,
	type = "l", lwd = lwd_raw,
	xlab = "", ylab = ""
)
lines(
	Date, caTools::runmean(df_modc$field3, 24),
	type="l", lwd = .8,
	col = "blue4",
	xlab= "", ylab= ""
)
par(new = TRUE)
par(mar = mar, pty = "m")
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0,1),
	axes = FALSE
)
text(
	-.04, .9,
	labels = "Rz [deg]",
	col = "black",
	pos = 4,
	cex = 1
)
box(lwd = .1)

# PLOT :: VERTICAL TILT + CUMULATIVE MOVEMENT

par(new = FALSE)
par(mar = mar, pty ="m")
	
#	plot hist cumulatice tilt

plot(
	NA, type = "n",
	axes = FALSE,
	xlab = "", ylab = "",
	ylim = ylim_cumtilt,
	xlim = range(df_mod$Date)
)
box(lwd = .1)
axis(
	side = 4, las=2,
	at = at_cumtilt,
	cex.axis = .9,
	mgp = c(3, .6, 0)
)
abline(
	v = ticks,
	h = at_cumtilt,
	lty = 3, lwd = .75,
	col = "lightgrey"
)
lines(
	Date, df_modc$wtilt,
	type = "h",
	col = "grey85"
)
lines(
	Date, df_modc$wtilt,
	type = "l", lwd = 0,
	col = "grey10"
)

#	tilt recording

par(new = TRUE)
plot(
	NA, type = "n",
	axes = FALSE,
	xlab = "", ylab = "",
	ylim = ylim_tilt,
	xlim = range(Date)
)
lines(
	Date, jumps,
	type = "l", lwd = 1, 
	col = "orange2"
)
lines(
	Date, ramps,
	type = "l", lwd = .8,
	col = "black"
)
axis(
	side = 2, las = 2,
	at = at_tilt,
	cex.axis = .9,
	mgp = c(3, .6, 0)
)
axis.POSIXct(
	1, Date,
	format = "%m.%y",
	at = ticks,
	mgp = c(3, .6, 0),
	cex.axis = .9
)
par(new = TRUE)
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0,1),
	axes = FALSE
)
legend(
	"topleft", 1,
	legend = c(
		"Tilt [deg]",
		"Jumps"
	),
	horiz = FALSE,
	lty = c(1, 1),
	lwd = c(.8, 1),
	col = c("black", "orange2"),
	cex = 1,
	bty = "n"
)
legend(
	"topright", 1,
	legend = "Weekly cumulative tilt [deg]",
	pch = 15, 
	pt.cex = 2,
	col = "grey80",
	cex = 1,
	bty = "n"
)

quartz.save(
    file  = file.path(dir_exp, "plot_41.png"), 
    type = "png", 
    dpi  = 300
)