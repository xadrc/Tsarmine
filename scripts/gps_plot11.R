require(raster)
require(GISTools)
require(fields)

quartz(
	title="Perm_GPS position",
	width=6, height=4,
	pointsize=10
)
par(omi = c(rep(0, 4)))
mar <- c(2, 2, 0, 0)

E <- df_gps$cooE
N <- df_gps$cooN
U <- df_gps$cooU
S <- df_gps$speed_week

P0 <- head(na.omit(df_gps), 1)
Pn <- tail(na.omit(df_gps), 1)

dE <- abs(Pn$cooE - P0$cooE)
dN <- abs(Pn$cooN - P0$cooN)
dU <- abs(Pn$cooU - P0$cooU)
dT_3D <- sqrt((dE^2)+(dN^2)+(dU^2))
dT_2D <- sqrt((dE^2)+(dN^2))

# plot window
par(mar = mar, pty = "m")
plot(
	NA,
	type = "n",
	axes = FALSE,
	ylab = "", xlab = "",
	ylim = extendrange(N),
	xlim = extendrange(E),
	asp = 1
)
box(lwd = .1) ; axis(1) ; axis(2) ; grid()
mtext(
	"S <> N",
	side = 2,
	line = 2,
	las = 0
)
mtext(
	"W <> E",
	side = 1,
	line= 2,
	las = 0
)

# trajectory EN
col	<- colorRampPalette(c("yellow", "red3"))
col	<- col(5)
s	<- (round(S,0)-3)
lines(
	E, N,
	type = "l",
	lty = 1,
	lwd = .01
)
lines(
	E, N,
	type = "p",
	pch = 20, cex = .103,
	col = col[s]
)

# date points
dp <- seq.Date(
	from = as.Date("2015-05-01"),
	to = as.Date("2016-05-01"),
	by = "month"
)
dp 		<- as.POSIXct(dp, format = "%d,%m.%y %H")
dp 		<- data.frame(Date = dp)
dp 		<- merge(
	dp, df_gps,
	by.x = "Date",
	all.x = FALSE
)
dp_m	<- subset(dp, is.na(dp$cooE))
dp 		<- na.omit(dp)
points(
	dp$cooE, dp$cooN,
	pch = "|",
	cex = .8
)
text(
	dp$cooE, dp$cooN +.25,
	labels = strftime(dp$Date, "%m.%y"),
	cex = 0.8
)
gps_x 		<- df_gps
gps_x$cooE	<- na.approx(gps_x$cooE, na.rm = FALSE)
gps_x$cooN	<- na.approx(gps_x$cooN, na.rm = FALSE)

dp_m		<- data.frame(Date = dp_m$Date)
dp_m 		<- merge(dp_m, gps_x, by.x = "Date", all.x = FALSE)
rm(gps_x)
points(
	dp_m$cooE, dp_m$cooN,
	pch = "|",
	cex = .8,
	col = "grey40"
)
text(
	dp_m$cooE, dp_m$cooN +.25,
	labels = paste0("",strftime(dp_m$Date, "%m.%y"), "*"),
	cex = 0.8,
	col = "grey40"
)

# map attributes
# legend
legend(
	"topleft", 1,
	pch = NA,
	legend = c("Coordinate system [CH1903]"),
	bty = "n",
	bg="white"
)
smax	<- max(S, na.rm = T)
smax_at	<- subset(df_gps,  df_gps$speed_week == smax)
smax_at	<- smax_at[!duplicated(smax_at$speed_week), ]
smax_at	<- smax_at$Date
legend(
	"bottomright", 1,
	pch = c(rep(NA, 5)),
	legend = c(
		paste0(
			"From  ",
			strftime(head(df_gps$Date, n=1), "%d.%m.%Y"),
			"  to  ",
			strftime(tail(df_gps$Date, n=1), "%d.%m.%Y")
		),
		paste0(
			"Total 3d shift = ",
			round(dT_3D, 2),
			" m"
		),
		paste0(
			"(E = ",
			round(dE, 2),
			"m | N = ",
			round(dN, 2),
			"m | U = ",
			round(dU, 2),
			"m)"
		),
		paste0(
			"Max velocity = ",
			round(max(S, na.rm=1), 2),
			"m/yr  (",
			strftime(smax_at, "%m.%Y)")
		),
		paste0(
			"( * estimated positions )"
		)
	),
	text.col = c(rep("black", 4), "grey40"),
	horiz = FALSE,
	cex = .95,
	bty = "n",
	bg="white"
)
# scale
scalelength	<- diff(extendrange(E))
scalelength	<- 20 * scalelength /100
if (floor(scalelength) == 0){
	scalelength <- floor(scalelength*10)/10
} else {
	scalelength <- floor(scalelength)
}
raster::scalebar(
	scalelength,
	type = "bar",
	below = "m"
)

# speed scalebar
par(new = TRUE)
par(mar = mar, pty = "m")
plot(
	0, 0,
	type = "n",
	xlim = c(0, 1), ylim = c(0, 1),
	axes = FALSE,
	xlab = "", ylab = ""
)
GISTools::north.arrow(
	0.01, .17,
	len = 0.01,
	lab = "N", cex.lab = 1,
	tcol="black"
)
n_speed 		<- 5
colsc_xy		<- c(.7, .95)
colsc_length	<- .3
colsc_inc 		<- colsc_length / n_speed
colsc_col 		<- colorRampPalette(c("yellow", "red3"))
colsc_col 		<- colsc_col( n_speed*1 )
fields::colorbar.plot(
	colsc_xy[1], colsc_xy[2],
	adj.x = 0, adj.y = .5,
	strip = as.matrix(seq(1, length(colsc_col))),
	strip.width = .015,
	strip.length = colsc_length +.125,
	col = colsc_col,
	horizontal = TRUE
)
colsc_labs_posy	<- rep(colsc_xy[2], n_speed) -.04
colsc_labs_posx	<- seq(
	colsc_xy[1] + colsc_inc/2,
	colsc_xy[1] + colsc_inc/2 + (n_speed-1) * colsc_inc,
	by = colsc_inc
)
colsc_labs	<- c(4, 5, 6, 7, 8)
text(
	colsc_labs_posx, colsc_labs_posy,
	labels = colsc_labs,
	cex = .8
)
colsc_title	<- "Perm. GPS 3D speed [m/yr]"
text(
	colsc_xy[1] + colsc_length/2, colsc_xy[2] + 0.04,
	labels = colsc_title,
	cex = 1
)

rm(
	col, colsc_col, colsc_inc,colsc_labs, colsc_labs_posx, 
	colsc_labs_posy, colsc_length, colsc_title, colsc_xy, 
	dE, dN, dp, dp_m, dT_2D, dT_3D, dU, E, mar, N, 
	n_speed, P0, Pn, s, S, scalelength, smax, smax_at, U
)

###

quartz.save(
	file = file.path(dir_exp, "plot11.png"),
	type = "png",
	dpi  = 300
)

###
