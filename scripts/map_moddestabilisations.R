
#	1. DATA
if(!exists("df_modcoo")){
    df_modcoo <- readRDS(file.path(dir_raw, "data_modcoo.rds"))
}


#	2. MAP
quartz(
	"Destabilized blocs",
	height = 6,
	width = 6,
	pointsize = 12
)
par(omi=c(rep(0, 4)))

Elim <- extendrange(df_modcoo[,"cooE"])
Nlim <- extendrange(df_modcoo[,"cooN"])
Ulim <- extendrange(df_modcoo[,"cooU"])

col_disp <- rep("black", 30)
col_disp[c(6, 7, 8, 9, 14, 15, 16, 19, 20, 25, 30) ] <- "grey60"

shifted			<- df_modcoo[ c( 7, 11, 12, 18, 24, 25, 27, 30 ), ]
shifted$shift 	<- c( 2.33, 3.87, 28.12, 1.01, 1.32, 17.50, 6.29, 42.81 )

#___Window___

par(mar = c( 2, 2, 0, 0), pty = "m")
plot(
	NA,
	type = "n",
	axes = FALSE,
	ylab = "", xlab = "",
	ylim = Nlim,
	xlim = Elim,
	asp = 1
)
box(lwd = .1) ; axis(1) ; axis(2) ; grid()

#___Magnitude_shift__
col <- rgb( 180/255, .1/255, .1/255 )
points(
	shifted$cooE, shifted$cooN,
	pch = 1, lwd = .8,
	cex = shifted$shift/2,
	col = col
)
points(
	shifted$cooE, shifted$cooN,
	pch = 16,
	cex = shifted$shift/2,
	col = paste(col, "1A", sep = "")
)

#___Coo_Modules___
points(
	df_modcoo$cooE, df_modcoo$cooN,
	pch = "+",
	cex = .8,
	col = "black"
)
text(
	df_modcoo$cooE +3, df_modcoo$cooN +2,
	labels = df_modcoo$TAG,
	cex = 1,
	col = "black"
)
points(
	shifted$cooE, shifted$cooN,
	pch = "+",
	cex = .9,
	col = col,
)

#___Coo_Perm_GPS___

points(
	605229.409, 99443.994,
	pch = 17,
	cex = 1.2,
	col = "red"
)
text(
	605229.409, 99443.994-3,
	labels = " ",
	col = "red"
)

#___Map_attr___
require(raster)
require(GISTools)
raster::scalebar(
	20,
	xy = c(605160, 99480),
	type = "bar",
	divs = 2,
	below = "m",
	#lonlat = TRUE,
	label = c(0, NA, 20)
)
GISTools::north.arrow(
	605190, 99481,
	len = 1,
	lab = "N"
)

#___Legend___
legend(
	"bottomrigh", 1,
	title = "Coordinate system [CH03]",
	legend = c(
		"Modules",
		"Permanent GPS",
		"Rock glacier front",
		"[ 22.04.2015 ]"
	),
	cex = .9,
	col = c(
		"black",
		"red",
		"black",
		NA
	),
	text.col = c(
		"black",
		"black",
		"black",
		"grey30"
	),
	pt.cex = c(1, 1,  NA, 0.2),
	lty = c(NA, NA, 1, NA),
	pch = c(3, 17, NA, NA),
	horiz = FALSE,
	bty = "n",
	bg = "white"
)

par(new = TRUE)
par(mar = c(2, 2, 0, 0), pty = "m")
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0, 1),
	axes = FALSE,
	xlab = "", ylab = ""
)

scale_leg <- data.frame(
	x = rep(.11, 4),
	y = c(.75, .58, .44, .34),
	z = c(40, 30, 20, 10)
)
points(
	scale_leg$x, scale_leg$y,
	pch = 16,
	cex = scale_leg$z/2,
	col = rgb(180/255, 180/255, 180/255, .05)
)
points(
	scale_leg$x, scale_leg$y,
	pch = 1,
	lwd = .25,
	cex = scale_leg$z/2,
	col = "grey30"
)
text(
	scale_leg$x, scale_leg$y,
	labels = paste0(scale_leg$z,"°"),
	col = "grey50"
)

#___Save___

quartz.save(
	file = file.path(dir_plt, "maps", "map_destabilisations.png"),
	type = "png",
	dpi = 300
)
