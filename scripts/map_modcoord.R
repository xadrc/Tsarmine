require(raster)
require(GISTools)

#	1. DATA

if(!exists("df_modcoo")){
    df_modcoo <- readRDS(file.path(dir_raw, "data_modcoo.rds"))
}


#	2. MAP
quartz(
	"Modules GPS coordinates",
	height = 6,
	width = 6,
	pointsize = 12
)
par(omi=c(rep(0, 4)))

Elim <- extendrange(df_modcoo[ ,"cooE"])
Nlim <- extendrange(df_modcoo[ ,"cooN"])
Ulim <- extendrange(df_modcoo[ ,"cooU"])

#disp <- rep("black", 30)
#disp[c(6, 7, 8, 9, 14, 15, 16, 19, 20, 22, 23, 25, 30) ] <- "grey70"

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

#___Coo_Modules___
points(
	df_modcoo$cooE, df_modcoo$cooN,
	pch = "+",
	cex = .8,
	col = "black"
)
text(
	df_modcoo$cooE+3, df_modcoo$cooN+2,
	labels = df_modcoo$TAG,
	cex = 1,
	col = "black"
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

#___Traj_Perm_GPS___
# avoid NAs
#fromE	<- na.omit(gps$cooE)
#fromN	<- na.omit(gps$cooN)
#fromE	<- head(fromE, 1)
#fromN	<- head(fromN, 1)
#toE		<- na.omit(gps$cooE)
#toN		<- na.omit(gps$cooN)
#toE		<- tail(toE, 1)
#toN		<- tail(toN, 1)

#E	<- na.omit(gps$cooE)
#N	<- na.omit(gps$cooN)
#U	<- na.omit(gps$cooU)
#dE	<- abs(max(E)-min(E))
#dN	<- abs(max(N)-min(N))
#dU	<- abs(max(U)-min(U))

#dT 	<- round(sqrt((dE^2)+(dN^2)+(dU^2)), 2)
#span	<- round(difftime(gps$Date[nrow(gps)], gps$Date[1], units = "days"), 2)

#___Map_attr___
raster::scalebar(
	20,
	xy = c(605170, 99480),
	type = "bar",
	divs = 2,
	below = "m",
	#lonlat = TRUE,
	label = c(0, NA, 20)
)
GISTools::north.arrow(
	605162, 99480,
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


#___Save___
quartz.save(
	file = file.path(dir_plt, "maps", "map_modcoordinates.png"),
	type = "png",
	dpi = 300
)
