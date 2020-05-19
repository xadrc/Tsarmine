
#	1. DATA
if(!exists("df_modcoo")){
    df_modcoo <- readRDS(file.path(dir_raw, "data_modcoo.rds"))
}
if (!exists("df_meanshifts")){
	df_meanshifts <- readRDS(file.path(dir_raw, "data_modmeanshifts.rds"))
}

#	2. MAP
quartz(
	"Weekly mean shifts",
	height = 6,
	width = 6,
	pointsize = 12
)
par(omi=c(rep(0, 4)))

Elim <- extendrange(df_modcoo[ ,"cooE"])
Nlim <- extendrange(df_modcoo[ ,"cooN"])
Ulim <- extendrange(df_modcoo[ ,"cooU"])

col_disp <- rep( "black", 30 )
col_disp [ c(6, 7, 8, 9, 14, 15, 16, 19, 20, 25, 30) ] <- "grey60"

df_modcoo$w_meanshifts	<- df_meanshifts$w_meanshifts
df_modcoo$n_weeks 		<- df_meanshifts$n_weeks
df_modcoo$term			<- 0
df_modcoo$n_weeks[is.na(df_modcoo$n_weeks)] <- 0

for (i in 1:30){
	if (df_modcoo$n_weeks[ i ] <= 13){
		df_modcoo$term[ i ] = 1
	}
	if (df_modcoo$n_weeks[ i ] <= 26 & df_modcoo$n_weeks[ i ] > 13){
		df_modcoo$term[ i ] = 2
	}
	if (df_modcoo$n_weeks[ i ] <= 39 & df_modcoo$n_weeks[ i ] > 26){
		df_modcoo$term[ i ] = 3
	}
	if (df_modcoo$n_weeks[ i ]  >  39){
		df_modcoo$term[ i ] = 4
	}
}

moved	<- df_modcoo[c(6, 7, 8, 9, 14, 15, 16, 19, 20, 25, 30), ]
stayed 	<- df_modcoo[c(1, 2, 3, 4, 5, 10, 11, 12, 17, 18, 22, 23, 24, 26, 27), ]

shifted			<- df_modcoo[c(7, 11, 12, 18, 24, 25, 27, 30 ), ]
shifted$shift 	<- c(2.33, 3.87, 28.12, 1.01, 1.32, 17.50, 6.29, 42.81 )

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
box(lwd = .1 ) ; axis(1) ; axis(2) ; grid()

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

#___Magnitude_shift__

col <- c(
	rgb(106/255, 174/255, 209/255),
	rgb(64/255, 146/255, 193/255),
	rgb(30/255, 114/255, 174/255),
	rgb(5/255, 82/255, 150/255)
)
points(
	df_modcoo$cooE, df_modcoo$cooN,
	pch = "+",
	cex = .8,
	col = "black"
)
points(
	df_modcoo$cooE, df_modcoo$cooN,
	pch = 16,
	cex = df_modcoo$w_means*10,
	col = paste(col[df_modcoo$term], "1A", sep = "")
)
points(
	df_modcoo$cooE, df_modcoo$cooN,
	pch = 1, lwd = 1,
	cex = df_modcoo$w_means*10,
	col = col [ df_modcoo$term ]
)
text(
	df_modcoo$cooE+3, df_modcoo$cooN+2,
	labels = df_modcoo$TAG,
	cex = .9,
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


par(new = TRUE)
par(mar = c(2, 2, 0, 0), pty = "m")
plot(
	NA,
	type = "n",
	xlim = c(0, 1), ylim = c(0, 1),
	axes = FALSE,
	xlab = "", ylab = ""
)
legend(
	"bottomright", 1,
	title ="lengths of recordings \n (weeks)",
	legend = c( "< 13", "< 26", "< 39", "< 52"),
	pch = 1, cex = .9,
	col = c( col[1], col[2], col[3], col[4]),
	text.col = "grey10",
	xjust = .5,
	bty = "n", bg = "white"
)
legend(
	.75, -.05,
	xjust = 1,
	yjust = 0,
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
	pt.cex = c(1, 1, NA, 0.2),
	lty = c(NA, NA, 1, NA),
	pch = c(3, 17, NA, NA),
	horiz = FALSE,
	bty = "n",
	bg = "white"
)

scale_leg <- data.frame(
	x = rep( .12, 3 ),
	y = c( .80, .64, .54 ),
	z = c( 15, 10, 5 )
)
points(
	scale_leg$x, scale_leg$y,
	pch = 16,
	cex = scale_leg$z,
	col = rgb( 180/255, 180/255, 180/255, .07 )
)
points(
	scale_leg$x, scale_leg$y,
	pch = 1,
	lwd = .2,
	cex = scale_leg$z,
	col = "grey30"
)
text(
	scale_leg$x, scale_leg$y,
	labels = paste0(scale_leg$z/10),
	col = "grey50",
	cex = .9
)
text(
	scale_leg$x[1], scale_leg$y[3] - .07,
	labels = "[deg/week]",
	col = "grey50",
	cex  = .9
)

#___Save___
quartz.save(
	file = file.path(dir_plt, "maps", "map_weeklyshifts.png"),
	type = "png",
	dpi = 300
)
