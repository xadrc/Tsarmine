quartz(
	title="Radial Plot",
	width=4, height=4,
	pointsize=10
)
par(omi=c(.4, 0, 0, 0))

#		___[  data  ]___

tilt		<- df_modc$tilt_24	;	tilt[ is.na( tilt ) ]	<- 0
dir			<- df_modc$azgeo_24	;	dir[ is.na( dir) ]		<- 0
scope		<- diff(range(tilt))
maxt		<- max(tilt)		;	mint 	<- min(tilt)
maxd		<- max(dir)			;	mind	<- min(dir)
delTilt		<- maxt - mint		;	delDir	<- maxd - mind

cumTilt		<- sum( df_modc$speed, na.rm = TRUE )
meanDir		<- weighted.mean( dir, tilt, na.rm = TRUE )

'%R%' <- function(p1, r1)
{
	p2 <- p1 - r1
	if ( p2 >= 360 )	{ p2 <- p2 - 360 }
	if ( p2 < 0 )		{ p2 <- p2 + 360 }
	return( p2 )
}

if(meanDir < 180)
{
	for(i in 1:length(tilt))
	{
		dir[ i ] <- dir[ i ]  %R%  180
	}
		meanDir <- meanDir  %R%  180
}

#		___[  plot  ]___

par(mar = c(0, 0, 0, 0), pty = "m")
require(plotrix)
plotrix::polar.plot(
	tilt, dir,
	point.symbols=".", cex = 1,
	line.col = 4, point.col = 4,
	start = 90,
	clockwise = TRUE,
	rp.type = "s", lwd = .5,
	radial.lim = c(0, scope)
)

ang	<- meanDir %R% 90
ang	<- 360 - ang

require(circular)
p1	<- delTilt * cos(circular::rad(ang))
p2	<- delTilt * sin(circular::rad(ang))

arrows(
	0, 0,
	p1, p2,
	col = "red",
	length = .1
)

mtext("N",	side = 3, col = "red", 		line = 1, las = 1)
mtext("S", side = 1, col = "orange",	line = 1, las = 1)
mtext("E", side = 4, col = "green", 	line = 1, las = 1)
mtext("W", side = 2, col = "blue", 		line = 1, las = 1)

#legend ("topright",-1,
		#	title="",
		#	legend=c("measured points"),
		#	pch = ".",
		#	col=4,
		#	cex=0.8,
		#	horiz=TRUE,
		#	bty = "n",
		#	bg="white"
		#	)

mtext(
	paste0("Tilt scope : ", round(scope, 2), " [deg]"),
	1, 2
)
#mtext( paste0("Cumulative tilt : ", round(cumTilt, 2), " [deg]"), 1, 3)
mtext(
	paste0("Mean tilt direction : ~", round(meanDir, 2), " [deg]"),
	1, 3
)

rm(tilt, dir, scope, maxt, mint, maxd, mind, delTilt, delDir, cumTilt, meanDir, ang, p1, p2)

quartz.save(
    file  = file.path(dir_exp, "plot_33.png"), 
    type = "png", 
    dpi  = 300
)