quartz(
	title="Frequencies analysis",
	width=10, height=5,
	pointsize=12
)
par(omi=c(0, 0, 0, 0))

#	spectrogram
sp1 <- stats::spectrum(na.omit(df_modc$field1), method="ar", plot=FALSE)	#Ry
sp2 <- stats::spectrum(na.omit(df_modc$field2), method="ar", plot=FALSE)	#Rx
sp3 <- stats::spectrum(na.omit(df_modc$field3), method="ar", plot=FALSE)	#Rz
sp4 <- stats::spectrum(na.omit(df_modc$field4), method="ar", plot=FALSE)	#TempAcc

#	local maximums
require(quantmod)
p1 <- quantmod::findPeaks(as.numeric(sp1$spec))		#Ry
p2 <- quantmod::findPeaks(as.numeric(sp2$spec))		#Rx
p3 <- quantmod::findPeaks(as.numeric(sp3$spec))		#Rz
p4 <- quantmod::findPeaks(as.numeric(sp4$spec))		#TempAcc

fmax	<- max(c(max(sp1$spec), max(sp2$spec), max(sp3$spec), max(sp4$spec)))
fmin	<- min(c(min(sp1$spec), min(sp2$spec), min(sp3$spec), min(sp4$spec)))
y		<- log(sp1$spec)

par(mar = c(3, 3, 0, 0), pty = "m")
plot(
	sp1$freq, sp1$spec,
	log= "y", type="l",
	col="blue4",
	ylim=c(fmin, fmax),
	axes = FALSE,
	xlab = "", ylab = ""
)													#Ry
lines(
	sp2$freq, sp2$spec,
	type="l",
	col="red4"
)													#Rx
lines(
	sp3$freq, sp3$spec,
	type="l",
	col="green4"
)													#Rz
lines(
	sp4$freq, sp4$spec,
	type="l",
	col="orange"
)													#TempAcc
axis(
	side = 1,
	labels = TRUE,
	tick = TRUE,
	cex.axis = 1.2
)
mtext(
	"Index",
	side = 1,
	line = 2,
	las = 0,
	cex = 1.2
)
axis(
	side = 2,
	labels = TRUE,
	tick = TRUE,
	cex.axis = 1.2
)
mtext(
	"Spec",
	side = 2,
	line = 2,
	las = 0,
	cex = 1.2
)
grid()	;	box(lwd = .15)

#abline( v = p1[1]/1000, lty = 2, lwd=.5, col="blue4" )		#Ry
#abline( v = p2[1]/1000, lty = 2, lwd=.5, col="red4" )		#Rx
#abline( v = p3[1]/1000, lty = 2, lwd=.5, col="green4" )	#Rz
abline(
	v = p4[1]/1000,
	lty = 2,
	lwd=.5,
	col="orange"
)												#TempAcc
legend(
	"topright", 1,
	legend = c("Freq. Ry", "Freq. Rx", "Freq. Rz", "Freq. TempAcc"),
	pch = "_",
	col = c( "blue4", "red4", "green4", "orange" ),
	lwd = c( 1, 1, 1, 1),
	horiz = TRUE,
	bty = "n",
	bg = "white"
)

rm(sp1, sp2, sp3, sp4, p1, p2, p3, p4, fmax, fmin, y)
quartz.save(
    file  = file.path(dir_exp, "plot_21.png"), 
    type = "png", 
    dpi  = 300
)