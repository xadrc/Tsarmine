quartz( title = "reconstructed points", width = 10, height  = 5, pointsize = 12 )
par( mfrow = c( 3, 1), omi = c(.3, .3, 0, 0) )
layout( matrix( c(1, 2, 3), byrow = FALSE), heights = c( rep(1, 3) ) )

mar <- rep(0, 4)

#		___[  data  ]___

#	spot fixed values
df_modc$fix <- TRUE
df_modc$fix[ df_modc$clust.obs == 0 ]	<- FALSE
df_modc$fix[ df_modc$val == 1 ]			<- FALSE

df_modc$field1_fx <- df_modc$field1
df_modc$field2_fx <- df_modc$field2
df_modc$field3_fx <- df_modc$field3

df_modc$field1_fx[ df_modc$fix != 1 ] <- NA
df_modc$field2_fx[ df_modc$fix != 1 ] <- NA
df_modc$field3_fx[ df_modc$fix != 1 ] <- NA

#		___[  date ticks  ]___

ticks <- seq.Date(
	from=as.Date("2015-05-01"),
	to=as.Date("2016-05-01"),
	by = "month"
)
ticks <- as.POSIXct( ticks, format = "%d,%m.%y %H")

#		___[  field_1  ]___

par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field1,
	type = "l", lwd = .15,
	col = "green4",
	axes = FALSE,
	xlab = "", ylab = "",
	xlim = range(df_mod$Date)
)
lines(
	df_modc$Date, df_modc$field1_fx,
	type = "p", pch = "+",
	cex = 0.6,
	col = "green4"
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Ry  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field1)),
	lty = 3, col = "lightgrey"
)
box( lwd = .15 )

#		___[  field_2  ]___

par(new = FALSE )
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field2,
	type = "l", lwd = .15,
	col = "red4",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
lines(
	df_modc$Date, df_modc$field2_fx,
	type = "p", pch = "+",
	cex = 0.6,
	col = "red4"
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Rx  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field2)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#		___[  field_3  ]___

par(new = FALSE )
par(mar = mar, pty ="m")
plot(
	df_modc$Date, df_modc$field3,
	type = "l", lwd = .15,
	col = "blue4",
	xlab = "", ylab = "",
	xlim = range(df_mod$Date),
	axes = FALSE
)
lines(
	df_modc$Date, df_modc$field3_fx,
	type = "p", pch = "+",
	cex = 0.6,
	col = "blue4"
)
axis(
	side = 2,
	at = NULL,
	cex.axis = 1.2
)
legend(
	"topleft", 1,
	legend = c("Rz  [deg]"),
	cex = 1.4,
	bty = "n"
)
abline(
	v=ticks,
	h= pretty(extendrange(df_modc$field3)),
	lty = 3,
	col = "lightgrey"
)
box(lwd = .15)

#		___[  attributes  ]___

axis.POSIXct(
	side = 1,
	df_mod$Date,
	format = "%m.%y",
	at = ticks,
	cex.axis = 1.2
)
#	mtext( "Date", side = 1, line = 2.5, las = 0, cex = 1.2)
legend(
	"bottomleft", 1,
	pch = c("+"), col = c("black"),
	legend = c("Reconstructed points"),
	bty = "n", cex = 1.2
)
#df_modc[ , c( "fix", "field1_fx", "field2_fx", "field3_fx") ] 	<- NULL

#		___[  save plot  ]___

rm(ticks, mar)
quartz.save(
    file  = file.path(dir_exp, "plot_13.png"), 
    type = "png", 
    dpi  = 300
)
