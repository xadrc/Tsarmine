quartz(
    title	= "Temp/prec",
    width	= 10,
    height	= 4,
    pointsize	= 10
)
par(
    mfrow	= c(1,1),
    omi	= c(.36, .36, 0, .36)
)
mar <- rep(0, 4)

require(caTools)

# ticks + date

ticks <- seq.Date(
    from = as.Date("2015-05-01"),
    to = as.Date("2016-05-01"),
    by = "month"
)
ticks <- as.POSIXct(ticks, format = "%d,%m.%y %H")
xlim <- c(min(df_evo$Date), max(df_evo$Date))

# windows + snow cov

par(new = FALSE)
par(mar = mar, pty ="m")

require(GISTools)
col <- GISTools::add.alpha("#eeeeee", .95)

plot(
    df_evo$Date, df_evo$SnowCov,
    type = "h",
    lwd = 4,
    col = col,
    xlim = xlim,
    ylim = c(.1, .9),
    xlab = "", ylab = "",
    axes = FALSE
)
par(new = TRUE)
plot(
    NA,
    xlim = xlim,
    ylim = c(-20, 30),
    xlab = "", ylab = "",
    axes = FALSE
)
box(lwd = .1)
axis.POSIXct(
    side = 1,
    df_evo$Date,
    format = "%m.%y",
    at = ticks,
    cex.axis = 1.2
)
mtext(
    "Date",
    side = 1,
    line = 2,
    las = 0,
    cex = 1.2
)
abline(
    v=ticks,
    h = c(-20, -10, 0, 10, 20, 30),
    lty = 3,
    col = "lightgrey"
)

# precipitations

par(new = TRUE)
Prec <- df_evo$Prec
Prec[df_evo$Prec == 0] <- NA
plot(
    df_evo$Date, Prec,
    xlim = xlim,
    ylim = c(-10, 15),
    type ="h",
    col="#6FB1DE",
    lwd=1.5,
    xlab="", ylab="",
    axes = FALSE,
    bty = "n"
)
axis(
    4,
    ylim= c(0, 15),
    cex.axis = 1.2
)
legend(
    "topright", 1,
    legend = c("P  [ mm/h ]"),
    cex = 1.2,
    bty = "n"
)

# temperatures

par(new = TRUE)
plot(
    df_evo$Date, df_evo$Temp,
    xlim = xlim,
    ylim = c(-20, 30),
    type="l", lwd=.1,
    col = "red3",
    axes = FALSE,
    xlab="", ylab=""
)
lines(
    df_evo$Date, caTools::runmean(df_evo$Temp, 24),
    type ="l", lwd=.8,
    col ="red"
)
axis(
    2,
    ylim = c(-20, 30),
    cex.axis = 1.2
)
legend(
    "topleft", 1,
    legend = c("T  [ °C ]"),
    cex = 1.2,
    bty = "n"
)

# attributes

legend(
    "bottomleft",
    legend = c(
        "Temperatures [°C]",
        "Temperatures [°C] (daily mean)",
        "Precipitations [mm/h]",
        "Snow cover"
    ),
    pch = c(
        NA,
        NA,
        NA,
        15
    ),
    col = c(
        "red",
        "red3",
        "skyblue3",
        "lightgrey"
    ),
    lwd = c(
        .2,
        1,
        2,
        NA
    ),
    horiz = FALSE,
    bty = "n",
    cex = 1
)

rm(
    Prec, col, mar, ticks, xlim
)

# save

quartz.save(
    file = file.path(dir_exp, "plot11.png"),
    type = "png",
    dpi  = 300
)
