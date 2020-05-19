if(!exists(dir_root)){stop()}

#	1. IMPORT

df_gps <- read.csv(
	file = file.path(dir_raw, "data_gps.csv"),
	header = TRUE,
	sep    = ";"
)

#	2. FORMAT

df_gps$Date 			<- df_gps$epoch_utc
df_gps$epoch_utc 		<- NULL
df_gps$station_name 	<- NULL

df_gps$Date <- as.POSIXct(
	df_gps$Date,
	format = "%d.%m.%y %H",
	tz = "Etc/GMT"
)
df_gps <- df_gps[!duplicated(df_gps$Date),]

df_gps <- subset(
	df_gps,
	Date >= "2015-04-24 07:00:00",
	resetRownames = TRUE
)
df_gps <- subset(
	df_gps,
	Date <= "2016-04-24 07:00:00",
	resetRownames = TRUE
)

df_gps$cooE	<- as.numeric(sub(",",".", df_gps$cooE))
df_gps$cooN	<- as.numeric(sub(",",".", df_gps$cooN))
df_gps$cooU	<- as.numeric(sub(",",".", df_gps$cooU))

# convert CH1903+ to CH1903
df_gps$cooE	<- df_gps$cooE - 2*10^6
df_gps$cooN	<- df_gps$cooN - 1*10^6

# format date
df_gps$Date <- as.POSIXct(
	df_gps$Date,
	format = "%d.%m.%y %H",
	tz = "Etc/GMT"
)
df_gps <- df_gps[!duplicated(df_gps$Date), ]

#	3. RESHAPE

# coerce to one observation per hour
t	<- data.frame(Date = seq(df_gps$Date[nrow(df_gps)], df_gps$Date[1], by = "hours"))
df_gps	<- merge(
	t, df_gps,
	by.x  = "Date",
	all.x = TRUE
)
rm(t)

#	4. COMPUTE

# GPS weekly speed
# function :
gps_wspeed <- function(data_gps){
	require("xts")
	require("zoo")
	# gps_data must be organised as : Date ; East, North coordinates and altitude
	names(data_gps) <- c("D", "E", "N", "U")
	seq	<- seq(1:nrow(data_gps))
	# check if observations are complete
	act	<- ifelse(
		is.na(data_gps$E) | is.na(data_gps$N) | is.na(data_gps$U),
		FALSE, TRUE
	)
	# get position of 1st and last observation per week
	tmin <- as.numeric(xts::apply.weekly(zoo::zoo(seq, data_gps$D), base::min))
	tmax <- as.numeric(xts::apply.weekly(zoo::zoo(seq, data_gps$D), base::max))
	# declare new array for velocities values
	k <- length(tmin)
	V <- rep(0, k)
	#
	for (i in 1:k){
		streak <- subset(data_gps, seq < tmax[i] & seq >= tmin[i])
		streak <- na.omit(streak)
		if (nrow(streak) > 2){
			# first & last coordinates of week
			pos_1 <- streak[1, c(2:4)]
			pos_2 <- streak[nrow(streak), c(2:4)]
			# hourly time difference between pos_1 & pos_2
			diff_t <- abs(
				difftime(
					streak$D[1], streak$D[nrow(streak)],
					units = "hours"
				)
			)
			# distance travelled by the GPS between the two positions
			diff_s	<- sqrt((pos_2[,1]-pos_1[,1])^2+(pos_2[,2]-pos_1[,2])^2+(pos_2[,3]-pos_1[,3])^2)
			# speed (distance travelled / time difference) in meters / hour
			m_h		<- diff_s /as.numeric(diff_t)
			# speed in meters / year
			m_yr	<- m_h *8765.82
			V[i]	<- m_yr
		} else {
			V[i]	<- NA
		}
	}
	t			<- data.frame(data_gps$D[tmin], V)
	names(t)	<- c("D", "V")
	data_gps	<- merge(
		data_gps, t,
		by.x = "D",
		all.x = TRUE
	)
	data_gps$V	<- zoo::na.locf(data_gps$V, na.rm = FALSE)
	data_gps$V	<- zoo::na.locf(data_gps$V, na.rm = FALSE, fromLast = TRUE)
	data_gps$V[act == FALSE] <- NA
	return(data_gps$V)
}
# apply :
df_gps$speed_week <- gps_wspeed(df_gps)

#	3. EXPORT
saveRDS(
	object = df_gps,
	file = file.path(dir_rds, "data_gps.rds")
)

#   6. PLOTS

plots <- TRUE

if(plots){
    dir_exp = file.path(dir_plt, "gps")
    if(!dir.exists(dir_exp)){
        dir.create(
            path         = dir_exp, 
            showWarnings = FALSE, 
            recursive    = TRUE
        )
    }
    # permanent gps position
    source(file.path(dir_scr, "gps_plot11.r")) ; dev.off()
    # permanent gps speed
    source(file.path(dir_scr, "gps_plot12.r")) ; dev.off()
    rm(dir_exp)
}