########################
#___SUMMARY_&_EXPORT___#
########################

#	summary
lrec	 <- nrow( df_mod )
onset	 <- df_mod [1, "Date"]
end 	 <- df_mod [ lrec, "Date" ]
length_d <- difftime(end, onset, units="days")
length_w <- difftime(end, onset, units="weeks")
obs.rec	 <- nrow(df_mod)						# n obs rec
obs.th 	 <- nrow(df_modc)						# n obs exp
obs.na 	 <- obs.th - obs.rec					# n obs NA
obs.out  <- nrow(na.omit(df_modout))			# n outliers
obs.miss <- obs.na + obs.out 					# n total missing
obs.fix	 <- obs.fix								# n obs fixed
obs.val	 <- length(na.omit(df_modc$field1))		# n obs valid
sub.seq  <- max(df_modc$clust.obs, na.rm=TRUE)	# n sound subseq

if(obs.fix > obs.miss){stop("obs.fix > obs.miss")}

data_stat1	<- matrix(
    data = c(	
        paste0(onset),
        paste0(end),
        paste0(round(length_d,1)),
        paste0(round(length_w,1))
    ),
    nrow = 4,
    ncol = 1
)

data_stat1			  <- as.data.frame(data_stat1)

colnames(data_stat1)  <- NULL
row.names(data_stat1) <- c("Sart", "End", "Length (days)", "Length (weeks)")

print("RECORDING: ")
print(data_stat1)

data_stat2	<- matrix(
    data = c(
        obs.th,
        obs.rec,
        obs.na,
        obs.out,
        obs.fix,
        obs.val
    ),
    ncol = 1
)

data_stat2				<- as.data.frame(data_stat2)
colnames(data_stat2)	<- c("n")
rownames(data_stat2)	<- c("Expected", "Recorded", "Missing", "Outliers", "Reconstructed", "Final")
data_stat2$rat			<- round(data_stat2[,1] *100 /obs.th, 1)
colnames(data_stat2)[2]	<- c("%")

print("OBSERVATIONS: ")
print(data_stat2)

print("MOVEMENTS: ")
print(df_blocmvt)

capture.output(
    list(
        stat.rec = data_stat1,
        stat.obs = data_stat2,
        stat.mvt = df_blocmvt
    ),
    file = paste0(dir_tab, "/", "mod", sprintf("%02d", mod_id), "_summary.txt")
)

rm(lrec, onset, length_d, length_w)
rm(obs.rec, obs.th, obs.na, obs.out, obs.miss, obs.fix, obs.val, sub.seq)
rm(data_stat1, data_stat2)

t <- data.frame(
    Date = seq(
        df_mod$Date[1], df_mod$Date[nrow(df_mod)], 
        by = "hours"
    )
)
df_mod <- merge(
    t, df_mod,
    by.x = "Date",
    all.x = TRUE
)

# Bloc features

features <- data.frame(
    Date	= df_modc$Date,
    tilt	= df_modc$tilt_24,
    csum	= df_modc$wtilt,
    azimuth	= df_modc$azgeo_24,
    jumps	= df_modc$jumps,
    ramps	= df_modc$ramps
)

onset	<- as.POSIXct("2015-04-24 05:00:00")
end		<- as.POSIXct("2016-04-24 20:00:00")
t <- seq(onset, end, by = "hours")
t <- data.frame(Date = t)

features <- merge(
    t, features,
    all.x = TRUE,
    by.x  = "Date"
)

if(nrow(features) != 8800){warning()}

saveRDS(
    features, 
    file = paste0(dir_tab, "/", data_featmod, sprintf("%02d", mod_id),".rds")
)

rm(features, onset, end, t)