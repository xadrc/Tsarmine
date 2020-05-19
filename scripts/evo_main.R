if(!exists(dir_root)){stop()}

###

df_evo <- read.csv(
	file = file.path(dir_raw, "data_evo.csv"),
	sep = ";", dec = ".",
	header = TRUE
)
df_cam <- read.csv(
	file = file.path(dir_raw, "data_cam.csv"),
	header = TRUE,
	sep = ";"
)

df_evo$Temp	<- df_evo$LT
df_evo$Prec	<- df_evo$NS
df_evo$LT	<- NULL
df_evo$NS	<- NULL
df_evo$WG	<- NULL

# temp gradient

df_evo$Temp <- df_evo$Temp - ( 0.6*(2496-1825)/100 )
df_evo$Temp[df_evo$Temp < -100] <- NA

# format date

df_evo$Date <- as.POSIXct(
	df_evo$Date,
	format = "%d.%m.%y  %H",
	tz="Etc/GMT"
)
df_cam$Date <- as.POSIXct(
	df_cam$Date,
	format = "%d.%m.%Y %H",
	tz = "Etc/GMT"
)
df_evo <- df_evo[!duplicated(df_evo$Date), ]

# format

df_evo <- merge(
	df_evo, df_cam,
	all.x = TRUE,
	by.x = "Date"
)
rm(df_cam)

df_evo <- subset(
	df_evo,
	Date >= "2015-04-24 07:00:00",
	resetRownames = TRUE
)
df_evo <- subset(
	df_evo,
	Date <= "2016-04-24 07:00:00",
	resetRownames = TRUE
)

df_evo$SnowCov <- as.logical(df_evo$SnowCov)
df_evo$SnowCov <- na.locf(df_evo$SnowCov, na.rm = FALSE)
df_evo$SnowCov <- na.locf(df_evo$SnowCov, na.rm = FALSE, fromLast = TRUE)

df_evo[df_evo == -10000000] <- NA

# Precipitations :: comp weekly totals

df_wprec <- xts::apply.weekly(
    xts::xts(
        df_evo$Prec,
        order.by = df_evo$Date
    ),
    colSums,
    na.rm = FALSE
)
df_wprec <- data.frame(
    Date = zoo::index(df_wprec),
    df_wprec = df_wprec[,1]
)
rownames(df_wprec) <- NULL
df_evo$wprec <- merge(
    data.frame(Date = df_evo$Date), df_wprec,
    by.x = "Date",
    all.x = TRUE
)[,2]
df_evo$wprec	<- zoo::na.locf(
    df_evo$wprec,
    fromLast = T,
    na.rm = FALSE
)
rm(df_wprec)

# Temperatures :: weekly means

df_wtemp <- xts::apply.weekly(
    xts::xts(
        df_evo$Temp,
        order.by = df_evo$Date
    ),
    mean,
    na.rm = FALSE
)

df_wtemp <- data.frame(
    Date = zoo::index(df_wtemp),
    df_wtemp = df_wtemp [,1]
)

rownames(df_wtemp) <- NULL

df_evo$wtemp <- merge(
    data.frame(Date = df_evo$Date), df_wtemp,
    by.x = "Date",
    all.x = TRUE
)[,2]

df_evo$wtemp <- zoo::na.locf(
    df_evo$wtemp,
    fromLast = TRUE,
    na.rm = FALSE
)

rm(df_wtemp)

### 

saveRDS(
	object = df_evo,
	file = file.path(dir_rds, "data_evo.rds")
)

###	###	###	###

plots <- TRUE

if(plots){
    dir_exp <- file.path(dir_plt, "evo")
    if(!dir.exists(dir_exp)){
        dir.create(
            path         = dir_exp, 
            showWarnings = FALSE, 
            recursive    = TRUE
        )
    }
    source(file.path(dir_scr, "evo_plot11.r")) ; dev.off()
    rm(dir_exp)
}