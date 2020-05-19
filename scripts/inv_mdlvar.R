#	___ Response

destabilizations	<- df_jumps$act

#	___ Independent variables

temperatures		<- df_evo$Temp

precipitations		<- df_evo$Prec
precipitations[precipitations == 0] <- 0

snowpack 	<- df_evo$SnowCov

rainfall	<- precipitations
rainfall[temperatures <= -1] <- 0

snowfall	<- precipitations
snowfall[temperatures >- 1] <- 0

rain_on_snowpack	<- precipitations
rain_on_snowpack[temperatures <= -1] <- 0
rain_on_snowpack[snowpack == FALSE ] <- 0

rain_on_bareground	<- precipitations
rain_on_bareground [temperatures <= -1] <- 0
rain_on_bareground [snowpack == TRUE] 	<- 0

htemp_w_snowpack	<- temperatures
htemp_w_snowpack[snowpack == FALSE]		<- 0
htemp_w_snowpack[htemp_w_snowpack < 0] 	<- 0

temp_wo_htsp	<- temperatures
temp_wo_htsp[htemp_w_snowpack != 0] <- 0

#	___ Save in Df
df_modelvar <- data.frame(
    destabilizations	= as.logical(destabilizations),
    temperatures 		= temperatures,
    precipitations		= precipitations,
    snowpack			= snowpack,
    htemp_w_snowpack	= htemp_w_snowpack,
    rainfall 			= rainfall,
    snowfall 			= snowfall,
    rain_on_snowpack	= rain_on_snowpack,
    rain_on_bareground	= rain_on_bareground,
    gps_w_speed			= df_gps$speed_week,
    temp_wo_htsp		= temp_wo_htsp
)

# ___ Exp 

saveRDS(
    object 	= df_modelvar,
    file 	= file.path(dir_rds, "data_modelvar.rds")
)