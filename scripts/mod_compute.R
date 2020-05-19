#####################
#___COMPUTE_DATA____#
#####################

# Angular rotational values to acceleration rates
# (pitch, roll, yaw) to (gX, gY, gZ)

fun_angtoacc <- function(pitch, roll, yaw){
    matRy <- matrix(
        data = c(	
            cos(pitch), 0, sin(pitch),
            0, 1, 0,
            -sin(pitch), 0, cos(pitch)
        ),
        nrow  = 3, 
        ncol  = 3,
        byrow = TRUE,
        dimnames = NULL
    )
    matRx <- matrix(
        data = c(
            1, 0, 0,
            0, cos(roll), -sin(roll),
            0, sin(roll), cos(roll)
        ),
        nrow  = 3, 
        ncol  = 3,
        byrow = TRUE,
        dimnames = NULL
    )
    matRz <- matrix(
        data = c(
            cos(yaw), -sin(yaw), 0,
            sin(yaw), cos(yaw), 0,
            0, 0, 1
        ),
        nrow  = 3, 
        ncol  = 3,
        byrow = TRUE,
        dimnames = NULL
    )
    mat0 <- matrix(
        data = c(0, 0, -1),
        nrow = 3, 
        ncol = 1,
        dimnames = NULL
    )
    return((matRy %*% matRx %*% matRz) %*% mat0)
}

for (i in 1:nrow(df_modc)){
    pitch <- (df_modc$field1[i] *pi) /180			# R(Y)
    roll  <- (df_modc$field2[i] *pi) /180			# R(X)
    yaw	  <- (df_modc$field3[i] *pi) /180			# R(Z)
    df_modc$X[i] <- fun_angtoacc(pitch, roll, yaw)[1,]
    df_modc$Y[i] <- fun_angtoacc(pitch, roll, yaw)[2,]
    df_modc$Z[i] <- fun_angtoacc(pitch, roll, yaw)[3,]
}

rm(pitch, roll, yaw)

# Set virtual initial position
# (reset gX and gY to 0 --> g[0; 0; -1])

P1	<- na.omit(df_modc$field1)[1] ; P1 <- (P1 *pi) /180	  # 1st Pitch measurement
R1	<- na.omit(df_modc$field2)[1] ; R1 <- (R1 *pi) /180	  # 1st Roll measurement

df_modc$Xr	<- df_modc$X*cos(P1) - df_modc$Z*sin(P1)
df_modc$Yr	<- df_modc$X*sin(R1)*sin(P1) + df_modc$Y*cos(R1) + df_modc$Z*sin(R1)*cos(P1)
df_modc$Zr	<- df_modc$X*cos(R1)*sin(P1) - df_modc$Y*sin(R1) + df_modc$Z*cos(R1)*cos(P1)

rm(P1, R1)

# Calculate Tilt and Azimuth from cart. coordinates

fun_sphrcoo <- function(x_coo, y_coo, z_coo){
    # data
    cart_coo	<- data.frame(cbind(x_coo, y_coo, z_coo))
    missval		<- as.numeric(as.logical(is.na(x_coo)))
    # comp
    tilt	<- acos(-cart_coo[,3])	# [rad]
    tilt	<- tilt *180 /pi		# [deg]
    az	<- base::atan2(cart_coo[,1], cart_coo[,2])	# [rad]
    az	<- az *180 /pi					    		# [deg]
    # output
    sphr_coo	<- data.frame(cbind(tilt, az))
    return(sphr_coo)
}

df_modc$tilt <- fun_sphrcoo(df_modc$Xr, df_modc$Yr, df_modc$Zr)[,1]
df_modc$dir	 <- fun_sphrcoo(df_modc$Xr, df_modc$Yr, df_modc$Zr)[,2]

# Function to convert calc. mathematical angle to geographical azimuth

fun_azgeo <- function(math_ang, az_ang, az_axis){
    math_ang	<- as.vector( math_ang )
    az_ang		<- as.numeric( az_ang )
    az_axis		<- as.character( az_axis )
    na_index	<- is.na( math_ang )
    math_ang[is.na(math_ang)]	<- 0
    geo_ang		<- math_ang
    n			<- length(math_ang)
    #	convert [-180° ; +180°] to [0° ; 360°]
    for (i in 1:n){
        if(geo_ang[i] < 0 & geo_ang[i] >= -180){
            geo_ang[i] <- 360 - math_ang[i] * (-1)
        }else if(geo_ang[i] > 0 & geo_ang[i] < 180){
            geo_ang[i] <- math_ang[i]
        }
    }
    #	set 0° to geo North (90°)
    for(i in 1:n){
        if(geo_ang[i] >= 90 & geo_ang[i] <= 360){
            geo_ang[i] <- geo_ang[i] - 90
        }else if(geo_ang[i] < 90){
            geo_ang[i] <- geo_ang[i] - 90 + 360
        }
    }
    #	convert counter-clockwise to clockwise
    geo_ang	<- 360 - geo_ang
    #	correct orientation with field records
    '%R%' <<- function(p1, r1){
        p2	<- p1 - r1
        if(p2 >= 360) {p2 <- p2-360}
        if(p2 < 0)	  {p2 <- p2+360}
        return(p2)
    }
    if(az_axis == "X"){
        az_ang <- az_ang - 90
    }
    for(i in 1:n){
        geo_ang[i] <- geo_ang[i]  %R%  az_ang
    }
    #	output -- exit
    geo_ang[na_index == TRUE] <- NA
    return(geo_ang)
}

#	azimuth record

az 		<- c(NA,NA)
az[1]	<- df_modcoo$Az[mod_id]				        # Retrieve azimuth field record
az[2]	<- as.character(df_modcoo$axis[mod_id])		# Recorded on axis "..."

#	apply function

df_modc$azgeo	<- fun_azgeo(df_modc$dir, az[1], az[2])

# Remove 24h-oscillations due to temperatures' influences on modules

require("caTools")
df_modc$tilt_24		<- caTools::runmean(df_modc$tilt,  k=24, endrule = "mean")
df_modc$azgeo_24	<- caTools::runmean(df_modc$azgeo, k=24, endrule = "mean")
df_modc$tilt_24[df_modc$clust.obs == 0]  <- NA
df_modc$azgeo_24[df_modc$clust.obs == 0] <- NA

# Isolate periodicity

df_modc$tilt_per	<- df_modc$tilt - df_modc$tilt_24
df_modc$azgeo_per	<- df_modc$azgeo - df_modc$azgeo_24

# Derivatives

df_modc$speed		<- c(0, diff(df_modc$tilt_24))
df_modc$acc			<- c(0, diff(df_modc$speed))

# Automatic jumps detection

fun_detectjumps <- function( datetime, data, k, thres ){
    # function to detect discrete accelerations in
    # rotational speed of the module by setting a slope threshold
    # and comparing pairwise k-spaced observations' slopes
    # to this threshold
    require("zoo")
    data <- as.vector(data)
    n 	 <- length(data)
    k	 <- as.numeric(k)
    if(k%%2 !=1){
        k <- k+1
        message(paste0("'k' changed to ", k))
    }
    m 		<- (k-1) /2
    thres 	<- as.numeric(thres) ; thres <- thres *pi /180
    # Declare 2 logical arrays for positive and negative jumps
    j_up 	<- rep(F, n)
    j_dw 	<- rep(F, n)
    # Set values to TRUE if slope threshold is exceeded
    for(i in (m+1):(n-m)){
        x 	  <- data[c(i-m, i+m)]
        diff  <- x[2] - x[1]
        slope <- base::atan(abs(diff) /k)
        if(slope >= thres & diff > 0){
            j_up[c((i-m):(i+m))] <- j_up[c((i-m):(i+m))] | TRUE
        }
        if(slope >= thres & diff < 0){
            j_dw[c((i-m):(i+m))] <- j_dw[c((i-m):(i+m))] | TRUE
        }
        if(slope < thres){
            next
        }
    }
    if(sum(j_up, na.rm = TRUE) > 0){
        j_up 	<- fun_clustID(j_up, 24)
    }
    if(sum(j_dw, na.rm = TRUE ) > 0){
        j_dw 	<- fun_clustID(j_dw, 24)
    }
    # Remove ends if detection has surpassed actual jump
    data <- zoo::zoo( data )
    if(max(j_up, na.rm = T) >= 1){
        for(i in 1:max(j_up, na.rm = T)){
            x		<- subset(data, j_up == i)
            x_min	<- index(x)[which.min(x)]
            x_max	<- index(x)[which.max(x)]
            if(index(x)[1] != x_min){
                j_up[c(index(x)[1]:x_min)] <- 0
            }
            if(x_max != index(x)[length(x)]){
                j_up[c(x_max:index(x)[length(x)])] <- 0
            }
        }
    }
    if(max(j_dw, na.rm = T)>=1){
        for (i in 1:max(j_dw, na.rm = T)){
            x 		<- subset(data, j_dw == 0)
            x_max 	<- index(x)[which.max(x)]
            x_min 	<- index(x)[which.min(x)]
            if(index(x)[1] != x_max){
                j_dw[c(index(x)[1]:x_max)] <- 0
            }
            if(x_min != index(x)[length(x)]){
                j_dw[c(x_min:index(x)[length(x)])] <- 0
            }
        }
    }
    rm(x)
    if(sum(j_up, na.rm = T)>0){
        j_up 	<- fun_clustID(j_up, 24)
    }
    if(sum(j_dw, na.rm = T) > 0){
        j_dw 	<- fun_clustID(j_dw, 24)
    }
    data 	<- as.vector(data)
    # Combine positive and negative jumps in one data frame and exit
    return(
        data.frame(
            absDate = datetime, 
            j_up    = as.logical(j_up), 
            j_dw    = as.logical(j_dw)
        )
    )
}

# Set threshold of 1° per day (24h)

thres	<- atan(1/24) *180 /pi

# Declare output list

list	<- list()

# Apply function within sound clusters

df_modc$absDate	<- seq( 1, nrow(df_modc))

for (i in 1:(max(df_modc$clust.obs, na.rm = TRUE))){
    x		  <- subset(df_modc, clust.obs == i)
    jumps	  <- fun_detectjumps(x$absDate, x$tilt_24, 25, thres)
    list[[i]] <- jumps
    rm(jumps, x)
}

# Reshape list to dataframe and combine to df_modc

jumps	<- as.data.frame(matrix(NA, ncol = 3))
colnames(jumps)	<- c("absDate", "j_up", "j_dw")

for (i in 1:(max(df_modc$clust.obs, na.rm = TRUE))){
    j 		<- as.data.frame(list[[i]])
    jumps	<- rbind(jumps, j)
}

jumps			<- jumps [-1,]
df_modc			<- merge(df_modc, jumps, by.x="absDate", all.x=TRUE)
df_modc$jumps	<-  df_modc$j_up | df_modc$j_dw
df_modc$ramps	<- !df_modc$jumps

rm(list, j, thres, jumps)

# Total weekly vertical shifts

require(xts)
fun_wtot <- function(date, data){
    if(!inherits(date, "POSIXct")){
        stop()
    }
    t		<- seq(date[1], date[length(date)], by = "hours")
    t		<- data.frame(Date = t)
    data	<- as.vector(data)
    diff	<- abs(c(NA, diff(data)))
    diff[is.na(diff)]	<- 0
    x		<- xts::xts(x = diff, order.by = date)
    wtot	<- xts::apply.weekly(x = x, FUN = base::sum)
    wtot	<- data.frame(Date = index(wtot), tot = as.vector(wtot[,1]))
    wtot$tot[is.na(wtot$tot)] <- -999
    wtot	 <- merge(t, wtot, by.x = "Date", all.x = TRUE)
    wtot$tot <- na.locf(wtot$tot, fromLast = TRUE, na.rm = FALSE)
    wtot$tot[wtot$tot == -999]	<- NA
    wtot$tot[is.na(data)]		<- NA
    return(wtot)
}

# apply :

df_modc$wtilt	<- fun_wtot(df_modc$Date, df_modc$tilt_24)[,2]

#	SORTING
#	bloc movements sorting
df_modc$id_ev	<- fun_clustID(df_modc$jumps, 24)
n				<- max(df_modc$id_ev, na.rm = TRUE)
df_blocmvt		<- data.frame()

if(n != 0){
    for(i in 1:n){
        x <- df_modc[is.element(df_modc$id_ev, i),]
        # names
        df_blocmvt[i, "ID"]			<- i
        df_blocmvt[i, "onset"]		<- min(x$absDate)
        df_blocmvt[i, "end"]		<- max(x$absDate)
        df_blocmvt[i, "length_h"]	<- 0
        df_blocmvt[i, "length_d"]	<- 0
        df_blocmvt[i, "diff_tilt"]	<- round(tail(x$tilt_24)[1]-head(x$tilt_24)[1], 2)
    }
    df_blocmvt$onset	<- df_modc[df_blocmvt$onset, "Date"]
    df_blocmvt$end		<- df_modc[df_blocmvt$end, "Date"]
    df_blocmvt$length_h	<- difftime(df_blocmvt$end, df_blocmvt$onset, units = "hours")+1
    df_blocmvt$length_h	<- as.numeric(round(df_blocmvt$length_h, 2))
    df_blocmvt$length_d	<- difftime(df_blocmvt$end, df_blocmvt$onset, units = "days")
    df_blocmvt$length_d	<- as.numeric(round(df_blocmvt$length_d, 2))
    actspan				<- as.numeric(sum(df_blocmvt$length_h))
} else {
    df_blocmvt	<- 0
    actspan		<- 0
}

rm(n, x)