###################
#___CLEAN_DATA____#
###################

# Look for abnormal extreme values
test_ 	<- rep(TRUE, nrow(df_mod))		# declare test array for stats tests
trim_ 	<- rep(TRUE, nrow(df_mod))		# declate new array to trim data

# Test movements
# (if a change of value in Ry, Rx or Rz corresponds to a movement)
# (==0&0&0 or !=0&!=0&!=0)

fun_mvt <- function( rot1, rot2, rot3 ){
    test <- ifelse(
        ifelse( diff(rot1)==0 & diff(rot2)==0 & diff(rot3)==0, TRUE, FALSE)
        |
        ifelse( diff(rot1)!=0 & diff(rot2)!=0 & diff(rot3)!=0, TRUE, FALSE),
        TRUE, FALSE
    )
    return(c(NA,test))
}

test_ <- fun_mvt(df_mod$field1, df_mod$field2, df_mod$field3)
trim_ [test_ == FALSE] <- FALSE

# Spot abnormal temperature records from module

fun_temp <- function( temps ){
    tmin	<- -60
    tmax 	<- 60
    temps	<- as.vector(temps)
    temps[temps >= tmax] <- FALSE
    temps[temps <= tmin] <- FALSE
    return(temps)
}

test_ <- fun_temp(df_mod$field4)
trim_[test_ == FALSE] <- FALSE

# Spot collective outliers
# (if running median absolute deviation >= defined threshold)

fun_badreadings <- function(data){
    k		<- 25
    thres	<- 5
    data	<- as.vector(data)
    rMad	<- 5* caTools::runmad(data, k)
    data[ rMad > thres ]	<- FALSE
    return(data)
}
test_ <- fun_badreadings(df_mod$field1) & 
    fun_badreadings(df_mod$field2) & 
    fun_badreadings(df_mod$field3)
trim_[test_ == FALSE] <- FALSE

# Locate isolated extreme values
# (values too far from running median)

fun_outliers <- function(data){
    data	<- as.vector(data)
    m		<- .4
    k		<- 17
    IQ		<- as.numeric(quantile(data,na.rm=TRUE,3/4)-quantile(data,na.rm=TRUE,1/4))
    data [data >= runmed(data, k) + m*IQ] 	<- FALSE
    data [data <= runmed(data, k) - m*IQ] 	<- FALSE
    return(data)
}

test_ <- fun_outliers(df_mod$field1) & 
    fun_outliers(df_mod$field2) & 
    fun_outliers(df_mod$field3)
trim_[test_ == FALSE] <- FALSE

# Sort : Dissociate outliers

df_modout <- df_mod		# declare new data frame to isolate outliers
df_modout$field1 [trim_ == TRUE] <- NA
df_modout <- na.exclude( df_modout )

# Sort : dissociate clean data

df_modc <- df_mod		# declare data frame without outliers
df_modc$field1 [trim_ != TRUE] <- NA
df_modc <- na.exclude( df_modc )

rm(test_, trim_)