#####################
#___RESHAPE_DATA____#
#####################

# Coerce recordings to one observation per hour
n		<- nrow( df_modc )
t		<- seq( df_modc$Date[ 1 ], df_modc$Date[ n ], by="hours")
t		<- data.frame(Date = t)
df_modc	<- merge(t, df_modc, by.x="Date", all.x=TRUE)

rm( n, t )

# Cluster sound data

fun_clustID <- function(data, intermin){
    # cluster sound data, ie data with number of consecutive missing
    # values not exceeding a defined threshold intermin
    ## Observations with consecutive missing values < 'intermin'
    ## are defined to belong to a same cluster.
    ### A number a missing consecutive values greater then the threshold
    ### leads to the declaration of a new cluster
    require("zoo")
    data[is.na(data)]	<- 0
    data[1]	     		<- 0
    if (sum(as.logical(data), na.rm=T) == 0){
        clust_1	<- 0
    }else{
        activity	<- ifelse(data != 0, TRUE, FALSE)
        inc			<- intermin
        streak		<- rep(NA, length(data))
        streak[1]	<- inc+1
        for (i in 2:length(data)){
            if(activity[i] == TRUE){
                inc			<- 0
                streak[ i ]	<- 0
            }else if(activity[i] == FALSE){
                inc			<- inc+1
                streak[ i ]	<- inc
            }
        }
        ID		<- 0
        clust_1	<- rep(NA, length(data))
        for(i in 1:length(data)){
            if(activity[i] == FALSE){
                clust_1[i]	<- 0
            }else if(activity[i] == TRUE & streak[(i-1)] <= intermin){
                clust_1[i]	<- ID
            }else if(activity[i] == TRUE & streak[(i-1)] >  intermin){
                ID			<- ID + 1
                clust_1[i]	<- ID
            }
        }
        clust_2	<- zoo::zoo(clust_1)
        clust_2[clust_2 == 0] <- NA
        for(k in min(clust_2, na.rm=TRUE) : max(clust_2, na.rm=TRUE)){
            x		<- clust_2 [clust_2 == k]
            index	<- zoo::index(x)
            for(j in max(index) : min(index)){
                clust_1[j]	<- k
            }
        }
    }
    clust_1[1]	<- clust_1[2]
    return(clust_1)
}

df_modc$val			<- as.numeric(!is.na(df_modc$field1))
df_modc$clust.obs	<- fun_clustID(df_modc$val, 48)

# Ignore values outside the clusters
for(i in 1:nrow(df_modc)){
    if (df_modc$clust.obs[i] == 0){
        df_modc[i, c(2:5)]	<- NA
    }
}

# Reconstruct missing values inside the clusters using Stineman's algorithm

obs.fix	<- length(df_modc$field1) - length(na.omit(df_modc$field1))

require("stinepack")
df_modc$field1	<- stinepack::na.stinterp(df_modc$field1, na.rm=F)
df_modc$field2	<- stinepack::na.stinterp(df_modc$field2, na.rm=F)
df_modc$field3	<- stinepack::na.stinterp(df_modc$field3, na.rm=F)
df_modc$field4	<- stinepack::na.stinterp(df_modc$field4, na.rm=F)

df_modc$field1 [df_modc$clust.obs == 0]	<- NA
df_modc$field2 [df_modc$clust.obs == 0]	<- NA
df_modc$field3 [df_modc$clust.obs == 0]	<- NA
df_modc$field4 [df_modc$clust.obs == 0]	<- NA

obs.fix <- obs.fix - (length(df_modc$field1) - length(na.omit(df_modc$field1)))
