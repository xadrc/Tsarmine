###############
#___IMPORT____#
###############

# remove previous data
rm(list=ls(all=TRUE))

# Set tag number
message("Module number: ")
cat()	;	mod_id <- readline()
mod_id <- as.numeric(mod_id)

# bench_1
t1 <- Sys.time()

# modules positions
df_mod	<- read.csv(
    file = paste0("data_mod", sprintf("%02d", mod_id), ".csv"),
    header = TRUE,
    sep = ","
)

#	modules coordinates
df_modcoo	<- read.csv(
    file   = "data_modcoo.csv",
    header = TRUE,
    sep    = ","
)

