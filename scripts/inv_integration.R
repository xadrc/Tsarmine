# retrieve .rds 

# data :: all modules
df_allmods <- readRDS(file.path(dir_rds, "data_featallmods.rds"))
# data :: permanent gps
df_gps <- readRDS(file.path(dir_rds, "data_gps.rds"))
# data :: weather station evolene villa
df_evo <- readRDS(file.path(dir_rds, "data_evo.rds"))
# data :: modules coordinates
df_modcoo <- readRDS(file.path(dir_rds, "data_modcoo.rds"))

# align dates 

Date <- df_allmods[[1]]$Date
Date <- data.frame(Date = Date)

df_evo <- merge(
    Date, df_evo,
    by.x = "Date",
    all.x = TRUE
)
df_gps <- merge(
    Date, df_gps,
    by.x = "Date",
    all.x = TRUE
)
