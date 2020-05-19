###############
#___FORMAT____#
###############

# format date
df_mod$Date 		<- df_mod$created_at
df_mod$created_at 	<- NULL
df_mod$entry_id 	<- NULL

df_mod$Date <- as.POSIXct(
    df_mod$Date,
    format = "%Y-%m-%d %H",
    tz     = "Etc/GMT"
)
df_mod <- df_mod [ !duplicated( df_mod$Date), ]

# Field Work 4.7.15
shifted_mods <- c(6,14,15,16,7,8,20,19,23,22,25,30,9)

if(mod_id %in% shifted_mods){
    df_mod <- subset(
        df_mod,
        Date         >= "2015-07-04 07:00:00",
        resetRownames = TRUE
    )
} else {
    df_mod <- subset(
        df_mod,
        Date         >= "2015-04-24 07:00:00",
        resetRownames = TRUE
    )
}
df_mod <- subset(
    df_mod,
    Date         <= "2016-04-24 07:00:00",
    resetRownames = TRUE
)

rm(shifted_mods)