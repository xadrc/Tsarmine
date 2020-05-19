# ___ all blocs' destabilisations

valid <-  c(1:12, 14:20, 22:27, 30)

df_jumps <- data.frame(Date = Date)

for (i in 1:length(valid)){
    df_jumps[ , i+1] <- df_allmods[[valid[i]]]$jumps
}
for (i in 2:length(df_jumps)){
    df_jumps[, i] <- as.numeric(df_jumps[, i])
}
df_jumps$act <- NA
for (i in 1:nrow(df_jumps)){
    df_jumps[i, "act"] <- sum(df_jumps[i, c(2:(length(df_jumps)-1))], na.rm = TRUE)
}
df_jumps$nact <- NA
for (i in 1 : nrow(df_jumps)){
    df_jumps[i, "nact"] <- sum(!is.na(df_jumps[i, c(2:(length(df_jumps)-2))]))
}

saveRDS(
    object = df_jumps,
    file = file.path(dir_rds, "data_destabilisations.rds")
)

# ___ compute weekly mean tilts

df_wtilts <- data.frame(Date = Date)

for (i in 1:length(valid)){
    df_wtilts[,i+1] <- df_allmods[[valid[i]]]$csum
}

df_wtilts$sum <- NA

for (i in 1:nrow(df_wtilts)){
    df_wtilts[i, "sum"] <- sum(df_wtilts[i, c(2:(length(df_wtilts)-1))], na.rm = TRUE)
}

df_wtilts$nact <- df_jumps$nact
df_wtilts$mean <- df_wtilts$sum / df_wtilts$nact

saveRDS(
    object = df_wtilts,
    file = file.path(dir_rds, "data_wmeantilts.rds")
)

# ___ compute cumulative tilts

df_difftilts <- data.frame(Date = Date)
for (i in 1:length(valid)){
    df_difftilts[, i+1] <- c(
        NA,
        abs(diff(df_allmods[[valid[i]]]$tilt))
    )
}
df_difftilts$sum 	<- NA

for (i in 1:nrow(df_wtilts)){
    df_difftilts[i, "sum"] <- sum(
        df_difftilts[i, c(2:(length(df_difftilts)-1))],
        na.rm = TRUE
    )
}
df_difftilts$nact <- NA

# ___ number of simultaneously active modules per hour

for (i in 1:nrow(df_difftilts)){
    df_difftilts[i, "nact"] <- sum(
        !is.na(df_difftilts[i, c(2:(length(df_difftilts)-2))])
    )
}

# ___ cumulative sum / n active modules

df_difftilts$mean <- df_difftilts$sum / df_difftilts$nact
w_means	<- xts::xts(
    df_difftilts$mean,
    order.by = df_difftilts$Date
)

# ___ weekly means

data_wmeanshifts <- readRDS(file.path(dir_rds, "data_wmeanshifts.rds"))

w_means	<- xts::apply.weekly(w_means, sum)
w_means	<- data.frame(
    Date = zoo::index(w_means),
    means = as.vector(w_means[,1])
)
w_means$means[!is.finite(w_means$means)] <- 999

t <- seq(
    df_difftilts$Date[1], df_difftilts$Date[nrow(df_difftilts)],
    by = "hours"
)
t <- data.frame(Date = t)

w_means <- merge(
    t, w_means,
    by.x = "Date",
    all.x = TRUE
)
w_means$means <- zoo::na.locf(
    w_means$means,
    fromLast = TRUE,
    na.rm = FALSE
)
w_means$means[w_means$means == 999] <- NA
df_difftilts$w_means <- w_means$means

rm(w_means)

saveRDS(
    object	= df_difftilts,
    file	= file.path(dir_rds, "data_difftilts.rds")
)