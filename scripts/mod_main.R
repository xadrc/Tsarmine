if(!exists(dir_root)){stop()}

modules <- c(1:12, 14:20, 22:27, 30)
plots   <- TRUE

for (m in modules){
    # module ID
    mod_id <- m
    # module data
    df_mod <- read.csv(
        file   = paste0(dir_raw, "/data_mod", sprintf("%02d", mod_id), ".csv"), 
        header = TRUE, 
        sep    = ","
    )
    # modules' coordinates
    df_modcoo	<- read.csv(
        file   = paste0(dir_raw, "/", "data_modcoo.csv"),
        header = TRUE,
        sep    = ","
    )
    # format
    source(file.path(dir_scr, "mod_format.r"))
    # clean
    source(file.path(dir_scr, "mod_clean.r"))
    # reshape
    source(file.path(dir_scr, "mod_reshape.r"))
    # compute
    source(file.path(dir_scr, "mod_compute.r"))
    # plots
    if(plots){
        dir_exp <- paste0(dir_plt, "/", "modules/module_", sprintf("%02d", mod_id))
        if(!file.exists(dir_exp)){
            dir.create(
                path         = dir_exp, 
                showWarnings = FALSE, 
                recursive    = TRUE
            )
        }
        # raw data
        source(file.path(dir_scr, "mod_plot11.r")) ; dev.off()
        # outliers
        source(file.path(dir_scr, "mod_plot12.r")) ; dev.off()
        # reconstruction
        source(file.path(dir_scr, "mod_plot13.r")) ; dev.off()
        # cleaned data
        source(file.path(dir_scr, "mod_plot14.r")) ; dev.off()
        # summary data
        source(file.path(dir_scr, "mod_plot15.r")) ; dev.off()
        # frequencies
        source(file.path(dir_scr, "mod_plot21.r")) ; dev.off()
        # diurnal temperature influence
        source(file.path(dir_scr, "mod_plot22.r")) ; dev.off()
        # movements
        source(file.path(dir_scr, "mod_plot31.r")) ; dev.off()
        # cumulative tilts
        source(file.path(dir_scr, "mod_plot32.r")) ; dev.off()
        # movements - radial
        source(file.path(dir_scr, "mod_plot33.r")) ; dev.off()
        # report
        source(file.path(dir_scr, "mod_plot41.r")) ; dev.off()
        rm(dir_exp)
    }
    # export
    source(file.path(paste0(dir_scr, "/", "mod_export.r")))
}

# concatenate all modules .rds
df_allmods <- list()
for (i in 1:30){
    if(file.exists(paste0("data_featmod", sprintf("%02d", i), ".rds"))){
        df_allmods[[i]] <- readRDS(
            paste0(dir_rds, "/data_featmod", sprintf("%02d", i), ".rds")
        )
    } else {
        df_allmods[[i]] <- NA
    }
}
# save
saveRDS(
    df_allmods, 
    file = file.path(dir_rds, "data_featallmods.rds")
)