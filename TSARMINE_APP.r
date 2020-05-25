
#___DIRECTORIES___

dir_root <- getwd()
  message(paste0("Root:"))
  message(paste0("  ~ ", dir_root))

dir_scr <- paste0(dir_root, "/scripts")    # scripts
dir_raw <- paste0(dir_root, "/data/raw")   # input: raw data
dir_rds <- paste0(dir_root, "/data/rds")   # clean db
dir_plt <- paste0(dir_root, "/plots")      # plots

#___PACKAGES___

req <- readLines(paste0(dir_scr, "/", "requirements.txt"), warn = FALSE)

for (package in req){
    if (!require(package, character.only = TRUE, quietly = TRUE)){
        install.packages(package)
        library(package, character.only = TRUE)
    }
}

rm(req, package)

message("Required packages installed")

#___SCRIPTS___

# global positionning system
source(file.path(dir_scr, "gps_main.r"))

# weather station evolene
source(file.path(dir_scr, "evo_main.r"))

# individual modules
source(file.path(dir_scr, "mod_main.r"))

# integration & modelisation
source(file.path(dir_scr, "inv_main.r"))

# rm(list=ls(all=TRUE))
