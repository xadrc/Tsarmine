if(!exists("dir_root")){stop()}

# ___ integration ___

# integrate all data
source(file.path(dir_scr, "inv_integration.r"))
# weekly stats / all modules
source(file.path(dir_scr, "inv_modstats.r"))

# ___ map bloc movements ___

# map modules coordinates
source(file.path(dir_scr, "map_modcoor.r"))
# map modules destabilisations
source(file.path(dir_scr, "map_moddestabilisations.r"))
# map modules mean weekly shifts
source(file.path(dir_scr, "map_modweeklyshifts.r"))


# ___ models ___

# model variables
source(file.path(dir_scr, "inv_mdlvar.r"))
# covariance matrix between each bloc's movements
source(file.path(dir_scr, "inv_mdl11.r"))
# reglin :: creep / bloc tilts
source(file.path(dir_scr, "inv_mdl21.r"))
# reglin :: precipitations / bloc tilts
source(file.path(dir_scr, "inv_mdl22.r"))
# reglin :: temperatures / bloc tilts
source(file.path(dir_scr, "inv_mdl23.r"))
# reglog :: creep / destabilisations
source(file.path(dir_scr, "inv_mdl41.r"))
# reglog :: precipitations / destabilisations
source(file.path(dir_scr, "inv_mdl42.r"))
# reglog :: rainfalls + snowfalls / destabilisations
source(file.path(dir_scr, "inv_mdl43.r"))
# reglog :: rainfalls & bare ground + rainfall & snowpack / destabilisations
source(file.path(dir_scr, "inv_mdl44.r"))
# reglog :: temperatures / destabilisations
source(file.path(dir_scr, "inv_mdl45.r"))
# reglog :: temperatures + tempratures & snowpack / destabilisations
source(file.path(dir_scr, "inv_mdl46.r"))
# reglog :: temperatures + tempratures & snowpack / destabilisations
source(file.path(dir_scr, "inv_mdl46.r"))











