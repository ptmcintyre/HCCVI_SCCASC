library(raster)
library(ecoclim)

#years<-seq(1950,2005, 1) #for historic
years<-seq(2006,2100, 1) #for future
#base_name<-"LOCA_CCSM4_historical"
base_name<-"LOCA_CCSM4_rcp85"

i=1
for(i in 1:length(years)) {
#clim.files <- list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/historic", pattern=paste0(years[i]), full.names =T)
clim.files <- list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85", pattern=paste0(years[i]), full.names =T)
clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4,25,29,30,31,32,33,34,35,36,26,27,28,13,17,18,19,20,21,22,23,24,14,15,16)]
clim.year<-stack(clim.files)
k <- calc(clim.year, fun=function(x)biovariables(x))
  for (j in 1:19){
    writeRaster(k[[j]], paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_year/", 
                          base_name,"_", years[i], "_", "bio",j, ".tif"), "GTiff", overwrite=TRUE)
    }
}

years<-seq(1950,2005, 1) #for historic
base_name<-"LOCA_CCSM4_historical"

i=1
for(i in 1:length(years)) {
  clim.files <- list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/historic", pattern=paste0(years[i]), full.names =T)
  #clim.files <- list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85", pattern=paste0(years[i]), full.names =T)
  clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4,25,29,30,31,32,33,34,35,36,26,27,28,13,17,18,19,20,21,22,23,24,14,15,16)]
  clim.year<-stack(clim.files)
  k <- calc(clim.year, fun=function(x)biovariables(x))
  for (j in 1:19){
    writeRaster(k[[j]], paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_year/", 
                               base_name,"_", years[i], "_", "bio",j, ".tif"), "GTiff", overwrite=TRUE)
  }
}
rm()
gc()

##comparing extentents (seeing what stacks)
# clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4,25,29,30,31,32,33,34,35,36,26,27,28,13,17,18,19,20,21,22,23,24,14,15,16)]
# clim.year<-stack(clim.files)
# 
# 
# clim.files <- list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85", pattern=paste0(years[i]), full.names =T)
# clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4)]
# clim.files<-clim.files[c(25,29,30,31,32,33,34,35,36,26,27,28)]
# 
# clim.files<-clim.files[c(13,17,18,19,20,21,22,23,24,14,15,16)]
# clim.year<-stack(clim.files)
# clim.year
# 
# 
# 26,27,28,13,17,18,19,20,21,22,23,24


