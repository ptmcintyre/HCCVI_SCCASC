
##Patrick_McIntyre@natureserve.org
#Prepare distribution rasters for HCCVI work from Landfire systems or NVC maps
#read in BPS systems raster, crop to extent of climate layers, and save each system as an individual raster file
#next step is to upscale to resolution of climate rasters
#Slow- could be updated to different workflow
gc()
library(raster)
library(rasterDT)
library(here)
library(doParallel)

#Read in CSV with names and numbers for target systems
target_systems<-read.csv(here("system_distributions/CEMML_systems.csv"), as.is=T)
#following line reads table to select only new adds (for adding in systems)
target_systems<-subset(target_systems, add=="yes")
map_values<-target_systems$NS.Map.Value.code
map_values<-unique(map_values)

#map_values<-c("7132", "7421")
#i=2
#read in a climate raster as a template for cropping vegetation systems
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
template<-raster(here("biovars/historic", historic.biovars[1]))

#Read in systems raster (here filtered to only have target systems), crop to extent of climate layers
CEMML_systems_BPS<-raster(here("system_distributions/IVC_BPS_CEMML_v846", "IVC_BPS_CEMML_v846.tif")) #clipped version

#CEMML_systems_BPS<-raster("F:/Projects/CEMML/EcosystemGrids/IVC_potv846_3sys_clip.tif")
#CEMML_systems_BPS<- crop(CEMML_systems_BPS, template)

detectCores()
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)
i=1
foreach(i=1:length(map_values)) %dopar% {
  .libPaths("C:/Users/patrick_mcintyre/Documents/R/win-library/3.5")
  library(raster)
  library(here)
  focal_system<-CEMML_systems_BPS
  focal_system<-focal_system %in% map_values[i]
  #focal_system[focal_system!=map_values[i]]<-NA
  #veg <- reclassify(veg, rcl=c(-1,1,NA,  1,Inf,1), right=F)
  select_system_info<-subset(target_systems, target_systems$NS.Map.Value.code==map_values[i])
  writeRaster(focal_system, filename=here("system_distributions/system_rasters_raw", paste(select_system_info$system_name[1],".grd", sep="")), overwrite=T)
}

stopCluster(cl)

