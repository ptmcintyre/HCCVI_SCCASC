
##Patrick_McIntyre@natureserve.org
#Prepare distribution rasters for HCCVI work from Landfire systems or NVC maps
#read in BPS systems raster, crop to extent of climate layers, and save each system as an individual raster file
#next step is to upscale to resolution of climate rasters
#Slow- could be updated to different workflow
gc()
rm()

library(raster)
library(rasterDT)
library(here)
library(doParallel)
library(terra)

#Read in CSV with names and numbers for target systems
target_systems<-read.csv(here("system_distributions/SCASC_systems.csv"), as.is=T)
#following line reads table to select only new adds (for adding in systems)
target_systems<-subset(target_systems, project=="sccasc")
map_values<-target_systems$NS.Map.Value.code
map_values<-unique(map_values)

#map_values<-c("7132", "7421")
#i=2
#read in a climate raster as a template for cropping vegetation systems
historic.biovars<-list.files(here("process_initial_climate_data/biovars_year"), pattern=".tif", full.names = T)
historic.biovars
#template<-raster(historic.biovars[1])  #using raster package
template<-rast(historic.biovars[1])  #using new terra package

#Read in systems raster (here filtered to only have target systems), crop to extent of climate layers
#CEMML_systems_BPS<-raster(here("system_distributions/IVC_BPS_CEMML_v846", "IVC_BPS_CEMML_v846.tif")) #clipped version
#CEMML_systems_BPS<-rast("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_Ecosystems_potential_NatureServe_v846.tif")
#CEMML_systems_BPS<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_BPS846_wgs84.tif")
#CEMML_systems_BPS<-rast("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_BPS846_wgs84.tif")
#activeCat(CEMML_systems_BPS, layer=1)
#setCats(CEMML_systems_BPS, layer=1,cats(CEMML_systems_BPS)[1]$Value)


detectCores()
cpus <- 2
cl <- makeCluster(cpus)
registerDoParallel(cl)
i=1
foreach(i=1:length(map_values)) %dopar% {
  #.libPaths("C:/Users/Patrick_McIntyre/Documents/R/win-library/4.1")
  .libPaths("C:/Users/patrick_mcintyre/Documents/R/win-library/4.1")  
  #library(raster)
  library(here)
  library(terra)
  CEMML_systems_BPS<-rast("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_BPS846_wgs84.tif")
  activeCat(CEMML_systems_BPS, layer=1)
  setCats(CEMML_systems_BPS, layer=1,cats(CEMML_systems_BPS)[1]$Value)
  focal_system<-CEMML_systems_BPS
  focal_system<-focal_system %in% map_values[i]
  #focal_system[focal_system!=map_values[i]]<-NA
  #veg <- reclassify(veg, rcl=c(-1,1,NA,  1,Inf,1), right=F)
  select_system_info<-subset(target_systems, target_systems$NS.Map.Value.code==map_values[i])
  writeRaster(focal_system, filename=here("system_distributions/system_rasters_raw/", paste(select_system_info$system_name[1],".tif", sep="")), overwrite=T)
}

stopCluster(cl)


?foreach
