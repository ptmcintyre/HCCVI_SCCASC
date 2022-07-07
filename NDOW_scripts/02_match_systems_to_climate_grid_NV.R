

# This script upscales 90m veg distribution rasters to match the resolution of the climate raster being used
# saves them as .tif files in presence/absence based on 15cells (or user specified) or continuous counts 
# careful with projections (things should be WGS84 EPSG:4326, but climate files are sometimes odd)
   

library(dplyr)
library(raster)
library(here)
library(rasterDT)
library(exactextractr)
library(sf)
library(terra)

outdir <- here("system_distributions/LOCA_rasters")

vegtypes <- list.files("S:/Projects/NV_SWAP_HCCVI/analysis/new_systems", pattern=".tif", full.names=T)

##testr<-raster(vegtypes[1]) #check that values are 0 or 1

vegtypes<-gsub(".tif", "",vegtypes)
vegtypes.short <- list.files("S:/Projects/NV_SWAP_HCCVI/analysis/new_systems", pattern=".tif")
vegtypes.short<-gsub(".tif", "",vegtypes.short)
#vegtypes.short<-vegtypes.short[c(2,14)]

#Read in climate file to creat template raster for creating summed values of ecosystems

historic.biovars<-list.files(here("process_initial_climate_data/biovars_yearWGS84"), pattern=".tif")
historic.biovars
#template<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA Conus polygon/LOCA_WGS84_raster_template.tif")
template<-raster(here("process_initial_climate_data/biovars_yearWGS84", historic.biovars[1]))
template2<-raster(here("process_initial_climate_data/biovars_yearWGS84", historic.biovars[1]))


#?why do I jave this bit about reclassifying
template[template<30]<-0
plot(template)


#template <- reclassify(template, c(-Inf, Inf, NA))
#template <- classify(template, c(-Inf, Inf, NA))


# define the subset of types to run
#test_rast<-raster(vegtypes[1])



vegtypes_run <- vegtypes

#polygon LOCA template for summarizing bps raster values in and converting to upscaped raster 

clim_poly<-st_read("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA_grid_SCCASC_WGS84.shp") #updated with CRS- simple features no longer recognizing some older CRS refererences
## realiezed clim_poly in GCS_wgs84 (no negatives, goes 0-360)
st_crs(clim_poly)
crs(veg)

CEMML_systems_BPS<-raster("S:/Projects/NV_SWAP_HCCVI/analysis/clip_LF_BPS_aea.tif")

#template<-project(template, "EPSG:5070",method ="near")
template<-projectRaster(template, crs="+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")

clim_poly<-st_transform(clim_poly, crs=5070)

#veg=vegtypes[2]

for(veg in vegtypes_run){
      name <- vegtypes.short[match(veg, vegtypes)]
      outfile <- paste0(outdir, "/", name)
      if(file.exists(outfile)) next()
      print(veg)
      
      # open raster
      veg <- raster(paste0(veg,".tif") )
      #veg<-raster(vegtypes[1])
      plot(veg)
      
      # reclassify to either NA or 1, based on extraction of number of pixels of the sytem within MACA climate raster cells
      my.zone<-exact_extract(veg, clim_poly, 'sum')
      clim_poly$veg<-my.zone
      my.rast<-fasterizeDT(clim_poly, template, fun='sum',field="veg" )
      
      my.rast<-projectRaster(my.rast, template2 )
      upscaled <- reclassify(my.rast, rcl=c(-1,2,NA,  2,Inf,1)) #reclassifies vlaues from -1 to 14.5 as NA, and from 14.5 to 1
      
      
      
      writeRaster(upscaled, filename=paste(outfile, ".tif", sep=""), overwrite=T)
      writeRaster(my.rast, filename=paste(outfile, "_continuous.tif", sep=""), overwrite=T)
}

extent(clim_poly)
extent(template)

rm()
gc()

max(clim_poly$veg)
