## This script create thresholded zscore rasters to use for masking values in the exposure
## summary table. 

library(raster)
library(rgdal)
library(ecoclim)
library(rgeos)
#library(doParallel)



###Load in rasters from appropriate time period and emissions scenario
dz <- parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/z_score_rasters/bl_near_85", drops=c(".aux",".ovr", ".xml"))
# deltas <- motleyStack(dz$path[dz$stat=="delta"])
zscores <- stackBands(dz$path, 2) # second band is zscore (1st is delta)
names(zscores) <- dz$variable
#z=names(zscores)[10]
####### create threshold rasters for each biovar ##########
for (z in names(zscores)){

  zras <- subset(zscores, z)

  thresholdRaster <- Which(zras>=1| zras<=-1)
  thresholdRaster[thresholdRaster==0]=NA

  names(thresholdRaster) <- paste0(z, "_threshold")
  writeRaster(thresholdRaster, filename=paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/near_85/masked_rasters/thresholded_zscores/", names(thresholdRaster)), format="GTiff", overwrite=T)
}

thresholds <- stack(list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/near_85/masked_rasters/thresholded_zscores/", full.names=TRUE))

######### mask deltas, zscores, and baseline means to the threshold raster per biovar ##############
bm <- parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/historic", drops=c(".aux",".ovr", ".xml")) 
bm <- motleyStack(bm$path)

for (var in unique(dz$variable)){
      #var="bio1"
       delta <- stackBands(dz$path[dz$variable==var],1)
       zscore <- stackBands(dz$path[dz$variable==var],2)

       
       mean<-bm
       names(mean)<-dz$variable
       mean <- subset(mean, var)
       
       threshold <- subset(thresholds, paste0(var, "_threshold"))

       delta_crop <- crop(delta, threshold)
       #threshold <- crop(threshold, delta)
       delta_crop <- raster::mask(delta_crop, threshold)
       zscore_crop <- crop(zscore, threshold)
       zscore_crop <- raster::mask(zscore_crop, threshold)

      mean_crop <- crop(mean, threshold)
      #threshold <- crop(threshold, mean)
      mean_crop <- raster::mask(mean_crop, threshold)
      
      writeRaster(delta_crop, filename=paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/near_85/masked_rasters/masked_deltas/", var, "_delta"), format="GTiff", overwrite=T)
      writeRaster(zscore_crop, filename=paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/near_85/masked_rasters/masked_zscores/", var, "_zscore"), format="GTiff", overwrite=T)
      writeRaster(mean_crop, filename=paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/near_85/masked_rasters/masked_baseline_means/", var, "_mean"), format="GTiff",overwrite=T)
}



