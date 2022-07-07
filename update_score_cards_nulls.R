library(arcgisbinding)
library(ecoclim)
library(raster)
library(exactextractr)
library(here)
library(sf)
library(rgdal)
library(fasterize)
library(dplyr)
library(doParallel)

arc.check_product()

mygdb<-"S:/Projects/CEMML_HCCVI/updated_outputs2022/archived/Scorecards_CEMML_land.gdb"  #geodatabase with target data
subset(ogrDrivers(), grepl("GDB", name))
fc_list2 <- ogrListLayers(mygdb) #list of Feature Classes in geodatabase
layer_names<-fc_list2[1:29]





detectCores()
cpus <- 4
cl <- makeCluster(cpus)
registerDoParallel(cl)
i=4
foreach(i=1:length(layer_names), .packages=c("arcgisbinding", 'rgdal')) %dopar% {
  arc.check_product()
    mygdb<-"S:/Projects/CEMML_HCCVI/updated_outputs2022/archived/Scorecards_CEMML_land.gdb"  #geodatabase with target data
  subset(ogrDrivers(), grepl("GDB", name))
  fc_list <- ogrListLayers(mygdb) #list of Feature Classes in geodatabase
  fc <- readOGR(dsn=mygdb,layer=layer_names[i])
  
  
  names(fc)
  hist(fc$ExposureFut_85)
  hist(fc$DepartureFut_85)
  fc$SuitabilityFut_85
  mean(fc$DepartureFut_85, na.rm=T)
  
  
  fc$DepartureFut_85[is.na(fc$DepartureFut_85)]<-.001
  fc$ExposureFut_85<-rowMeans(cbind(fc$SuitabilityFut_85, fc$DepartureFut_85), na.rm=T)
  hist(fc$ExposureFut_85)
  
  fc$DepartureFut_45[is.na(fc$DepartureFut_45)]<-.001
  fc$ExposureFut_45<-rowMeans(cbind(fc$SuitabilityFut_45, fc$DepartureFut_45), na.rm=T)
  hist(fc$ExposureFut_45)
  
  fc$DepartureNr_85[is.na(fc$DepartureNr_85)]<-.001
  fc$ExposureNr_85<-rowMeans(cbind(fc$SuitabilityNr_85, fc$DepartureNr_85), na.rm=T)
  hist(fc$ExposureNr_85)
  
  fc$DepartureNr_45[is.na(fc$DepartureNr_45)]<-.001
  fc$ExposureNr_45<-rowMeans(cbind(fc$SuitabilityNr_45, fc$DepartureNr_45), na.rm=T)
  hist(fc$ExposureNr_45)
  
  fc$HCCVI_Near_45<- rowMeans(cbind(fc$ExposureNr_45 , fc$ResilienceC), na.rm=T)
  fc$HCCVI_Near_85<- rowMeans(cbind(fc$ExposureNr_85 , fc$ResilienceC), na.rm=T)
  fc$HCCVI_Fut_45<- rowMeans(cbind(fc$ExposureFut_45 , fc$ResilienceC), na.rm=T)
  fc$HCCVI_Fut_85<- rowMeans(cbind(fc$ExposureFut_85 ,fc$ResilienceC), na.rm=T)
  
  arc.write(path=paste0("S:/Projects/CEMML_HCCVI/updated_outputs2022/Scorecards_CEMML_updated2022.gdb/", layer_names[i]), fc, overwrite=T)
  
}

stopCluster(cl)


mygdb2<-"S:/Projects/CEMML_HCCVI/updated_outputs2022/Scorecards_CEMML_updated2022.gdb"  #geodatabase with target data
subset(ogrDrivers(), grepl("GDB", name))
fc_list3 <- ogrListLayers(mygdb) #list of Feature Classes in geodatabase
layer_names2<-fc_list2


layer_names2
