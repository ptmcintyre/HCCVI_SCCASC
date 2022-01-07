## Red List of Ecosystem Statistics for Canadian KBA analysis 
library (raster)
require(rgdal)
require(exactextractr)
require(sf)
library("arcgisbinding", lib.loc="T:/R/win-library/3.5")
library(adehabitatHR)
library(rgeos)
arc.check_product()

mygdb<-"F:/Projects/CEMML/analysis/Scorecards_CEMML_rud_updated.gdb"  #geodatabase with target data
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(mygdb) #list of Feature Classes in geodatabase
RasList <- arc.open(mygdb)@children$RasterDataset #List of rasters in geodatabase
fc_list
RasList


hex<- st_read(dsn="F:/Projects/KBAs/KBA_Canada/Data/Analyis/KBA_analysis_exp.gdb", layer="Hex100km_KBA_exp")


arc.write(path=paste0("F:/Projects/CEMML/analysis/Scorecards_CEMML_rud_updated.gdb/", type, "rud_update"), maca_focal_aea, overwrite=TRUE)


fc_list[1]
str(fc_list)


Name<-vector()
Low<-vector()
Mod<-vector()
High<-vector()
Very_High<-vector()
i=29
for (i in 1:29){
grid<- st_read(dsn="F:/Projects/CEMML/analysis/Scorecards_CEMML_rud_updated.gdb", layer=fc_list[i])
grid$HCCVI_Fut_85
Name[i]<-fc_list[i]
Very_High[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85<.25])/length(grid$HCCVI_Fut_85)
High[i] <-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.25 & grid$HCCVI_Fut_85< 0.5])/length(grid$HCCVI_Fut_85)
Mod[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.50 & grid$HCCVI_Fut_85< 0.75])/length(grid$HCCVI_Fut_85)
Low[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.75])/length(grid$HCCVI_Fut_85)

}



Name<-vector()
Low_HCCVI_Fut_85<-vector()
Mod_HCCVI_Fut_85<-vector()
High_HCCVI_Fut_85<-vector()
Very_High_HCCVI_Fut_85<-vector()

Low_Exposure_Fut_85<-vector()
Mod_Exposure_Fut_85<-vector()
High_Exposure_Fut_85<-vector()
Very_High_Exposure_Fut_85<-vector()

Low_Departure_Fut_85<-vector()
Mod_Departure_Fut_85<-vector()
High_Departure_Fut_85<-vector()
Very_High_Departure_Fut_85<-vector()

Low_Suitability_Fut_85<-vector()
Mod_Suitability_Fut_85<-vector()
High_Suitability_Fut_85<-vector()
Very_High_Suitability_Fut_85<-vector()

Low_AdaptCap_Fut_85<-vector()
Mod_AdaptCap_Fut_85<-vector()
High_AdaptCap_Fut_85<-vector()
Very_High_AdaptCap_Fut_85<-vector()

Low_Sensitivity_Fut_85<-vector()
Mod_Sensitivity_Fut_85<-vector()
High_Sensitivity_Fut_85<-vector()
Very_High_Sensitivity_Fut_85<-vector()



for (i in 1:29){

  grid<- st_read(dsn="F:/Projects/CEMML/analysis/Scorecards_CEMML_land.gdb", layer=fc_list[i])
  grid$ExposureNr_45
  na.omit(grid)
  
  Name[i]<-fc_list[i]
  
  Low_Sensitivity_Fut_85[i]<-length(grid$SensitivityC[grid$SensitivityC>=.75])/length(grid$SensitivityC)
  Mod_Sensitivity_Fut_85[i]<-length(grid$SensitivityC[grid$SensitivityC>=.50 & grid$SensitivityC< 0.75])/length(grid$SensitivityC)
  High_Sensitivity_Fut_85[i] <-length(grid$SensitivityC[grid$SensitivityC>=.25 & grid$SensitivityC< 0.5])/length(grid$SensitivityC) 
  Very_High_Sensitivity_Fut_85[i]<-length(grid$SensitivityC[grid$SensitivityC<.25])/length(grid$SensitivityC)
  
  Low_AdaptCap_Fut_85[i]<-length(grid$AdaptCapC[grid$AdaptCapC>=.75])/length(grid$AdaptCapC)
  Mod_AdaptCap_Fut_85[i]<-length(grid$AdaptCapC[grid$AdaptCapC>=.50 & grid$AdaptCapC< 0.75])/length(grid$AdaptCapC)
  High_AdaptCap_Fut_85[i] <-length(grid$AdaptCapC[grid$AdaptCapC>=.25 & grid$AdaptCapC< 0.5])/length(grid$AdaptCapC) 
  Very_High_AdaptCap_Fut_85[i]<-length(grid$AdaptCapC[grid$AdaptCapC<.25])/length(grid$AdaptCapC)
  
  
  Low_Departure_Fut_85[i]<-length(na.omit(grid$DepartureFut_85[grid$DepartureFut_85>=.75]))/length(na.omit(grid$DepartureFut_85))
  Mod_Departure_Fut_85[i]<-length(na.omit(grid$DepartureFut_85[grid$DepartureFut_85>=.50 & grid$DepartureFut_85< 0.75]))/length(na.omit(grid$DepartureFut_85))
  High_Departure_Fut_85[i] <-length(na.omit(grid$DepartureFut_85[grid$DepartureFut_85>=.25 & grid$DepartureFut_85< 0.5]))/length(na.omit(grid$DepartureFut_85))
  Very_High_Departure_Fut_85[i]<-length(na.omit(grid$DepartureFut_85[grid$DepartureFut_85<.25]))/length(na.omit(grid$DepartureFut_85))
  
  Low_Suitability_Fut_85[i]<-length(grid$SuitabilityFut_85[grid$SuitabilityFut_85>=.75])/length(grid$SuitabilityFut_85)
  Mod_Suitability_Fut_85[i]<-length(grid$SuitabilityFut_85[grid$SuitabilityFut_85>=.50 & grid$SuitabilityFut_85< 0.75])/length(grid$SuitabilityFut_85)
  High_Suitability_Fut_85[i] <-length(grid$SuitabilityFut_85[grid$SuitabilityFut_85>=.25 & grid$SuitabilityFut_85< 0.5])/length(grid$SuitabilityFut_85) 
  Very_High_Suitability_Fut_85[i]<-length(grid$SuitabilityFut_85[grid$SuitabilityFut_85<.25])/length(grid$SuitabilityFut_85)
  
  Low_Exposure_Fut_85[i]<-length(grid$ExposureFut_85[grid$ExposureFut_85>=.75])/length(grid$ExposureFut_85)
  Mod_Exposure_Fut_85[i]<-length(grid$ExposureFut_85[grid$ExposureFut_85>=.50 & grid$ExposureFut_85< 0.75])/length(grid$ExposureFut_85)
  High_Exposure_Fut_85[i] <-length(grid$ExposureFut_85[grid$ExposureFut_85>=.25 & grid$ExposureFut_85< 0.5])/length(grid$ExposureFut_85) 
  Very_High_Exposure_Fut_85[i]<-length(grid$ExposureFut_85[grid$ExposureFut_85<.25])/length(grid$ExposureFut_85)
  
  Low_HCCVI_Fut_85[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.75])/length(grid$HCCVI_Fut_85)
  Mod_HCCVI_Fut_85[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.50 & grid$HCCVI_Fut_85< 0.75])/length(grid$HCCVI_Fut_85)
  High_HCCVI_Fut_85[i] <-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85>=.25 & grid$HCCVI_Fut_85< 0.5])/length(grid$HCCVI_Fut_85) 
  Very_High_HCCVI_Fut_85[i]<-length(grid$HCCVI_Fut_85[grid$HCCVI_Fut_85<.25])/length(grid$HCCVI_Fut_85)



  
}
my.results<- cbind(Name,
Low_Departure_Fut_85,Mod_Departure_Fut_85,High_Departure_Fut_85,Very_High_Departure_Fut_85,
Low_Suitability_Fut_85,Mod_Suitability_Fut_85,High_Suitability_Fut_85,Very_High_Suitability_Fut_85,
Low_Exposure_Fut_85,Mod_Exposure_Fut_85,High_Exposure_Fut_85,Very_High_Exposure_Fut_85,
Low_Sensitivity_Fut_85,Mod_Sensitivity_Fut_85,High_Sensitivity_Fut_85,Very_High_Sensitivity_Fut_85,
Low_AdaptCap_Fut_85,Mod_AdaptCap_Fut_85,High_AdaptCap_Fut_85,Very_High_AdaptCap_Fut_85,
Low_HCCVI_Fut_85,Mod_HCCVI_Fut_85,High_HCCVI_Fut_85,Very_High_HCCVI_Fut_85)



write.csv(my.results,("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/Summary_multiple.csv"))
plot(hex)
