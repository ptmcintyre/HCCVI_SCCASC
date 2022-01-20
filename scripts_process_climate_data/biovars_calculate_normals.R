library(terra)
library(raster)


bio_num<-seq(1,19,1)
bio_names<-paste0("bio", bio_num, ".tif")
#historical

i=1

for (i in 1:19){
bio.yearly<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern=bio_names[i], full.names = T)
bio.hist<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern="historical", full.names = T)
bio_years<-bio.yearly[bio.yearly%in%bio.hist]
bio_stack<-rast(c(bio_years[1:31]))
bio_stack_mean<-mean(bio_stack)

writeRaster(bio_stack_mean, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/bio_vars_normals/historic/", 
                      "LOCA_CCSM4_historical_1950_1980_", bio_names[i]), overwrite=TRUE)


}


i=1
##below does RCP85 for 2015-2045
for (i in 1:19){
  bio.yearly<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern=bio_names[i], full.names = T)
  bio.hist<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern="rcp85", full.names = T)
  bio_years<-bio.yearly[bio.yearly%in%bio.hist]
  bio_stack<-rast(c(bio_years[10:41])) #
  bio_stack_mean<-mean(bio_stack)
  
  writeRaster(bio_stack_mean, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/bio_vars_normals/future/", 
                                     "LOCA_CCSM4_RCP85_2015_2045_", bio_names[i]), overwrite=TRUE)
  
  
}




i=1
##below does RCP85 for 2015-2045
for (i in 1:19){
  bio.yearly<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern=bio_names[i], full.names = T)
  bio.hist<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern="rcp85", full.names = T)
  bio_years<-bio.yearly[bio.yearly%in%bio.hist]
  bio_stack<-rast(c(bio_years[41:72])) #
  bio_stack_mean<-mean(bio_stack)
  
  writeRaster(bio_stack_mean, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/bio_vars_normals/future/", 
                                     "LOCA_CCSM4_RCP85_2045_2075_", bio_names[i]), overwrite=TRUE)
  
  
}





bio_stack[31]

plot(bio_stack_mean)

bio_years

junk<-
years<-seq(1950, 2005, 1)
years<-paste0(years,"_")

list.files
?list.files()
#make normals
bio.yearly<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84")
bio.yearly

bio_num<-seq(1,19,1)
bio_names<-paste0("bio", bio_num, ".tif")