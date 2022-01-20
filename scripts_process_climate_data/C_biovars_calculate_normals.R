#this file takes yearly bioclim values and averages them accross years 
#to create averages for target periods (e.g. 1950-1980, 2035-2075)
rm(list=ls())
gc()

library(terra)
library(raster)
################################################################################
#USER INPUTS

#UPDATE NAME OF GCM for addding to output file names
GCM<- "CCSM4"   #enter name of GCM to include with file

##concatenate GCM name with other info, don't change
historic_name<-paste0("LOCA_", GCM,"_historical")
future_name<-paste0("LOCA_", GCM,"_rcp85")

#####define inputs- location for all biovars by year from 1950-2100 for a single GCM
biovars_yearly<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84"
#list.files(biovars_yearly)# check that location is correct

###define output locations
historic_out<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/bio_vars_normals/historic/"
historic_out<- paste0(historic_out, GCM)
dir.create(historic_out)

future_out<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/bio_vars_normals/future/"
future_out<-paste0(future_out, GCM)
dir.create(future_out)

###define bioclim variables by simple name
#and concatenate with .tif to filter files

bio_num<-seq(1,19,1)
bio_names<-paste0("bio", bio_num, ".tif")

####################################################################################
#loop through historical 

i=1

for (i in 1:19){
#pull out files with bio1 variable
bio.yearly<-list.files(biovars_yearly, pattern=bio_names[i], full.names = T)
#pull out historical files
bio.target<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84", pattern="historical", full.names = T)
bio_years<-bio.yearly[bio.yearly%in%bio.target]
bio_stack<-rast(c(bio_years[1:31])) ###1:31 corresponds to 1950-1980
#names(bio_stack)# can check that years are correct
bio_stack_mean<-mean(bio_stack) #average accross years

writeRaster(bio_stack_mean, paste0(historic_out,"/", historic_name, "_1950_1980_", bio_names[i]), overwrite=TRUE)

}

####################################################################################
#loop through near future (2015-2045) 

i=1

for (i in 1:19){
  #pull out files with bio1 variable
  bio.yearly<-list.files(biovars_yearly, pattern=bio_names[i], full.names = T)
  #pull out future files
  bio.target<-list.files(biovars_yearly, pattern="rcp85", full.names = T)
  bio_years<-bio.yearly[bio.yearly%in%bio.target]
  bio_stack<-rast(c(bio_years[10:40])) ###1:31 corresponds to 2015-2045
  #names(bio_stack)# can check that years are correct
  bio_stack_mean<-mean(bio_stack) #average accross years
  
  writeRaster(bio_stack_mean, paste0(future_out,"/", future_name, "_2015_2045_", bio_names[i]), overwrite=TRUE)
  
}

####################################################################################
#loop through later future (2045-2075) 

i=1

for (i in 1:19){
  #pull out files with bio1 variable
  bio.yearly<-list.files(biovars_yearly, pattern=bio_names[i], full.names = T)
  #pull out future files
  bio.target<-list.files(biovars_yearly, pattern="rcp85", full.names = T)
  bio_years<-bio.yearly[bio.yearly%in%bio.target]
  bio_stack<-rast(c(bio_years[40:70])) ###corresponds to 2045-20175
  #names(bio_stack)# can check that years are correct
  bio_stack_mean<-mean(bio_stack) #average accross years
  
  writeRaster(bio_stack_mean, paste0(future_out,"/", future_name, "_2045_2075_", bio_names[i]), overwrite=TRUE)
  
}
rm(list=ls())
gc()


