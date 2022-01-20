###This script uses the ecoclim package to convert monthl precip/tmin/tmax
#to bioclim variables 

##To install ecoclim 02/17/2021 run code below
#install.packages("remotes")
#remotes::install_github("matthewkling/ecoclim") 


rm(list=ls())
gc()


library(raster)
library(ecoclim)

#USER INPUTS#########################

#UPDATE NAME OF GCM for addding to output file names
GCM<- "CCSM4"   #enter name of GCM to include with file

##concatenate GCM name with other info, don't change
historic_name<-paste0("LOCA_", GCM,"_historical")
future_name<-paste0("LOCA_", GCM,"_rcp85")

##define output base location, will add on GCM name
#all files from a GCM for both historic and future will go to a single folder)
base_out<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_yearWGS84/"
output_loc<- paste0(base_out, GCM)
dir.create(output_loc)


#update location of historic and future (rcp85) monthly climate files. Should have just one GCM worths of files at a time here
#files should be precip/tmin/tmax for all years in historic(1950-2005) and future (2006-2100) in separate folders (could be updated to single)

historic_monthly<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/test/historic"
future_monthly<-"S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/test/future"

# shouldn't change- define years of interest for all historic and future years
historic_years<-seq(1950,2005, 1)
future_years<-seq(2006,2100, 1)

###############################################
#loop through historic data

i=2
j=1
for(i in 2:length(historic_years)) {
  #select a single year of files
  clim.files <- list.files(historic_monthly, pattern=paste0(historic_years[i]), full.names =T)
  #reorganize files into correct order, should be precip1_12,tmax1_12, tmin1_12
  clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4,13,17,18,19,20,21,22,23,24,14,15,16,25,29,30,31,32,33,34,35,36,26,27,28)]
  clim.year<-stack(clim.files)  #stack climate files to brick
  k <- calc(clim.year, fun=function(x)biovariables(x))  #apply biovariables function
  #output files
  for (j in 1:19){
    writeRaster(k[[j]], paste0(output_loc, "/", historic_name,"_", historic_years[i], "_", "bio",j, ".tif"), "GTiff", overwrite=TRUE)
  }
}

################################################
#loop through future data
i=2
j=1
for(i in 2:length(future_years)) {
  #select a single year of files
  clim.files <- list.files(future_monthly, pattern=paste0(future_years[i]), full.names =T)
  #reorganize files into correct order, should be precip1_12,tmax1_12, tmin1_12
  clim.files<-clim.files[c(1,5,6,7,8,9,10,11,12,2,3,4,13,17,18,19,20,21,22,23,24,14,15,16,25,29,30,31,32,33,34,35,36,26,27,28)]
  clim.year<-stack(clim.files)  #stack climate files to brick
  k <- calc(clim.year, fun=function(x)biovariables(x))  #apply biovariables function
  #output files
  for (j in 1:19){
    writeRaster(k[[j]], paste0(output_loc, "/", future_name,"_", future_years[i], "_", "bio",j, ".tif"), "GTiff", overwrite=TRUE)
  }
}

rm()
gc()
