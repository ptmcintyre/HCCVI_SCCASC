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

#update to cemml folder with VCC and other new info
inputs<-parseMetadata("F:/Projects/CEMML/Analysis/Inputs", pattern =".tif", drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))

#load in raster inputs
tri<-raster("F:/Projects/CEMML/Analysis/Inputs/TRI.tif ") #topographi roughness
ldloss<-raster("F:/Projects/CEMML/Analysis/Inputs/ForestInsectLoss.tif ") #forest loss insect and disease matching Jon's terminology
invasive<-raster("F:/Projects/CEMML/Analysis/Inputs/Agrass_weighted_PJM.tif") #invasive grasses
ruderal<-raster("F:/Projects/CEMML/Analysis/Inputs/EVT_2020_Ruderal_HA_per.tif")
land_cond<-raster("F:/Projects/CEMML/Analysis/Inputs/LCM.tif") #landscape condition (version from 2018)
Fire_VDep<-raster("F:/Projects/CEMML/Analysis/Inputs/LC16_VDep_200_for_analysis.tif") #fire regime departure


#read in target system (raw? probably so we can get count of cells); using the thresholded selected distribution of types. 
#veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
#veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))


#polygon maca template for summarizing bps raster values in and converting to upscaped raster 
#maca_poly<-st_read("F:/Projects/CEMML/ClimateGrids/MACA_CCSM4_Monthly_CONUS_Standard_Poly.shp")
#missing<-c(5, 11, 12, 14, 17, 18, 20)
#read in target system (raw? probably so we can get count of cells)
veg_rasters<- list.files(here("system_distributions/LOCA_rasters/NV"), pattern=".tif")
#veg_rasters<- veg_rasters[missing]

veggies <- raster::stack(here("system_distributions/LOCA_rasters/NV/", veg_rasters))
#veggies<-veggies[[c(5,6)]]
veggies<-veggies

names(veggies)
# define the subset of types to run
vegtypes_run <- names(veggies)



#raw
#vegtypes <- parseMetadata(here("system_distributions/MACA_rasters"), pattern=".tif", drops=c(".xml"))
#veg_focal<-raster(vegtypes[22])  #has #of grid cells 90m

#read in suitaability and convert
#inputs_nr45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/near_45", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#inputs_nr85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/near_85", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#inputs_fut45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/future_45", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
inputs_fut85<-parseMetadata("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/data_products/intermediate/change_suitability/future_85/NV/", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#inputs_nr45<-inputs_nr45
#inputs_nr85<-inputs_nr85
#inputs_fut45<-inputs_fut45
#inputs_fut85<-inputs_fut85[c(13,14),]
inputs_fut85<-inputs_fut85

#dep_nr45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_near_45", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#dep_nr85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_near_85", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#dep_fut45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_fut_45", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
dep_fut85<-parseMetadata("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/data_products/intermediate/climate_departure/bl_fut_85/NV", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
#dep_nr45<-dep_nr45
#dep_nr85<-dep_nr85
#dep_fut45<-dep_fut45
dep_fut85<-dep_fut85

#polygon maca template for summarizing bps raster values in and converting to upscaped raster 
maca_poly<-st_read("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids/LOCA_grid_SCCASC_WGS84.shp")

system_input_table<-read.csv("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/score_cards/system_score_card_reference_NV.csv", as.is=T)
system_input_table<-system_input_table
#system_input_table<-system_input_table[c(5,6),]
#selected forested systems- problem


# #system_input_table<-system_input_table[missing,]
# i=1
# detectCores()
# cpus <- 3
# cl <- makeCluster(cpus)
# registerDoParallel(cl)
# #i=5
# foreach(i=1:length(vegtypes_run), .packages=c("arcgisbinding", "raster", "exactextractr", 'rgdal',
#                                               "sf", "ecoclim", "fasterize")) %dopar% {
for(i in 1:length(vegtypes_run)){
selected_system<-names(veggies[[i]])
type<-selected_system
#system_input<-subset(system_input_table, system_input_table$system_name==selected_system)
system_input<-system_input_table[i,] 

#get cell counts for subsetting maca grid cells to focal type.
veg_maca<-exact_extract(veggies[[i]], maca_poly, 'mean')
maca_poly$count<-veg_maca
maca_focal<-subset(maca_poly, maca_poly$count>0)

#transform to equal area projection
maca_focal_aea<-st_transform(maca_focal, crs(tri))

focal_tri<-exact_extract(tri, maca_focal_aea, 'mean')
focal_tri<-round((focal_tri/100),3)
maca_focal_aea$TRI<-focal_tri

focal_land<-exact_extract(land_cond, maca_focal_aea, 'mean')
focal_land<-round((focal_land/100),3)
maca_focal_aea$ConditionC<-focal_land

##Continuous veg departure based on fire, need to reverse, so 1- the value
focal_Fire_VDep<-exact_extract(Fire_VDep, maca_focal_aea, 'mean')
focal_Fire_VDep<-1-round((focal_Fire_VDep/100),3)
maca_focal_aea$Fire_VDep<-focal_Fire_VDep


#follow variables are dependent on the scorecard inputs
maca_focal_aea$FunGroup<-system_input$Functional
maca_focal_aea$Keystone<-system_input$Keystone

#check scoring

  if(system_input$Forest_Insect_disease=="yes"){
      focal_fld<-exact_extract(ldloss, maca_focal_aea, 'mean')
      #focal_fld[focal_fld>99.999]<-100 #numeric rounding area
      focal_fld<-round((focal_fld/100),3)
      maca_focal_aea$LDLoss<-focal_fld
  } else {
      maca_focal_aea$LDLoss<-NA
  }

  if(system_input$Ruderal=="LF_ruderal"){
      focal_inv<-exact_extract(ruderal, maca_focal_aea, 'mean')
      focal_inv<-1-(round((focal_inv/100),3))
      maca_focal_aea$InvasiveC<-focal_inv
  } else {if(system_input$Ruderal=="invasive_model"){
     focal_inv<-exact_extract(invasive, maca_focal_aea, 'mean')
     focal_inv<-round((focal_inv/100),3)
     maca_focal_aea$InvasiveC<-focal_inv}
    else{
      maca_focal_aea$InvasiveC<-NA
    }
  }

maca_focal_aea$SensitivityC<- rowMeans(cbind(maca_focal_aea$Fire_VDep,maca_focal_aea$ConditionC, maca_focal_aea$InvasiveC, maca_focal_aea$LDLoss), na.rm=T )
maca_focal_aea$AdaptCapC<-rowMeans(cbind(maca_focal_aea$Keystone, maca_focal_aea$FunGroup,maca_focal_aea$TRI ), na.rm=T)
maca_focal_aea$ResilienceC<- rowMeans(cbind(maca_focal_aea$SensitivityC,maca_focal_aea$AdaptCapC), na.rm=T)


#temporary_exposure_metrics start with Near RCP4.5, then mid4.5 and then RCP8.5



# focal_inputNear45<-raster(inputs_nr45$path[i])
# focal_inputNear45[focal_inputNear45>0]<-0
# focal_inputNear45 <-focal_inputNear45 +1
# 
# focal_inputNear85<-raster(inputs_nr85$path[i])
# focal_inputNear85[focal_inputNear85>0]<-0
# focal_inputNear85 <-focal_inputNear85 +1
# 
# focal_inputFut45<-raster(inputs_fut45$path[i])
# focal_inputFut45[focal_inputFut45>0]<-0
# focal_inputFut45<-focal_inputFut45 +1

focal_inputFut85<-raster(inputs_fut85$path[i])
focal_inputFut85[focal_inputFut85>0]<-0
focal_inputFut85<-focal_inputFut85 +1

#alternative, not cut off at zero

# focal_suitNear45<-exact_extract(focal_inputNear45, maca_focal_aea, 'mean')
# focal_suitNear85<-exact_extract(focal_inputNear85, maca_focal_aea, 'mean')
# focal_suitFut45<-exact_extract(focal_inputFut45, maca_focal_aea, 'mean')
focal_suitFut85<-exact_extract(focal_inputFut85, maca_focal_aea, 'mean')

# maca_focal_aea$SuitabilityNr_45<-focal_suitNear45
maca_focal_aea$SuitabilityNr_45<-NA
# maca_focal_aea$SuitabilityNr_85<-focal_suitNear85
maca_focal_aea$SuitabilityNr_85<-NA
# maca_focal_aea$SuitabilityFut_45<-focal_suitFut45
maca_focal_aea$SuitabilityFut_45<-NA
maca_focal_aea$SuitabilityFut_85<-focal_suitFut85


#read in departure MD and covnert
# 
# focal_depNr45<-raster(dep_nr45[i])
# as.vector(focal_depNr45)
# focal_depNr45[focal_depNr45>5]<- 5
# focal_depNr45<-1-(focal_depNr45/5)
# 
# 
# focal_depNr85<-raster(dep_nr85[i])
# focal_depNr85[focal_depNr85>5]<- 5
# focal_depNr85<-1-(focal_depNr85/5)
# 
# 
# focal_depFt45<-raster(dep_fut45[i])
# focal_depFt45[focal_depFt45>5]<- 5
# focal_depFt45<-1-(focal_depFt45/5)


focal_depFt85<-raster(dep_fut85[i])
focal_depFt85[focal_depFt85>5]<- 5
focal_depFt85<-1-(focal_depFt85/5)
# 
# focal_departNear45<-exact_extract(focal_depNr45, maca_focal_aea, 'mean')
# focal_departNear85<-exact_extract(focal_depNr85, maca_focal_aea, 'mean')
# focal_departFuture45<-exact_extract(focal_depFt45, maca_focal_aea, 'mean')
focal_departFuture85<-exact_extract(focal_depFt85, maca_focal_aea, 'mean')


# 
# maca_focal_aea$DepartureNr_45<-focal_departNear45
# maca_focal_aea$DepartureNr_85<-focal_departNear85
# maca_focal_aea$DepartureFut_45<-focal_departFuture45
maca_focal_aea$DepartureFut_85<-focal_departFuture85
#rowMeans(cbind(maca_focal_aea$SensitivityC,maca_focal_aea$AdaptCapC), na.rm=T)
# maca_focal_aea$ExposureNr_45<-rowMeans(cbind(maca_focal_aea$DepartureNr_45, maca_focal_aea$SuitabilityNr_45), na.rm=T)
# maca_focal_aea$ExposureNr_85<-rowMeans(cbind(maca_focal_aea$DepartureNr_85, maca_focal_aea$SuitabilityNr_85 ), na.rm=T)
# maca_focal_aea$ExposureFut_45<-rowMeans(cbind(maca_focal_aea$DepartureFut_45, maca_focal_aea$SuitabilityFut_45), na.rm=T)
maca_focal_aea$ExposureFut_85<-rowMeans(cbind(maca_focal_aea$DepartureFut_85, maca_focal_aea$SuitabilityFut_85), na.rm=T)

# maca_focal_aea$HCCVI_Near_45<- rowMeans(cbind(maca_focal_aea$ExposureNr_45 , maca_focal_aea$ResilienceC), na.rm=T)
# maca_focal_aea$HCCVI_Near_85<- rowMeans(cbind(maca_focal_aea$ExposureNr_85 , maca_focal_aea$ResilienceC), na.rm=T)
# maca_focal_aea$HCCVI_Fut_45<- rowMeans(cbind(maca_focal_aea$ExposureFut_45 , maca_focal_aea$ResilienceC), na.rm=T)
maca_focal_aea$HCCVI_Fut_85<- rowMeans(cbind(maca_focal_aea$ExposureFut_85 ,maca_focal_aea$ResilienceC), na.rm=T)


maca_focal_aea$DepartureFut_85[is.na(maca_focal_aea$DepartureFut_85)]<-.001
maca_focal_aea$ExposureFut_85<-rowMeans(cbind(maca_focal_aea$SuitabilityFut_85, maca_focal_aea$DepartureFut_85), na.rm=T)
hist(maca_focal_aea$ExposureFut_85)
maca_focal_aea$HCCVI_Fut_85<- rowMeans(cbind(maca_focal_aea$ExposureFut_85 ,maca_focal_aea$ResilienceC), na.rm=T)



#write feature layer to GDB
arc.write(path=paste0("S:/Projects/NV_SWAP_HCCVI/analysis/new_output/Scorecards_NV_new.gdb/", type), maca_focal_aea, overwrite=TRUE)
}

stopCluster(cl)
gc()

file.create("C:/_automation/ShutMeDown.txt")

# 
# tri_crop<-crop (tri, veg_focal)
# tri_crop<-mask (tri, veg_focal)
# 
# plot(tri)
# 
# for_ins_dis<-raster(inputs[3])
# invasive<-raster(inputs[1])
# land_cond<-raster(inputs[8])
# FRCC<-raster(inputs[4])
# 
# ?exact_extract
# TRI_maca_<-exact_extract(tri, maca_poly, 'mean')



#logic- counts of target system within MACA polygon grid to select/clip Maca polygons
#average/mean of various metrics using exactextractr to get scores for target system per polygon
#rollup scores 
#write various fields using consisten names to feature layer? or shapefile for the system
