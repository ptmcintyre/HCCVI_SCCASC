library(arcgisbinding)
library(ecoclim)
library(raster)
library(exactextractr)
library(here)
library(sf)
library(rgdal)

arc.check_product()

inputs<-parseMetadata("F:/Projects/BLM_climate/Analysis/Inputs", pattern =".tif", drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))

#load in raster inputs
#tri <- as.raster(arc.raster(arc.open("F:/Projects/KBAs/KBA_Canada/Data/Analyis/KBA_analysisgdb.gdb/LAND_EVT_KBA_NTGP")))
tri<-raster(inputs[12]) #topographi roughness
ldloss<-raster(inputs[4]) #forest loss insect and disease matching Jon's terminology
invasive<-raster(inputs[2]) #invasive grasses
land_cond<-raster(inputs[10]) #landscape condition (version from 2018)
FRCC<-raster(inputs[6]) #fire regime departure
#keystone- constant 0.88 for WGSP
#funcdiversity<- constant 0.5 for WGSP


#read in target system (raw? probably so we can get count of cells)
veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))
#raw
vegtypes <- parseMetadata(here("system_distributions/MACA_rasters_continuous"), pattern=".tif", drops=c(".xml"))
veg_focal<-raster(vegtypes[22])  #has #of grid cells 90m

#polygon maca template for summarizing bps raster values in and converting to upscaped raster 
maca_poly<-st_read("F:/Projects/CEMML/ClimateGrids/MACA_CCSM4_Monthly_CONUS_Standard_Poly.shp")

#get cell counts for subsetting maca grid cells to focal type.
veg_maca<-exact_extract(veg_focal, maca_poly, 'mean')
maca_poly$count<-veg_maca

maca_focal<-subset(maca_poly, maca_poly$count>0)
plot(maca_focal)

#transform to equal area projection
maca_focal_aea<-st_transform(maca_focal, crs(tri))

focal_tri<-exact_extract(tri, maca_focal_aea, 'mean')
focal_tri<-round((focal_tri/100),3)
maca_focal_aea$TRI<-focal_tri


focal_fld<-exact_extract(ldloss, maca_focal_aea, 'mean')
focal_fld[focal_fld>99.999]<-100 #numeric rounding area
focal_fld<-round((focal_fld/100),3)
maca_focal_aea$LDLoss<-NA

focal_inv<-exact_extract(invasive, maca_focal_aea, 'mean')
focal_inv<-round((focal_inv/100),3)
maca_focal_aea$InvasiveC<-focal_inv

focal_land<-exact_extract(land_cond, maca_focal_aea, 'mean')
focal_land<-round((focal_land/100),3)
maca_focal_aea$ConditionC<-focal_land


focal_FRCC<-exact_extract(FRCC, maca_focal_aea, 'mean')
focal_FRCC<-round((focal_FRCC/100),3)
maca_focal_aea$FRCC_C<-focal_FRCC

#inputs<-parseMetadata("F:/Projects/BLM_climate/Analysis/Inputs", pattern =".tif", drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
maca_focal_aea$FunGroup<-0.5
maca_focal_aea$Keystone<-0.88

maca_focal_aea$SensitivityC<- (focal_FRCC + focal_land + focal_inv )/3
maca_focal_aea$AdaptCapC<-(maca_focal_aea$Keystone + maca_focal_aea$FunGroup + maca_focal_aea$TRI )/3
maca_focal_aea$ResilienceC<- (maca_focal_aea$SensitivityC +maca_focal_aea$AdaptCapC)/2


#temporary_exposure_metrics start with Near RCP4.5, then mid4.5 and then RCP8.5

#read in suitaability and convert
inputs_nr45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/near_45", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
inputs_nr85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/near_85", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
inputs_fut45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/future_45", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
inputs_fut85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/change_suitability/future_85", pattern =".tif" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))


focal_inputNear45<-raster(inputs_nr45$path[22])
focal_inputNear45[focal_inputNear45>0]<-0
focal_inputNear45 <-focal_inputNear45 +1

focal_inputNear85<-raster(inputs_nr85$path[22])
focal_inputNear85[focal_inputNear85>0]<-0
focal_inputNear85 <-focal_inputNear85 +1

focal_inputFut45<-raster(inputs_fut45$path[22])
focal_inputFut45[focal_inputFut45>0]<-0
focal_inputFut45<-focal_inputFut45 +1

focal_inputFut85<-raster(inputs_fut85$path[22])
focal_inputFut85[focal_inputFut85>0]<-0
focal_inputFut85<-focal_inputFut85 +1

#alternative, not cut off at zero

focal_suitNear45<-exact_extract(focal_inputNear45, maca_focal_aea, 'mean')
focal_suitNear85<-exact_extract(focal_inputNear85, maca_focal_aea, 'mean')
focal_suitFut45<-exact_extract(focal_inputFut45, maca_focal_aea, 'mean')
focal_suitFut85<-exact_extract(focal_inputFut85, maca_focal_aea, 'mean')

mean(focal_suitNear45, na.rm=T)
mean(focal_suitNear85, na.rm=T)
mean(focal_suitFut45, na.rm=T)
mean(focal_suitFut85, na.rm=T)

maca_focal_aea$SuitabilityNr_45<-focal_suitNear45
maca_focal_aea$SuitabilityNr_85<-focal_suitNear85
maca_focal_aea$SuitabilityFut_45<-focal_suitFut45
maca_focal_aea$SuitabilityFut_85<-focal_suitFut85


#read in departure MD and covnert
dep_nr45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_near_45", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
focal_depNr45<-raster(dep_nr45[22])
as.vector(focal_depNr45)
focal_depNr45[focal_depNr45>5]<- 5
focal_depNr45<-1-(focal_depNr45/5)

dep_nr85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_near_85", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
focal_depNr85<-raster(dep_nr85[22])
focal_depNr85[focal_depNr85>5]<- 5
focal_depNr85<-1-(focal_depNr85/5)

dep_fut45<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_fut_45", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
focal_depFt45<-raster(dep_fut45[22])
focal_depFt45[focal_depFt45>5]<- 5
focal_depFt45<-1-(focal_depFt45/5)

dep_fut85<-parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/intermediate/climate_departure/bl_fut_85", pattern ="sigma" , drops=c("xml", ".dbf", ".ovr", ".cpg", "Targets", "Archive"))
focal_depFt85<-raster(dep_fut85[22])
focal_depFt85[focal_depFt85>5]<- 5
focal_depFt85<-1-(focal_depFt85/5)

plot(focal_depNr45)
plot(maca_focal_aea[1])
plot(focal_inputNear45)

focal_departNear45<-exact_extract(focal_depNr45, maca_focal_aea, 'mean')
mean(focal_departNear45, na.rm=T)
focal_departNear85<-exact_extract(focal_depNr85, maca_focal_aea, 'mean')
mean(focal_departNear85, na.rm=T)
focal_departFuture45<-exact_extract(focal_depFt45, maca_focal_aea, 'mean')
mean(focal_departFuture45, na.rm=T)
focal_departFuture85<-exact_extract(focal_depFt85, maca_focal_aea, 'mean')
mean(focal_departFuture85, na.rm=T)


maca_focal_aea$DepartureNr_45<-focal_departNear45
maca_focal_aea$DepartureNr_85<-focal_departNear85
maca_focal_aea$DepartureFut_45<-focal_departFuture45
maca_focal_aea$DepartureFut_85<-focal_departFuture85



maca_focal_aea$ExposureNr_45<-(maca_focal_aea$DepartureNr_45 + maca_focal_aea$SuitabilityNr_45 )/2
maca_focal_aea$ExposureNr_85<-(maca_focal_aea$DepartureNr_85 + maca_focal_aea$SuitabilityNr_85 )/2
maca_focal_aea$ExposureFut_45<-(maca_focal_aea$DepartureFut_45 + maca_focal_aea$SuitabilityFut_45)/2
maca_focal_aea$ExposureFut_85<-(maca_focal_aea$DepartureFut_85 + maca_focal_aea$SuitabilityFut_85)/2

maca_focal_aea$HCCVI_Near_45<- (maca_focal_aea$ExposureNr_45 + maca_focal_aea$ResilienceC)/2
maca_focal_aea$HCCVI_Near_85<- (maca_focal_aea$ExposureNr_85 + maca_focal_aea$ResilienceC)/2
maca_focal_aea$HCCVI_Fut_45<- (maca_focal_aea$ExposureFut_45 + maca_focal_aea$ResilienceC)/2
maca_focal_aea$HCCVI_Fut_85<- (maca_focal_aea$ExposureFut_85 +maca_focal_aea$ResilienceC)/2



arc.write(path="F:/Projects/CEMML/analysis/Scorecards_CEMML_test.gdb/Western_Great_Plains_Shortgrass_Prairie", maca_focal_aea, overwrite=T)





tri_crop<-crop (tri, veg_focal)
tri_crop<-mask (tri, veg_focal)

plot(tri)

for_ins_dis<-raster(inputs[3])
invasive<-raster(inputs[1])
land_cond<-raster(inputs[8])
FRCC<-raster(inputs[4])

?exact_extract
TRI_maca_<-exact_extract(tri, maca_poly, 'mean')



#logic- counts of target system within MACA polygon grid to select/clip Maca polygons
#average/mean of various metrics using exactextractr to get scores for target system per polygon
#rollup scores 
#write various fields using consisten names to feature layer? or shapefile for the system
