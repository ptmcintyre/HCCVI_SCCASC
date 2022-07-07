###get mean sigma by type from various measuers of MD sigma (different vars, diff #pca)
## couldn't calculate in the function, so reading the results of the raw MD raster in and converting
## getting percentile of chi-square based on 2df (2 pca axis), then getting quantile with 1df, per Mahoney paper.


library(ecoclim)
library(raster)
library(tidyverse)
library(here)

#library(caret)



sigma_type<-parseMetadata(here("type_specific_modeling/climate_departure/bl_fut_85"), pattern="sigma")
i=1
my.sigma.hist<-png(here("mahalanobis_distance/Type_spec_6vars_2pcs_type_BL_fut85_SCASC.png"),  width=16, height=20, units="in", res=1000)
par(mfrow = c(3, 2))
#for (i in 1:3)
for (i in 1:length(sigma_type))
   {
  test.sigma<-raster(sigma_type[i])
  raster::hist(test.sigma, xlim=c(0, 9),cex.lab=2, cex.main=2, xaxt="n")
  axis(1, at = seq(0, 9, by=1), cex.axis=2)
}
dev.off()

#histograms for typicality
typicality_type<-parseMetadata(here("type_specific_modeling/climate_departure/bl_near_45"), drops="sigma")


i=1
my.typicality.hist<-png("I:/projects/CEMML_DOD/CEMML_HCCVI/mahalanobis_distance/Typicality_spec_6vars_2pcs_type_BL_near45.png",  width=16, height=20, units="in", res=1000)
par(mfrow = c(8, 3))
#for (i in 1:3)
for (i in 1:length(typicality_type))
{
  test.typicality<-stack(typicality_type[i])
  test.typicality<-test.typicality[[2]]
  raster::hist(test.typicality, xlim=c(0, 1))
  
}
dev.off()




