###get mean sigma by type from various measuers of MD sigma (different vars, diff #pca)
## couldn't calculate in the function, so reading the results of the raw MD raster in and converting
## getting percentile of chi-square based on 2df (2 pca axis), then getting quantile with 1df, per Mahoney paper.


library(ecoclim)
library(raster)
library(doParallel)
library(tidyverse)
library(here)
library(adehabitatLT)
#library(caret)








# load target rasters
md_rasters <- parseMetadata(here("mahalanobis_distance/Sigma_summary_national"))
md_rasters<-stack(md_rasters$path)

veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))


#target national raster- select
#nat_md<-md_rasters[[2]]
#type<-"Great_Basin_Pinyon_Juniper_Woodland"
for(type in names(veggies)) {
  veg <- subset(veggies, type)
  type_md<-crop(nat_md, veg)
  type_md<-mask(nat_md, veg)
  writeRaster (type_md, paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/mahalanobis_distance/MD_19vars_4pcs_rp45_fut_sigma", "/", type, ".tif"), format= "GTiff", overwrite=T)
}
  
  
#Plot histograms of MD for each type and save to PNG file. 
#sigma_type<-parseMetadata(here("mahalanobis_distance/MD_4vars_2pcs_rp45_bl_kling_method"))
#sigma_type<-parseMetadata(here("mahalanobis_distance/MD_19vars_4pcs_rp45_fut_sigma"))
#sigma_type<-parseMetadata(here("mahalanobis_distance/MD_19vars_4pcs_rp45_bl_sigma"))
#sigma_type<-parseMetadata(here("mahalanobis_distance/MD_4vars_2pcs_rp45_bl_kling_method"))
sigma_type<-parseMetadata("I:/projects/BLM/Data/Final/veg_type_exposure/current/niche_deltas", pattern="tif", drop=c("xml", "ovr"))
i=1
#my.sigma.hist<-png("I:/projects/CEMML_DOD/CEMML_HCCVI/mahalanobis_distance/Type_spec_6vars_2pcs_type_BL_fut85.png",  width=16, height=20, units="in", res=1000)
#par(mfrow = c(8, 3))
#for (i in 1:3)

stat.names<-vector()
delta_suitability<-vector()

i=1
for (i in 1:length(sigma_type))
   {
  test.sigma<-raster(sigma_type[i])
  test.sigma[test.sigma>0]<-0
  test.sigma<-test.sigma +1
  delta_suitability[i]<-cellStats(test.sigma, stat='mean', na.rm=T)
  stat.names[i]<-names(test.sigma)
  
}

suitability_df<-cbind(stat.names, delta_suitability)

write.csv(suitability_df, "I:/projects/CEMML_DOD/CEMML_HCCVI/GMU_project/suitability_df_BLM.csv")

sigma_type<-parseMetadata("I:/projects/BLM/Data/Final/veg_type_exposure/current/typicality", pattern="tif", drop=c("xml", "ovr"))
i=1
#my.sigma.hist<-png("I:/projects/CEMML_DOD/CEMML_HCCVI/mahalanobis_distance/Type_spec_6vars_2pcs_type_BL_fut85.png",  width=16, height=20, units="in", res=1000)
#par(mfrow = c(8, 3))
#for (i in 1:3)

stat.names<-vector()
typicality<-vector()

i=1
for (i in 1:length(sigma_type))
{
  test.sigma<-raster(sigma_type[i])
  #test.sigma[test.sigma>0]<-0
  #test.sigma<-test.sigma +1
  typicality[i]<-cellStats(test.sigma, stat='mean', na.rm=T)
  stat.names[i]<-names(test.sigma)
  
}

suitability_df<-cbind(stat.names, typicality)

write.csv(suitability_df, "I:/projects/CEMML_DOD/CEMML_HCCVI/GMU_project/typicality_df_BLM.csv")

sigma_type<-parseMetadata("I:/projects/BLM/Data/Final/veg_type_exposure/current/combined_exposure", pattern="tif", drop=c("xml", "ovr"))
i=1
#my.sigma.hist<-png("I:/projects/CEMML_DOD/CEMML_HCCVI/mahalanobis_distance/Type_spec_6vars_2pcs_type_BL_fut85.png",  width=16, height=20, units="in", res=1000)
#par(mfrow = c(8, 3))
#for (i in 1:3)

stat.names<-vector()
exposure<-vector()

i=1
for (i in 1:length(sigma_type))
{
  test.sigma<-raster(sigma_type[i])
  #test.sigma[test.sigma>0]<-0
  #test.sigma<-test.sigma +1
  exposure[i]<-cellStats(test.sigma, stat='mean', na.rm=T)
  stat.names[i]<-names(test.sigma)
  
}

suitability_df<-cbind(stat.names, exposure)

write.csv(suitability_df, "I:/projects/CEMML_DOD/CEMML_HCCVI/GMU_project/exposure_df_BLM.csv")


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







# load veg data DO WE WANT BPS or EVT?
veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))


mahal <- function(type, overwrite=F){
  
  outfile<- paste0(here("type_specific_modeling/testing_novelty"),"/", "MD_19var_4pcs_rp45Fut", ".tif")
  if(!overwrite & file.exists(outfile)){
    message("skipping")
    return("skipping")
  }
  message(type)
  
  start <- Sys.time()
  
  ### prep veg data
  veg <- subset(veggies, type)
  veg <- trimFast(extend(veg, 2))
  names(veg) <- "veg"
  
  ### prep climate data
  
  # restrict analysis to focal vars
  vars_used <- imp$var[imp$type == gsub("_", " ", type)] #[1:4]
  #vars_used<- c("bio5", "bio6","bio18", "bio19")
  
  b <- lapply(baseline$path, stack) %>%
    lapply(function(x){
      names(x) <- paste0("bio", 1:19)
      x}) %>%
    lapply(subset, subset=vars_used) %>%
    stack()
  
  # require(doParallel)
  # cl <- makeCluster(detectCores() - 1)
  # registerDoParallel(cl)
  # b <- foreach(x = baseline$path, .packages=c("raster")) %dopar% {
  #       x <- stack(x)
  #       names(x) <- paste0("bio", 1:19)
  #       crop(subset(x, vars_used), veg)
  # }
  # stopCluster(cl)
  # b <- stack(b)
  
  #PJM: reducing to 4 variables
  r <- subset(recent, vars_used) %>%
    crop(b) %>% extend(b)
  # ve <- extend(veg, r) %>% 
  #   crop(r) %>%
  #   mask(r[[1]])
  ve<-r[[1]]/r[[1]]
  names(ve)<-"veg"
  clim <- stack(ve, r, b)
  #clim<-crop(clim, crop.extent)
  
  beginCluster(15)
  md <- clusterR(clim, calc, args=list(fun=novelty), m = 100)
  #md <- crop(md, veg)
  endCluster()
  
  # md <- calc(clim, function(x){
  #       y <- try(novelty(x))
  #       if(class(y) == "try-error") browser()
  #       y
  #       })
  
  writeRaster(md, outfile, overwrite=T)
  
  message(Sys.time() - start)
  return("complete")
}


#types <- sample(names(veggies), length(names(veggies)))
types<-names(veggies)[1]
v <- purrr::map_chr(types, possibly(mahal, "fail"))

#read in test raster
md.rasters <- parseMetadata(here("type_specific_modeling/temporal_novelty"),pattern=".tif")
md.rasters <- parseMetadata(here("mahalanobis_distance/"),pattern=".tif")
md.rasters<-md.rasters$path
test.raster<-raster(md.rasters[3])[1]
test.raster[[1]]
i=3

names(md.rasters)
for(i in 1:length(md.rasters)){
  veg.md<-raster(md.rasters[i])
  veg.md<-veg.md[[1]]
  veg.md[!is.na(veg.md)]<-pchi(veg.md[!is.na(veg.md)], 2)# specify df
  veg.md[veg.md>.999999999999999]<-.9999999999999999
  veg.md[!is.na(veg.md)]<-qchi(veg.md[!is.na(veg.md)], 1)
  writeRaster(veg.md, paste0(here("type_specific_modeling/testing_novelty"),"/", names(veg.md), "_sigma.tif"), fomrat="GTiff", overwrite=T)
  
}


hist(veg.md)
pchi(12, 4)
qchi(.9999999999999999, 4)

my.vec2<-sample(veg.md, 1000)
my.vec<-my.vec2
my.vec[!is.na(my.vec)]<-pchi(my.vec[!is.na(my.vec)], 4)# specify df
my.vec[my.vec>.9999999999]<- 0.9999999999999999
my.vec<- my.vec[!is.na(my.vec)]<-qchi(my.vec[!is.na(my.vec)], 1)

?sample
max(my.vec, na.rm=T)
my.vec[my.vec>=1.000000000]

#for trimming to test function
my.vec[my.vec>.99999999]<-.99999999
str(my.vec)
extent(clim[[1]])

extent(trim(veggies[[1]]))

crop.extent<-c(-100, -95, 38, 42)
clim<-crop(clim, crop.extent)

clim2

my.test<-novelty(clim, m=100)


plot(ve)