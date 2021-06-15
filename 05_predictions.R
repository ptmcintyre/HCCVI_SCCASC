
# For each veg type, this script fits a Random Forest niche model
# and then makes baseline and recent suitability projections.
# Each type uses a distinct optimal set of 6 climate variables, 
# but all types use the same algorithm and parameters.
# SPECIFY APPROPRIATE TIME / EMISSION COMPARISONS (THIS VERSION HAS TWO EMISSIONS AND Historic, near time period, and future time period)

library(ecoclim)
library(raster)
library(ggplot2)
library(dismo)
library(doParallel)
library(dplyr)
library(tidyr)
library(caret)
library(mgcv)
library(randomForest)



# load baseline climate data
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))

dd[[4]]

# load recent climate data
near.biovars<-list.files(here("biovars/future_85"), pattern=".tif")
near.biovars
dr <- stack(here("biovars/future_85", near.biovars)) 
names(dr) <- sort(paste0("bio", 1:19))

# load veg data
veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))
veggies<-veggies[[c(1,14)]]


# # select CONUS types
# cnmx <- c("Sonora_Mojave_Creosotebush_White_Bursage_Desert_Scrub",
#           "Sonoran_Paloverde_Mixed_Cacti_Desert_Scrub",
#           "Apacherian_Chihuahuan_SemiDesert_Grassland_and_Steppe",
#           "Chihuahuan_Creosotebush_Desert_Scrub",
#           "Chihuahuan_Mixed_Desert_and_Thornscrub",
#           "Northwestern_Great_Plains_Mixedgrass_Prairie", 
#           "Rocky_Mountain_Foothill_Limber_Pine_Juniper_Woodland",
#           "Madrean_Pinyon_Juniper_Woodland",
#           "Northern_Rocky_Mountain_Subalpine_Woodland_and_Parkland")
# veggies <- subset(veggies, names(veggies)[!names(veggies) %in% cnmx])
#veggies<-veggies[[1]]


# load variable importance data
imp <- read.csv(here("type_specific_modeling/variable_selection/variable_importance.csv"), stringsAsFactors=F)
imp <- imp[imp$rank >= 14,]

# cluster stetup
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)

# loop
r <- foreach(type=names(veggies),
             .packages=c("raster", "ecoclim", "dismo", "dplyr", "caret","here", "randomForest", "ggplot2", "tidyr", "dplyr")) %dopar% {
                   
                   ### restructure veg data
                   veg <- subset(veggies, type)
                   veg <- trimFast(extend(veg, 2))
                   
                   names(veg) <- "veg"
                   veg <- reclassify(veg, c(NA, NA, 0))
                   px <- as.data.frame(rasterToPoints(veg))
                   names(px)[3] <- "veg"
                   
                   
                   ### prep climate data
                   vars_used <- imp$var[imp$type == gsub("_", " ", type)]
                   
                   climate <- crop(dd, veg) 
                   climate <- subset(climate, vars_used)
                   clim <- values(climate)  
                   
                   climate2 <- crop(dr, veg) 
                   climate2 <- subset(climate2, vars_used)
                   clim2 <- values(climate2)
                   
                   
                   ### classify presence/absence
                   pres <- px[px$veg > 0,]
                   abs <- px[px$veg == 0,]
                   pres$presence <- T
                   abs$presence <- F
                   prevalence <- nrow(pres) / nrow(px)
                   pixels <- rbind(pres, abs)
                   
                   train_pres <- pixels %>% filter(presence==T) %>% sample_n(1000)
                   train_abs <- pixels %>% filter(presence==F) %>% sample_n(1000)
                   coordinates(train_pres) <- c("x", "y")
                   coordinates(train_abs) <- c("x", "y")
                   
                   train_pres <- raster::extract(climate, train_pres)
                   train_abs <- raster::extract(climate, train_abs)
                   
                   
                   ### prep training data
                   train_presence <- as.data.frame(train_pres)
                   train_absence <- as.data.frame(train_abs)
                   vars <- names(train_presence)
                   train_presence$occurrence <- 1
                   train_absence$occurrence <- 0
                   train <- rbind(train_presence, train_absence)
                   
                   
                   ### fit multivariate model
                   form <- paste0("as.factor(occurrence) ~ ", paste(vars_used, collapse=" + "))
                   set.seed(05443)
                   fit <- randomForest(as.formula(form), data=na.omit(train), 
                                       ntree=10000, nodesize=8, mtry=1)
                   
                   
                   ### remember which rows were NA, then remove them to allow model prediction
                   na <- apply(clim, 1, function(x) is.na(sum(x)))      
                   clm <- clim[!na,]
                   na2 <- apply(clim2, 1, function(x) is.na(sum(x)))      
                   clm2 <- clim2[!na2,]
                   
                   
                   ### predict
                   pred <- predict(fit, as.data.frame(clm), "prob")
                   prediction <- clim[,1]
                   prediction[1:length(prediction)] <- NA
                   prediction[which(!na)] <- pred[,2]
                   prediction <- raster(as.matrix(prediction), template=climate[[1]])
                   
                   pred2 <- predict(fit, as.data.frame(clm2), "prob")
                   prediction2 <- clim2[,1]
                   prediction2[1:length(prediction2)] <- NA
                   prediction2[which(!na2)] <- pred2[,2]
                   prediction2 <- raster(as.matrix(prediction2), template=climate2[[1]])
                   
                   ### save results
                   preds <- writeRaster(stack(prediction, prediction2),
                               filename=paste0(here("type_specific_modeling/niche_models/future_85/rasters_timeslice/predictions_1975_2005_2035_2064_"), type),
                               format="GTiff", overwrite=T)
                   deltas <- writeRaster(prediction2 - prediction,
                                         filename=paste0(here("type_specific_modeling/niche_models/future_85/rasters_delta/deltas_1975_2005_2035_2064_"), type),
                                         format="GTiff", overwrite=T)
                   
             }

stopCluster(cl)

