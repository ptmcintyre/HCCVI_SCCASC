library(raster)
library(rgdal)
library(ecoclim)
library(rgeos)
library(doParallel)

# load veg data
# paths <- parseMetadata("I:/projects/BLM/Production/type_specific/veg_distributions/PRISM_800m/rasters", pattern=".tif")
# veg_list <- stack(paths)
# names(veg_list) <- sub(".tif", "", basename(paths))

veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veg_list <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))



########### Load Deltas and Z-score rasters for biovars #####################
#(I generated separate rasters that masked out areas that didn't meet the criteria defined by Pat.This was confusingly done further down on this script and 
#then commented out. This could certainly be done in a better way on the fly in r)

#dz <- parseMetadata("I:/projects/BLM/Workspace/auer/Biovars/westwide_deltas_zscores", drops=c("cv","sd"))
# deltas <- motleyStack(dz$path[dz$stat=="delta"])
 #zscores <- motleyStack(dz$path[dz$stat=="zscore"])
 
dz <- parseMetadata("I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/z_score_rasters/bl_fut_45", drops=c(".aux",".ovr", ".xml"))
 # deltas <- motleyStack(dz$path[dz$stat=="delta"])
 zscores <- stackBands(dz$path, 2) # second band is zscore (1st is delta)
 names(zscores) <- paste0(dz$variable, "_zscore")
 

### Load Masked means, deltas, and zscores #####
deltas <- motleyStack(list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/fut_45/masked_rasters/masked_deltas/", full.names=TRUE))
masked_zscores <- motleyStack(list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/fut_45/masked_rasters/masked_zscores/", full.names=TRUE))
means <- motleyStack(list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/fut_45/masked_rasters/masked_baseline_means/", full.names=TRUE))

### load thresholded rasters -- greater than 1SD, less than -1 
thresholds <- stack(list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/fut_45/masked_rasters/thresholded_zscores/", full.names=TRUE))

# cluster stetup (this is to utilize all cpus on the server so it will run fast)
cpus <- 10
cl <- makeCluster(cpus)
registerDoParallel(cl)

# loop
r <- foreach(type=names(veg_list),
             .packages=c("raster", "ecoclim", "rgdal")) %dopar% {
               
               #f <- paste0("I:/projects/BLM/Workspace/auer/summary_table/csv_simple/", type, "_biovar_summary.csv")
               #if(file.exists(f)) next()
               
               #type="Central_Tallgrass_Prairie"
               
               ### restructure veg data
               veggies <- subset(veg_list, type)
               #veggies <- trimFast(extend(veggies, 2))
               
               clip <- raster(dz$path[1])
               veggies <- crop(veggies, clip)
               veggies <- raster::mask(veggies, clip)
               veggies <- trimFast(extend(veggies, 2))

      ######### CECs ##########
      cec <- readOGR("F:/Projects/CEMML/boundaries", "NA_CEC_Diss_AEA_MACA_wgs84")
      cec<-cec[,-c(2,3)]         
      cec_names <- crop(cec, veggies)
      cec_df <- data.frame(NAME=cec_names$NAME, bio1_=0, bio2_=0, bio3_=0, bio4_=0, bio5_=0, bio6_=0, bio7_=0, bio8_=0, bio9_=0, bio10_=0, bio11_=0, bio12_=0, bio13_=0, bio14_=0, bio15_=0, bio16_=0, bio17_=0, bio18_=0, bio19_=0)
      cec_df <- cec_df[complete.cases(cec_df),]
      
      ################## create data frame of variables used in exposure index ##############
       #imp <- read.csv("I:/projects/BLM/Production/type_specific/variable_selection/conus/variable_importance.csv", stringsAsFactors=F)
       #imp <- imp[imp$rank >= 14,]
       #vars_used <- imp$var[imp$type == gsub("_", " ", type)]
       #imp2 <- imp[imp$type == gsub("_", " ", type),]
       #write.csv(imp2, file=paste0("I:/projects/BLM/Workspace/auer/summary_table/csv/", type, "_variable_importance.csv"))


####### create threshold rasters for each biovar ##########
# for (z in names(zscores)){
#   
#   zras <- subset(zscores, z)
#   
#   thresholdRaster <- Which(zras>=1| zras<=-1)
#   thresholdRaster[thresholdRaster==0]=NA
#   
#   names(thresholdRaster) <- paste0(z, "_threshold")
#   writeRaster(thresholdRaster, filename=paste0("I:/projects/BLM/Workspace/auer/summary_table/thresholded_zscores/", names(thresholdRaster)), format="GTiff")
# }

#thresholds <- stack(list.files("I:/projects/BLM/Workspace/auer/summary_table/thresholded_zscores/", full.names=TRUE))

######### mask deltas, zscores, and baseline means to the threshold raster per biovar ##############
# bm <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means/", drops="1981_2014")
# bm <- motleyStack(bm$path)

# for (var in unique(dz$variable)){
#        #delta <- raster(dz$path[grepl(paste0(var,"__delta.tif"), basename(dz$path))]) 
#        #zscore <- raster(dz$path[grepl(paste0(var,"__zscore.tif"), basename(dz$path))])
#       
#         mean <- subset(bm, paste0(var, "_1948_1980"))
#        
#         threshold <- subset(thresholds, paste0(var, "__zscore_threshold"))
#     
# #        delta_crop <- crop(delta, threshold)
# #        threshold <- crop(threshold, delta)
# #        delta_crop <- raster::mask(delta_crop, threshold)
# #        zscore_crop <- crop(zscore, threshold)
# #        zscore_crop <- raster::mask(zscore_crop, threshold)
#       
#       mean_crop <- crop(mean, threshold)
#       threshold <- crop(threshold, mean)
#       mean_crop <- raster::mask(mean_crop, threshold)
#        
#        #writeRaster(delta_crop, filename=paste0("I:/projects/BLM/Workspace/auer/summary_table/masked_deltas/", var, "_delta"), format="GTiff")
#        #writeRaster(zscore_crop, filename=paste0("I:/projects/BLM/Workspace/auer/summary_table/masked_zscores/", var, "_zscore"), format="GTiff")
#         writeRaster(mean_crop, filename=paste0("I:/projects/BLM/Workspace/auer/summary_table/masked_baseline_means/", var, "_mean"), format="GTiff")
# }

      ####### The % of pixels that are > 1 | <-1 zscore for the type per CEC 
       area <- function(x){
         (length(Which(x>=1| x<=-1, cells=TRUE))/ncell(x[x!=-Inf]))*100
       }
      
      area_df <- cec_df
       #cec=cec_df$NAME[21]
       #z = names(zscores)[1]
      for (cec in cec_df$NAME){
        for (z in names(zscores)){
          
          zras <- subset(zscores, z)
          
          veggies <- crop(veggies, zras)
          zras <- crop(zras, veggies)
          zras <- raster::mask(zras, veggies)     ### mask zsore raster to veg type
         
          cec_shape <- cec_names[grepl(cec, cec_names$NAME),] ### isolate CEC shape
          
          zras <- raster::mask(zras, cec_shape)  ### mask by type zscore to cec shape
        
         area_calc <- area(zras) ### calculating # of pixels that are >1 or <-1 across type
         area_df[grepl(cec, area_df$NAME), grepl(sub("_zscore", "", z), colnames(area_df))] = area_calc
        }  
      }
      # #### rename columns
       names(area_df) <- gsub("_", "_percent_area", names(area_df))
       
       
        ###### mask to veg type ######
        delta_crop <- crop(deltas, veggies)
        delta_crop <- raster::mask(delta_crop, veggies)
        zscore_crop <- crop(masked_zscores, veggies)
        zscore_crop <- raster::mask(zscore_crop, veggies)
        
        baseline_crop <- crop(means, veggies)
        baseline_crop <- raster::mask(baseline_crop, veggies)
        
        ################## create large summary table ############################
        baseline_mean <- raster::extract(baseline_crop, cec_names, fun="mean", na.rm=T, df=T, sp=T)
        baseline_mean <- as.data.frame(baseline_mean, row.names=cec_means[1,], na.rm=TRUE)
        baseline_mean$FIRST_NAME <- NULL
        #names(baseline_mean) <- gsub("_1948_1980", "_baseline", names(baseline_mean))
        
        delta_mean <- raster::extract(delta_crop, cec_names, fun="mean", na.rm=T, df=T, sp=T)
        delta_mean <- as.data.frame(delta_mean, row.names=cec_means[1,], na.rm=TRUE)
        names(delta_mean) <- gsub("_delta", "mean_delta", names(delta_mean))
        delta_mean$FIRST_NAME <- NULL
        
        delta_min <- raster::extract(delta_crop, cec_names, fun="min", na.rm=T, df=T, sp=T)
        dela_min <- as.data.frame(delta_min, row.names=cec_means[1,], na.rm=TRUE)
        names(delta_min) <- gsub("_delta", "min_delta", names(delta_min))
        delta_min$FIRST_NAME <- NULL
        
        delta_max <- raster::extract(delta_crop, cec_names, fun="max", na.rm=T, df=T, sp=T)
        dela_max <- as.data.frame(delta_max, row.names=cec_means[1,], na.rm=TRUE)
        names(delta_max) <- gsub("_delta", "max_delta", names(delta_max))
        delta_max$FIRST_NAME <- NULL
        
        zscore_mean <- extract(zscore_crop, cec_names, fun="mean", na.rm=T, df=T, sp=T)
        zscore_mean <- as.data.frame(zscore_mean, row.names=cec_means[1,], na.rm=TRUE)
        names(zscore_mean) <- gsub("_zscore", "mean_zscore", names(zscore_mean))
        zscore_mean$FIRST_NAME <- NULL
        
        ########## join extracted data frames by CEC names ##############
        
        multMerge <- function(x, y){
          df <- merge(x, y, by= "NAME", all.x= T, all.y= T)
          return(df)
        }
        
        summary <- Reduce(multMerge, (list(baseline_mean, delta_mean, delta_min, delta_max, zscore_mean, area_df)))
        
        ##### assign NA to cells that have NAN or -Inf ############ 
        summary[summary==-Inf|summary==Inf|summary==NaN] <- NaN
        
        final <- summary[!apply(summary[-1], 1, function(x) all(is.nan(x))),]
        
        #summary <- as.data.frame(cec_means, row.names=cec_means[1,], na.rm=TRUE)
        #final <- summary[,complete.cases(summary)]
        #rownames(final) <- NULL
        
        write.csv(format(final, digits=3), file=paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/biovar_summaries/", type, "_biovar_summary_bl_fut45.csv"))

     }

stopCluster(cl)
