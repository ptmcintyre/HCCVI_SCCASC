#Patrick_McIntyre@natureserve.org updating BLM HCCVI script for CEMML DOD project (February 2021)

#### this script implements a spatial block cross-validation version of
# recursive feature elimination (RFE, a method for culling unnecessary
# variables in the presence of correlation) for all CONUS BLM veg types,
# using variable importance as the elimination criterion.

##To install ecoclim 02/17/2021
#install.packages("remotes")
#remotes::install_github("matthewkling/ecoclim")



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
library(here)
here()
# load climate data
gc()
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))



# load veg data
#paths <- parseMetadata("I:/projects/BLM/Production/type_specific/veg_distributions/PRISM_800m/rasters")
veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))
names(veggies)

# select CONUS types
focal_types <- c("Northern_Tallgrass_Prairie", "Central_Mixedgrass_Prairie")
veggies <- subset(veggies, names(veggies)[names(veggies) %in% focal_types])


# this function fits a random forest model to a set of training data, 
# and returns the AUC for a set of evaluation data
nichefit <- function(mtry, nodesize, # randomForest params
                     climate_matrix, # matrix
                     rep, # only used for seed
                     #train_presence_points=NULL, # spatial points (for maxent only)
                     train_presence, train_absence, # matrices of climate data
                     eval_presence, eval_absence, # spatial points
                     raster_template){   # raster layer
      
      # prep training data
      train_presence <- as.data.frame(train_presence)
      train_absence <- as.data.frame(train_absence)
      vars <- names(train_presence)
      names(train_absence) <- vars
      train_presence$occurrence <- 1
      train_absence$occurrence <- 0
      train <- rbind(train_presence, train_absence)
      
      # fit model
      form <- paste0("as.factor(occurrence) ~ ", paste(vars, collapse=" + "))
      set.seed(rep)
      fit <- randomForest(as.formula(form), data=na.omit(train), ntree=500, mtry=mtry, nodesize=nodesize)
      
      # predictions for evaluation data
      if(is.null(dim(climate_matrix))){
            clm <- raster::raster(as.matrix(climate_matrix), template=raster_template)
      } else{clm <- stackMatrix(climate_matrix, raster_template)}
      eval_pres <- as.data.frame(raster::extract(clm, eval_presence))
      eval_abs <- as.data.frame(raster::extract(clm, eval_absence))
      names(eval_pres) <- names(eval_abs) <- vars
      eval_pres <- predict(fit, eval_pres, "prob")[,2]
      eval_abs <- predict(fit, eval_abs, "prob")[,2]
      
      # evaluation
      evaluate(p=as.vector(eval_pres), a=as.vector(eval_abs))@auc
}

detectCores()
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)

#foreach(i=1:length(names(veggies)) %dopar% 
i=1

for(type in names(veggies)) {
      #type <- names(veggies)[5]
      #type <- "Southern_Rocky_Mountain_Juniper_Woodland_and_Savanna"
      
      # slave coordination
      #outfile <- paste0("I:/projects/BLM/Production/type_specific/variable_selection/conus/rfe_results/rfe_", type, ".rds")
      outfile <- paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/type_specific_modeling/variable_selection/rfe_results/rfe_", type, ".rds")
      if(file.exists(outfile)) next()
      saveRDS(type, outfile)
      print(type)
      
      ### restructure veg data
      veg <- subset(veggies, type)
      #veg<-veggies[[i]]
      veg <- trimFast(extend(veg, 2))
      
      
      ### climate data prep
      climate <- crop(dd, veg) 
      #names(climate) <- vars
      clim <- values(climate)  
      vars <- names(climate)
      vrs <- vars
      
      ### veg data prep
      names(veg) <- "veg"
      veg <- crop(veg, climate)
      veg <- reclassify(veg, c(NA, NA, 0))
      px <- as.data.frame(rasterToPoints(stack(veg, climate[[1]])))
      names(px)[3:4] <- c("veg", "clim")
      px <- px[!is.na(px$clim),]
      px <- px[,1:3]
      
      pres <- px[px$veg > 0,]
      abs <- px[px$veg == 0,]
      pres$presence <- T
      abs$presence <- F
      prevalence <- nrow(pres) / nrow(px)
      pixels <- rbind(pres, abs)
      
      ### set up progress bar
      cullreps <- 19; repreps <- 20; splitreps <- 8
      pb1 <- txtProgressBar(min = 0, max = cullreps*repreps*splitreps, style = 3)
      k <- 0
      
      ### recursive passes through variable set, eliminating the least important each time
      i <- 1
      culled <- c()
      for(cull in 1:19){
            climate <- subset(climate, vrs)
            clim <- clim[,vrs] 
            
            ### repeat randomized processes (selection of traning/evaluation points, RF model, etc) to reduce noise -- RFE variance is high
            for(rep in 1:20){
                  
                  ### spatial block cross validation: define folds and then loop through them
                  xsplits <- quantile(pres$x)
                  ysplits <- quantile(pres$y)
                  for(split in 1:8){
                        pixels$train <- T
                        if(split < 5) pixels$train[pixels$x >= xsplits[split] & pixels$x < xsplits[split+1]] <- F
                        if(split > 4) pixels$train[pixels$y >= ysplits[split-4] & pixels$y < ysplits[split-4+1]] <- F
                        
                        ### select training points
                        set.seed(rep)
                        train_pres <- pixels %>% filter(presence==T & train==T) %>% sample_n(1000)
                        set.seed(rep)
                        train_abs <- pixels %>% filter(presence==F & train==T) %>% sample_n(1000)
                        coordinates(train_pres) <- c("x", "y")
                        coordinates(train_abs) <- c("x", "y")
                        
                        ### select evaluation points
                        evalpx <- min(1000, min(table(pixels$presence, pixels$train)[,1]))
                        set.seed(rep)
                        eval_pres <- pixels %>% filter(presence==T & train==F) %>% sample_n(evalpx)
                        set.seed(rep)
                        eval_abs <- pixels %>% filter(presence==F & train==F) %>% sample_n(evalpx)
                        coordinates(eval_pres) <- c("x", "y")
                        coordinates(eval_abs) <- c("x", "y")
                        set.seed(rep)
                        eval_points <- try(pwdSamplePA(train_pres, train_abs, eval_pres, eval_abs, tr=.2)) # subsample to remove spatial sorting bias
                        if(class(eval_points) == "try-error") next() # on the occasional random draw, no solution is found and an error results
                        
                        train_pres <- raster::extract(climate, train_pres)
                        train_abs <- raster::extract(climate, train_abs)
                        
                        ### fit and evaluate full random forest model
                        auc <- nichefit(climate_matrix=clim, 
                                        train_presence=na.omit(train_pres),
                                        train_absence=na.omit(train_abs), 
                                        eval_presence=eval_points[[1]], 
                                        eval_absence=eval_points[[2]], 
                                        raster_template=climate[[1]],
                                        mtry=1,
                                        nodesize=8,
                                        rep=rep)
                        
                        ### permutation test of variable importance, same idea as RandomForest default but with custom evaluation data,
                        ### (necessary because RandomForest accepts user-defined test data but uses it only for accuracy stats, not variable importance...)
                        for(var in vrs){
                              
                              perm <- list(
                                    clim = clim,
                                    train_pres = train_pres,
                                    train_abs = train_abs
                              )
                              
                              perm <- lapply(perm, function(x){
                                    x <- as.data.frame(x)
                                    set.seed(rep)
                                    x[,match(var, vrs)] <- x[sample(nrow(x), nrow(x)),match(var, vrs)]
                                    return(x)
                              })
                              
                              auc_perm <- nichefit(climate_matrix=perm$clim, 
                                                   train_presence=perm$train_pres,
                                                   train_absence=perm$train_abs, 
                                                   eval_presence=eval_points[[1]], 
                                                   eval_absence=eval_points[[2]], 
                                                   raster_template=climate[[1]],
                                                   mtry=1,
                                                   nodesize=8,
                                                   rep=rep)
                              
                              ### log results
                              df <- data.frame(veg=type,
                                               cull=cull,
                                               fold=split,
                                               rep=rep,
                                               variable=var, 
                                               auc_perm=auc_perm, 
                                               auc=auc)
                              df$decrease <- df$auc - df$auc_perm
                              if(i==1) d <- df else(d <- rbind(d, df))
                              saveRDS(d, outfile)
                              i <- i + 1
                              
                        }
                        
                        # update progress bar
                        k <- k + 1
                        setTxtProgressBar(pb1, k)
                  }
            }
            
            ### identify the least important variable and cull it
            d2 <- d[d$cull==cull,]
            d2 <- d2 %>% 
                  group_by(variable) %>% 
                  summarize(decrease=mean(decrease)) %>%
                  mutate(variable = as.character(variable))
            culled <- c(culled, d2$variable[d2$decrease==min(d2$decrease)])
            vrs <- d2$variable[d2$decrease!=min(d2$decrease)]
            
      }
      
      ### save veg type results
      results <- list(culled, d)
      saveRDS(results, outfile)
      
      
      close(pb1)
}

#stopCluster(cl)

plot(eval_pres)
plot(eval_abs)
plot(climate[[1]])
