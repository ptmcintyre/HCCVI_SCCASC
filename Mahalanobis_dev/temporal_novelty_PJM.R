

library(ecoclim)
library(raster)
library(doParallel)
library(tidyverse)
library(here)
#library(caret)



# load baseline TopoPrism data
b <- parseMetadata(here("biovars/biovars_by_year_multiband/baseline"))
#b[-grep("rcp85", b$path), ]#drop rcp85
b <- dplyr::arrange(b, year)
b <- b[b$year <= 2005,]
baseline <- b

# load recent normals
#r <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means", pattern=".tif")
r <- parseMetadata(here("biovars/near"), pattern=".tif")
#r <- r[grepl("1981_2014", r$path),]
vars <- r$variable
recent <- motleyStack(r$path)
names(recent) <- vars

# load veg data DO WE WANT BPS or EVT?
veg_rasters<- list.files(here("system_distributions/MACA_rasters"), pattern=".tif")
veggies <- raster::stack(here("system_distributions/MACA_rasters", veg_rasters))

## filter out cnmx types
#cnmx <- read.csv("I:/projects/DOCE/workspace/auer/identifying_cnmx_types/cnmx_types.csv")
#cnmx <- as.character(cnmx$type)
#veggies <- subset(veggies, names(veggies)[!names(veggies) %in% cnmx])

# load variable importance data
imp <- read.csv(here("type_specific_modeling/variable_selection/variable_importance.csv"), stringsAsFactors=F)
imp <- imp[imp$rank >= 14,]


novelty <- function(x, ...){
      
      # these lines are redundant, but it's faster since most cells are is.na(x[1])
      if(is.na(x[1])) return(c(NA, NA))
      if(any(is.na(x))) return(c(NA, NA))
      
      # unpack vector into matrix
      x <- x[2:length(x)]
      m <- matrix(x, ncol=4, byrow=T)
      bl <- m[2:nrow(m),]
      
      # eliminate zero-variance variables
      keep <- rep(T, 4)
      for(i in 1:4) keep[i] <- sd(bl[,i]) != 0
      keep <- which(keep)
      bl <- bl[, keep]
      rc <- matrix(m[1, keep], nrow=1)
      
      # reduce to first 2 principal components
      pc <- prcomp(bl, scale.=T)
      pcx <- pc$x[,1:2]
      pcy <- predict(pc, rc)[,1:2]
      
      # mahalanobis distance
      means <- colMeans(pcx)
      cvm <- cov(pcx)
      mdx <- mahalanobis(pcx, means, cvm)
      mdy <- mahalanobis(pcy, means, cvm)
      mdp <- mean(mdy >= mdx)
      
      c(mdy, mdp)
}

#type<-"Great_Basin_Pinyon_Juniper_Woodland"
mahal <- function(type, overwrite=F){
      
      outfile<- paste0(here("type_specific_modeling/temporal_novelty"),"/", type, ".tif")
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
      vars_used <- imp$var[imp$type == gsub("_", " ", type)][1:4]
     
      
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
      ve <- extend(veg, r) %>% 
            crop(r) %>%
            mask(r[[1]])
      clim <- stack(ve, r, b)
      
      beginCluster(15)
      md <- clusterR(clim, calc, args=list(fun=novelty), m = 100)
      md <- crop(md, veg)
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
types<-names(veggies)
v <- purrr::map_chr(types, possibly(mahal, "fail"))
