

library(ecoclim)
library(raster)
library(doParallel)
library(tidyverse)
#library(caret)



# load baseline TopoPrism data
b <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/yearly/biovars_w_PRISM/rasters")
b <- dplyr::arrange(b, year)
b <- b[b$year <= 1980,]
#b <- b[b$year <= 1955,]
baseline <- b
#test.raster<-motleyStack(b$path)

#test.raster[[4]]

# load recent normals
r <- parseMetadata("I:/climate_data/TopoWx/v2014/derived/normals/biovars/rasters_means", pattern=".tif")
r <- r[grepl("1981_2014", r$path),]
vars <- r$variable
recent <- motleyStack(r$path)
names(recent) <- vars

# load veg data
paths <- parseMetadata(#"I:/projects/BLM/Manuscript/production/veg_distributions/rasters_800m", 
      #"I:/projects/DOCE/production/type_specific/veg_distributions/rasters_800m", 
      "I:/projects/DOCE/production/type_specific/veg_distributions/rasters_800m_cover_EVT", 
      pattern=".tif", drops = c(".aux", ".xml", ".ovr"))
veggies <- stack(paths)
names(veggies) <- sub(".tif", "", basename(paths))

## filter out cnmx types
#cnmx <- read.csv("I:/projects/DOCE/workspace/auer/identifying_cnmx_types/cnmx_types.csv")
#cnmx <- as.character(cnmx$type)
#veggies <- subset(veggies, names(veggies)[!names(veggies) %in% cnmx])

# load variable importance data
imp <- read.csv("I:/projects/DOCE/production/type_specific/variable_selection/variable_importance.csv", stringsAsFactors=F)
imp <- imp[imp$rank >= 16,]


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


mahal <- function(type, overwrite=F){
      
      outfile <- paste0("I:/projects/DOCE/production/type_specific/temporal_novelty/rasters/", 
                        type, ".tif")
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
      vars_used <- imp$var[imp$type == gsub("_", " ", type)]
      
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


types <- sample(names(veggies), length(names(veggies)))
v <- purrr::map_chr(types, possibly(mahal, "fail"))
