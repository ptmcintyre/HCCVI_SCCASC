

library(ecoclim)
library(raster)
library(doParallel)
library(tidyverse)
library(here)
library(adehabitatLT)
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
      mdchi <- mdy
      mdchi[!is.na(mdchi)]<-pchi(mdchi[!is.na(mdchi)],2)
      mds<-mdchi
      mds[!is.na(mds)]<-qchi(mds[!is.na(mds)],1)
      c(mdy, mdp, mdchi, mds)
}

#type<-"Central_Tallgrass_Prairie"
mahal <- function(type, overwrite=F){
      
      outfile<- paste0(here("type_specific_modeling/testing_novelty"),"/", type, ".tif")
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
      #vars_used <- imp$var[imp$type == gsub("_", " ", type)][1:4]
      vars_used<-c("bio12", "bio5", "bio18", "bio1")
      
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
      #for testing, make r small, extent of central tallgrass
      r<-crop(r, veg)
      b<-crop(b, r)
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
types<-types[1]
v <- purrr::map_chr(types, possibly(mahal, "fail"))



#read in test raster
md.rasters <- parseMetadata(here("type_specific_modeling/temporal_novelty"),pattern=".tif")
i=1

names(md.rasters)
for(i in length(md.rasters)){
  veg.md<-raster(md.rasters[[i]][1])
  veg.md[!is.na(veg.md)]<-pchi(veg.md[!is.na(veg.md)], 2)
  veg.md[!is.na(veg.md)]<-qchi(veg.md[!is.na(veg.md)], 1)
  writeRaster(veg.md, paste0(here("type_specific_modeling/testing_novelty"),"/", names(veg.md), "_sigma.tif"), fomrat="GTiff", overwrite=T)

}
test.raster<-raster(testing)

test.raster[[1]]
pchi_raster<-pchi(test.raster[[1]], 2)

max(test.raster[[1]])
test.vector<-as.vector(test.raster[[1]])


pchi_raster[test.raster[[1]]>0]<-pchi(test.vector, 2)
psig<-qchi(pchi_raster,1)


test.rast2<-test.raster[[1]]

test.rast2[!is.na(test.rast2)]<-pchi(test.rast2[!is.na(test.rast2)], 2)


test.rast3<-pchi(test.rast2[!is.na(test.rast2)], 2)
as.vector(test.rast3)

test.rast2[!is.na(test.rast2)]<-test.rast3

as.vector(test.rast2)


test.rast2[!is.na(test.rast2)]<-qchi(test.rast2[!is.na(test.rast2)], 1)
cellStats(test.rast2, stat="mean")
?cellStats
