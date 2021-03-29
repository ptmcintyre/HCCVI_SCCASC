###working as of 3/19/2020 PJM, going for a version based on 19 variables, saving a two layer md and typicality (percentage of years)
## couldn't calculate in the function, so reading the results of the raw MD raster in and converting
## getting percentile of chi-square based on 2df (2 pca axis), then getting quantile with 1df, per Mahoney paper.


library(ecoclim)
library(raster)
library(doParallel)
library(tidyverse)
library(here)
library(adehabitatLT)
#library(caret)



# load baseline  data
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
imp <- imp[imp$rank >= 1,]#setting to all 19


novelty <- function(x, ...){
  
  # these lines are redundant, but it's faster since most cells are is.na(x[1])
  if(is.na(x[1])) return(c(NA, NA))
  if(any(is.na(x))) return(c(NA, NA))
  
  # unpack vector into matrix
  #PJM ncol=number of variables changing this to reflect # of variables works.
  x <- x[2:length(x)]
  m <- matrix(x, ncol=6, byrow=T)
  bl <- m[2:nrow(m),]
  
  # eliminate zero-variance variables
  keep <- rep(T, 6)
  for(i in 1:6) keep[i] <- sd(bl[,i]) != 0
  keep <- which(keep)
  bl <- bl[, keep]
  rc <- matrix(m[1, keep], nrow=1)
  
  # reduce to first 4 principal components PJM based on scree plot
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
  
  outfile<- paste0(here("type_specific_modeling/testing_novelty"),"/", "MD_6var_2pcs_rp45bl_b1b5b7b12b17b18", ".tif")
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
  vars_used<- c("bio1", "bio5","bio7", "bio12", "bio17",  "bio18")
  
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
#md.rasters <- parseMetadata(here("type_specific_modeling/temporal_novelty"),pattern=".tif")
md.rasters <- parseMetadata(here("mahalanobis_distance/"),pattern=".tif")
md.rasters<-md.rasters$path
test.raster<-md.rasters[3]
i=3

names(md.rasters)
for(i in 4:5){
  veg.md<-raster(md.rasters[[i]][1])
  veg.md[!is.na(veg.md)]<-pchi(veg.md[!is.na(veg.md)], 4)# specify df
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