
# temporal mahalanobis distances using 18 biovars

library(ecoclim)
library(doParallel)
library(raster)


#################################################################

mahal <- function(x, nvars, split){
      if(is.na(x[1])) return(NA)
      x <- matrix(x, ncol=nvars)
      
      baseline <- x[1:split,]
      #recent <- x[(split+1):nrow(x),]
      recent<-x[31:60]
      
      
      recent <- colMeans(recent)
      means <- colMeans(baseline)
      cvm <- cov(baseline)
      
      mahalanobis(recent, means, cvm)      
}

mahal_prob <- function(x, nvars, split, byrow){
      if(is.na(x[1])) return(NA)
      x <- matrix(x, ncol=nvars, byrow=byrow)
      
      baseline <- x[1:split,]
      recent <- x[(split+1):nrow(x),]
      
      recent <- colMeans(recent)
      means <- colMeans(baseline)
      cvm <- cov(baseline)
      
      md_recent <- mahalanobis(recent, means, cvm)
      md_baseline <- mahalanobis(baseline, means, cvm)
      length(md_baseline[md_baseline>=md_recent]) / length(md_baseline)
}



mahal_mixed <- function(x, nvars, split, mean_start, byrow){
      if(is.na(x[1])) return(NA)
      x <- matrix(x, ncol=nvars, byrow=byrow)
      
      baseline_var <- x[1:split,]
      baseline_mean <- x[mean_start:split,]
      recent <- x[(split+1):nrow(x),]
      
      recent <- colMeans(recent)
      means <- colMeans(baseline_mean)
      cvm <- cov(baseline_var)
      
      mahalanobis(recent, means, cvm)
}

##################################################################


# which 5 variables? use the sequential culling order from the spatial correlation matrix
vu <- readRDS("I:/projects/BLM/Workspace/kling/veg/mahalanobis_distribution_model/variable_removal_sensitivity/variables_used.rds")
vu <- vu[[14]] # slot with 5 variables
vu <- sub("Bio", "b", vu)

# prep climate rasters
d <- parseMetadata("I:/climate_data/PRISM/derived/yearly/biovars/rasters", pattern=".grd")
d <- dplyr::arrange(d, year)
d <- d[d$year > 1949,]
s <- stack(d$path)
s <- subset(s,  names(s)[grepl(paste(vu,collapse="|"), names(s))]) # cull variables


ext <- extent(s)
#ext@xmin <- -90.5

# cluster stetup
cpus <- 6
cl <- makeCluster(cpus)
registerDoParallel(cl)

# compute 
nchunks <- cpus
r <- foreach(i=1:nchunks, 
             .packages=c("raster", "ecoclim")) %dopar% {
                   tile <- partitionRaster(s, nchunks, i, subextent=ext)
                   calc(tile, 
                        fun=function(x){mahal_prob(x, nvars=5, split=30, byrow=T)})
             }

stopCluster(cl)


# combine tiles and save
r <- do.call("merge", r)
writeRaster(r, filename="I:/projects/BLM/Workspace/kling/indices/mahalanobis/5_biovars/mahalanobis_typicality_5biovars_1950_1980_2013", format="raster", overwrite=T)
