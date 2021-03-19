
# temporal mahalanobis distances using 5 biovars

library(ecoclim)
library(doParallel)
library(raster)
library(here)



mahal_mixed <- function(x, nvars, mean_start, mean_end, cov_start, cov_end, typ_start, typ_end, rec_start, rec_end, byrow){
      if(is.na(x[1])) return(NA)
      x <- matrix(x, ncol=nvars, byrow=byrow)
      
      recent <- x[rec_start:rec_end,]
      baseline_var <- x[cov_start:cov_end,]
      baseline_mean <- x[mean_start:mean_end,]
      typ <- x[typ_start:typ_end,]
      
      recent <- colMeans(recent)
      means <- colMeans(baseline_mean)
      cvm <- cov(baseline_var)
      
      md_recent <- mahalanobis(recent, means, cvm)
      md_typ <- mahalanobis(typ, means, cvm)
      length(md_typ[md_typ>=md_recent]) / length(md_typ)
}

##################################################################


# which 5 variables? use the sequential culling order from the spatial correlation matrix
vu <- readRDS("I:/projects/BLM/Workspace/kling/veg/mahalanobis_distribution_model/variable_removal_sensitivity/variables_used.rds")
vu <- vu[[14]] # slot with 5 variables
vu <- sub("Bio", "b", vu)

# prep climate rasters
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))

d <- parseMetadata("I:/climate_data/PRISM/derived/yearly/biovars/rasters", pattern=".grd")

d <- parseMetadata(here("biovars/biovars_raster_by_year"), pattern=".tif")

d <- dplyr::arrange(d, year)
s <- stack(d$path)
s <- subset(s,  names(s)[grepl(paste(vu,collapse="|"), names(s))]) # cull variables


ext <- extent(s)
#ext@xmin <- -90.5

# cluster stetup
cpus <- 6
cl <- makeCluster(cpus)
registerDoParallel(cl)
nchunks <- cpus
r <- foreach(i=1:nchunks, 
             .packages=c("raster", "ecoclim")) %dopar% {
                   tile <- partitionRaster(s, nchunks, i, subextent=ext)
                   calc(tile, fun=function(x){mahal_mixed(x, nvars=19, 
                                                          mean_start=1, mean_end=30, 
                                                          cov_start=1, cov_end=30, 
                                                          typ_start=1, typ_end=30, 
                                                          rec_start=31, rec_end=60, byrow=T)})
             }
stopCluster(cl)
r <- do.call("merge", r)
#writeRaster(r, filename="I:/projects/BLM/Workspace/kling/indices/mahalanobis/5_biovars/mahalanobis_typicality_5biovars_1895m_1950v_1980_2013", format="raster", overwrite=T)
writeRaster(r, filename=paste0(here("Mahalanobis_dev/mahal19_"), "test"),format="raster", overwrite=T)