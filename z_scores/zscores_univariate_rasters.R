###modified for CEMML HCCVI Patrick McIntyre 3/26/2021


library(ecoclim)
library(raster)
library(doParallel)



# DATA ORGANIZATION ##############################################################

# near future means for each variable UPDATE TO CORRECT FUTURE/NEAR time period and 45 vs. 85 emissions
#rm <- parseMetadata("I:/climate_data/ClimateNA/future/derived/RCP85_ensemble_2025/biovars")
#rm <- d[grepl("1981_2012", d$path),]
rm <- parseMetadata(here("biovars/near_85"), pattern=".tif")
#rm <- parseMetadata(here("biovars/future_45"), pattern=".tif")


# vars <- r$variable
# recent <- motleyStack(r$path)
# names(recent) <- vars

# load baseline  data
b <- parseMetadata(here("biovars/biovars_by_year_multiband/baseline"))
#b[-grep("rcp85", b$path), ]#drop rcp85
b <- dplyr::arrange(b, year)
b <- b[b$year <= 2005,]

baseline <- b

testraster<-stack(b$path[1])
testraster[[11]]

outdir <- "I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/z_score_rasters/bl_near_85"


msk <- raster(rm$path[1])


# PROCESSING ################################################################

cpus <- 8
cl <- makeCluster(cpus)
registerDoParallel(cl)

foreach(var=unique(rm$variable), 
        .packages=c("raster", "ecoclim")) %dopar% {
              
              #var <- "bio18"
              
              # housekeeping
              rasterOptions(tmpdir="I:/temp")
              short_vr <- sub("bio", "", var)
              vr<- sub("io", "", var)
              
              # load data PJM- a bit dangerous. references biolayer by band #, tiff files were with bioclim vars in order by band
              y <- stackBands(b$path[as.integer(b$year) <= 2005], as.numeric(short_vr))
              #y<- stack(b$path)
              #y <- stackBands(b$path[as.integer(b$year) <= 1980], vr)
              r <- stack(rm$path[rm$variable == var])
              
              # remove values that should be NA, and sync extents
              r <- mask(r, msk)
              y <- crop(y, r)
              
              # calculate and save 
              dz <- function(x){
                    r <- x[1]
                    if(is.na(r)) return(c(NA, NA))
                    y <- x[2:length(x)]
                    m <- mean(y)
                    s <- sd(y)
                    d <- r - m
                    z <- d / s
                    c(d,z)
              }
              s <- calc(stack(r, y), dz)
              names(s) <- c("delta", "zscore")
              writeRaster(s,
                        filename = paste0(outdir, "/", var, ".tif"),
                        format="GTiff", overwrite=T)  
        }

stopCluster(cl)

