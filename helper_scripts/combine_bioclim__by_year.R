##Script to combine biovars by year into a multiband raster
##easier to work with M. Klings scripts, which are set up for multiband rasters 
## with all biovars from a year as bands in a sinlge raster named by yearr)

library(ecoclim)
library(raster)
library(doParallel)
library(stringr)


# load baseline TopoPrism data
b <- parseMetadata(here("biovars/biovars_raster_by_year"), pattern=".tif")
b <- dplyr::arrange(b, year, variable)
#b <- b[b$year <= 2005,]
baseline <- b

my.years<-unique(baseline$year)



i=1
#years <- 1895:1949

names(b)
names(b)[3]<-"time"
b[1000,]
#realized there are two scenarios for futre, so twice as many files, need to select one scenario
#my.years2<-my.years[1:31]
my.years2<-my.years[66:96]


#b<-b[-grep("rcp45", b$path), ]

cpus <- 3
cl <- makeCluster(cpus)
registerDoParallel(cl)


#year=1977
#RENAME file by scenario chosen, manual at moment, not reading from file

foreach(year= unique(my.years2),
        .packages=c("raster", "dplyr", "caret","here")) %dopar% {
          print(year)
          start <- Sys.time()
          
          focal_year<-subset(b, time==year)
          
          s <- stack(focal_year$path)
          names(s) <- sort(paste0("bio", 1:19))
          s<- s[[c(1,12,13,14,15,16,17,18,19,2,3,4,5,6,7,8,9,10,11)]]
          

          
          writeRaster(s, filename=paste0(here("biovars/biovars_by_year_multiband"),"/biovars_LOCA_rcp85_", year, ".tif"), 
                      format="GTiff", overwrite=T)
          
        
          
          
}
stopCluster(cl)



