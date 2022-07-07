
library(XML)
library(rvest)
library(httr)

h <- 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/sand/mean/0_5/'
  
  
  
  my.rasts <- XML::getHTMLLinks(
    h, 
    xpQuery = "//a/@href['.tif'=substring(., string-length(.) - 3)]"
  )
my.rasts

my.urls<-paste0(h,my.rasts)
destination<-"S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/polaris_individual/sand/"
my.dest<-paste0(destination, my.rasts)

for(i in 1:length(my.urls)){
  httr::GET(my.urls[i] ,
  httr::write_disk(my.dest[i]))
  
}


myzips<-dir.create("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/polaris_individual/sand2")
## save the current directory path for later
wd <- getwd()
## change working directory for the download
setwd("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/polaris_individual/sand2")
## create all the new files
file.create(zips)
## download them all
lapply(paste0(h, zips), function(x) download.file(x, basename(x)))
## reset working directory to original
setwd(wd)


h
zips
http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/sand/mean/0_5/lat3435_lon-78-77.tif
"S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/polaris_individual/"

download.file("http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/sand/mean/0_5/lat3435_lon-78-77.tif", "S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/lat3435_lon-78-77.tif", mode="wb")
, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"),
              headers = NULL, …)


httr::GET("http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/sand/mean/0_5/lat3435_lon-79-78.tif" ,httr::write_disk("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/soils/lat3435_lon-79-78.tif"))

h




