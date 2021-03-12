

# comparing normals with yearly / monthly for raster resoluton, extent, stats, at project start



library(dplyr)
library(raster)
library(here)
library(rasterDT)
library(exactextractr)
library(sf)

# load baseline climate data- normals
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))


#load a year of bioclim vars
yearly.bio<-list.files("F:/Projects/CEMML/ClimateGrids/yearly_monthly/biovars_raster_by_year", pattern=".tif", full.names = T)
test.year<-stack(yearly.bio[1:19])
names(test.year) <- sort(paste0("bio", 1:19, "_1977"))

cellStats(test.year, stat='mean')
cellStats(dd, stat='mean')

mycomp<-cbind(cellStats(test.year, stat='mean'), cellStats(dd, stat='mean'))
my.comp<-as.data.frame(mycomp)

joint<-stack(dd, test.year)


#loop  comparing bioclim values
  for(i in 1:70) {
my.years<-c(1+(19*i), 19+(19*i))
test.year<-stack(yearly.bio[my.years[1]:my.years[2]])
my.stats<-cellStats(test.year, stat='mean')
my.comp[paste0("year", i)]<- as.vector(my.stats)
}


#load a year of monthly vars
monthly.bio<-list.files("F:/Projects/CEMML/ClimateGrids/yearly_monthly/mo_raster_by_year", pattern=".tif", full.names = T)
test.monthly<-stack(monthly.bio[37:72])

my.month_stats<-cellStats(test.monthly, stat='mean')

joint2<-stack(dd, test.monthly)

data.month<-as.data.frame(my.month_stats)
