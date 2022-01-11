library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(terra)


#point to directory of ndcf files
clim.files<-list.files("T:/LOCA/CCSM4/rcp85/pr/", full.names = T)
clim.files
loca.template<-raster(clim.files[1])

#define month and days for slicing netcdf file
month_num<-c(1,2,3,4,5,6,7,8,9,10,11,12)
start_day<-c(1,32,60,91,121,152,182,213,244,274,305,335)
end_day<-c(31,59,90,120,151,181,212,243,273,304,334,365)
month_day<-as.data.frame(cbind(month_num, start_day, end_day))
#define years
#years<-seq(1950,2005, 1)
years<-seq(2006,2100, 1)


#base name of file
base_name<-"LOCA_CCSM4_Monthly_rcp85_PPTmm"

i=1
j=1
for (i in 1:length(clim.files)){
nc_data <- nc_open(clim.files[i])
#print(nc_data)

lon <- ncvar_get(nc_data, "lon")-360 #convert to appropriate degrees
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

fillvalue <- ncatt_get(nc_data, "pr", "_FillValue")
fillvalue
pr.array <- ncvar_get(nc_data, "pr")
pr.array[pr.array == fillvalue$value] <- NA
pr.array<-pr.array

str(pr.array)
dim(pr.array) 

    for (j in 1:12){
    pr.slice <- pr.array[, , month_day$start_day[j]:month_day$end_day[j]] 
    pr.slice<-rowSums(pr.slice, dims=2)
    dim(pr.slice)
    str(pr.slice)
    
    r <- raster(t(pr.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
    r<-flip(r)
    r<-r*86400  #multiply by 86400 to convert to mm/day (seconds in a day)
    # writeRaster(r, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/historic/", 
    #                       base_name,"_", years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
    
    writeRaster(r, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85/", 
                          base_name,"_", years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
    
    }
}


##historical
clim.files<-list.files("T:/LOCA/CCSM4/Historical_correct/pr/", full.names = T)
clim.files
loca.template<-raster(clim.files[1])

base_name<-"LOCA_CCSM4_Monthly_historical_PPTmm"
years<-seq(1950,2005, 1)

month_num<-c(1,2,3,4,5,6,7,8,9,10,11,12)
start_day<-c(1,32,60,91,121,152,182,213,244,274,305,335)
end_day<-c(31,59,90,120,151,181,212,243,273,304,334,365)
month_day<-as.data.frame(cbind(month_num, start_day, end_day))

i=1
j=1
for (i in 1:length(clim.files)){
    nc_data <- nc_open(clim.files[i])
    #print(nc_data)
    
    lon <- ncvar_get(nc_data, "lon")-360 #convert to appropriate degrees
    lat <- ncvar_get(nc_data, "lat", verbose = F)
    t <- ncvar_get(nc_data, "time")
    
    fillvalue <- ncatt_get(nc_data, "pr", "_FillValue")
    fillvalue
    pr.array <- ncvar_get(nc_data, "pr")
    pr.array[pr.array == fillvalue$value] <- NA
    pr.array<-pr.array
    
    str(pr.array)
    dim(pr.array) 
    
    for (j in 1:12){
        pr.slice <- pr.array[, , month_day$start_day[j]:month_day$end_day[j]] 
        pr.slice<-rowSums(pr.slice, dims=2)
        dim(pr.slice)
        str(pr.slice)
        
        r <- raster(t(pr.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r<-flip(r)
        r<-r*86400  #multiply by 86400 to convert to mm/day (seconds in a day)
         writeRaster(r, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/historic/", 
                               base_name,"_", years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
        #writeRaster(r, paste0("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/rcp85/", 
         #                     base_name,"_", years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
}

rm()
gc()
