###Patrick_McIntyre (ptmcintyre@gmail.com/patrick_mcintyre@natureserve.org)
#This script takes netcdf files of daily LOCA climate data
#and translates them to individual rasters of monthly target data by year
#loca data is contained from download site in separate pr tasmin, tasmax folders
#and historial vs. rcp85 folders (we are ignoring rcp45)
#coud be streamlined and functionized. working as of 1/20/2022

rm(list=ls())
gc()

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(terra)  #newer appraoch to rasters from the "raster package" team

#####################################################################
#USER INPUTS

###UPDATE NAME OF GCM for adddinh to output file nemas
GCM<- "CCSM4"   #enter name of GCM to include with file

#define hist file locations, update to reflect correct GCM 
hist.pr.files<-list.files("T:/LOCA/CCSM4/historical/pr/", full.names = T)
hist.tmin.files<-list.files("T:/LOCA/CCSM4/historical/tasmin/", full.names = T)
hist.tmax.files<-list.files("T:/LOCA/CCSM4/historical/tasmax/", full.names = T)

#define rc85 file locations
rcp85.pr.files<-list.files("T:/LOCA/CCSM4/rcp85/pr/", full.names = T)
rcp85.tmin.files<-list.files("T:/LOCA/CCSM4/rcp85/tasmin/", full.names = T)
rcp85.tmax.files<-list.files("T:/LOCA/CCSM4/rcp85/tasmax/", full.names = T)

##define output base location
historic_out<- "S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/test/historic"
future_out<- "S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/monthly_vals_year/test/future"
dir.create(historic_out) #creates directory if doesn't exist
dir.create(future_out) #creates directory if doesn't exist

##PARTS BELOW PROBABLY DON'T CHANGE
#define years of interest for all historic and future years
historic_years<-seq(1950,2005, 1)
future_years<-seq(2006,2100, 1)

loca.template<-raster(hist.pr.files[1]) #selecting first file to act as a template for both hist and futre (proj, coords, cells)
#nc_data <- nc_open(hist.pr.files[1]) #to explore one netcdf file

#define month and days for slicing netcdf file,includes leap years
month_num<-c(1,2,3,4,5,6,7,8,9,10,11,12)
start_day<-c(1,32,60,91,121,152,182,213,244,274,305,335)
end_day<-c(31,59,90,120,151,181,212,243,273,304,334,365)
leap_start<-c(1,32,61,92,122,153,183,214,245,275,306,336)
leap_end<-c(31,60,91,121,152,182,213,244,274,305,335,366)
month_day<-as.data.frame(cbind(month_num, start_day, end_day, leap_start, leap_end))

#################################################################################
#PART 1 loop for historic data
# long, would be good to simplify

i=1 #for troublshooting loop
j=1#for troublshooting loop
for (i in 1:length(hist.pr.files)){
        #define and get precip data
    pr_data <- nc_open(hist.pr.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(pr_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(pr_data, "lat", verbose = F) 
    t <- ncvar_get(pr_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    pr.fillvalue <- ncatt_get( pr_data, "pr", "_FillValue")
    pr.array <- ncvar_get( pr_data, "pr")
    pr.array[pr.array == pr.fillvalue$value] <- NA
    pr.array<- pr.array
    
    #loop through precip
        for (j in 1:12){
            if(length(t)<366){
                pr.slice <- pr.array[, , month_day$start_day[j]:month_day$end_day[j]]
            } else {
                pr.slice <- pr.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
            }
        pr.slice<-rowSums(pr.slice, dims=2) #sums precip
    
        r.precip<- raster(t(pr.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.precip<-flip(r.precip)  #netcdf files are wierd, need to be flipped
        r.precip<-r.precip*86400  #multiply by 86400 to convert to mm/day (from kg/m2/seconds in a day)
    
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.precip)=xmin(r.precip)-360 ; xmax(r.precip)=xmax(r.precip)-360
        #plot(r) 
        
        writeRaster(r.precip, paste0(historic_out, "/", historic_name, "_", "PPTmm_",historic_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
        }
    rm(pr.array);gc()
    
    #define and get tmax data
    tmax_data <- nc_open(hist.tmax.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(tmax_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(tmax_data, "lat", verbose = F) 
    t <- ncvar_get(tmax_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    tmax.fillvalue <- ncatt_get(tmax_data, "tasmax", "_FillValue")
    tmax.array <- ncvar_get(tmax_data, "tasmax")
    tmax.array[tmax.array == tmax.fillvalue$value] <- NA
    tmax.array<- tmax.array
    
    #loop through tmax
    for (j in 1:12){
        if(length(t)<366){
            tmax.slice <- tmax.array[, , month_day$start_day[j]:month_day$end_day[j]]
        } else {
            tmax.slice <- tmax.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
        }
        tmax.slice<-rowMeans(tmax.slice, dims=2) #sums for year
        
        r.tmax<- raster(t(tmax.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.tmax<-flip(r.tmax)  #netcdf files are wierd, need to be flipped
        r.tmax<-r.tmax-273.15  #convert to C from K)
        
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.tmax)=xmin(r.tmax)-360 ; xmax(r.tmax)=xmax(r.tmax)-360
        #plot(r) 
        
        writeRaster(r.tmax, paste0(historic_out, "/", historic_name, "_", "Tmax_",historic_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
    rm(tmax.array);gc()
    
    #define and get tmin data
    tmin_data <- nc_open(hist.tmin.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(tmin_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(tmin_data, "lat", verbose = F) 
    t <- ncvar_get(tmin_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    tmin.fillvalue <- ncatt_get(tmin_data, "tasmin", "_FillValue")
    tmin.array <- ncvar_get(tmin_data, "tasmin")
    tmin.array[tmin.array == tmin.fillvalue$value] <- NA
    tmin.array<- tmin.array
    
    for (j in 1:12){
        if(length(t)<366){
            tmin.slice <- tmin.array[, , month_day$start_day[j]:month_day$end_day[j]]
        } else {
            tmin.slice <- tmin.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
        }
        tmin.slice<-rowMeans(tmin.slice, dims=2) #sums for year
        
        r.tmin<- raster(t(tmin.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.tmin<-flip(r.tmin)  #netcdf files are wierd, need to be flipped
        r.tmin<-r.tmin-273.15  #convert to C from K)
        
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.tmin)=xmin(r.tmin)-360 ; xmax(r.tmin)=xmax(r.tmin)-360
        #plot(r) 
        
        writeRaster(r.tmin, paste0(historic_out, "/", historic_name, "_", "tmin_",historic_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
    rm(tmin.array);gc()
}



#################################################################################
#PART 2 loop for RCP85 data

i=1 #for troublshooting loop
j=1#for troublshooting loop
for (i in 1:length(rcp85.pr.files)){
    #define and get precip data
    pr_data <- nc_open(rcp85.pr.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(pr_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(pr_data, "lat", verbose = F) 
    t <- ncvar_get(pr_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    pr.fillvalue <- ncatt_get( pr_data, "pr", "_FillValue")
    pr.array <- ncvar_get( pr_data, "pr")
    pr.array[pr.array == pr.fillvalue$value] <- NA
    pr.array<- pr.array
    
    #loop through precip
    for (j in 1:12){
        if(length(t)<366){
            pr.slice <- pr.array[, , month_day$start_day[j]:month_day$end_day[j]]
        } else {
            pr.slice <- pr.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
        }
        pr.slice<-rowSums(pr.slice, dims=2) #sums precip
        
        r.precip<- raster(t(pr.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.precip<-flip(r.precip)  #netcdf files are wierd, need to be flipped
        r.precip<-r.precip*86400  #multiply by 86400 to convert to mm/day (from kg/m2/seconds in a day)
        
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.precip)=xmin(r.precip)-360 ; xmax(r.precip)=xmax(r.precip)-360
        #plot(r) 
        
        writeRaster(r.precip, paste0(future_out, "/", future_name, "_", "PPTmm_",future_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
    rm(pr.array);gc()
    
    #define and get tmax data
    tmax_data <- nc_open(rcp85.tmax.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(tmax_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(tmax_data, "lat", verbose = F) 
    t <- ncvar_get(tmax_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    tmax.fillvalue <- ncatt_get(tmax_data, "tasmax", "_FillValue")
    tmax.array <- ncvar_get(tmax_data, "tasmax")
    tmax.array[tmax.array == tmax.fillvalue$value] <- NA
    tmax.array<- tmax.array
    
    #loop through tmax
    for (j in 1:12){
        if(length(t)<366){
            tmax.slice <- tmax.array[, , month_day$start_day[j]:month_day$end_day[j]]
        } else {
            tmax.slice <- tmax.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
        }
        tmax.slice<-rowMeans(tmax.slice, dims=2) #sums for year
        
        r.tmax<- raster(t(tmax.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.tmax<-flip(r.tmax)  #netcdf files are wierd, need to be flipped
        r.tmax<-r.tmax-273.15  #convert to C from K)
        
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.tmax)=xmin(r.tmax)-360 ; xmax(r.tmax)=xmax(r.tmax)-360
        #plot(r) 
        
        writeRaster(r.tmax, paste0(future_out, "/", future_name, "_", "Tmax_",future_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
    rm(tmax.array);gc()
    
    #define and get tmin data
    tmin_data <- nc_open(rcp85.tmin.files[i])
    #get time, lat, lon as vectors
    lon <- ncvar_get(tmin_data, "lon") #coordinates still 0 to 360
    lat <- ncvar_get(tmin_data, "lat", verbose = F) 
    t <- ncvar_get(tmin_data, "time")
    #creates an array from data & replaces missing data value with "NA"
    tmin.fillvalue <- ncatt_get(tmin_data, "tasmin", "_FillValue")
    tmin.array <- ncvar_get(tmin_data, "tasmin")
    tmin.array[tmin.array == tmin.fillvalue$value] <- NA
    tmin.array<- tmin.array
    
    for (j in 1:12){
        if(length(t)<366){
            tmin.slice <- tmin.array[, , month_day$start_day[j]:month_day$end_day[j]]
        } else {
            tmin.slice <- tmin.array[, , month_day$leap_start[j]:month_day$leap_end[j]]
        }
        tmin.slice<-rowMeans(tmin.slice, dims=2) #sums for year
        
        r.tmin<- raster(t(tmin.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), template=loca.template)
        r.tmin<-flip(r.tmin)  #netcdf files are wierd, need to be flipped
        r.tmin<-r.tmin-273.15  #convert to C from K)
        
        ##part below converts from coordinates of 0 to 360 to -180 to 180
        xmin(r.tmin)=xmin(r.tmin)-360 ; xmax(r.tmin)=xmax(r.tmin)-360
        #plot(r) 
        
        writeRaster(r.tmin, paste0(future_out, "/", future_name, "_", "tmin_",future_years[i], "_", month_day$month_num[j], ".tif"), "GTiff", overwrite=TRUE)
        
    }
    rm(tmin.array);gc()
}

rm(list=ls())
gc()
