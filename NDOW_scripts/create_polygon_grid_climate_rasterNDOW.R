
library(raster)
library(sf)


#clim_temp<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_year/", full.names = T)
maca_poly<-st_read("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/EcosystemGrids/MACA_poly_aea.shp")                

#maca_poly_aea<-st_transform(maca_poly, proj4string="aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ") 
plot(maca_poly_aea, add=T)
?st_transform

testr<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/NorthAmerica_IVC_Ecosystems_potential_NatureServe_v846.tif", RAT=F)
testr<-raster("S:/Projects/SCCASC_HCCVI/SCCASC_GIS/EcosystemGrids/IVC_pot_maca846_rcl.tif", RAT=F)
testr<-crop(testr,maca_poly)
plot(testr)


clim_grid<-rasterToPolygons(testr)
clim_grid<-st_as_sf(clim_grid)
LOCA_grid_SCCASC<-clim_grid
st_write(LOCA_grid_SCCASC, "S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids", "maca_grid_90m.shp", driver='ESRI Shapefile')

?rasterToPolygons
?ratify

?raster
