
library(raster)
library(sf)

clim_temp<-list.files("S:/Projects/SCCASC_HCCVI/HCCVI_SCCASC_R_Project/process_initial_climate_data/biovars_year/", full.names = T)
                
testr<-raster(clim_temp[1])



clim_grid<-rasterToPolygons(testr)
clim_grid<-st_as_sf(clim_grid)
LOCA_grid_SCCASC<-clim_grid
st_write(LOCA_grid_SCCASC, "S:/Projects/SCCASC_HCCVI/SCCASC_GIS/ClimateGrids", "LOCA_grid_SCCASC.shp", driver='ESRI Shapefile')

