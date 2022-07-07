
# This script combines task 1 typicality exposure and task 2
# bioclimatic niche exposure into a final exposure index
# layer for each veg type.


library(ecoclim)
library(raster)
library(doParallel)


# typicality and niche raster inputs
d1 <- list.files("I:/projects/BLM/Production/type_specific/trend_typicality/historic_conus/rasters", full.names=T)
d2 <- list.files("I:/projects/BLM/Production/type_specific/niche_models/historic_conus/rasters_delta", full.names=T)
d1 <- d1[!grepl(".aux|.ovr", d1)]
d2 <- d2[!grepl(".aux|.ovr", d2)]
veggies <- substr(basename(d1), regexpr("_2014_", basename(d1)) + 6, nchar(basename(d1))-4)

# veg distribution data
# load veg data
paths <- parseMetadata("I:/projects/BLM/Production/type_specific/veg_distributions/PRISM_800m/rasters", pattern=".tif")
paths <- paths[!grepl(".aux|.ovr", paths)]
distrib <- stack(paths)
names(distrib) <- sub(".tif", "", basename(paths))


# cluster stetup
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)

# loop
r <- foreach(type=veggies,
             .packages=c("raster", "ecoclim", "ggplot2", "viridis")) %dopar% {
                   
                   # open input rasters
                   s <- stack(d1[grepl(type, d1)],
                              d2[grepl(type, d2)])
                   names(s) <- c("t1r", "t2r")
                   
                   # rescale inputs from 0 (good) to .5 (bad) exposure range
                   s$t1 <- (1 - s$t1r) / 2
                   s$t2 <- (-1 * s$t2r + 1) / 4
                   
                   # save 2d sum
                   index <- s$t1 + s$t2
                   try(writeRaster(index,
                                   paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/rasters/historic_exposure_index_", type), 
                                   format="GTiff", overwrite=T))
                   
                   typename <- gsub(".tif", "", type)
                   db <- subset(distrib, typename)
                   db <- crop(db, s)
                   s <- mask(s, db)
                   index <- mask(index, s[[1]])
                   s <- stack(s, index, db)
                   names(s) <- c("t1r", "t2r", "t1", "t2", "index", "veg")
                   
                   try(writeRaster(s[[1:5]],
                                   paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/rasters_clipped/historic_exposure_components_", type), 
                                   format="GTiff", overwrite=T))
                   
                   d <- as.data.frame(rasterToPoints(s))
                   d <- d[!is.na(d$veg),]
                   
                   # scatterplot
                   p <- ggplot(d[sample(nrow(d), min(nrow(d), 10000)),], aes(t1, t2, color=index)) +
                         geom_vline(xintercept=c(0,.5), color="gray") +
                         geom_hline(yintercept=c(0,.25,.5), color="gray") +
                         geom_point(size=2, alpha=.3) +
                         geom_density2d(color="black") +
                         geom_abline(slope=-1, intercept=.25) +
                         geom_abline(slope=-1, intercept=.5) +
                         geom_abline(slope=-1, intercept=.75) +
                         xlim(0,.5) + ylim(0,.5) +
                         coord_fixed() +
                         scale_color_viridis(limits=c(0,1)) +
                         whiteness() +
                         labs(x = "Task 1 exposure (rescaled)",
                              y = "Task 2 exposure (rescaled)",
                              color = "combined\nindex",
                              title = paste0("Exposure index for ", gsub("_", " ", typename)))
                   ggsave(paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/charts/historic_exposure_scatter_", typename, ".png"),
                          width=8, height=6)
                   
                   
                   # prep state boundary data
                   states <- rgdal::readOGR("I:/projects/BLM/Workspace/kling/shapefiles", layer="US_states")
                   ext <- extent(s)
                   ext <- as(ext, "SpatialPolygons")
                   proj4string(ext) <- CRS(proj4string(states))
                   states <- rgeos::gIntersection(states, ext, byid=T)
                   states.points <- fortify(states)
                   states.points$region <- "states"
                   states.points$polygon <- as.numeric(states.points$piece) + 1000
                   
                   # map: index
                   p <- ggplot() +
                         geom_polygon(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                      color="white", fill="gray90", size=.6) +
                         geom_raster(data=d, aes(x, y, fill=index)) +
                         geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                   color="white", fill=NA, size=.6) +
                         coord_fixed(ratio=1.2) +
                         scale_fill_viridis(limits=c(0,1)) +
                         whiteness() +
                         labs(fill = "Final\nindex",
                              title = paste0("Exposure index for ", gsub("_", " ", typename))) +
                         theme(axis.title=element_blank(), axis.text=element_blank(), 
                               axis.ticks=element_blank(), panel.grid=element_blank())
                   ggsave(paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/charts/historic_exposure_map_", typename, ".png"),
                          width=8, height=6)
                   
                   # map: task 1
                   p <- ggplot() +
                         geom_polygon(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                      color="white", fill="gray90", size=.6) +
                         geom_raster(data=d, aes(x, y, fill=t1r)) +
                         geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                   color="white", fill=NA, size=.6) +
                         coord_fixed(ratio=1.2) +
                         scale_fill_gradientn(colours=rev(viridis_pal()(256)), limits=c(0,1)) +
                         whiteness() +
                         labs(fill = "Task 1\nindex",
                              title = paste0("Task 1 typicality for ", gsub("_", " ", typename))) +
                         theme(axis.title=element_blank(), axis.text=element_blank(), 
                               axis.ticks=element_blank(), panel.grid=element_blank())
                   ggsave(paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/charts/historic_exposure_map_t1_", typename, ".png"),
                          width=8, height=6)
                   
                   # map: task 2
                   orangepurple <- c('#b35806','#f1a340','#fee0b6','#f7f7f7','#d8daeb','#998ec3','#542788')
                   p <- ggplot() +
                         geom_polygon(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                      color="white", fill="gray90", size=.6) +
                         geom_raster(data=d, aes(x, y, fill=t2r)) +
                         geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                   color="white", fill=NA, size=.6) +
                         coord_fixed(ratio=1.2) +
                         scale_fill_gradientn(colours=orangepurple,
                                              limits=c(-1,1)) +
                         whiteness() +
                         labs(fill = "Task 2\nindex",
                              title = paste0("Task 2 niche trend for ", gsub("_", " ", typename))) +
                         theme(axis.title=element_blank(), axis.text=element_blank(), 
                               axis.ticks=element_blank(), panel.grid=element_blank())
                   ggsave(paste0("I:/projects/BLM/Production/type_specific/combined_exposure/historic_conus/charts/historic_exposure_map_t2_", typename, ".png"),
                          width=8, height=6)
                   
                   d$veg <- typename
                   d
             }

stopCluster(cl)
