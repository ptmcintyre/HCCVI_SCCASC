# FUTURE
#2021- this script is not updated from 2017 version, as it was not needed for the CEMML project

library(ecoclim)
library(raster)
library(ggplot2)
library(dismo)
library(doParallel)
library(dplyr)
library(tidyr)
library(caret)
library(mgcv)
library(randomForest)

extract <- raster::extract

# palettes from colorbrewer
#redblue <- c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac')
#browngreen <- c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','#c7eae5','#5ab4ac','#01665e')
orangepurple <- c('#b35806','#f1a340','#fee0b6','#f7f7f7','#d8daeb','#998ec3','#542788')

# load baseline climate data
dd <- parseMetadata("I:/climate_data/ClimateNA/historic/derived/normals/biovars/normals", pattern="1948_1980", drops="tif.")
dd <- stack(dd$path)
names(dd) <- sub("_1948_1980", "", names(dd))
dd <- subset(dd, paste0("bio", 1:19))
vars <- names(dd)

# load model predictions
preds <- list.files("I:/projects/BLM/Production/type_specific/niche_models/future/rasters_timeslice",
                    full.names=T)


# load veg data
paths <- parseMetadata("I:/projects/BLM/Production/type_specific/veg_distributions/ClimateNA_1km/rasters", pattern=".tif", drops="tif.")
veggies <- stack(paths)
names(veggies) <- sub(".tif", "", basename(paths))

# load variable importance data for all conus and cnmx types
imp <- rbind(read.csv("I:/projects/BLM/Production/type_specific/variable_selection/conus/variable_importance.csv", stringsAsFactors=F),
             read.csv("I:/projects/BLM/Production/type_specific/variable_selection/cnmx/variable_importance.csv", stringsAsFactors=F))
imp <- imp[imp$rank >= 14,]


outd <- "I:/projects/BLM/Production/type_specific/niche_models/future/charts"
outdrgb <- "I:/projects/BLM/Production/type_specific/niche_models/future/rasters_rgb"


# cluster stetup
cpus <- 8
cl <- makeCluster(cpus)
registerDoParallel(cl)

# loop
r <- foreach(type=names(veggies),
             .packages=c("viridis", "raster", "ecoclim", "dismo", "dplyr", "caret",
                         "randomForest", "ggplot2", "tidyr", "dplyr", "gridExtra", "grid")) %dopar% {
                               
                               #type <- names(veggies)[19]
                               outdir <- paste(outd, type, sep="/")
                               dir.create(outdir)
                               if(length(list.files(outdir)) == 15) return(NULL)
                               
                               pred <- stack(preds[grepl(type, preds)])
                               
                               # elevation data
                               elev <- raster("I:/projects/BLM/Workspace/auer/ClimateNA_processing/NA_Reference_files_ASCII/ClimateNA_clip.tif")
                               elev <- crop(elev, pred)
                               
                               # climate data
                               climate <- crop(dd, pred) 
                               names(climate) <- vars
                               
                               # prep state boundary data
                               states <- rgdal::readOGR("I:/projects/BLM/Workspace/kling/shapefiles", layer="US_states")
                               ext <- extent(pred)
                               ext <- as(ext, "SpatialPolygons")
                               proj4string(ext) <- CRS(proj4string(states))
                               states <- rgeos::gIntersection(states, ext, byid=T)
                               states.points <- fortify(states)
                               states.points$region <- "states"
                               states.points$polygon <- as.numeric(states.points$piece) + 1000
                               
                               ### restructure veg data
                               veg <- subset(veggies, type)
                               veg <- crop(veg, pred)
                               names(veg) <- "veg"
                               
                               veg_bin <- reclassify(veg, c(NA,NA,0,  0,100000,1), right=T)
                               
                               f <- as.data.frame(rasterToPoints(stack(veg_bin, pred, elev, climate)))
                               names(f) <- c("x", "y", "veg_binary", "pred_baseline", "pred_recent", "elevation", names(climate))
                               
                               f$pred_delta <- f$pred_recent - f$pred_baseline
                               
                               #f <- na.omit(f)
                               fp <- f[f$veg_binary == 1,]
                               
                               s <- f[sample(nrow(f), min(10000, nrow(f))),]
                               sp <- fp[sample(nrow(fp), min(10000, nrow(fp))),]
                               
                               typename <- gsub("_", " ", type)
                               
                               
                               
                               ############# colorwheel charts
                               fade <- "gray50"
                               left <- "white"
                               right <- "black"
                               top <- "purple"
                               bottom <- "orange"
                               
                               cwds <- cw(data=na.omit(s), xvar="pred_baseline", yvar="pred_delta",
                                          fade=fade, resolution=20, origin=c(.5, 0),
                                          left=left, right=right, top=top, bottom=bottom)
                               
                               p1 <- ggplot(cwds$data, aes(pred_baseline, pred_delta, alpha=distance, color=angle)) +
                                     axes("xy") +
                                     geom_point(color=fade, alpha=1) +
                                     geom_point() +
                                     cwds$scales +
                                     whiteness() +
                                     labs(x="baseline suitability",
                                          y="suitability change") +
                                     geom_abline(intercept=c(.5), slope=c(-1), linetype=5) +
                                     geom_abline(intercept=c(-.5), slope=c(1), linetype=5) +
                                     annotate(geom="text", 
                                              x=c(.8, .5, .2, .5), 
                                              y=c(0, .25, 0, -.25),
                                              label=c("stable", "leading", "unsuitable", "trailing"),
                                              size=10, fontface="bold")
                               
                               p1b <- cwds$legend_constrained +
                                     axes("xy") +
                                     whiteness() +
                                     labs(x="baseline suitability",
                                          y="suitability change") +
                                     geom_abline(intercept=c(.5), slope=c(-1), linetype=5) +
                                     geom_abline(intercept=c(-.5), slope=c(1), linetype=5) +
                                     annotate(geom="text", 
                                              x=c(.8, .5, .2, .5), 
                                              y=c(0, .25, 0, -.25),
                                              label=c("stable", "leading", "unsuitable", "trailing"),
                                              size=10, fontface="bold") +
                                     coord_fixed()
                               
                               cwdf <- cw(data=na.omit(f), xvar="pred_baseline", yvar="pred_delta",
                                          fade=fade, resolution=20, origin=c(.5, 0),,
                                          left=left, right=right, top=top, bottom=bottom)
                               
                               p2 <- ggplot() +
                                     geom_raster(data=cwdf$data, aes(x, y),fill=fade, alpha=1) +
                                     geom_raster(data=cwdf$data, aes(x, y, alpha=distance, fill=angle)) +
                                     geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                               color="white", fill=NA, size=.6) +
                                     cwdf$scales +
                                     theme(axis.title=eb(), axis.text=eb(), axis.ticks=eb()) +
                                     whiteness() +
                                     coord_fixed(ratio=1.2)
                               
                               
                               ### export rgb raster for mapping in ArcGIS
                               
                               getColors <- function(angle, distance, pal, center){
                                     angle <- angle / 360
                                     cl <- ceiling(angle * 4) + 1
                                     fl <- floor(angle * 4) + 1
                                     col <- matrix(NA, length(angle), 3)
                                     center <- as.vector(center)
                                     mx <- max(distance)
                                     for(i in 1:length(angle)){
                                           interp <- angle[i] * 4 - fl[i] + 1
                                           col_angle <- (as.vector(pal[,cl[i]]) * interp + 
                                                               as.vector(pal[,fl[i]]) * (1-interp))
                                           col[i,] <- col_angle * distance[i] / mx + center * (1 - distance[i]/mx)
                                     }
                                     return(col)
                               }
                               
                               clrs <- getColors(angle=cwdf$data$angle,
                                                 distance=cwdf$data$distance,
                                                 col2rgb(c(right, top, left, bottom, right)),
                                                 col2rgb(fade))
                               colors <- matrix(NA, nrow(f), 3)
                               colors[which(!is.na(rowMeans(f))),] <- clrs
                               colstack <- stackMatrix(colors, template=climate[[1]])
                               writeRaster(colstack, paste0(substr(paste0(outdrgb, "/future_start_change_RGB_", type), 1, 255), ".tif"), overwrite=T) 
                               #plotRGB(colstack)
                               
                               # png plot
                               main <- textGrob(paste0("Modeled bioclimate migration\n", gsub("_", " ", type)), gp=gpar(fontsize=30))
                               p <- arrangeGrob(p1b, p2, ncol=2, widths=c(.4, .6))
                               p <- arrangeGrob(main, p, ncol=1, heights=c(1, 10))
                               png(paste0(substr(paste0(outdir, "/future_colorwheel_combo_", type), 1, 255), ".png"),
                                   width=16, height=12, units="in", res=1000)
                               grid.draw(p)  
                               dev.off()
                               
                               
                               ######
                               
                               p <- ggplot(s, aes(pred_baseline, fill=factor(veg_binary))) + 
                                     geom_density(alpha=.4, color=NA) +
                                     theme(legend.position="top") +
                                     labs(title=paste0(typename, ":\nmodeled suitability for locations with and without type"))
                               #ggsave(paste0(substr(paste0(outdir, "/histogram_baseline_", type), 1, 255), ".png"), p, width=8, height=6) 
                               
                               p <- ggplot(s, aes(pred_baseline, pred_delta, color=factor(veg_binary))) + 
                                     axes("x") +
                                     geom_point(size=1, alpha=.5) + 
                                     scale_color_manual(values=c("red", "darkblue")) + ############################################
                               labs(title=paste0(typename, ":\nbaseline suitability vs suitability change"),
                                    color="current\noccurrence") +
                                     whiteness()
                               ggsave(paste0(substr(paste0(outdir, "/future_scatter_start_change_", type), 1, 255), ".png"), p, width=6, height=6) 
                               
                               p <- ggplot(f, aes(pred_delta, fill=factor(veg_binary, levels=c(1, 0)))) + 
                                     geom_histogram(position="stack", binwidth=.01) +
                                     axes("y") +
                                     labs(fill="presence",
                                          x="change in suitability",
                                          y="number of pixels",
                                          title=paste0(typename, ":\nmodeled suitability changes for locations with and without type"))
                               #ggsave(paste0(substr(paste0(outdir, "/histogram_deltas_", type), 1, 255), ".png"), p, width=8, height=6)       
                               
                               # side-by-side maps of actual vs predited presence
                               d <- na.omit(f) %>%
                                     select(x, y, veg_binary, pred_baseline) %>%
                                     mutate(actual = veg_binary, predicted = pred_baseline) %>%
                                     select(-veg_binary, -pred_baseline) %>%
                                     gather(variable, value, -x, -y)
                               p <- ggplot() +
                                     geom_raster(data=d, aes(x, y, fill=value)) +
                                     geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                               color="white", fill=NA, size=.6) +
                                     #scale_fill_gradientn(colours=c("gray80", "yellowgreen", "darkgreen"), limits=c(0, 1)) +
                                     scale_fill_gradientn(colours=rev(viridis_pal()(256))) + #############################################
                               facet_grid(.~variable) +
                                     labs(fill="presence probability  ",
                                          title=paste0(typename, ":\nactual vs. predicted presence")) +
                                     whiteness() +
                                     coord_fixed(ratio=1.2) +
                                     theme(legend.position="top", axis.title=eb(), axis.text=eb(), axis.ticks=eb())
                               ggsave(paste0(substr(paste0(outdir, "/future_map_actual_predicted_", type), 1, 255), ".png"), p, width=12, height=9)       
                               
                               # side-by-side maps of suitability change, in all or just presence pixels
                               d <- f %>% select(x, y, veg_binary, pred_delta)
                               d$extent <- "full"
                               d2 <- d
                               d2$extent <- "current distribution"
                               d <- rbind(d, d2[d2$veg_binary == 1,])
                               d <- na.omit(d)
                               mag <- max(abs(d$pred_delta))
                               p <- ggplot() +
                                     geom_raster(data=d, aes(x, y, fill=pred_delta)) +
                                     geom_path(data=states.points[states.points$piece==1,], aes(x=long, y=lat, group=id),
                                               color="white", fill=NA, size=.6) +
                                     #scale_fill_gradient2(low="red", high="limegreen", mid="gray80", midpoint=0) +
                                     scale_fill_gradientn(colours=orangepurple, limits=c(-mag, mag)) +
                                     facet_grid(.~extent) +
                                     labs(fill="change in suitability  ",
                                          title=paste0(typename, ":\nmodeled change in suitability")) +
                                     whiteness() +
                                     theme(legend.position="top", axis.title=eb(), axis.text=eb(), axis.ticks=eb()) +
                                     coord_fixed(ratio=1.2)
                               ggsave(paste0(substr(paste0(outdir, "/future_map_change_", type), 1, 255), ".png"), p, width=12, height=9)       
                               
                               # latitudinal and elevational gradients
                               d <- f %>%
                                     mutate(elevation = plyr::round_any(elevation, 200),
                                            latitude = plyr::round_any(y, 1)) %>%
                                     group_by(elevation, latitude, veg_binary) %>%
                                     summarize(pred_delta=mean(pred_delta),
                                               count=n())
                               
                               mag <- max(abs(d$pred_delta[d$veg_binary==1]))
                               p <- ggplot(d[d$veg_binary==1,], 
                                           aes(elevation, latitude, color=pred_delta, size=count)) + 
                                     geom_point() +
                                     #scale_color_gradient2(low="red", mid="gray70", high="green", midpoint=0) +
                                     scale_color_gradientn(colours=orangepurple, limits=c(-mag, mag)) +
                                     whiteness() +
                                     scale_size(range=c(3, 14)) +
                                     labs(color="mean\nsuitability\nchange", size="# pixels",
                                          title=paste0(typename, ":\nchange in suitability by elevation and latitude, within current range"))
                               ggsave(paste0(substr(paste0(outdir, "/future_dotplot_delta_", type), 1, 255), ".png"), p, width=8, height=6)  
                               
                               
                               #### pairwise scatterplots/heatmaps
                               for(cull in c(T)){
                                     
                                     ### baseline suitability vs actual presence/absence
                                     d <- pairsData(s[sample(nrow(s), min(1000, nrow(s))),], 
                                                    vars, 
                                                    c("pred_baseline", "veg_binary"))
                                     vars_used <- imp$var[imp$type == gsub("_", " ", type)]
                                     if(cull) d <- d[d$x_var %in% vars_used & d$y_var %in% vars_used,]
                                     d <- gather(d, variable, value, pred_baseline, veg_binary)
                                     d$variable <- as.character(d$variable)
                                     d$variable[d$variable=="pred_baseline"] <- "modeled"
                                     d$variable[d$variable!="modeled"] <- "actual"
                                     
                                     p <- ggplot(d, aes(x_value, y_value, color=value)) +
                                           geom_point(size=1) +
                                           facet_grid(y_var~x_var+variable, scales="free") +
                                           scale_color_gradient(low="black", high="limegreen") +
                                           whiteness() +
                                           theme(axis.title=eb(), legend.position="top") +
                                           labs(color="presence   \nprobability   ",
                                                title=paste0(typename, ":\nactual vs. modeled suitability in climate space"))
                                     #ggsave(paste0(substr(paste0(outdir, "/future_climate_space_scatter_probability_", "cull", cull, "_", type), 1, 255), ".png"), p, width=20, height=12)  
                                     
                                     ds <- na.omit(d) %>%
                                           group_by(x_var, y_var) %>%
                                           mutate(x_value=plyr::round_any(x_value, (max(x_value) - min(x_value)) / 10),
                                                  y_value=plyr::round_any(y_value, (max(y_value) - min(y_value)) / 10)) %>%
                                           group_by(x_var, y_var, x_value, y_value, variable) %>%
                                           summarise(value=mean(value))
                                     p <- ggplot(ds, aes(x_value, y_value, fill=value)) +
                                           geom_raster() +
                                           facet_grid(y_var~x_var+variable, scales="free") +
                                           #scale_fill_gradientn(colours=c("black", "limegreen", "limegreen"), values=c(0,.8, 1), limits=c(0,1)) +
                                           scale_fill_viridis(limits=c(0,1)) +
                                           whiteness() +
                                           theme(axis.title=eb(), strip.text=element_text(size=24), legend.position="top") +
                                           labs(fill="presence   \nprobability   ",
                                                title=paste0(typename, ":\nactual vs. modeled suitability in climate space"))
                                     ggsave(paste0(substr(paste0(outdir, "/future_climate_space_heatmap_probability_", "cull", cull, "_", type), 1, 255), ".png"), p, width=20, height=12)  
                                     
                                     
                                     #### change in suitability
                                     d <- pairsData(sp[sample(nrow(sp), min(1000, nrow(sp))),], 
                                                    vars, 
                                                    "pred_delta")
                                     
                                     vars_used <- imp$var[imp$type == gsub("_", " ", type)]
                                     if(cull) d <- d[d$x_var %in% vars_used &
                                                           d$y_var %in% vars_used,]
                                     
                                     p <- ggplot(d, aes(x_value, y_value, color=pred_delta)) + 
                                           geom_point(size=.5) + 
                                           facet_grid(y_var~x_var, scales="free") +
                                           scale_color_gradient2(low="red", high="limegreen", mid="gray70", midpoint=0) +
                                           whiteness() +
                                           theme(axis.title=eb()) +
                                           labs(color="suitability\nchange",
                                                title=paste0(typename, ":\nmodeled suitability shifts in climate space, within current range"))
                                     #ggsave(paste0(substr(paste0(outdir, "/future_climate_space_scatter_delta_", "cull", cull, "_", type), 1, 255), ".png"), p, width=15, height=15)  
                                     
                                     
                                     # and summarized into bins
                                     ds <- na.omit(d) %>%
                                           group_by(x_var, y_var) %>%
                                           mutate(x_value=plyr::round_any(x_value, (max(x_value) - min(x_value)) / 10),
                                                  y_value=plyr::round_any(y_value, (max(y_value) - min(y_value)) / 10)) %>%
                                           group_by(x_var, y_var, x_value, y_value) %>%
                                           summarise_each(funs="mean", pred_delta)
                                     
                                     mag <- max(abs(ds$pred_delta))
                                     p <- ggplot(ds, aes(x_value, y_value, fill=pred_delta)) + 
                                           geom_raster() +
                                           facet_grid(y_var~x_var, scales="free") +
                                           #scale_fill_gradient2(low="red", high="limegreen", mid="gray70", midpoint=0) +
                                           scale_fill_gradientn(colours=orangepurple, limits=c(-mag, mag)) +
                                           whiteness() +
                                           theme(title=element_text(size=25), strip.text=element_text(size=24), axis.title=eb()) +
                                           labs(fill="suitability\nchange", size="# pixels",
                                                title=paste0(typename, ":\nmodeled suitability change in climate space, current range"))
                                     ggsave(paste0(substr(paste0(outdir, "/future_climate_space_heatmap_delta_", "cull", cull, "_", type), 1, 255), ".png"), p, width=15, height=15)  
                                     
                               }
                               
                               
                               
                         }
stopCluster(cl)
