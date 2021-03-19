
library(raster)
library(ecoclim)
library(ggplot2)


folder <- "I:/projects/BLM/Workspace/kling/indices/mahalanobis/5_biovars"

d <- as.data.frame(rasterToPoints(raster(paste0(folder, "/mahalanobis_typicality_5biovars_1950_1980_2013.grd"))))

states <- readRDS("I:/projects/BLM/Workspace/kling/shapefiles/western_states/western_states_fortified.rds")


# build plot
d <- d[d$x<(-96),]
p <- ggplot(d) +
      geom_raster(aes(x, y, fill=(layer))) +
      geom_path(data=states[states$piece==1,], aes(x=long, y=lat, group=id),
                color="white", fill=NA, size=.25) +
      scale_fill_gradientn(colours=c("black", "darkred", "red", "orange", "gray80"), 
                           #values=c(0, .75, .95, .99,  1),
                           na.value="white", limits=c(0, 1)) +
      whiteness() +
      theme(axis.text=eb(), axis.title=eb(), axis.ticks=eb(), strip.background=eb(),
            legend.position="top", legend.direction="horizontal",
            text=element_text(size=20)) +
      guides(fill=guide_colourbar(barwidth=20, barheight=1)) +
      labs(fill="mahalanobis typicality  ") +
      annotate(geom="text", x=-136, y=47.5, 
               label="Mahlanobis\ntypicality\non 5 biovars:\n1950-1979 vs.\n1980-2013", 
               hjust=0, vjust=1, size=12) +
      scale_x_continuous(limits=c(-137, max(d$x)),
                         expand=c(0,0)) +
      theme(plot.margin=grid::unit(rep(0, 4), "in"),
            panel.margin.x=grid::unit(0, "in"),
            legend.position=c(.4, .1)) +
      labs(title=NULL)

ggsave(paste0(folder, "/mahalanobis_biovars_1950_1980_2013.png"), p, width=12, height=9, units="in")


