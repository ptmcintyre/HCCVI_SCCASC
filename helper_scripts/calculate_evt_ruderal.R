# evt_lookup<-read.csv("F:/Projects/CEMML/analysis/EVT_2020_lookup_2.csv", as.is=T)
# 
# evt_lookup$reclass[evt_lookup$Naturalnes!="Ruderal"]<-0
# evt_lookup$reclass[evt_lookup$Naturalnes=="Ruderal"]<-1
# 
# evt_reclass<-as.data.frame(cbind(evt_lookup$Value, evt_lookup$reclass))
# names(evt_reclass)<-c("is", "becomes")

#evt_lookup<-subset(evt_lookup, Naturalnes=="Ruderal")
#ruderal_vals<-unique(evt_lookup$Value)


#evt_2020[!(evt_2020 %in% ruderal_values)]<-0

            
evt_ruderal<-raster("F:/Projects/CEMML/analysis/reclass_evt_ruderal_zeros.tif")
#evt_ruderal[evt_ruderal>1]<-0
#plot(evt_ruderal)

NAvalue(evt_ruderal)
maca_poly<-st_read("F:/Projects/CEMML/ClimateGrids/MACA_CCSM4_Monthly_CONUS_Standard_Poly.shp")

historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
template<-raster(here("biovars/historic", historic.biovars[1]))
template[template<30]<-0
plot(template)
template <- reclassify(template, c(-Inf, Inf, NA))


my.zone<-exact_extract(evt_ruderal, maca_poly, 'sum')
maca_poly$ruderal<-my.zone
my.rast<-fasterizeDT(maca_poly, template, fun='sum',field="ruderal" )
writeRaster(my.rast, filename="F:/Projects/CEMML/analysis/EVT_2020_Ruderal.tif", overwrite=T)

#evt_ruderal<-reclassify(evt_2020, evt_reclass)

#writeRaster(template, filename="F:/Projects/CEMML/analysis/4km_raster_template.tif", overwrite=T)
