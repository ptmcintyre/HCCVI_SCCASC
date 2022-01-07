#library(raster, "C:/Users/patrick_mcintyre/Documents/R/win-library/3.5")
#require(exactextractr)
library(raster)
library(sf)
library(doParallel)
library(rasterDT)
library(sp)
library(plyr)
library(tidyverse)
library(reshape2)
library(here)
#detach("package:raster", unload=T)
#my.paths<-.libPaths()
#install.packages('raster',lib=my.paths[1])
#my.paths<-.libPaths()
#.libPaths(Raster)
#dev.off()

#find.package("raster")
#find.package("doParallel")
#tmp = foreach(j = 1:12) %dopar% {.libPaths()}



#read in BPS systems_clipped to analysis exten
list.files("F:/Projects/CEMML/NorthAmerican_Ecosystems_and_Macrogroups/")
#CEMML_systems_BPS<-raster("F:/Projects/CEMML/NorthAmerican_Ecosystems_and_Macrogroups/BPA_mask_int_clip.tif")
#CEMML_systems_BPS<-raster("F:/Projects/CEMML/EcosystemGrids/IVC_potv846_3sys_90_aea.tif") #clipped version
#cec_names <- crop(cec, veggies)

#read in a climate raster as a template for cropping vegetation systems
#historic.biovars<-list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/historic/bio1_MACA_CCSM4_Monthly_historical_1976-2005_metric.tif", pattern=".tif")
#historic.biovars
template<-raster("I:/projects/CEMML_DOD/CEMML_HCCVI/biovars/historic/bio1_MACA_CCSM4_Monthly_historical_1976-2005_metric.tif")

#Read in systems raster (here filtered to only have target systems), crop to extent of climate layers
#CEMML_systems_BPS<-raster(here("system_distributions/IVC_BPS_CEMML_v846", "IVC_BPS_CEMML_v846.tif")) #clipped version

#CEMML_systems_BPS<-raster("F:/Projects/CEMML/EcosystemGrids/IVC_potv846_3sys_clip.tif")
CEMML_systems_BPS<-raster("F:/Projects/CEMML/EcosystemGrids/NorthAmerica_IVC_Ecosys_pot_NatureServe_v846_wgs84.tif")
select_systems<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/system_distributions/CEMML_systems.csv", as.is=T)
select_systems<-select_systems[14,]
CEMML_systems_BPS<- crop(CEMML_systems_BPS, template)
CEMML_systems_BPS<-CEMML_systems_BPS%in%select_systems$NS.Map.Value.code

#read in climate layer to crop BPS distributions to
historic.biovars<-list.files(here("biovars/historic"), pattern=".tif")
historic.biovars
dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))
dd<-dd[[1]]
dd<-projectRaster(dd, crs=crs(CEMML_systems_BPS))

#crop BPS to climate(analysis) extent
#CEMML_systems_BPS<-crop(CEMML_systems_BPS, dd)
#CEMML_systems_BPS<-mask(CEMML_systems_BPS, dd)


#Read in simple dissolved CEC region polygon
#cec_st<-st_read("F:/Projects/CEMML/boundaries/NA_CEC_Diss_AEA_MACA.shp")

cec_st<-st_read("F:/Projects/CEMML/boundaries/NA_CEC_Diss_AEA_MACA_wgs84.shp")
cec_st<-as_Spatial(cec_st)
CEC_names<-as.character(unique(cec_st$NAME))
#crop 
cec_st<-crop(cec_st, CEMML_systems_BPS )
CEC_names<-as.character(unique(cec_st$NAME))
plot(cec_st)

detectCores()
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)

length(CEC_names)
#note PJM need to subset cemm_systems_bps to only focal sysems
i=1
system.time(results<- foreach(i=1:length(CEC_names)) %dopar% {
  .libPaths("C:/Users/patrick_mcintyre/Documents/R/win-library/3.5")
  library("raster")
  library("rasterDT")
  cec_sub<-subset(cec_st, cec_st$NAME==CEC_names[i])
  cec.mask<-raster::crop(CEMML_systems_BPS, cec_sub)
  cec.mask<-raster::mask(cec.mask, cec_sub)
  cec.freq<-as.data.frame(rasterDT::freqDT(cec.mask))
})
stopCluster(cl)


save(results, file="I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calc8_3_21.RData")
results

target_systems<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/system_distributions/CEMML_systems.csv", as.is=T)
target_systems<-target_systems[14,]
systems_table<-target_systems[,c(1:2)]
blank<-data.frame("blank", 0)
names(blank)<-names(systems_table)
systems_table<-rbind(blank, systems_table)
systems_table2<-systems_table
systems_table3<-systems_table[order(systems_table$NS.Map.Value.code),]
#
i=1
for (i in 1:length(results)){
  temp.df<-as.data.frame(results[[i]], as.is=T) 
  temp.df<-subset(temp.df, ID!="NA")
  colnames(temp.df)[2]<-CEC_names[i]
  my.table<-as.data.frame(merge(systems_table2, temp.df, by.x="NS.Map.Value.code", by.y="ID", all=T))
  my.table<-my.table[order(my.table$NS.Map.Value.code),]
  systems_table3[, 2+i]<-my.table[,2+1] 
  #systems_table3<-cbind(systems_table3, my.table[,2+i])
  colnames(systems_table3)[2+i]<-colnames(temp.df)[2]
}

library(dplyr)
systems_table3<- systems_table3%>% 
  mutate_at(vars(3:91), .funs = funs(. *(90*90)/1000000))


Melt_systmes<-melt(systems_table3, id=c("system_name", "NS.Map.Value.code"))    #transpose data with labels for type and name

melt_systems<-subset(Melt_systmes, !is.na(Melt_systmes$value))
melt_systems<-subset(melt_systems, melt_systems$system_name!="blank")


write.csv(melt_systems, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_km_DMI_oak_8242021.csv")

?mutate_at
####older



#vpu1<-raster("F:/Projects/FIA_west_NVC/NVC/vpu04_nvc.img")
NVC_table<-read.csv("I:/projects/NVC_west/US_200NVC/CSV_Data/LF_200NVC_05142020.csv", as.is=T)
Melt_Exposure<-melt(expMetrics, id=c("type", "NAME"))    #transpose data with labels for type and nameNVC_table<-NVC_table[, c("VALUE", "Group_Code", "Group")]


names(NVC_table)

NVC<-raster("I:/projects/NVC_west/US_200NVC/Grid/US_200NVC")
lvl4<-st_read("I:/projects/NVC_west/us_eco_l4_no_st.shp")
lv4<-as_Spatial(lvl4)
lv4_codes<-as.character(unique(lvl4$US_L4CODE))

unique(NVC)


#lvl4_codes_short<-lv4_codes[1:3]
#lvl4_sub<-subset(lvl4, lvl4$US_L4CODE==lv4_codes[2])

#lvl4.mask<-crop(NVC, lvl4_sub)
#lvl4.mask<-mask(lvl4.mask, lvl4_sub)
#plot(lvl4.mask)
#lvl4.freq<-as.data.frame(freqDT(lvl4.mask))
i=1
trys=1:2?foreach
i=1
detectCores()
cpus <- 12
cl <- makeCluster(cpus)
registerDoParallel(cl)

length(lv4_codes)
length(levels(lvl4$US_L4CODE))

system.time(results<- foreach(i=1:length(lv4_codes)) %dopar% {
  .libPaths("C:/Users/patrick_mcintyre/Documents/R/win-library/3.5")
  library("raster")
  library("rasterDT")
  lvl4_sub<-subset(lvl4, lvl4$US_L4CODE==lv4_codes[i])
  lvl4.mask<-raster::crop(NVC, lvl4_sub)
  lvl4.mask<-raster::mask(lvl4.mask, lvl4_sub)
  lvl4.freq<-as.data.frame(rasterDT::freqDT(lvl4.mask))
})
stopCluster(cl)
save(results, file="I:/projects/NVC_west/full results.RData")
#my.test<-load("I:/projects/NVC_west/full results.RData")


NVC_table2<-NVC_table
NVC_table3<-NVC_table[order(NVC_table$VALUE),]


for (i in 1:length(results)){
 temp.df<-as.data.frame(results[[i]], as.is=T) 
 temp.df<-subset(temp.df, ID!="NA")
 colnames(temp.df)[2]<-lv4_codes[i]
 my.table<-as.data.frame(merge(NVC_table2, temp.df, by.x="VALUE", by.y="ID", all=T))
 my.table<-my.table[order(my.table$VALUE),]
 NVC_table3[, 3+i]<-my.table[,3+1] 
 #NVC_table3<-cbind(NVC_table3, my.table[,3+i])
 colnames(NVC_table3)[3+i]<-colnames(temp.df)[2]
}

#write.csv(NVC_table3, file="I:/projects/NVC_west/NVC_group_ecoregion_11_13_2020_double_check.csv" )
dim(NVC_table3)
NVC_manip<-as.data.frame(NVC_table3)
NVC_manip[,4:970]<-NVC_manip[,4:970]*.09
write.csv(NVC_manip, file="I:/projects/NVC_west/NVC_group_ecoregion_11_12_2020_hectares_NARM_test.csv" )


NVC_manip2<-NVC_manip[,c(2, 4:970)]
names(NVC_manip2)

NVC3<-as_tibble(NVC_manip2)
NVC3<-NVC3 %>% group_by(Group_Code) %>% summarize_all(sum, na.rm=T)
NVC4<-as.data.frame(NVC3)
write.csv(NVC4,"I:/projects/NVC_west/NVC_group_ecoregion_11_12_2020_hectares_simple_nrm.csv" )
NVC_test_names<-as.data.frame(unique(NVC_table3[,2:3]))
my.merge<-merge(NVC_test_names, NVC4, by="Group_Code")


co


write.csv(my.merge, "I:/projects/NVC_west/NVC_group_ecoregion_11_12_2020_hectares_simple_combo_NRM3.csv")


#change colnames
full_code<-paste(lvl4$NA_L3CODE, lvl4$US_L4CODE, sep=".")
full_code<-(unique(full_code))
names(my.merge)
colnames(my.merge)[3:969]<-full_code
dim(my.merge)
length(full_code)

dis<-list()
my.dis<-vector()
k=1
for (k in 1:nrow(my.merge)){
  my.merge2<-my.merge[k,]
  for (i in 1:(ncol(my.merge2)-2)){
      if(isTRUE(my.merge2[,2+i]>10)==TRUE) {
      dis[i+2]<- paste(colnames(my.merge2)[2+i],":P", sep="")
    } else if(isTRUE(my.merge2[,2+i]>0 & my.merge2[,2+i]<10)==TRUE) {
      dis[i+2]<- paste(colnames(my.merge2)[2+i],":?", sep="")
    } 
    else{dis[i+2]<-NA
    }
  }
my.dist<-c(unlist(dis))
my.dist<-my.dist[!is.na(my.dist)]
my.dis[k]<-paste(my.dist, sep="", collapse=", ")  
}

my.dis[1:2]
str(my.merge)
gc()
my.summary<-cbind(my.merge[,1:2], my.dis)

write.csv(my.summary,"I:/projects/NVC_west/NVC_groups_by_ecoregion_11_13_2020_from_landfire2020_long_code.csv" )
levels(group_code)
### extra junk
names(my.sub)
my.dist<-c(unlist(dis))
my.dist<-my.dist[!is.na(my.dist)]

paste(my.dist, sep="", collapse=", ") 


str(my.dist)

head(my.merge)

names(NVC4)
extent(lvl4_sub)
i=3
lvl4_sub<-subset(lvl4, lvl4$US_L4CODE==lv4_codes[i])
extent(lvl4_sub)

lvl4.mask


?dopar
?crop

test2<-lvl4_sub[,1]

test<-raster::mask(lvl4.mask, lvl4_sub[,1])

plot(lvl4_sub[,1])

#trials <- 1000
#iris_subset <- iris[iris$Species != 'setosa',]
#result <- foreach(1:trials) %dopar% {
#  model <- glm(Species ~., data = iris_subset, family = 'binomial')
#}

plot(lvl4_sub[,1])


plot(NVC)

list.files("I:/projects/NVC_west/US_200NVC/Grid/")


vpu2<-vpu1
vpu2[!vpu2==6018]<-0

plot(vpu1[vpu1==6018])

hex<- st_read(dsn="F:/Projects/KBAs/KBA_Canada/Data/Analyis/KBA_analysis_exp.gdb", layer="Hex100km_KBA_exp")
ecoregions<- st_read("F:/Projects/FIA_west_NVC/us_eco_l3/us_eco_l3.shp")

eco_vpu1<-exact_extract(vpu1, eco2)

eco2<-subset(ecoregions, ecoregions$US_L3CODE==18)

eco2<-eco2[,1]

?crop
plot(eco2)
plot(eco2, add=T)
vpu1.mask<-crop(vpu1, extent(eco2))

my.freq<-freqDT(vpu1.mask)

vpu1.mask<-crop(NVC, extent(eco2))
vpu1.mas2<-mask(vpu1.mask, eco2)
my.freq<-
  
 v3<- as.data.frame(my.freq)


plot(vpu1.mask)
plot(vpu1.mas2)

freq(vpu1.mas2)

v1<-getValues(vpu1.mas2)
table(v1)

extent(vpu1)
extent(eco2)

hex_EVT_count<-exact_extract(vpu1[vpu1==6061], hex, 'count')
hex_EVT_count<-exact_extract(vpu1, hex, 'count')
str(ecoregions)

?exact

length(hex_EVT_count)

table(hex_EVT_count)

hist(hex_EVT_count)
plot(hex)
gc()


hex_EVT_count2<-exact_extract(vpu1, hex)

plot(vpu1)
plotdev.off()

plot(hex, add=T)

?exact_extract


str(hex_EVT_count2)
max(hex_EVT_count2)
unlist(hex)