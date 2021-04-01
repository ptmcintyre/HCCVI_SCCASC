# August 10, 2018, Patrick_McIntyre@natureserve.org
# This script manipulates a csv file on future climate exposre metrics into 
# a format for a MS Access database managed by Mary Harkness and a
# Format for visual review in MS Excel
# The script also adds in additional information such as vegetation type code and area of extent


# Install libraries if not present
required_lib =c('reshape', 'shapefiles', 'rgdal', 'sp', 'dplyr', 'tidyr');
install_required_libs<-function(){
  for(i in 1:length(required_lib)){
    if(required_lib[i] %in% rownames(installed.packages()) == FALSE)
    {install.packages(required_lib[i], repos= "https://cran.cnr.berkeley.edu/")}
  }
}
install_required_libs()
lapply(required_lib, require, character.only=T)

time.period = "fut" # chose "near" or "fut"
scenario=45 # choose 45 or 85
Time_Frame<- 2 #choose 1 for near or 2 for future

#read in csv file with exposure infomration
#expMetrics<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/biovar_summaries/allveg_biovar_summaries_bl_near45.csv", as.is=T)
expMetrics<-read.csv(paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/biovar_summaries/allveg_biovar_summaries_bl_",
                     time.period, scenario, ".csv"), as.is=T)



expMetrics<-subset(expMetrics, select=-c(1,2))


#manipulate and cleanup data into format for HCCVI Access db
Melt_Exposure<-melt(expMetrics, id=c("type", "NAME"))    #transpose data with labels for type and name
Melt_Exposure$variable<-sub("m", "_m", Melt_Exposure$variable)
Melt_Exposure$variable<-sub("__", "_", Melt_Exposure$variable)
Melt_Exposure$variable<-sub("_", "&", Melt_Exposure$variable)
Melt_Exposure <-  separate(Melt_Exposure, variable, c("BioClimVar", "Statistic"),"&") # split variable column into two columns at location of & in the string
Melt_Exposure$Statistic<-sub("_", " ", Melt_Exposure$Statistic)  # replace _ with a blank space

cross_walk<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/crosswalk_type_system.csv")
Melt_Exposure<-merge(Melt_Exposure, cross_walk, by="type")

#Read in tables with system and ecoregion codes, exported from db (could also connect to DB, and read that way)
systems<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/d_Ecological Systems.csv", as.is=T)
systems<-unique(systems[,c("ELEMENT_GLOBAL_ID", "ELCODE", "ESLF_CD",  "GLOBAL_NAME")])
colnames(systems)[4]<-"system_name"

Exposure_systems<-merge(Melt_Exposure, systems, by="system_name", all.x=F)
colnames(Exposure_systems)[colnames(Exposure_systems)=="NAME"]<-"Ecoreg_Name"


regions<-read.csv("D:Tosh/data/HCCVI/d_CEC_Ecoregions.csv", as.is=T)
regions<-unique(regions[,c("Level3", "Ecoreg_Name")])
Exposure_regions<-merge(Exposure_systems, regions, by="Ecoreg_Name", all.x=F)

colnames(Exposure_regions)[colnames(Exposure_regions)=="Level3"]<- "CEC ID"
colnames(Exposure_regions)[colnames(Exposure_regions)=="ELEMENT_GLOBAL_ID.x"]<- "Sys ID"
colnames(Exposure_regions)[colnames(Exposure_regions)=="value"]<- "Value"

Exposure_table<-Exposure_regions[,c("Sys ID", "CEC ID", "BioClimVar", "Statistic", "Value")]
Exposure_table$Time_Frame_ID<-Time_Frame

Exposure_table$ScoreDate<-"3/28/2021"
Exposure_table$SourceData<-"MACA_45_CEMML"



Exposure_table<-Exposure_table[,c("Sys ID", "CEC ID", "BioClimVar","Time_Frame_ID",  "Statistic", "Value", "SourceData", "ScoreDate")]
Exposure_table$Value[Exposure_table$Value=="NaN"]<-"NA"
Exposure_table$Value<-as.numeric(Exposure_table$Value)
Exposure_table2<-na.omit(Exposure_table)
#write.csv(Exposure_table2, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/draft_exposure_output_future_with_more.csv", row.names = F)

#WORKS THROUGH HERE

#write.csv(Exposure_regions, "C:/temp/draft_exposure_output_future_extradata2.csv", row.names = F)



####filtering by 50 km
#skm50<- read.csv("C:/data/HCCVI/System Area50sqkm.csv") old (only original 30s)

skm50<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_system_km_maca.csv")
skm50<-subset(skm50, skm50$SqKM_system_ecoregion>=50)


colnames(skm50)[colnames(skm50)=="ELEMENT_GLOBAL_ID"]<- "Sys ID"
colnames(skm50)[colnames(skm50)=="Level3"]<- "CEC ID"
colnames(Exposure_table2)

exposure_50km<-merge(Exposure_table2, skm50, by=c("Sys ID", "CEC ID"), all.x=F)
#exposure_50km$Percent_of_Total_Distribution<-exposure_50km$BPS_SqKm/exposure_50km$System_BPS_SqKm


percent_areas<-subset(exposure_50km, Statistic=="percent area")
percent_areas<-percent_areas[,c("CEC ID", "Sys ID", "Statistic", "BioClimVar" ,"Value")]
colnames(percent_areas)[colnames(percent_areas)=="Value"]<- "Percent_ecoregion_affected"

exposure50km2<-merge(exposure_50km, percent_areas, by= c("Sys ID", "CEC ID", "BioClimVar"))
exposure50km3<-unique(exposure50km2)
exposure50km3<-subset(exposure50km3, Statistic.x!="percent area")

bioclim<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/Bioclim_names.csv")

exposure50km3<-merge (exposure50km3, bioclim, by="BioClimVar")


#write.csv(exposure50km3, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/exposure_50km_bl_near45_all.csv")
write.csv(exposure50km3, paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/exposure_50km_bl_", time.period,
           scenario, "_all.csv"), row.names=F)


my.data<-read.csv(paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/exposure_50km_bl_", time.period,
                         scenario, "_all.csv"))

#my.cast<-cast(my.data, system + CEC.ecoregion + BioClimVar + Time_Frame_ID +Area_SqKm + Percent_ecoregion_affected +SqKm_ecoregion_affected 
#              +Area_SqMi + Percent.of.Total.Distribution + eslf + cec_ID + Sys.ID + CEC.ID  +ScoreDate ~ Statistic, value="Value")

#my.cast$sqkm_of_system<-my.cast$Area_SqKm/ (my.cast$Percent.of.Total.Distribution/100)
colnames(my.data)
colnames(my.data)[colnames(my.data)=="system_name"]<- "system"
colnames(my.data)[colnames(my.data)=="ecoregion"]<- "CEC.ecoregion"
colnames(my.data)[colnames(my.data)=="SqKM_system_ecoregion"]<- "Area_SqKm"
colnames(my.data)[colnames(my.data)=="ELSF_CD"]<- "eslf"
colnames(my.data)[colnames(my.data)=="CEC.ID"]<- "cec_ID"
colnames(my.data)[colnames(my.data)=="BPS_area_system_sq_KM"]<- "Total_SqKM"

my.data$SqKm_ecoregion_affected<-my.data$Percent_ecoregion_affected*my.data$Area_SqKm
my.data$Percent.of.Total.Distribution<-my.data$Area_SqKm/my.data$Total_SqKM


my.cast<-cast(my.data, Time_Frame_ID + system + CEC.ecoregion + Bioclim.Variable + Units +Area_SqKm + Percent_ecoregion_affected +SqKm_ecoregion_affected 
              + Percent.of.Total.Distribution + eslf + cec_ID + Sys.ID + ScoreDate + Total_SqKM~ Statistic.x, value="Value")

colnames(my.cast)[colnames(my.cast)=="mean"]<-"mean(baseline)"
colnames(my.cast)[colnames(my.cast)=="Area_SqKm"]<-"SqKM_Ecoreg_system"

my.cast$Percent_of_Total_Distribution<- round(my.cast$Percent.of.Total.Distribution*100, 2)
my.cast$Percent_ecoregion_affected<-round(my.cast$Percent_ecoregion_affected, 2)
my.cast$Scenario<-paste0("rmp_", scenario)
my.cast$Percent_system_affected<-my.cast$SqKM_Ecoreg_system/my.cast$Total_SqKM
colnames(my.cast)[colnames(my.cast)=="Total_SqKM"]<-"Total_SqKM_system_BPS"

  
my.cast2<- my.cast [ , c("Time_Frame_ID", "Scenario", "system", "CEC.ecoregion", "Bioclim.Variable", "Units",  "mean zscore",
                        "mean(baseline)", "mean delta", "min delta", "max delta", "Percent_ecoregion_affected",
                        "SqKM_Ecoreg_system", "SqKm_ecoregion_affected", "Percent_of_Total_Distribution",
                        "Percent_system_affected", "Total_SqKM_system_BPS", "Sys.ID", "cec_ID", "ScoreDate"  )]


write.csv(my.cast2, paste0("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/final_table_bl_", time.period, scenario, ".csv"), row.names = F)

# 
# 
# ?trunc
# #my.cast$sqkm_of_system<-my.cast$Area_SqKm/my.cast$Percent.of.Total.Distribution
# 
# m
# 
# 
# 
# str(bioclim)
# 
# my.cast<-merge(my.cast, bioclim, by="BioClimVar")
# names (my.data)
# 
# 
# my.areas<-read.csv("C:/data/HCCVI/temp_ESLF_system_areas.csv")
# 
# names(my.areas)
# 
# 
# colnames(my.areas)[colnames(my.areas)=="System_ID"]<- "Sys.ID"
# 
# my.areas<-my.areas[, c(1,3)]
# 
# my.cast2<-merge(my.cast, my.areas, by="Sys.ID")
# 
# write.csv(my.cast2, "C:/data/HCCVI/exposure_50km_futures_PJM.csv")
# 
# my.base<-read.csv("D:/Tosh/data/HCCVI/Bioclimate Variable Statistics.csv")
# 
# 
# 
# my.base<-my.base[, c(1,2,3,5,6)]
# my.base<-subset(my.base, Statistic=="baseline")
# my.base2<-my.base
# my.base2<-my.base2[, c(1,2,3,5)]
# colnames(my.base2)[colnames(my.base2)=="Value"]<- "Baseline"
# 
# my.merge3<-merge(my.cast2, my.base2, by=c("Sys.ID" ,    "CEC.ID"  ,   "BioClimVar"), all.x=T)
# write.csv(my.merge3,"C:/data/HCCVI/exposure_50km_futures_PJM3.csv")
# 
# 
# 
# bio_vars<-read.csv ("C:/data/HCCVI/d_bioclimate_variables.csv")
# bio_vars<-bio_vars[,3:4]
# colnames(bio_vars)[colnames(bio_vars)=="BioVarCode"]<- "BioClimVar"
# 
# units<-read.csv("C:/data/HCCVI/exposure_50km_futures_PJM3.csv")
# units2<-merge(units, bio_vars, by="BioClimVar")
# 
# write.csv(units2, "C:/data/HCCVI/exposure_50km_futures_units_all.csv")
# 
# 
# 
# 
# 
# 
# 
# #creating table with totals by system for MACA area
# #cec_areas<-read.csv("D:/Tosh/data/HCCVI/CEC_area_table2.csv")
# #
# # sys_cecARea<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_system_km_maca.csv")
# # my.table2<-aggregate(SqKM_system_ecoregion ~ system_name, sys_cecARea, sum)
# # #write.csv(my.table2,"I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp_total_area_system.csv")
# 
# 
# 
# 
# # ###ASSORTED MERGING
# # ###merging in EcoRegion NAme
# # ecoregions<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/d_CEC_Ecoregions.csv")
# # area_ecoregions<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_regions_areaKM_MACA.csv")
# 
# my.merge<-merge(area_ecoregions, ecoregions, by.x="NAME", by.y="Ecoreg_Name", complete.cases=T)
# my.merge<-as.data.frame(my.merge)
# 
# write.csv(my.merge, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_regions_areaKM_MACA_full.csv")
# 
# 
# 
# my.table2<-aggregate(area_km ~ LEVEL3, cec_areas, sum)
# colnames(my.table2)[colnames(my.table2)=="LEVEL3"]<- "CEC.ID"
# colnames(my.table2)[2]<- "Size_Ecoregion_SqKm"
# 
# 
# units3<-merge(units2, my.table2, by="CEC.ID")
# 
# 
# my.agg2<-units3[ ,c("area_Km", "CEC.ID")]
# my.agg2<-unique(my.agg2)
# my.agg3<-aggregate(Area_SqKm_ecoreg_in_system ~ CEC.ID, my.agg2, sum)
# write.csv(units3, "C:/data/HCCVI/exposure_50km_futures_units_CEC_area.csv")
# 
# systems<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/crosswalk_type_system.csv")
# area_ecoregions<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_system_km_maca.csv")
# 
# my.merge2<-merge(area_ecoregions, systems, by.x="NS.Map.Value.code", by.y="NS.Map.Value.code")
# my.merge2$percent_of_Total_Distribution<-my.merge2$SqKM_system_ecoregion/my.merge2$BPS_area_system_sq_KM
# 
# write.csv(my.merge2, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/CEC_system_km_maca_with_totals.csv")
# 
# 
# area_ecoregions<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_system_km_maca.csv")
# 
# ecoregions<-read.csv("I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/CEC_system_calcs/CEC_regions_areaKM_MACA.csv")
# ecoregions<-subset(ecoregions, select=c("NAME", "Level3"))
# 
# my.merge3<-merge(area_ecoregions, ecoregions, by.x="ecoregion", by.y="NAME")
# write.csv(my.merge3, "I:/projects/CEMML_DOD/CEMML_HCCVI/summary_table/temp/CEC_system_km_maca_with_lvl3.csv")
