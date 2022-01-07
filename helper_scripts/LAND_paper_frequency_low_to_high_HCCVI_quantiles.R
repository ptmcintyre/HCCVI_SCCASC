## Red List of Ecosystem Statistics for Canadian KBA analysis 
require(rgdal)
require(sf)
library("arcgisbinding", lib.loc="T:/R/win-library/3.5")
library(rgeos)
library("reshape2")
arc.check_product()

mygdb<-"F:/Projects/CEMML/analysis/Scorecards_CEMML_land.gdb"  #geodatabase with target data
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(mygdb) #list of Feature Classes in geodatabase
fc_list

template_score<-st_read(dsn="F:/Projects/CEMML/analysis/Scorecards_CEMML_land.gdb", layer=fc_list[1])
variables<-names(template_score)
variables<- variables[c(3, 4,5,6,7,8,9,10,11,12,16,20,24, 28)]
variables
length(variables)

breaks<-c(0, 0.25, 0.5, 0.75, 1)

low2high<-function (x){
  table(cut(na.omit(x), breaks=breaks))/length(na.omit(x))
}
low2high(grid$AdaptCapC)
i=1
j =1
test_per<-list()
my.list<-list()
Name<-vector()

for (i in 1:length(fc_list)) {
  grid<- st_read(dsn="F:/Projects/CEMML/analysis/Scorecards_CEMML_land.gdb", layer=fc_list[i])
  grid$ExposureNr_45
  na.omit(grid)
  Name[i]<-fc_list[i]
 for(j in 1:14){
  var_name<-paste0("grid$",variables[j])
  variable<-eval(parse(text=var_name))
  if(length(variable)==1){
    test_per[[j]]<-NA
    names(test_per)[j]<-variables[j]
  }
  else {
    test_per[[j]]<-low2high(variable)
   names(test_per)[j]<-variables[j]
 }
 }
my.list[[i]]<-as.data.frame.matrix(do.call(rbind, test_per))
names(my.list)[i]<-Name[i]
}




my.temp<-as.data.frame(my.list[[1]])
names(my.temp)<-c("very high", "high","moderate","low")
my.temp$type<-row.names(my.temp)
my.temp<-dcast(melt(my.temp, id.var="type"), 1~variable+type)  
my.temp$system<-NA

my.df3<-as.data.frame(matrix(NA, 0, 58))
colnames(my.df3)<-colnames(my.temp)
for(i in 1:length(my.list)){
  my.df<-as.data.frame(my.list[[i]])
  names(my.df)<-c("very high", "high","moderate","low")
  my.df$type<-row.names(my.df)
  my.df2<-dcast(melt(my.df, id.var="type"), 1~variable+type)  
  my.df2$system<-names(my.list)[i]
 #my.df3[i,]<-rbind(my.df3, my.df2)
  
  my.df3[i,]<- my.df2
}
my.df3

my.df4<-my.df3[, c("system", "low_HCCVI_Fut_85"  , "moderate_HCCVI_Fut_85", "high_HCCVI_Fut_85", "very high_HCCVI_Fut_85",
                   "low_ExposureFut_85" , "moderate_ExposureFut_85", "high_ExposureFut_85", "very high_ExposureFut_85",
                   "low_ResilienceC" ,  "moderate_ResilienceC","high_ResilienceC" ,"very high_ResilienceC",
                   "low_AdaptCapC"  , "moderate_AdaptCapC" , "high_AdaptCapC", "very high_AdaptCapC",
                   "low_TRI" , "moderate_TRI", "high_TRI", "very high_TRI",
                   "low_FunGroup"  , "moderate_FunGroup", "high_FunGroup" , "very high_FunGroup" ,
                   "low_Keystone" , "moderate_Keystone" ,   "high_Keystone"  ,    "very high_Keystone" ,
                   "low_SensitivityC", "moderate_SensitivityC" , "high_SensitivityC", "very high_SensitivityC" ,
                   "low_ConditionC", "moderate_ConditionC", "high_ConditionC", "very high_ConditionC",
                   "low_InvasiveC" ,"moderate_InvasiveC"  , "high_InvasiveC", "very high_InvasiveC" ,
                   "low_LDLoss" , "moderate_LDLoss" ,    "high_LDLoss", "very high_LDLoss" ,
                   "low_Fire_VDep",  "moderate_Fire_VDep" ,  "high_Fire_VDep", "very high_Fire_VDep"
                   )]
               

write.csv(my.df4, "I:/projects/CEMML_DOD/CEMML_HCCVI/data_products/table_land_8_21_2021.csv")

