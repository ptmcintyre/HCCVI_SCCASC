### This script adds a column of the veg type name to every csv file in a folder
### The csvs in "biovar_summaries" were generated from this script: I:\projects\BLM\Workspace\auer\summary_table\exposure_summary_table_simple_FUTURE.R

#files <- list.files("I:/projects/BLM/Workspace/harkness/exposure_summary_tables/topoWX/biovar_summaries", full.names=TRUE)
files <- list.files("I:/projects/CEMML_DOD/CEMML_HCCVI/biovar_summaries/base/bl_near45", full.names=TRUE)

### add column

for (f in files){
  table <- read.csv(f,stringsAsFactors=F)
  type <- sub("_biovar_summary_bl_fut45.csv", "", basename(f))
  type <- gsub("_", " ", type, fixed=TRUE) 
  table$type <- type
  table <- subset(table, select=c(type, NAME:bio19_percent_area))
  write.csv(table, file=f)
  
}

### load csv files and combine them into one table using rbind ###

dataframe <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
dataframe <- dataframe[!is.na(dataframe$NAME),]

write.csv(dataframe, file="I:/projects/CEMML_DOD/CEMML_HCCVI/biovar_summaries/base/BASE_allveg_biovar_summaries_bl_near45.csv")


##### get variable importance for all types
var_imp_conus <- read.csv("I:/projects/BLM/Production/type_specific/variable_selection/conus/variable_importance.csv")
var_imp_cnmx <- read.csv("I:/projects/BLM/Production/type_specific/variable_selection/cnmx/variable_importance.csv")
var_imp_addons <- read.csv("I:/projects/BLM/Production/Add_ons/variable_selection/variable_importance.csv")

var_imp <- rbind(var_imp_conus, var_imp_cnmx, var_imp_addons)
write.csv(var_imp, "I:/projects/BLM/Workspace/harkness/exposure_summary_tables/future/variable_importance_52types.csv")