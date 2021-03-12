dd <- stack(here("biovars/historic", historic.biovars)) 
names(dd) <- sort(paste0("bio", 1:19))

near.biovars<-list.files(here("biovars/near"), pattern=".tif")
near.biovars
dr <- stack(here("biovars/near", near.biovars)) 
names(dr) <- sort(paste0("bio", 1:19))


future.biovars<-list.files(here("biovars/future"), pattern=".tif")
future.biovars
df <- stack(here("biovars/future", future.biovars)) 
names(df) <- sort(paste0("bio", 1:19))



bio1_1970_2005<-seq(1, 570, by=19)

yearly.bio<-list.files("F:/Projects/CEMML/ClimateGrids/yearly_monthly/biovars_raster_by_year", pattern=".tif", full.names = T)
test.year<-stack(yearly.bio[c(bio1_1970_2005)])
names(test.year) <- sort(paste0("bio", 1:19, "_1977"))


years_bio1<-cellStats(test.year, stat='mean')

bio1_norm<-cellStats(dd[[1]], stat='mean')

bio1_rec<-cellStats(dr[[1]],stat='mean')
bio1_fut<-cellStats(df[[1]], stat='mean')

plot(jitter(years_bio1), ylim=c(10, 15))
abline(h=bio1_norm, col='blue')
abline(h=bio1_rec, col='orange')
abline(h=bio1_fut, col='red')
mean(years_bio1)
