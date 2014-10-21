#set library which will be used
library(raster)
library(rgdal)
library(SDMTools)
library(sp)
library(tiff)
library(foreign)
library(rgeos)
library(ggplot2)
library(pracma)
library(spatial.tools)

#data input
ldabase.initial<-"C:/QUES_B_DJB/Merangin_2000/QuESB_database_Merangin_2000.ldbase"
ldabase.final<-"C:/QUES_B_DJB/Merangin_2005/QuESB_database_Merangin_2005.ldbase"

#load initial database
load(ldabase.initial)
#rename initial variables
sumtab.init<-sumtab1
teci_zstat.init<-teci_zstat
foc.area.init<-habitat
lu.init<-lu1
mwfile.init<-mwfile
plotbio.init<-plotbio
foc.area.stats.init<-foc.area.stats
year.init<-year
AUC.init<-AUC2
remove(sumtab1,teci_zstat,habitat,lu1,mwfile,plotbio,foc.area.stats, year,AUC2)

#load initial database
load(ldabase.final)
#rename initial variables
sumtab.final<-sumtab1
teci_zstat.final<-teci_zstat
foc.area.final<-habitat
lu.final<-lu1
mwfile.final<-mwfile
plotbio.final<-plotbio
foc.area.stats.final<-foc.area.stats
year.final<-year
AUC.final<-AUC2
remove(sumtab1,teci_zstat,habitat,lu1,mwfile,plotbio,foc.area.stats, year,AUC2)


#Focal area decrement and increment
chk_decrement<-foc.area.init>foc.area.final
chk_increment<-foc.area.init<foc.area.final
foc.area.decrement<-(foc.area.init-foc.area.final)*chk_decrement;#integration increase
foc.area.increment<-(foc.area.final-foc.area.init)*chk_increment;#integration decrease


#TECI increment and decrement except nodata
mwfile.init.chk<-mwfile.init
mwfile.final.chk<-mwfile.final

chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
chk_teci_decrement <- reclassify(chk_teci_decrement, cbind(0,NA))
chk_teci_increment<-mwfile.init.chk<mwfile.final.chk
chk_teci_increment <- reclassify(chk_teci_increment, cbind(0,NA))
mwfile.decrement<-(mwfile.init.chk-mwfile.final.chk)*chk_teci_decrement;#TECI value decrement
mwfile.increment<-(mwfile.final.chk-mwfile.init.chk)*chk_teci_increment;#TECI value increment



#TECI loss and gain in NA data
mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
mwfile.init.NA<-((mwfile.init.NA/999)==1)

mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
mwfile.final.NA<-((mwfile.final.NA/999)==1)

mwfile.loss.NA<-mwfile.init.chk*mwfile.final.NA;#TECI loss in NA area
mwfile.gain.NA<-mwfile.final.chk*mwfile.init.NA;#TECI loss in NA area



#FOCAL AREA INTEGRATION
mwfile.gain.NA <- reclassify(mwfile.gain.NA, cbind(0, NA))
foc.area.int<-mosaic(mwfile.decrement, mwfile.gain.NA, fun="max")

#FOCAL AREA SEGREGATION
mwfile.loss.NA <- reclassify(mwfile.loss.NA, cbind(0, NA))
foc.area.seg<-mosaic(mwfile.increment, mwfile.loss.NA, fun="max")


setwd("C:/QUES_B_DJB/gainloss_test/")

writeRaster(mwfile.decrement,  filename="mwfile_decrement", format="GTiff", overwrite=TRUE)
writeRaster(mwfile.increment,  filename="mwfile_increment", format="GTiff", overwrite=TRUE)

writeRaster(mwfile.loss.NA,  filename="mwfile_loss", format="GTiff", overwrite=TRUE)
writeRaster(mwfile.gain.NA,  filename="mwfile_gain", format="GTiff", overwrite=TRUE)

writeRaster(foc.area.seg,  filename="foc_area_segregation", format="GTiff", overwrite=TRUE)
writeRaster(foc.area.int,  filename="foc_area_integration", format="GTiff", overwrite=TRUE)

writeRaster(foc.area.init,  filename="foc_area_init", format="GTiff", overwrite=TRUE)
writeRaster(foc.area.final,  filename="foc_area_final", format="GTiff", overwrite=TRUE)


#Combine patch statistics
#teci_zstat<-cbind(teci_zstat.init,teci_zstat.final)
#teci_zstat.init-teci_zstat.final

#Combine class STATS
foc.area.stats<-cbind(foc.area.stats.init,foc.area.stats.final)
col.init<-paste('class.stats.',year.init, sep='')
colnames(foc.area.stats)<-c(paste('class.stats.',year.init, sep=''),paste('class.stats.',year.final, sep=''))

#zonal stat for habitat integration and segregation
zstat.segregation<-ZonalStat(foc.area.seg, zone, FUN = "all") 
zstat.integration<-ZonalStat(foc.area.int, zone, FUN = "all")

#CONNECT with preques database
