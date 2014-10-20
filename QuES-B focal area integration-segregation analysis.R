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
remove(sumtab1,teci_zstat,habitat,lu1,mwfile,plotbio)

#load initial database
load(ldabase.final)
#rename initial variables
sumtab.final<-sumtab1
teci_zstat.final<-teci_zstat
foc.area.final<-habitat
lu.final<-lu1
mwfile.final<-mwfile
plotbio.final<-plotbio
remove(sumtab1,teci_zstat,habitat,lu1,mwfile,plotbio)


#Focal area decrement and increment
chk_decrement<-foc.area.init>foc.area.final
chk_increment<-foc.area.init<foc.area.final
foc.area.decrement<-(foc.area.init-foc.area.final)*chk_decrement
foc.area.increment<-(foc.area.final-foc.area.init)*chk_increment


#TECI increment and decrement except nodata
mwfile.init.chk<-mwfile.init
mwfile.final.chk<-mwfile.final

chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
chk_teci_increment<-mwfile.init.chk<mwfile.final.chk
mwfile.decrement<-(mwfile.init.chk-mwfile.final.chk)*chk_teci_decrement;#TECI value decrement
mwfile.increment<-(mwfile.final.chk-mwfile.init.chk)*chk_teci_increment;#TECI value increment


#TECI loss and gain in NA data
mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
mwfile.init.NA<-((mwfile.init.NA/999)==1)

mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
mwfile.final.NA<-((mwfile.final.NA/999)==1)

mwfile.loss.NA<-mwfile.init.chk*mwfile.final.NA;#TECI loss in NA area
mwfile.gain.NA<-mwfile.final.chk*mwfile.init.NA;#TECI loss in NA area



#FOCAL AREA SEGREGATION
mwfile.gain.noNA <- reclassify(mwfile.gain, cbind(NA, 0))
foc.area.seg<-mwfile.loss.NA+mwfile.gain.noNA


#FOCAL AREA INTEGRATION
mwfile.loss.noNA <- reclassify(mwfile.loss, cbind(NA, 0))
foc.area.int<-mwfile.gain.NA+mwfile.loss.noNA




NAmap.init<-mwfile.init.chk^0
setwd("C:/QUES_B_DJB/gainloss_test/")
writeRaster(chk_teci_loss,  filename="chk_teci_loss", format="GTiff", overwrite=TRUE)
writeRaster(chk_teci_gain,  filename="chk_teci_gain", format="GTiff", overwrite=TRUE)
writeRaster(mwfile.loss,  filename="mwfile_loss", format="GTiff", overwrite=TRUE)
writeRaster(mwfile.gain,  filename="mwfile_gain", format="GTiff", overwrite=TRUE)
writeRaster(foc.area.loss,  filename="foc_area_loss", format="GTiff", overwrite=TRUE)
writeRaster(foc.area.gain,  filename="foc_area_gain", format="GTiff", overwrite=TRUE)

writeRaster(foc.area.init,  filename="foc_area_init", format="GTiff", overwrite=TRUE)
writeRaster(foc.area.final,  filename="foc_area_final", format="GTiff", overwrite=TRUE)
foc.area.init


chk_teci_loss<-mwfile.init.1>mwfile.final.1
chk_teci_gain<-mwfile.init.1<mwfile.final.1
mwfile.loss<-(mwfile.init-mwfile.final)*chk_teci_loss
mwfile.gain<-(mwfile.final-mwfile.init)*chk_teci_gain





