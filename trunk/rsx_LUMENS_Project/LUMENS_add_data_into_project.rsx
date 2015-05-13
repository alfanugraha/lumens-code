##[LUMENS]=group
##passfilenames
##project=file
##data=file
##data_type=number 1
##period=number 1990

library(sp)
library(raster)
library(rgdal)
library(tiff)
library(spatial.tools)

#READ PROJECT DESCRIPTION FILE AND IDENTIFY RDATA
proj_desc<-read.table(project, header = TRUE, sep = "")
db_name<- substr(basename(project), 1, nchar(basename(project)) - 4)
db_name<-paste(db_name,".lpd", sep="")
working_directory<-as.character(proj_desc[1,])
setwd(working_directory)

#LOAD LUMENS DATABASE
lumens_database<-list.files (path=working_directory, pattern="lpd")

#LOAD RDATA INTO LAZZYLOAD DATABASE
load(lumens_database)

#LOADING DATA INTO LUMENS DATABASE-READ USER INPUT
data.input<-cbind(data,data_type,period)

#CREATE LOOkup TABLE OF DATA TYPE
lut.data.type<-c(1,2,3,4,5,6)
lut.data.desc<-c("landuse", "pu","rec_pu", "lut.c", "lut.lc", "lut.pu")
lut.data<-cbind(lut.data.type,lut.data.desc)
colnames(lut.data)[1]<-"data_type"

#CREATE RESAVE FUNCTION
resave <- function(..., list = character(), file) {
previous  <- load(file)
var.names <- c(list, as.character(substitute(list(...)))[-1L])
for (var in var.names) assign(var, get(var, envir = parent.frame()))
save(list = unique(c(previous, var.names)), file = file)
}

#IMPORTING DATA INTO LUMENS DATABASE
data.input<-merge(data.input,lut.data, by="data_type")
data_type_key<-as.numeric(levels(data.input[1,1]))[data.input[1,1]]
if (data_type_key==1) {
command="raster"
data_name<-as.character(data.input[1,4])
landuse.index<-landuse.index+1
period.index<-period.index+1
eval(parse(text=(paste(data_name,"_t", landuse.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_t", landuse.index, "<-spatial_sync_raster(",data_name,"_t", landuse.index, ',', 'ref, method = "ngb")', sep=""))))
eval(parse(text=(paste(data_name,"_t", landuse.index, "<-", data_name,"_t", landuse.index, "*1",  sep=""))))
eval(parse(text=(paste("freq_",data_name,"_t", landuse.index, "<-as.data.frame(na.omit(freq(", data_name,"_t", landuse.index, ")))",  sep=""))))
data_name<-paste(data_name, "_t", landuse.index, sep="")
eval(parse(text=(paste("resave(", data_name, ",landuse.index, file=lumens_database)", sep=""))))
eval(parse(text=(paste("resave(freq_", data_name, ",landuse.index, file=lumens_database)", sep=""))))
period_i<-paste("period", period.index, sep="")
eval(parse(text=(paste(period_i, "<-period", sep="" ))))
eval(parse(text=(paste("resave(", period_i, ",period.index, file=lumens_database)", sep=""))))
} else {
if (data_type_key==2){
command="raster"
data_name<-as.character(data.input[1,4])
pu.index<-pu.index+1
eval(parse(text=(paste(data_name,"_pu", pu.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_pu", pu.index, "<-spatial_sync_raster(",data_name,"_pu", pu.index, ',', 'ref, method = "ngb")', sep=""))))
data_name<-paste(data_name, "_pu", pu.index, sep="")
eval(parse(text=(paste("resave(", data_name, ",pu.index, file=lumens_database)", sep=""))))
} else {
if (data_type_key==3) {
command="raster"
data_name<-as.character(data.input[1,4])
pu_rec.index<-pu_rec.index+1
eval(parse(text=(paste(data_name,"_pu_rec", pu_rec.index, "<-", command,'("', data, '")', sep=""))))
eval(parse(text=(paste(data_name,"_pu_rec", pu_rec.index, "<-spatial_sync_raster(",data_name,"_pu_rec", pu_rec.index, ',', 'ref, method = "ngb")', sep=""))))
data_name<-paste(data_name, "_pu_rec", pu_rec.index, sep="")
eval(parse(text=(paste("resave(", data_name, ",pu_rec.index, file=lumens_database)", sep=""))))
} else {
if (data_type_key>3) {
command="read.table"
data_name<-as.character(data.input[1,4])
eval(parse(text=(paste(data_name,"<-", command,'("', data, '", header=TRUE, sep=",",)', sep=""))))
eval(parse(text=(paste("resave(", data_name, ', file=lumens_database)', sep=""))))
}
}
}
}



#CLEAN ENVIRONMENT
rm(list=ls(all.names=TRUE))
