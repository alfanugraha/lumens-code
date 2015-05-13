##[QUES]=group
##Wdir=folder
##LU1=raster
##grsize=number 10000
##period=number 1990
##location=string
##descript=file
##econ=file
##blook=file
##FCAfile=file
##sumtab2=output table
##passfilenames
##report

#set library which will be used
library(DBI)
library(raster)
library(rasterVis)
library(rgdal)
library(RSQLite)
library(SDMTools)
library(sp)
library(tiff)
library(foreign)
library(rgeos)
library(ggplot2)
library(pracma)
library(spatial.tools)
library(plyr)
library(rtf)
library(gridExtra)

#set working directory
#setwd(Wdir)
#year<-(period)
#lu1<-raster(LU1)
#lu1_path<-paste(LU1)
#gridres<-(grsize)
#fca<-paste(FCAfile)

#classdesc<-paste(descript)
#edgecon<-paste(econ)
#outpath<-paste(Wdir)
#gridres<-(grsize)

time_start<-proc.time()
#time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")


#set working directory
Wdir<-("C:/BIO_PAPER_14/01_Main_data/DIFA_charts/04_Batang_Gadis//ring2//2000_2005/")
setwd(Wdir)
outpath<-paste(getwd())

#time series 1 requirements
lu1<-raster("C:/BIO_PAPER_14/01_Main_data/buffer_lu_map/btgadis_ring2_2000.tif")
lu1_path<-paste("C:/BIO_PAPER_14/01_Main_data/buffer_lu_map/btgadis_ring2_2000.tif")
period1<-2000

#time series 2 requirements
lu2<-raster("C:/BIO_PAPER_14/01_Main_data/buffer_lu_map/btgadis_ring2_2005.tif")
lu2_path<-paste("C:/BIO_PAPER_14/01_Main_data/buffer_lu_map/btgadis_ring2_2005.tif")
period2<-2005

#zone map requirements
#zone<-raster("C:/BIO_PAPER_14/01 Main data/buffer_lu_map/zone_merangin_reclass.tif")
#zone_lookup<-"C:/QUES_B_DJB/Data_QUESB/Merangin/Tabel_zona_Merangin.csv"

#fragstats requirements
gridres<-5000
windowsize<-1000
window.shape<-1; #0= square, 1=circle
raster.nodata<-0
classdesc<-paste("C:/BIO_PAPER_14/Sumatra_ALREDDI/TECI_analysis/descriptors_sumatra.csv")
edgecon<-paste("C:/BIO_PAPER_14/Sumatra_ALREDDI/TECI_analysis/contrast_sumatra.csv")


#PREQUES database:
#ldabase.preques<-"C:/QUES_B_DJB/preques_merangin_2000_2005/PreQuES_database_Merangin_2000_2005.ldbase"
#ldabase.preques<-''

#project properties
location<-('Merangin')
habitat.reclass.lookup<-paste("C:/QUES_B_DJB/Data_QUESB/Merangin/habitat_class.csv")

ref.map.id<-1 ;#options: 1 initial landuse as refrence, 2 final landuse as reference, 3 zone map as reference

#options: 1 initial landuse as refrence, 2 final landuse as reference, 3 zone map as reference
if (ref.map.id==1){
  print("Initial land use/cover map as reference")
  ref.map<-lu1
} else if (ref.map.id==2){
  print("Final land use/cover map as reference")
  ref.map<-lu2
#} else if (ref.map.id==3){
#  print("Planning unit/zone map as reference")
#  ref.map<-zone
} else {
  stop("No map has been selected as reference")
}


#projection handling
if (grepl("+units=m", as.character(ref.map@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(ref.map)[1]*res(ref.map)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-B will automatically generate data in Ha unit")
} else{
  stop("Raster map projection is not in meter unit, please reproject your map")
}

#Extent handling and raster resolution land-cover maps

#checking landuse1
if (as.character(ref.map@crs)==as.character(lu1@crs)){
  print("Final land use/cover map has the same projection")
  if (res(ref.map)[1]==res(lu1)[1]){
    print("initial land use/cover map has the same extent with the reference map")
  } else{
    print("initial land use/cover map doesn't have the same extent with the reference map, synchronising land-cover map...")
    lu1<-spatial_sync_raster(lu1, ref.map, method = "ngb")
  }
} else{
  print("initial land use/cover map doesn't have the same projection with the reference map, synchronising land-cover map...")
  lu1<-spatial_sync_raster(lu1, ref.map, method = "ngb")
}

#checking landuse2
if (as.character(ref.map@crs)==as.character(lu2@crs)){
  print("Final land use/cover map has the same projection")
  if (res(ref.map)[1]==res(lu2)[1]){
    print("Final land use/cover map has the same extent with the reference map")
  } else{
    print("Final land use/cover map doesn't have the same extent with the reference map, synchronising land-cover map...")
    lu2<-spatial_sync_raster(lu2, ref.map, method = "ngb")
  }
} else{
  print("Final land use/cover map doesn't have the same projection with the reference map, synchronising land-cover map...")
  lu2<-spatial_sync_raster(lu2, ref.map, method = "ngb")
}

#checking zone map
if (as.character(ref.map@crs)==as.character(zone@crs)){
  print("Planning unit/zone map has the same projection")
  if (res(ref.map)[1]==res(zone)[1]){
    print("planning unit/zone map has the same extent with the reference map")
  } else{
    print("planning unit/zone map doesn't have the same extent with the reference map, synchronising land-cover map...")
    zone<-spatial_sync_raster(zone, ref.map, method = "ngb")
  }
} else{
  print("planning unit/zone map doesn't have the same projection with the reference map, synchronising land-cover map...")
  zone<-spatial_sync_raster(zone, ref.map, method = "ngb")
}

#grid preparation
xl1<-xmin(lu1)
yl1<-ymin(lu1)
xu1<-xmax(lu1)
yu1<-ymax(lu1)
pjg<-xu1-xl1
lbr<-yu1-yl1
ncellx<-pjg/gridres
ncelly<-lbr/gridres
ncellx<-ceiling(ncellx)
ncelly<-ceiling(ncelly)

newproj<-proj4string(lu1)
r<-raster(xmn=xl1, xmx=xu1, ymn=yl1, ymx=yu1, ncol=ncellx, nrow=ncelly, crs=newproj)
res(r)<-gridres
vals <- 1:ncell(r)
r<-setValues(r,vals)
sampling.rast<-resample(r,lu1, method="ngb"); #sampling grid raster file



#Apply nodata and synchronize nodata
lu1<- reclassify(lu1, cbind(raster.nodata,NA))
lu2<- reclassify(lu2, cbind(raster.nodata,NA))
lu1<- reclassify(lu1, cbind(128,NA))
lu2<- reclassify(lu2, cbind(128,NA))

#Calculate total Area
allarea.init<-na.omit(as.data.frame(freq(lu1)))
colnames(allarea.init)<-c("ID","COUNT")
totarea.init<-sum(allarea.init$COUNT)
totarea.init<-(totarea.init*Spat_res)

allarea.final<-na.omit(as.data.frame(freq(lu2)))
colnames(allarea.final)<-c("ID","COUNT")
totarea.final<-sum(allarea.final$COUNT)
totarea.final<-(totarea.final*Spat_res)

totarea<-max(totarea.final,totarea.init)

lu1.temp <- lu1>=0
lu2.temp <- lu2>=0
lu.nodata.check<-lu1.temp*lu2.temp

lu1<-lu1*lu.nodata.check; #syncronized nodata raster map
lu2<-lu2*lu.nodata.check; #syncronized nodata raster map





#write new raster data
writeRaster(lu1,  filename="lu1", format="GTiff", overwrite=TRUE, NAflag=255)
lu1_path<-paste(getwd(),"/lu1.tif", sep='')
writeRaster(lu2,  filename="lu2", format="GTiff", overwrite=TRUE, NAflag=255)
lu2_path<-paste(getwd(),"/lu2.tif", sep='')

#DEFINE FOCAL AREA
#modify biodiversity lookup table
lookup_bh<- read.table(classdesc, header=TRUE, sep=",")
lookup_z<- read.table(zone_lookup, header=TRUE, sep=",")
colnames(lookup_z)[1]<-c("ZONE")
lookup_bh[lookup_bh==TRUE]<-1
lookup_bh[lookup_bh==FALSE]<-0
lookup_bh[4]<-NULL
colnames(lookup_bh)<-c("ID", "Name", "BIODIV")


#INITIAL FOCAL AREA
#focal area lookup table
foc.area.reclass.init<-merge(allarea.init,lookup_bh,by="ID")
foc.area.reclass.init$COUNT<-NULL
foc.area.reclass.init$Name<-NULL

foc.area.init<- reclassify(lu1, foc.area.reclass.init)
tothab.init<-zonal(foc.area.init, sampling.rast, 'sum')
tothab.init<-as.data.frame(tothab.init)

colnames(tothab.init) <- c("ID", "sum")
tothab.init$sum<-((tothab.init$sum/totarea)*100)

#FINAL FOCAL AREA
foc.area.reclass.final<-merge(allarea.final,lookup_bh,by="ID")
foc.area.reclass.final$COUNT<-NULL
foc.area.reclass.final$Name<-NULL

foc.area.final<- reclassify(lu2, foc.area.reclass.final)
tothab.final<-zonal(foc.area.final, sampling.rast, 'sum')
tothab.final<-as.data.frame(tothab.final)

colnames(tothab.final) <- c("ID", "sum")
tothab.final$sum<-((tothab.final$sum/totarea)*100)


#FRAGSTATS MOVING WINDOW
#Define centroids
polygrid<-rasterToPolygons(r, n=4, na.rm=FALSE, digits=12, dissolve=TRUE)
centro<- gCentroid(polygrid,byid=TRUE)

#Prepare fca file for processing tif
modid=1

internal<-paste('')
cpf<-paste('')
io<-paste('[BAND:1]')
desc<-paste('')
drlib<-paste('GDAL')
drname<-paste('GeoTIFF grid (.tif)')
drid<-paste('63B45E15-C8E5-44f6-A9AB-60E1852CDB5D')

#extent input of file 1
xl1<-xmin(lu1)
yl1<-ymin(lu1)
xu1<-xmax(lu1)
yu1<-ymax(lu1)

#cell size input of file 1
csize1<-xres(lu1)
#row and column size input of file 1
rowc1<-nrow(lu1)
colc1<-ncol(lu1)


aczero="1"
#no data value input
nodata=255
bvalue=999

#common tables input

contab<-read.table(file=edgecon, header=TRUE, sep=',', skip=1)
contab2<-round(contab, digits=2)

#check raster file directory for lu1
dirname_raster<-dirname(lu1_path)
setwd(dirname_raster)

for (i in 1:3){
  mwout<-paste(lu1_path,'_mw',i, sep='')
  teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
  if (file.exists(teci.dir)==TRUE){
    mwout2<-paste(lu1_path,'_mw',i, sep='')
    unlink(mwout2, recursive=TRUE)
    print(paste(i,"deleting previous raster file found, algorithm continue..."))
  }else{
    print(paste(i,"no previous raster file found, algorithm continue..."))
  }
}

#connect to fragstats' .fca file


if (file.exists(paste(Sys.getenv("R_USER"),'\\LUMENS\\teciuf.fca',sep=''))){
fca<-paste(Sys.getenv("R_USER"),'\\LUMENS\\teciuf.fca', sep='')
print("Fragstats' model found!")
} else if (file.exists(paste(Sys.getenv("R_USER"),'\\Documents\\LUMENS\\teciuf.fca',sep=''))){
} else { stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
}

SQLite(max.con = 200, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=as.character(fca))

#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)

input_desc<-paste("UPDATE frg_table_strings SET value='",classdesc,"' WHERE rec_id=5;",sep="")
input_edge<-paste("UPDATE frg_table_strings SET value='",edgecon,"' WHERE rec_id=2;",sep="")
input_out<-paste("UPDATE frg_table_strings SET value='",outpath,"' WHERE rec_id=6;",sep="")
input_window_size_sqr<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;"); #change square window radius
input_window_size_circ<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=19;"); #change circle window radius
input_window_type<-paste("UPDATE frg_table_numerics SET value=",window.shape,"WHERE rec_id=13;")
ll <- dbSendQuery(con, input_desc)
ll <- dbSendQuery(con, input_edge)
ll <- dbSendQuery(con, input_out)
ll <- dbSendQuery(con, input_window_size_sqr)
ll <- dbSendQuery(con, input_window_size_circ)
ll <- dbSendQuery(con, input_window_type)

landlayer1<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu1_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl1,"','",yl1,"','",xu1,"','",yu1,"','",csize1,"','",rowc1,"','",colc1,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")

ll <- dbSendQuery(con, landlayer1)

if (file.exists("C:/Program Files (x86)/Fragstats 4")){
  setwd("C:/Program Files (x86)/Fragstats 4/")
} else{
  setwd("C:/Program Files/Fragstats 4/")
}

#execute fragstats for lu1
sysout<-paste(Wdir, "/fragout", sep="")
f <- paste('frg -m',shQuote(fca),' -o',sysout)
system(f)

#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)


#extent input of file 2
xl2<-xmin(lu2)
yl2<-ymin(lu2)
xu2<-xmax(lu2)
yu2<-ymax(lu2)

#cell size input of file 2
csize2<-xres(lu2)
#row and column size input of file 2
rowc2<-nrow(lu2)
colc2<-ncol(lu2)

#check raster file directory for lu2
dirname_raster<-dirname(lu2_path)
setwd(dirname_raster)

for (i in 1:3){
  mwout<-paste(lu2_path,'_mw',i, sep='')
  teci.dir<-paste(mwout,"/",list.files(mwout), sep='')
  if (file.exists(teci.dir)==TRUE){
    mwout2<-paste(lu2_path,'_mw',i, sep='')
    unlink(mwout2, recursive=TRUE)
    print(paste(i,"deleting previous raster file found, algorithm continue..."))
  }else{
    print(paste(i,"no previous raster file found, algorithm continue..."))
  }
}

landlayer2<-paste("INSERT INTO frg_landscape_layers(model_id, name_internal, name_external, io_info, driver_lib, driver_name, driver_id, xll, yll, xur, yur, cell_size, row_count, col_count, allow_class_zero, no_data_value, background_value) VALUES ('",modid,"','",internal,"','",lu2_path,"','",io,"','",drlib,"','",drname,"','",drid,"','",xl2,"','",yl2,"','",xu2,"','",yu2,"','",csize2,"','",rowc2,"','",colc2,"','",aczero,"','",nodata,"','",bvalue,"');",sep="")
ll <- dbSendQuery(con, landlayer2)

if (file.exists("C:/Program Files (x86)/Fragstats 4")){
  setwd("C:/Program Files (x86)/Fragstats 4/")
} else{
  setwd("C:/Program Files/Fragstats 4/")
}

#execute fragstats for lu2
sysout<-paste(Wdir, "/fragout", sep="")
f <- paste('frg -m',shQuote(fca),' -o',sysout)
system(f)


#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)
dbGetStatement(ll)
dbHasCompleted(ll)

#End of Fragstats TECI moving window analysis
setwd(Wdir)
mwout1<-paste(lu1_path,'_mw1', sep='')
teci.dir.init<-paste(mwout1,"/",list.files(mwout1), sep='')
mwout2<-paste(lu2_path,'_mw1', sep='')
teci.dir.final<-paste(mwout2,"/",list.files(mwout2), sep='')


#INITIAL TECI MW Handling
tryCatch({
  mwfile.init<-raster(teci.dir.init)
},error=function(e){cat("No moving window output file found, re-check your inputs :",conditionMessage(e), "\n")})

NAvalue(mwfile.init)<-(999*-1)
#extract value from MW TECI with points
tecival.init<-extract(mwfile.init, centro, method='simple', na.rm=T, df=T)

poly.data<-as.data.frame(polygrid,xy=TRUE) #NEW
colnames(poly.data)<-c("ID.centro","x","y","ID.grid") #ID = id grid

#combine dataframe of teci and habitat
colnames(tecival.init)<-c("ID.centro","teci")
colnames(tothab.init)<-c("ID.grid","sum")
ctab<-merge(tothab.init,poly.data,by="ID.grid")
ctab<-merge(ctab,tecival.init,by="ID.centro")
sort.ctab.init <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
sort.ctab.init <- sort.ctab.init[!(sort.ctab.init$sum==0),]
habcum.init= cumsum(sort.ctab.init$sum)
sumtab1.init<-cbind(sort.ctab.init, Cum.Sum=habcum.init)
cumax<-max(sumtab1.init$Cum.Sum, na.rm=TRUE)
row.names(sumtab1.init)<-1:nrow(sumtab1.init)
sumtab1.init[nrow(sumtab1.init)+1, ] <- c(sumtab1.init$ID.centro[nrow(sumtab1.init)], sumtab1.init$ID.grid[nrow(sumtab1.init)],100,sumtab1.init$x[nrow(sumtab1.init)],sumtab1.init$y[nrow(sumtab1.init)],100,cumax)

#Calculate area under the curve
AUC.init = round((trapz(na.omit(sumtab1.init$teci),sumtab1.init$Cum.Sum))/100,digits=2)

difa.init<-ggplot(sumtab1.init, aes(x =sumtab1.init$teci, y =sumtab1.init$Cum.Sum, xend=100, yend=100)) +
  geom_area(position='') + ggtitle(paste(period1,' - ','AUC=',AUC.init,'%',sep='')) +
  labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')



#EXPORT DATA
sumtab2.init<-round(sumtab1.init,digits=2)
colnames(sumtab2.init)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.init, "QUES-B Summary calculation-initial.csv", row.names = FALSE, sep=",")


file.teci.init<-paste('TECI_',location,'_',period1,'_NA',sep='')
file.habitat.name.init<-paste('focal_area_',location,'_',period1, sep='')
writeRaster(mwfile.init, filename=file.teci.init, format="GTiff", overwrite=TRUE)
writeRaster(foc.area.init, filename=file.habitat.name.init, format="GTiff", overwrite=TRUE)


#FINAL TECI MW Handling
tryCatch({
  mwfile.final<-raster(teci.dir.final)
},error=function(e){cat("No moving window output file found, re-check your inputs :",conditionMessage(e), "\n")})
NAvalue(mwfile.final)<-(999*-1)
#extract value from MW TECI with points
tecival.final<-extract(mwfile.final, centro, method='simple', na.rm=T, df=T)

poly.data<-as.data.frame(polygrid,xy=TRUE) #NEW
colnames(poly.data)<-c("ID.centro","x","y","ID.grid") #ID = id grid

#combine dataframe of teci and habitat
colnames(tecival.final)<-c("ID.centro","teci")
colnames(tothab.final)<-c("ID.grid","sum")
ctab<-merge(tothab.final,poly.data,by="ID.grid")
ctab<-merge(ctab,tecival.final,by="ID.centro")
sort.ctab.final <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
sort.ctab.final <- sort.ctab.final[!(sort.ctab.final$sum==0),]
habcum.final= cumsum(sort.ctab.final$sum)
sumtab1.final<-cbind(sort.ctab.final, Cum.Sum=habcum.final)
cumax<-max(sumtab1.final$Cum.Sum, na.rm=TRUE)
row.names(sumtab1.final)<-1:nrow(sumtab1.final)
sumtab1.final[nrow(sumtab1.final)+1, ] <- c(sumtab1.final$ID.centro[nrow(sumtab1.final)], sumtab1.final$ID.grid[nrow(sumtab1.final)],100,sumtab1.final$x[nrow(sumtab1.final)],sumtab1.final$y[nrow(sumtab1.final)],100,cumax)

#Calculate area under the curve
AUC.final = round((trapz(na.omit(sumtab1.final$teci),sumtab1.final$Cum.Sum))/100,digits=2)


difa.final<-ggplot(sumtab1.final, aes(x =sumtab1.final$teci, y =sumtab1.final$Cum.Sum, xend=100, yend=100)) +
  geom_area(position='') + ggtitle(paste(period2,' - ','AUC=',AUC.final,'%',sep='')) +
  labs(x = "Sorted TECI value (%)", y='Cumulative Proportion of Focal Areas (%)')


#EXPORT DATA
sumtab2.final<-round(sumtab1.final,digits=2)
colnames(sumtab2.final)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.final, "QUES-B Summary calculation-final.csv", row.names = FALSE, sep=",")

file.teci.final<-paste('TECI_',location,'_',period2,'_NA',sep='')
file.habitat.name.final<-paste('focal_area_',location,'_',period2, sep='')
writeRaster(mwfile.final, filename=file.teci.final, format="GTiff", overwrite=TRUE)
writeRaster(foc.area.final, filename=file.habitat.name.final, format="GTiff", overwrite=TRUE)


file.difa.init<-paste('DIFA_',location,'_',period1,'.png', sep='')
ggsave(filename=file.difa.init, plot=difa.init)

file.difa.final<-paste('DIFA_',location,'_',period2,'.png', sep='')
ggsave(filename=file.difa.final, plot=difa.final)

