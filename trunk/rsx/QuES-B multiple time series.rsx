##[QUES]=group
##Wdir=folder
##LU1=raster
##LU2=raster
##zone=raster

##year1=number 1990
##year2=number 2000
##location=string
##gridres=number 10000
##windowsize=number 1000
##window.shape= number 0
##raster.nodata= number 0
##classdesc=file
##edgecon=file
##ldabase.preques=file
##habitat.reclass.lookup=file
##zone_lookup=file
##ref.map.id= number 1
##passfilenames


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

time_start <- proc.time()

#set working directory
setwd(Wdir)
outpath<-paste(getwd())

#time series 1 requirements
lu1<-raster(LU1)
lu1_path<-paste(LU1)

#time series 2 requirements
lu2<-raster(LU2)
lu2_path<-paste(LU2)

#zone map requirements
zone<-raster(zone)

#options: 1 initial landuse as refrence, 2 final landuse as reference, 3 zone map as reference
if (ref.map.id==1){
  print("Initial land use/cover map as reference")
  ref.map<-lu1
} else if (ref.map.id==2){
  print("Final land use/cover map as reference")
  ref.map<-lu2
} else if (ref.map.id==3){
  print("Planning unit/zone map as reference")
  ref.map<-zone
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

#Apply nodata and synchronize nodata
lu1<- reclassify(lu1, cbind(raster.nodata,NA))
lu2<- reclassify(lu2, cbind(raster.nodata,NA))

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

#CHECK FRAGSTATS MODEL AVAILABILITY
#if (file.exists("C:/Program Files (x86)/LUMENS//teciuf.fca")){
#  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
#} else if (file.exists("C:/Program Files/LUMENS//teciuf.fca")){
#  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
#} else{
#  stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
#}


#connect to fragstats' .fca file
if (file.exists("C:/Program Files (x86)/LUMENS//teciuf.fca")){
  fca<-paste('C:/Program Files (x86)/LUMENS//teciuf.fca')
} else if (file.exists("C:/Program Files/LUMENS//teciuf.fca")){
  fca<-paste('C:/Program Files (x86)/LUMENS//teciuf.fca')
} else{
  stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
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
input_window_size<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;")
input_window_type<-paste("UPDATE frg_table_numerics SET value=",window.shape,"WHERE rec_id=12;")
ll <- dbSendQuery(con, input_desc)
ll <- dbSendQuery(con, input_edge)
ll <- dbSendQuery(con, input_out)
ll <- dbSendQuery(con, input_window_size)

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
sumtab1.init[nrow(sumtab1.init)+1, ] <- c(sumtab1.init$ID.grid[nrow(sumtab1.init)],100,sumtab1.init$ID.centro[nrow(sumtab1.init)],sumtab1.init$x[nrow(sumtab1.init)],sumtab1.init$y[nrow(sumtab1.init)],100,cumax)
difa.init<-ggplot(sumtab1.init, aes(x =sumtab1.init$teci, y =sumtab1.init$Cum.Sum, xend=100, yend=100)) + geom_area(position='')+ labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)')

#Calculate area under the curve
AUC.init = round((trapz(na.omit(sumtab1.init$teci),sumtab1.init$Cum.Sum))/100,digits=2)

#EXPORT DATA
sumtab2.init<-round(sumtab1.init,digits=2)
colnames(sumtab2.init)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.init, "QUES-B Summary calculation-initial.csv", row.names = FALSE, sep=",")


file.teci.init<-paste('TECI_',location,'_',year1,'_NA',sep='')
file.habitat.name.init<-paste('focal_area_',location,'_',year1, sep='')
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
sumtab1.final[nrow(sumtab1.final)+1, ] <- c(sumtab1.final$ID.grid[nrow(sumtab1.final)],100,sumtab1.final$ID.centro[nrow(sumtab1.final)],sumtab1.final$x[nrow(sumtab1.final)],sumtab1.final$y[nrow(sumtab1.final)],100,cumax)
difa.final<-ggplot(sumtab1.final, aes(x =sumtab1.final$teci, y =sumtab1.final$Cum.Sum, xend=100, yend=100)) + geom_area(position='')+ labs(x = "Sorted TECI value (%)", y='Focal area proportion (%)')

#Calculate area under the curve
AUC.final = round((trapz(na.omit(sumtab1.final$teci),sumtab1.final$Cum.Sum))/100,digits=2)

#EXPORT DATA
sumtab2.final<-round(sumtab1.final,digits=2)
colnames(sumtab2.final)<-c("ID.centroid","ID.grid","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2.final, "QUES-B Summary calculation-final.csv", row.names = FALSE, sep=",")


file.teci.final<-paste('TECI_',location,'_',year2,'_NA',sep='')
file.habitat.name.final<-paste('focal_area_',location,'_',year2, sep='')
writeRaster(mwfile.final, filename=file.teci.final, format="GTiff", overwrite=TRUE)
writeRaster(foc.area.final, filename=file.habitat.name.final, format="GTiff", overwrite=TRUE)

#Zonal statistics on QUES-B

#generate zonal statistics
zstat.init<-ZonalStat(mwfile.init, zone, FUN = "all")
zstat.init[3]<-NULL
zstat.init[3]<-NULL
zstat.init[3]<-NULL
#rcl.mean.init<-cbind(zstat.init$zone,zstat.init$mean)
#teci_zstat_mean.init<-reclassify(zone, rcl.mean.init);# PU teci value mean


zstat.final<-ZonalStat(mwfile.final, zone, FUN = "all")
zstat.final[3]<-NULL
zstat.final[3]<-NULL
zstat.init[3]<-NULL
#rcl.mean.final<-cbind(zstat.final$zone,zstat.final$mean)
#teci_zstat_mean.final<-reclassify(zone, rcl.mean.final);# PU teci value mean

#SDM Tools fragstats; mean patch area calculation; patch number calculation
foc.area.stats.init<- ClassStat(foc.area.init,bkgd=0, cellsize=(res(foc.area.init)[1]/100))
foc.area.stats.init<-t(as.data.frame(foc.area.stats.init))
foc.area.stats.init<-round(foc.area.stats.init[,1], digits=2)

foc.area.stats.final<- ClassStat(foc.area.final,bkgd=0, cellsize=(res(foc.area.final)[1]/100))
foc.area.stats.final<-t(as.data.frame(foc.area.stats.final))
foc.area.stats.final<-round(foc.area.stats.final[,1], digits=2)

#Combine class STATS
foc.area.stats<-cbind(foc.area.stats.init,foc.area.stats.final)
foc.area.stats.temp1<-foc.area.stats[2:4,c('foc.area.stats.init','foc.area.stats.final')]
total.edge<-(foc.area.stats[6:6,c('foc.area.stats.init','foc.area.stats.final')]*100)
foc.area.stats.temp3<-(foc.area.stats[10:13,c('foc.area.stats.init','foc.area.stats.final')])
foc.area.stats<-rbind(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)
rm(foc.area.stats.temp1,total.edge,foc.area.stats.temp3)

col.init<-paste('class.stats.',year1, sep='')
colnames(foc.area.stats)<-c(paste('class.stats.',year1, sep=''),paste('class.stats.',year2, sep=''))
foc.area.stats.filename<-paste("Focal_area_class_metrics",location,'_',year1,'_',year2,'.csv', sep='')
write.csv(foc.area.stats, foc.area.stats.filename, row.names=TRUE)

#combine teci_zstat with planning unit name

#QUES-B database
dbase.quesb.name<-paste("QuESB_database_", location,'_',year1,'_',year2,'.ldbase', sep='')
save(lu1_path,lu1,year1,lu2_path,lu2,year2,zone,zone_lookup,location,totarea,lookup_bh,polygrid,sumtab1.init,difa.init,AUC.init,foc.area.init,mwfile.init,zstat.init,foc.area.stats.init,sumtab1.final,difa.final,AUC.final,mwfile.final,zstat.final,foc.area.stats.final, file=dbase.quesb.name)
#load(dbase.quesb.name)

#MULTI-TEMPORAL ANALYSIS 

#Focal area decrement and increment
chk_loss<-foc.area.init>foc.area.final
chk_gain<-foc.area.init<foc.area.final
foc.area.loss<-(foc.area.init-foc.area.final)*chk_loss;#integration increase
foc.area.gain<-(foc.area.final-foc.area.init)*chk_gain;#integration decrease

if (maxValue(foc.area.gain)==0 & minValue(foc.area.gain)==0){foc.area.gain<-paste("NO FOCAL AREA RECOVERED")} else {foc.area.gain}


#Habitat loss (TECI increment) and Habitat recovery (decrement) except nodata
mwfile.init.chk<-mwfile.init
mwfile.final.chk<-mwfile.final

chk_teci_decrement<-mwfile.init.chk>mwfile.final.chk
chk_teci_decrement <- reclassify(chk_teci_decrement, cbind(0,NA))
chk_teci_increment<-mwfile.init.chk<mwfile.final.chk
chk_teci_increment <- reclassify(chk_teci_increment, cbind(0,NA))
habitat.recovery<-(mwfile.init.chk-mwfile.final.chk)*chk_teci_decrement;#TECI value decrement
habitat.degradation<-(mwfile.final.chk-mwfile.init.chk)*chk_teci_increment;#TECI value increment


#TECI loss and gain in NA data
mwfile.init.NA <- reclassify(mwfile.init.chk, cbind(NA, 999))
mwfile.init.NA<-((mwfile.init.NA/999)==1)

mwfile.final.NA <- reclassify(mwfile.final.chk, cbind(NA, 999))
mwfile.final.NA<-((mwfile.final.NA/999)==1)

habitat.loss.NA<-mwfile.init.chk*mwfile.final.NA;#TECI loss in NA area
habitat.gain.NA<-mwfile.final.chk*mwfile.init.NA;#TECI gain in NA area


#Habitat gain and recovery
habitat.gain.NA<- reclassify(habitat.gain.NA, cbind(0, NA))
habitat.gain.recovery<-mosaic(habitat.recovery, habitat.gain.NA, fun="max")

#Habitat loss and degradation
habitat.loss.NA<- reclassify(habitat.loss.NA, cbind(0, NA))
habitat.loss.degradation<-mosaic(habitat.degradation, habitat.loss.NA, fun="max")


#focal area loss evaluation: generate focal area loss map contained with final land-cover types
foc.area.loss<-chk_loss*lu2
foc.area.loss <- reclassify(foc.area.loss, cbind(0, NA))
#writeRaster(foc.area.loss,  filename="focal area loss", format="GTiff", overwrite=TRUE)
foc.area.loss.att<-na.omit(as.data.frame(freq(foc.area.loss)))
foc.area.loss.att$prop<-(foc.area.loss.att$count/sum(foc.area.loss.att$count))*100

colnames(foc.area.loss.att)[1]<-c("ID")
foc.area.loss.att<-merge(lookup_bh, foc.area.loss.att, by="ID")
foc.area.loss.att$BIODIV<-NULL
colnames(foc.area.loss.att)<-c("ID", "LULC", "AREA", "PROPORTION")
foc.area.loss.att$AREA<-foc.area.loss.att$AREA*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)
foc.area.loss.att<-arrange(foc.area.loss.att, -PROPORTION)
foc.area.loss.att$PROPORTION<-round(foc.area.loss.att$PROPORTION, digits=2)
foc.area.loss.att.filename<-paste("Focal_area_loss_source",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(foc.area.loss.att, foc.area.loss.att.filename)



#zonal stat for focal area gain/loss
zstat.foc.area<-as.data.frame(zonal((foc.area.final-foc.area.init), zone, fun='sum'))
colnames(zstat.foc.area) =c("ZONE","foc.area")
zstat.foc.area$foc.area<-zstat.foc.area$foc.area*(res(foc.area.init)[1]*res(foc.area.init)[2]/10000)

#zonal stat for habitat integration and segregation
zstat.habitat.gain.recovery<-ZonalStat(habitat.gain.recovery, zone, FUN = "all")
colnames(zstat.habitat.gain.recovery)[1] ="ZONE"
zstat.habitat.gain.recovery<-merge(lookup_z, zstat.habitat.gain.recovery, by="ZONE")
zstat.habitat.gain.recovery[4]<-NULL
zstat.habitat.gain.recovery[4]<-NULL
zstat.habitat.gain.recovery[4]<-NULL
zstat.habitat.gain.recovery[7]<-NULL
zstat.habitat.gain.recovery$max<-round(zstat.habitat.gain.recovery$max, digits=2)
zstat.habitat.gain.recovery$min<-round(zstat.habitat.gain.recovery$min, digits=2)
zstat.habitat.gain.recovery$mean<-round(zstat.habitat.gain.recovery$mean, digits=2)
zstat.habitat.gain.recovery$sd<-round(zstat.habitat.gain.recovery$sd, digits=2)
zstat.habitat.gain.recovery<-merge(zstat.habitat.gain.recovery, zstat.foc.area, by="ZONE")
zstat.habitat.gain.recovery$norm.mean<-zstat.habitat.gain.recovery$mean/abs(zstat.habitat.gain.recovery$foc.area)
zstat.habitat.gain.recovery$norm.mean<-round(zstat.habitat.gain.recovery$norm.mean, digits=3)
zstat.habitat.gain.recovery<-arrange(zstat.habitat.gain.recovery, -norm.mean)


zstat.habitat.loss.degradation<-ZonalStat(habitat.loss.degradation, zone, FUN = "all")
colnames(zstat.habitat.loss.degradation)[1] ="ZONE"
zstat.habitat.loss.degradation<-merge(lookup_z, zstat.habitat.loss.degradation, by="ZONE")
zstat.habitat.loss.degradation[4]<-NULL
zstat.habitat.loss.degradation[4]<-NULL
zstat.habitat.loss.degradation[4]<-NULL
zstat.habitat.loss.degradation[7]<-NULL
zstat.habitat.loss.degradation$max<-round(zstat.habitat.loss.degradation$max, digits=2)
zstat.habitat.loss.degradation$min<-round(zstat.habitat.loss.degradation$min, digits=2)
zstat.habitat.loss.degradation$mean<-round(zstat.habitat.loss.degradation$mean, digits=2)
zstat.habitat.loss.degradation$sd<-round(zstat.habitat.loss.degradation$sd, digits=2)
zstat.habitat.loss.degradation<-merge(zstat.habitat.loss.degradation, zstat.foc.area, by="ZONE")
zstat.habitat.loss.degradation$norm.mean<-zstat.habitat.loss.degradation$mean/abs(zstat.habitat.loss.degradation$foc.area)
zstat.habitat.loss.degradation$norm.mean<-round(zstat.habitat.loss.degradation$norm.mean, digits=3)
zstat.habitat.loss.degradation<-arrange(zstat.habitat.loss.degradation, -norm.mean)

#write zonal stats table
zstat.gain.recover.filename<-paste("Habitat_gain_recovery_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.gain.recovery, zstat.gain.recover.filename)

zstat.loss.degradation.filename<-paste("Habitat_loss_degradation_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
write.dbf(zstat.habitat.loss.degradation, zstat.loss.degradation.filename)



#PREQUES data handling
if (grepl(".ldbase", as.character(ldabase.preques))){
  print(paste("loading Pre-QuES database from", ldabase.preques,sep=' '))
  #QuES-B and Pre-QuES integration
  #load final database
  load(ldabase.preques)
  #rename initial variables
  prqs.proj.prop<-proj_prop
  prqgs.lucdb<-data_merge_sel
  prqs.ov.chg<-Ov_chg
  prqs.lutm<-cross_temp.melt.dbf
  prqs.luchg<-luchg
  prqs.luchg.att<-luchg_att
  prqs.year.init<-period1
  prqs.year.final<-period2
  prqs.location<-location
  remove(proj_prop,data_merge_sel, Ov_chg, cross_temp.melt.dbf, luchg, luchg_att, period1, period2, location)
  
  
  #identify contributing land use change to focal area segregation
  habitat.loss.degradation.bol<-habitat.loss.degradation/habitat.loss.degradation; #create boolean segregation map
  
  if (as.character(prqs.luchg@crs)==as.character(habitat.loss.degradation.bol@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.loss.degradation.bol)[1]){
      print("change map has the same extent with the habitat degradation map")
    } else{
      print("change map doesn't have the same extent with the habitat degradation map, synchronising habitat degradation map...")
      habitat.loss.degradation.bol<-spatial_sync_raster(habitat.loss.degradation.bol, prqs.luchg, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat degradation map, synchronising habitat degradation map...")
    habitat.loss.degradation.bol<-spatial_sync_raster(habitat.loss.degradation.bol, prqs.luchg, method = "ngb")
  }
  
  luchg.seg<-prqs.luchg*habitat.loss.degradation.bol; #focal area segregation LUC
  luchg.seg.att<-na.omit(as.data.frame(freq(luchg.seg)))
  colnames(luchg.seg.att)<-c("ID","CHANGE")
  luchg.seg.att<-merge(luchg.seg.att,prqs.luchg.att,by="ID")
  luchg.seg.att<-as.data.frame(cbind(luchg.seg.att[1],luchg.seg.att[2],luchg.seg.att[4],luchg.seg.att[5],luchg.seg.att[12],luchg.seg.att[13], luchg.seg.att[14]))
  luchg.seg.att<-luchg.seg.att[ order(-luchg.seg.att[,2]), ]
  luchg.seg.db.filename<-paste("LUCHG_segregation_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
  write.dbf(luchg.seg.att, luchg.seg.db.filename)
  
  #top 10 habitat loss and degradation
  luchg.seg.10<-luchg.seg.att[1:10,]
  
  #habitat loss and degradation in persistent LULC
  luchg.seg.10.no.change<-luchg.seg.10[ which(as.character(luchg.seg.10$ID_LC2)==as.character(luchg.seg.10$ID_LC1)),];#habitat loss and degradation in persistent landuse/landcover
  luchg.seg.10.no.change<-as.data.frame(cbind(luchg.seg.10.no.change[1],luchg.seg.10.no.change[2],luchg.seg.10.no.change[5]))
  colnames(luchg.seg.10.no.change)<-c("ID", "AREA", "LULC")
  luchg.seg.10.no.change$LULC<-paste("Persistent", luchg.seg.10.no.change$LULC, sep=' ')
  
  #habitat loss and degradation with LULC
  luchg.seg.10.with.change<-luchg.seg.10[ which(as.character(luchg.seg.10$ID_LC2)!=as.character(luchg.seg.10$ID_LC1)),];#habitat loss and degradation with landuse/landcover change
  luchg.seg.10.with.change<-as.data.frame(cbind(luchg.seg.10.with.change[1],luchg.seg.10.with.change[2],luchg.seg.10.with.change[7]))
  colnames(luchg.seg.10.with.change)<-c("ID", "AREA", "LULC")
  
  #bind habitat loss and degradation with LULC and persistent LULC
  luchg.seg.10<-rbind(luchg.seg.10.no.change,luchg.seg.10.with.change)
  luchg.seg.10<-arrange(luchg.seg.10, -AREA)
  
  
  
  
  
  #identify contributing land use change to focal area integration
  habitat.gain.recovery.bol<-habitat.gain.recovery/habitat.gain.recovery; #create boolean segregation map
  
  if (as.character(prqs.luchg@crs)==as.character(habitat.gain.recovery.bol@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.gain.recovery.bol)[1]){
      print("change map has the same extent with the habitat degradation map")
    } else{
      print("change map doesn't have the same extent with the habitat degradation map, synchronising habitat degradation map...")
      habitat.gain.recovery.bol<-spatial_sync_raster(habitat.gain.recovery.bol, prqs.luchg, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat degradation map, synchronising habitat degradation map...")
    habitat.gain.recovery.bol<-spatial_sync_raster(habitat.gain.recovery.bol, prqs.luchg, method = "ngb")
  }
  
  luchg.int<-prqs.luchg*habitat.gain.recovery.bol; #focal area segregation LUC
  luchg.int.att<-na.omit(as.data.frame(freq(luchg.int)))
  colnames(luchg.int.att)<-c("ID","CHANGE")
  luchg.int.att<-merge(luchg.int.att,prqs.luchg.att,by="ID")
  luchg.int.att<-as.data.frame(cbind(luchg.int.att[1],luchg.int.att[2],luchg.int.att[4],luchg.int.att[5],luchg.int.att[12],luchg.int.att[13], luchg.int.att[14]))
  luchg.int.att<-luchg.int.att[ order(-luchg.int.att[,2]), ]
  luchg.int.db.filename<-paste("LUCHG_segregation_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
  write.dbf(luchg.int.att, luchg.int.db.filename)
  
  #top 10 habitat gain and recovery
  luchg.int.10<-luchg.int.att[1:10,]
  
  #habitat gain and recovery in persistent LULC
  luchg.int.10.no.change<-luchg.int.10[ which(as.character(luchg.int.10$ID_LC2)==as.character(luchg.int.10$ID_LC1)),];#habitat gain and recovery in persistent landuse/landcover
  luchg.int.10.no.change<-as.data.frame(cbind(luchg.int.10.no.change[1],luchg.int.10.no.change[2],luchg.int.10.no.change[5]))
  colnames(luchg.int.10.no.change)<-c("ID", "AREA", "LULC")
  luchg.int.10.no.change$LULC<-paste("Persistent", luchg.int.10.no.change$LULC, sep=' ')
  
  #habitat gain and recovery with LULC
  luchg.int.10.with.change<-luchg.int.10[ which(as.character(luchg.int.10$ID_LC2)!=as.character(luchg.int.10$ID_LC1)),];#habitat gain and recovery with landuse/landcover change
  luchg.int.10.with.change<-as.data.frame(cbind(luchg.int.10.with.change[1],luchg.int.10.with.change[2],luchg.int.10.with.change[7]))
  colnames(luchg.int.10.with.change)<-c("ID", "AREA", "LULC")
  
  #bind habitat gain and recovery with LULC and persistent LULC
  luchg.int.10<-rbind(luchg.int.10.no.change,luchg.int.10.with.change)
  luchg.int.10<-arrange(luchg.int.10, -AREA)
  
} else{
  print("No previous Pre-QuES database loaded")
}


#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)



#Landuse 1 map
area_lc1<-as.data.frame(freq(lu1))
colnames(area_lc1)[1]<-'ID'
area_lc1<-merge(area_lc1, lookup_bh, by='ID')
colnames(area_lc1)[3]<-'CLASS_LC1'
area_lc1[4]<-NULL
myColors.lu <- myColors[1:length(unique(area_lc1$ID))]
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc1$ID, labels=area_lc1$CLASS_LC1, values=myColors.lu)
plot.LU1<-gplot(lu1, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#Landuse 2 map
area_lc2<-as.data.frame(freq(lu2))
colnames(area_lc2)[1]<-'ID'
area_lc2<-merge(area_lc2, lookup_bh, by='ID')
colnames(area_lc2)[3]<-'CLASS_LC2'
area_lc2[4]<-NULL
ColScale.lu<-scale_fill_manual(name="Land Use Class", breaks=area_lc2$ID, labels=area_lc2$CLASS_LC2, values=myColors.lu)
plot.LU2<-gplot(lu2, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))


myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)


#zone map
area_zone<-lookup_z
colnames(area_zone)[1]<-'ID'
colnames(area_zone)[2]<-'ZONE'
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Zone Class", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)


#Focal area loss
plot(chk_loss)
lookup_loss<-as.data.frame(cbind(0,NA))
lookup_loss<-rbind(lookup_loss, cbind(1,2))
foc.area.loss.reclass<- reclassify(chk_loss, lookup_loss)
foc.area.change<-mosaic(foc.area.init, foc.area.loss.reclass, fun="max")

plot(foc.area.change, legend=F, col=cbind("#E6E6E6","#006D2C", "#E41A1C"))
legend("bottomright",legend = c("Non foc.area","intact foc.area", "Foc.area loss"), fill = cbind("#E6E6E6","#006D2C", "#E41A1C"))

#Initial TECI moving window map plotting
teci.color <- brewer.pal(9,"Oranges")
background<-lu1/lu1
plot(background, legend = FALSE, col = rev(gray(0.9)))
plot(mwfile.init,add=T, col =teci.color)

#Initial TECI moving window map plotting
teci.color <- brewer.pal(9,"Oranges")
plot(background, legend = FALSE, col = rev(gray(0.9)))
plot(mwfile.final,add=T, col =teci.color)


#HABITAT CHANGE ANALYSIS

habitat.reclass<- read.table(habitat.reclass.lookup,header=TRUE, sep=",")
habitat.reclass.mat<-as.matrix.data.frame(habitat.reclass[1:nrow(habitat.reclass),1:3], byrow=TRUE)
habitat.rec.init<-reclassify(mwfile.init, habitat.reclass.mat, right=NA)
habitat.rec.final<-reclassify(mwfile.final, habitat.reclass.mat, right=NA)

habitat.rec.init.freq<-as.data.frame(freq(habitat.rec.init))
colnames(habitat.rec.init.freq)<-c('ID','AREA.INITIAL')
habitat.rec.init.freq$AREA.INITIAL<-habitat.rec.init.freq$AREA.INITIAL*Spat_res
habitat.rec.final.freq<-as.data.frame(freq(habitat.rec.final))
colnames(habitat.rec.final.freq)<-c('ID','AREA.FINAL')
habitat.rec.final.freq$AREA.FINAL<-habitat.rec.final.freq$AREA.FINAL*Spat_res

lookup_habitat<-habitat.reclass[1:nrow(habitat.reclass),3:4]
habitat.change<-merge(lookup_habitat, habitat.rec.init.freq, by="ID")
habitat.change<-merge(habitat.change, habitat.rec.final.freq, by="ID")

time.elapsed<-proc.time() - time_start
time.elapsed