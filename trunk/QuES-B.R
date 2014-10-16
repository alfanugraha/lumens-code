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
library(R2HTML)
library(HTMLUtils)
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
library(knitr)
library(markdown)
library(SVGAnnotation)

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

#set working directory
setwd("C:/QUES_B_DJB/Merangin_2000")
year<-2000
lu1<-raster("C:/QUES_B_DJB/Data_QUESB/Merangin/raster/lc_2000_Mrg1.tif")

lu1_path<-paste("C:/QUES_B_DJB/Data_QUESB/Merangin/raster/lc_2000_Mrg1.tif")
gridres<-(10000)
windowsize<-1000
raster.nodata<-255


classdesc<-paste("C:/QUES_B_DJB/Data_QUESB/Merangin/description_merangin.csv")
edgecon<-paste("C:/QUES_B_DJB/Data_QUESB/Merangin/contrast_merangin.csv")
outpath<-paste(getwd())
Wdir<-outpath
blook<-paste("C:/QUES_B_DJB/Data_QUESB/Merangin/habitat_merangin.csv")

location<-('Merangin')
period<-(year)

#projection handling
if (grepl("+units=m", as.character(lu1@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(lu1)[1]*res(lu1)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-B will automatically generate data in Ha unit")
} else{
  stop("Raster map projection is not in meter unit, please reproject your map")
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
sampling.rast<-setValues(r,vals)
sampling.rast<-resample(sampling.rast,lu1, method="ngb"); #sampling grid raster file


#Calculate total Area
allarea<-ratify(lu1, filename='allarea.grd',count=TRUE,overwrite=TRUE)
areadf<-as.data.frame(levels(allarea))
totarea<-sum(areadf$COUNT)
totarea<-(totarea*Spat_res)

#DEFINE HABITAT
lookup_bh<- read.table(classdesc, header=TRUE, sep=",")
lookup_bh[lookup_bh==TRUE]<-1
lookup_bh[lookup_bh==FALSE]<-0
lookup_bh[4]<-NULL
colnames(lookup_bh)<-c("ID", "Name", "BIODIV")
levels(allarea)<-merge((levels(allarea)),lookup_bh,by="ID")
habitat<- deratify(allarea,'BIODIV')
tothab<-zonal(habitat, sampling.rast, 'sum')
tothab<-as.data.frame(tothab)

colnames(tothab) <- c("ID", "sum")
tothab$sum<-((tothab$sum/totarea)*100)
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
nodata=0
bvalue=raster.nodata

#common tables input

contab<-read.table(file=edgecon, header=TRUE, sep=',', skip=1)
contab2<-round(contab, digits=2)

#CHECK THE RASTER DIRECTORIES
dirname_raster<-dirname(lu1_path)
setwd(dirname_raster)

for (i in 1:10){
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
if (file.exists("C:/Program Files (x86)/LUMENS//teciuf.fca")){
  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
} else if (file.exists("C:/Program Files/LUMENS//teciuf.fca")){
  fca<-shQuote("C:/Program Files (x86)/LUMENS//teciuf.fca")
} else{
  stop("Fragstats model file is not found, please make sure the file is located in your LUMENS folder in Program files")
}


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

fa#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)

input_desc<-paste("UPDATE frg_table_strings SET value='",classdesc,"' WHERE rec_id=5;",sep="")
input_edge<-paste("UPDATE frg_table_strings SET value='",edgecon,"' WHERE rec_id=2;",sep="")
input_out<-paste("UPDATE frg_table_strings SET value='",outpath,"' WHERE rec_id=6;",sep="")
input_window_size<-paste("UPDATE frg_table_numerics SET value=",windowsize,"WHERE rec_id=18;")
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

#execute fragstats
sysout<-paste(outpath, "fragout", sep="")
f <- paste('frg -m',dQuote(fca),' -o',sysout)
system(f)


mwout<-paste(lu1_path,'_mw1', sep='')
teci.dir<-paste(mwout,"/",list.files(mwout), sep='')

setwd(Wdir)
direktori<-getwd()

newwdout<-paste(direktori,'/teci',location,year,'.tif',sep='')
file.rename(teci.dir, newwdout)

#remove old directory of teci file
mwout2<-paste(lu1_path,'_mw1', sep='')
unlink(mwout2, recursive=TRUE)

#get MW TECI file from wd
mwfile<-raster(newwdout)
NAvalue(mwfile)<-(raster.nodata*-1)
#extract value from MW TECI with points
tecival<-extract(mwfile, centro, method='simple', na.rm=T, df=T)

poly.data<-as.data.frame(polygrid,xy=TRUE) #NEW
colnames(poly.data)<-c("ID.centro","x","y","ID.grid") #ID = id grid

#combine dataframe of teci and habitat
colnames(tecival)<-c("ID.centro","teci")
colnames(tothab)<-c("ID.grid","sum")
ctab<-merge(tothab,poly.data,by="ID.grid")
ctab<-merge(ctab,tecival,by="ID.centro")
sort.ctab <- ctab[order(ctab$teci, decreasing=F, na.last=TRUE), ]
sort.ctab <- sort.ctab[!(sort.ctab$sum==0),]
sort.ctab[is.na(sort.ctab)] <- 0
habcum= cumsum(sort.ctab$sum)
sumtab1<-cbind(sort.ctab, Cum.Sum=habcum)
#sumtab$Cum.Sum<-(sumtab$Cum.Sum/sum(sumtab$sum))
#sumtab$Cum.Sum<-(sumtab$Cum.Sum)*100
cumax<-max(sumtab1$Cum.Sum, na.rm=TRUE)
#sumtab[nrow(sumtab)+1, ] <- c(100,100,100,cumax)
#sumtab1<-sumtab
sumtab1[nrow(sumtab1)+1, ] <- c(sumtab1$ID.grid[nrow(sumtab1)],100,sumtab1$ID.centro[nrow(sumtab1)],sumtab1$x[nrow(sumtab1)],sumtab1$y[nrow(sumtab1)],100,cumax)
plotbio<-ggplot(sumtab1, aes(x =sumtab1$teci, y =sumtab1$Cum.Sum, xend=100, yend=100)) + geom_area(position='')+ labs(x = "TECI (%)", y='Habitat Proportion(%)')

#Calculate area under the curve
AUC = (trapz(sumtab1$teci,sumtab1$Cum.Sum))/100
AUC2<-round(AUC,digits=2)

#EXPORT DATA
sumtab2<-round(sumtab1,digits=2)
colnames(sumtab2)<-c("ID","ID.centro","X.cor","Y.cor","Habitat Area (Ha)","TECI(%)", "Cumulative Habitat(%)")
write.table(sumtab2, "QUES-B Summary calculation.csv", row.names = FALSE, sep=",")
file.newwdout<-substr(basename(newwdout), 1, nchar(basename(newwdout)) - 4
writeRaster(mwfile, filename=file.newwdout, format="GTiff", overwrite=TRUE)

#delete all record from frg_landscape layer
del<-paste("DELETE FROM frg_landscape_layers")
ll <- dbSendQuery(con, del)
dbGetStatement(ll)
dbHasCompleted(ll)


#CHANGE LU STYLE
#display.brewer.all()
lu<-lu1
lu<-ratify(lu)
rat<- levels(lu)[[1]]
z<-as.data.frame(c(lookup_bh[2], lookup_bh[1]))
rat$landcover<-merge((levels(lu)),z,by="ID")
levels(lu)<- rat$landcover


#CREATE INTERACTIVE PLOT
graph="interactive_plot.svg"

i <- cbind(sumtab1$x,sumtab1$y)
j <- SpatialPoints(i)
spplot(j,pch=1,edge.col="black")

doc.tecimap = svgPlot(
{ 
  par(mfrow=c(1,2))
  plot(Cum.Sum~teci, data = sumtab1, type="p", col = "black",xlim=c(0,100), ylim=c(0,100),xlab="TECI (%)", ylab="Habitat Proportion (%)")
  plot(lu)
  plot(j,pch=1,add=TRUE)
}, width=12,height=6
)

script = system.file("JavaScript","link.js",package = "SVGAnnotation")
addECMAScripts(doc.tecimap, script, insertJS=TRUE)

doc.link = svgPlot(
{
  par(mfrow=c(1,2))
  plot(Cum.Sum~teci, data = sumtab1, type="p", col = "black", xlim=c(0,100), ylim=c(0,100),xlab="TECI (%)", ylab="Habitat Proportion (%)")
  plot(j,pch=1)
}, width=12,height=6
)

polygonPath = getPlotPoints(doc.link)
addToolTips(polygonPath[[1]],paste("ID: ",sumtab1$ID,", Cumulative Habitat(%) : ",sumtab1$Cum.Sum,", Teci(%) : ",sumtab1$teci))
addToolTips(polygonPath[[2]],paste("ID: ",sumtab1$ID,", Cumulative Habitat(%) : ",sumtab1$Cum.Sum,", Teci(%) : ",sumtab1$teci))
linkPlots(doc.link)

lp1<-xmlRoot(doc.tecimap)
lp2<-xmlRoot(doc.link)

kid1 <- xmlChildren(lp1[[4]])
kid2 <- xmlChildren(lp2[[4]])
length(kid1)
length(kid2)

lp1[[4]][[2]] <- lp2[[4]][[2]]
lp1[[4]][[length(kid1)]] <- lp2[[4]][[length(kid2)-1]]

saveXML(lp1, file.path(getwd(),graph))


#WRITE REPORT
report<-paste("Land Use Planning for Multiple Environmental Services
========================================================
***
              
# Lembar hasil analisis QUES-B:
# keanekaragaman hayati pada skala bentang lahan
              
***
              
***
# Peta penutupan lahan `r location` tahun `r period`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(lu, col.regions=rainbow)
```
              
***
              
              
# Peta Total Edge Contrast Index (TECI) `r location` tahun `r period`
*dalam %*
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(mwfile, col.regions=function(x)rev(topo.colors(x)))
```
              
***
              
# Grafik Degree of Integration of Focal Area (DIFA) `r location` tahun `r period`
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plotbio)
```
# Luas area dibawah kurva DIFA `r AUC2` %
              
***
              
<object type='image/svg+xml' data='interactive_plot.svg'>Your browser does not support SVG</object>
***")

write(report,file="reporthtml.Rmd")

knit2html("reporthtml.Rmd", options=c("use_xhml"))
