##[QUES]=group
##Wdir=folder
##Aname=string Bungo
##period1=number 2000
##period2=number 2005
##lu1=raster
##lu2=raster
##zonel=raster
#lu_key=file
#lu_lut=file
#lu_leg=file
##lu_landuse=file
##lu_zone=file
##lu_reclass=file
##lu_trajectories_final=output raster
##PreQUES_traj_database=output table
##Overall_trajectories=output table
##Zone_trajectories=output table
##passfilenames
##report

library(raster)
library(rasterVis)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(spatial.tools)
library(markdown)
library(knitr)

#set project properties
setwd(Wdir)
Area_name=Aname
tab_title<-as.data.frame(Aname)
Year_T1=period1
Year_T2=period2
Period=Year_T1-Year_T2
proj_prop<-as.data.frame(Aname)
proj_prop$Year_T1<-Year_T1
proj_prop$Year_T2<-Year_T2
proj_prop$period <- do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))

#load datasets (land use t1, land use t2, zone)
landuse1 <- raster(lu1)
landuse2 <- raster(lu2)
zone <- raster(zonel)

# load look up table (internal to PRE-QUES)
#lookup_traj<-read.table(lu_key, header=TRUE, sep=",")
#name_traj<-read.table(lu_lut, header=TRUE, sep=",")
#leg_traj<-read.table(lu_leg, header=TRUE, sep=",")

#substitute lookup table internal
trj<-c(11:17,22:27,32:37,42:44,46:47,52:57,62:67,77,88)
lookup_traj<-as.data.frame(trj)
remove(trj)
lookup_traj$Traj<-c("Stable natural forest","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to infrastructure","Other")
name_traj<-lookup_traj
name_traj$ID_trf<-c(5,3,7,6,1,4,2,3,7,6,1,4,2,8,7,6,1,4,2,8,7,6,4,2,8,7,6,1,4,2,8,7,6,1,4,2,2,9)
ID_T<-c(1:9)
leg_traj<-as.data.frame(ID_T)
remove(ID_T)
leg_traj$Trajectories<-c("Loss to cropland","Loss to infrastructure","Loss to logged-over forest","Loss to bare land and abandoned","Stable natural forest","Recovery to agroforest","Recovery to tree cropping","Recovery to forest","Other")

#load look up table (user input)
lookup_l<- read.table(lu_landuse, header=TRUE, sep=",",)
lookup_z <- read.table(lu_zone, header=TRUE, sep=",",)
lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")

#Update project properties
Data_T1<-lu1
Data_T2<-lu2
Lookup_LU<-lu_landuse
Lookup_Zone<-lu_zone
proj_prop$Data_T1<-Data_T1
proj_prop$Data_T2<-Data_T2
proj_prop$Lookup_LU<-Lookup_LU
proj_prop$Lookup_Zone<-Lookup_Zone

#set same extent
landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
zone<-spatial_sync_raster(zone, landuse1, method = "ngb")

# set raster attribute table (RAT)
landuse1<-ratify(landuse1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
landuse2<-ratify(landuse2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
zone<-ratify(zone, filename='ratify.grd',count=TRUE,overwrite=TRUE)

#create land use change database
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
area<-sum(area_zone$COUNT)
levels(landuse1)<-merge((levels(landuse1)),lookup_l,by="ID")
levels(landuse2)<-merge((levels(landuse2)),lookup_l,by="ID")
levels(zone) <- merge(area_zone,lookup_z,by="ID")
area_lc1<-as.data.frame(levels(landuse1))
area_lc2<-as.data.frame(levels(landuse2))
area_zone<-as.data.frame(levels(zone))
colnames(area_lc1)[2] = "COUNT_LC1"
colnames(area_lc1)[3] = "CLASS_LC1"
colnames(area_lc2)[2] = "COUNT_LC2"
colnames(area_lc2)[3] = "CLASS_LC2"
cross <- as.data.frame(crosstab((stack(landuse1,landuse2,zone))))
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
data_merge <- merge(cross,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_l,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))

#create trajectories database
colnames(lookup_lr)[1]="ID_LC1"
colnames(lookup_lr)[2]="CLASS1"
colnames(lookup_lr)[3]="ID_L1"
data_merge_tr<-as.data.frame(merge(data_merge,lookup_lr, by="ID_LC1"))
colnames(lookup_lr)[1]="ID_LC2"
colnames(lookup_lr)[2]="CLASS2"
colnames(lookup_lr)[3]="ID_L2"
data_merge_tr<-as.data.frame(merge(data_merge_tr,lookup_lr, by="ID_LC2"))
data_merge_tr$CLASS1<-data_merge_tr$CLASS2<-NULL
data_merge_tr$T1<-data_merge_tr$ID_L1*10
data_merge_tr$T2<-data_merge_tr$ID_L2
data_merge_tr$TR<-data_merge_tr$T1+data_merge_tr$T2
colnames(lookup_traj)[1]="TR"
PreQUES_traj_database<-as.data.frame(merge(data_merge_tr,lookup_traj, by="TR"))

#create trajectories map
landuse_tr1<-landuse1
landuse_tr2<-landuse2
lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
levels(landuse_tr1)<-merge((levels(landuse_tr1)),lookup_lr, by="ID")
levels(landuse_tr2)<-merge((levels(landuse_tr2)),lookup_lr, by="ID")
landuse_tr1<-deratify(landuse_tr1,'ID_L')
landuse_tr2<-deratify(landuse_tr2,'ID_L')
lu_trajectories<-overlay(landuse_tr1,landuse_tr2,fun=function(x,y){return((x*10)+y)})
lu_trajectories<-ratify(lu_trajectories, filename='lu_trajectories.grd',count=TRUE,overwrite=TRUE)
colnames(name_traj)[1]="ID"
levels(lu_trajectories)<-merge((levels(lu_trajectories)),name_traj,by='ID')
lu_trajectories_final<-deratify(lu_trajectories,'ID_trf')
lu_trajectories_final<-ratify(lu_trajectories_final, filename='lu_trajectories_final.grd',count=TRUE,overwrite=TRUE)
colnames(leg_traj)[1]="ID"
levels(lu_trajectories_final)<-merge((levels(lu_trajectories_final)),leg_traj,by='ID')

#calculate summary statistics by zone and overall
PreQUES_traj_database.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Traj'), measure.vars=c('COUNT'))
PreQUES_traj_database.zone <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ Z_NAME, fun.aggregate = sum)
PreQUES_traj_database.overal <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ ., fun.aggregate = sum)

#plot trajectories map
f_lu1<-levelplot(lu_trajectories_final,att='Trajectories',col.regions=rainbow, main=paste("Land Use Trajectories Map",Area_name,Year_T1,"-",Year_T2), scales=list(x=list(cex=0.4),y=list(cex=0.4)))
colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","variable", "Area"); #rename column names
plot_traj<-ggplot(PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+theme(text = element_text(size=15))
plot_traj


colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")


#export data
Overall_trajectories<-PreQUES_traj_database.overal
Zone_trajectories<-PreQUES_traj_database.zone
write.dbf(PreQUES_traj_database, "PreQUES_traj_database.dbf")
write.dbf(PreQUES_traj_database.overal, "Overall_trajectories.dbf")
write.table(PreQUES_traj_database.zone, "Zone_trajectories.csv", sep="\t")
writeRaster(lu_trajectories_final, filename="lulcc_trajectories_map.tif", format="GTiff", overwrite=TRUE)


report<-paste("
Land Use Planning for Multiple Environmental Services
========================================================
***

# Lembar hasil analisis Pre-QuES:
# Analisis Perubahan Penggunaan Lahan

***

***
# Peta kelompok perubahan penutupan lahan `r location` tahun `r period1`
```{r fig.width=10, fig.height=9, echo=FALSE}
f_lu1
```

***

# Grafik kelompok perubahan penutupan lahan `r location` tahun `r period2` berdasarkan unit perencanaan
```{r fig.width=10, fig.height=9, echo=FALSE}
plot_traj
```
***

# Luas area kelompok perubahan penutupan lahan `r location` tahun `r period2`
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(PreQUES_traj_database.overal)
```

***

# Tabel luas area kelompok perubahan lahan di `r location` tahun `r period1` - `r period2` tiap unit perencanaan

```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(PreQUES_traj_database.zone)
```

***
*dokumen ini secara otomatis dibuat oleh LUMENS,*
*World Agroforestry Centre, 2014*
")

#WRITE REPORT
write(report,file="reporthtml.Rmd")
knit2html("reporthtml.Rmd", options=c("use_xhml"))
