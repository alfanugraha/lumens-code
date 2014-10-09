##[QUES]=group
##working_directory=folder
##landuse_1=raster
##landuse_2=raster
##zone_l=raster
##period1=number 2000
##period2=number 2005
##location=string
##lookup_lc=file
##lookup_zo=file
##luchg=output raster
##proj_prop=output table
##data_merge_sel=output table
##Ov_chg=output table
##cross_temp.melt.cast=output table
##luchg_att=output table
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
library(spatial.tools)
library(knitr)
library(markdown)
library(pander)

#set project properties
setwd(working_directory)
Area_name=location
tab_title<-as.data.frame(Area_name)
Year_T1=period1
Year_T2=period2
Period=Year_T1-Year_T2
proj_prop<-as.data.frame(Area_name)
proj_prop$Year_T1<-Year_T1
proj_prop$Year_T2<-Year_T2
proj_prop$period <- do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))

#load datasets (land use t1, land use t2, zone)
landuse1 <- raster(landuse_1)
landuse2 <- raster(landuse_2)
zone <- raster(zone_l)

# load look up table
lookup_l<- read.table(lookup_lc, header=TRUE, sep=",",)
lookup_z <- read.table(lookup_zo, header=TRUE, sep=",",)

#Update project properties
#Data_T1<-landuse1
#Data_T2<-landuse2
#Lookup_LU<-lookup_lc
#Lookup_Zone<-lookup_zo
#proj_prop$Data_T1<-Data_T1
#proj_prop$Data_T2<-Data_T2
#proj_prop$Lookup_LU<-Lookup_LU
#proj_prop$Lookup_Zone<-Lookup_Zone

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
data_merge_sel <- data_merge[ which(data_merge$COUNT > 0),]
data_merge_sel$LU_CHG <- do.call(paste, c(data_merge_sel[c("LC_t1", "LC_t2")], sep = " to "))
lg_chg <- data_merge_sel
lg_chg$ID1<-as.numeric(lg_chg$ID_LC1)
lg_chg$ID2<-as.numeric(lg_chg$ID_LC2)
lg_chg$IDC<-lg_chg$ID1-lg_chg$ID2
lg_chg<-lg_chg[ which(lg_chg$IDC!=0),]
lg_chg <- as.data.frame(lg_chg[order(-lg_chg$COUNT),])
lg_chg$ID1<-lg_chg$ID2<-lg_chg$IDC<-NULL
lg_chg_top<-head(lg_chg, n=20)
lg_chg_top$LC_t1<-lg_chg_top$LC_t2<-NULL

chg_only<-ddply(lg_chg, 'LU_CHG', function(x) Summary=sum(x$COUNT))
chg_only<-as.data.frame(chg_only[order(-chg_only$V1),])
chg_only_top<-head(chg_only, n=10)

# calculate basic statistic
area_summary <- merge(area_lc1,area_lc2,by="ID")
Ov_chg<-as.data.frame(area_summary$CLASS_LC1)
Ov_chg$area_t1<-area_summary$COUNT_LC1
Ov_chg$area_t2<-area_summary$COUNT_LC2
colnames(Ov_chg)[1]="Land_use_type"
colnames(Ov_chg)[2]="t1_hectares"
colnames(Ov_chg)[3]="t2_hectares"
Ov_chg$Overall_change<-Ov_chg$t2_hectares-Ov_chg$t1_hectares
Ov_chg$Rate<-Ov_chg$Overall_change/(Ov_chg$t1_hectares*Period)
colnames(Ov_chg)<-c("Land_use_type", period1, period2, "Overall_change", "Rate")

Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type'), measure.vars=c(as.character(period1), as.character(period2)))
colnames(Ov_chg.melt)<-c("Land_use_type", "Year", "Area")

# create land use change map
cross_temp<-crosstab(landuse1,landuse2)
cross_temp$chkVar1<-as.numeric(is.na(cross_temp$Var1))
cross_temp$chkVar2<-as.numeric(is.na(cross_temp$Var2))
cross_temp$chkNull<-cross_temp$chkVar1+cross_temp$chkVar2
cross_temp <- cross_temp[ which(cross_temp$chkNull < 1),]
cross_temp$Var1r<-as.numeric(cross_temp$Var1)
cross_temp$Var2r<-as.numeric(cross_temp$Var2)
cross_temp$ID<-as.factor(cross_temp$Var1r+(cross_temp$Var2r*100))
colnames(cross_temp)[1] ="ID_LC1"
colnames(cross_temp)[2] = "ID_LC2"
colnames(cross_temp)[3] = "COUNT"
colnames(lookup_l)[1]="ID_LC1"
colnames(lookup_l)[2]="LC_t1"
cross_temp <- merge(cross_temp,lookup_l,by="ID_LC1")
colnames(lookup_l)[1]="ID_LC2"
colnames(lookup_l)[2]="LC_t2"
cross_temp <- as.data.frame(merge(cross_temp,lookup_l,by="ID_LC2"))
cross_temp$LU_CHG <- do.call(paste, c(cross_temp[c("LC_t1", "LC_t2")], sep = " to "))
luchg<-overlay(landuse1,landuse2,fun=function(x,y){return(x+(y*100))})
luchg<-ratify(luchg, filename='luchg.grd',count=TRUE,overwrite=TRUE)
levels(luchg)<-merge((levels(luchg)),cross_temp,by="ID")
luchg_att<-as.data.frame(levels(luchg))

#create land use transition matrix
cross_temp.melt <- melt(data = cross_temp, id.vars=c('LC_t1','LC_t2'), measure.vars=c('COUNT'))
cross_temp.melt.cast <- dcast(data = cross_temp.melt, formula = LC_t1 ~ LC_t2, fun.aggregate = sum)

write.table(proj_prop, "Pre_QUES_Project_properties.txt", sep="\t")
write.table(data_merge_sel, "land_use_change_database.txt", sep="\t")
write.table(Ov_chg, "Overall_change.txt", sep="\t")
write.table(cross_temp.melt.cast, "LU_transition_matrix.txt", sep="\t")
writeRaster(luchg, filename="lulcc_map.tif", format="GTiff", overwrite=TRUE)
write.table(luchg_att, "lulcc_map.txt", sep="\t")

report<-paste("
Land Use Planning for Multiple Environmental Services
========================================================
***

# Lembar hasil analisis Pre-QuES:
# Analisis Perubahan Penggunaan Lahan

***

***
# Peta penutupan lahan `r location` tahun `r period1`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(landuse1, col.regions=rainbow)
```

***

# Peta penutupan lahan `r location` tahun `r period2`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(landuse2, col.regions=rainbow)
```
***

# Peta unit perencanaan `r location`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(zone, col.regions=rainbow)
```

***


# Peta perubahan lahan di `r location` tahun `r period1` - `r period2`

```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(luchg, col.regions=rainbow)
```

***
#  Grafik perubahan tipe penutupan lahan  di `r location` tahun `r period1` - `r period2`

```{r fig.width=20, fig.height=20, echo=FALSE}
ov.change.plot

```
***
# Tabel intisari perubahan tipe penutupan lahan

```{r fig.width=20, fig.height=15, echo=FALSE}
pandoc.table(Ov_chg)

```
***

# Sepuluh jenis perubahan penutupan lahan terluas di `r location` tahun `r period1` - `r period2`
```{r fig.width=20, fig.height=9, echo=FALSE}
ggplot(chg_only_top, aes(LU_CHG,V1))+geom_bar(stat='identity',position='dodge')+theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+theme(text = element_text(size=30))

```
***
# Perubahan penutupan lahan terluas berdasarkan unit perencanaan di `r location` tahun `r period1` - `r period2`
```{r fig.width=25, fig.height=9, echo=FALSE}
ggplot(lg_chg_top,aes(LU_CHG,COUNT,fill=Z_NAME))+geom_bar(stat='identity',position='dodge')+theme(axis.text.x= element_text(angle=360,hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+theme(text = element_text(size=30))

```
***

# Tabel perubahan penutupan lahan terluas berdasarkan unit perencanaan di `r location` tahun `r period1` - `r period2`
```{r fig.width=25, fig.height=9, echo=FALSE}
pandoc.table(lg_chg_top_mod)

```
***

# Matriks perubahan penggunaan lahan di `r location` tahun `r period1` - `r period2`
```{r fig.width=25, fig.height=9, echo=FALSE}
pandoc.table(cross_temp.melt.cast)

```

***
*dokumen ini secara otomatis dibuat oleh LUMENS*

*World Agroforestry Centre, 2014*
")

#WRITE REPORT
write(report,file="reporthtml.Rmd")
knit2html("reporthtml.Rmd", options=c("use_xhml"))
