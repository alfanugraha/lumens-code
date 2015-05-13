##[QUES]=group
##Wdir=folder
##location=string Merangin
##period1=number 2000
##period2=number 2005
##lu1=raster
##lu2=raster
##zonel=raster
##lu_landuse=file
##lu_zone=file

##lu_trajectories_final=output raster
##PreQUES_traj_database=output table
##Overall_trajectories=output table
##Zone_trajectories=output table
##passfilenames


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
library(rtf)
library(dplyr)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#set project properties
setwd(Wdir)
Area_name=location
tab_title<-as.data.frame(location)
Year_T1=period1
Year_T2=period2
Period=Year_T1-Year_T2
proj_prop<-as.data.frame(location)
proj_prop$Year_T1<-Year_T1
proj_prop$Year_T2<-Year_T2
proj_prop$period <- do.call(paste, c(proj_prop[c("Year_T1", "Year_T2")], sep = " - "))

#load datasets (land use t1, land use t2, zone)
landuse1 <- raster(lu1)
landuse2 <- raster(lu2)
zone <- raster(zonel)

#load look up table (internal to PRE-QUES)
#lookup_traj<-read.table(lu_key, header=TRUE, sep=",")
#name_traj<-read.table(lu_lut, header=TRUE, sep=",")
#leg_traj<-read.table(lu_leg, header=TRUE, sep=",")

#substitute lookup table internal
trj<-c(11:17,22:27,32:37,42:44,46:47,52:57,62:67,77,88)
lookup_traj<-as.data.frame(trj)
remove(trj)
lookup_traj$Traj<-c("Stable natural forest","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to logged-over forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Recovery to forest","Recovery to tree cropping","Recovery to agroforest","Loss to cropland","Loss to bare land and abandoned","Loss to infrastructure","Loss to infrastructure","Other")
lookup_traj$Def<-c("Stable forest", "Forest degradation", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation","Stable forest", "Deforestation","Deforestation","Deforestation","Deforestation","Deforestation", "Reforestation", "Other","Other","Other","Other","Reforestation", "Other","Other","Other","Other","Reforestation","Other","Other","Other","Other","Other","Reforestation", "Other","Other","Other","Other","Other","Other","Other", "Others")
name_traj<-lookup_traj
name_traj$ID_trf<-c(5,3,7,6,1,4,2,3,7,6,1,4,2,8,7,6,1,4,2,8,7,6,4,2,8,7,6,1,4,2,8,7,6,1,4,2,2,9)
ID_T<-c(1:9)
leg_traj<-as.data.frame(ID_T)
remove(ID_T)
leg_traj$Trajectories<-c("Loss to cropland","Loss to infrastructure","Loss to logged-over forest","Loss to bare land and abandoned","Stable natural forest","Recovery to agroforest","Recovery to tree cropping","Recovery to forest","Other")

lu_class<-c(1,2,3,4,5,6,7,8)
lu_class<-as.data.frame(lu_class)
lu_class$Rec_LU<-c("Hutan primer", "Hutan sekunder", "Tanaman pohon monokultur", "Tanaman pohon campuran", "Tanaman pertanian semusim", "Semak, rumput dna lahan terbuka", "Pemukiman", "Lain-lain")


#load look up table (user input)
lookup_l<- read.table(lu_landuse, header=TRUE, sep=",",)
lookup_z <- read.table(lu_zone, header=TRUE, sep=",",)
#lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")

lookup_l2<-lookup_l
lookup_l2$ID_L<-0
countrow<-nrow(lookup_l2)
x<-rep("---", times=countrow)
xx<-x
lookup_lr_edit<-cbind(lookup_l2,x,xx)
xxx<-rep("---", times=countrow-(nrow(lu_class)))
xxxx<-xxx
V<-cbind(xxx,xxxx)
colnames(V)[1]<-"lu_class"
colnames(V)[2]<-"Rec_LU"
lookup_lr_edit2<-rbind(lu_class,V)
#lookup_lr_edit3<-cbind(lookup_lr_edit2, x,xx)
lookup_lr<-cbind(lookup_lr_edit, lookup_lr_edit2)
lookup_lr<-edit(lookup_lr)
lookup_lr<-subset(lookup_lr, select=c(1, 2, 3))


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
#landuse1<-ratify(landuse1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
#landuse2<-ratify(landuse2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
#zone<-ratify(zone, filename='ratify.grd',count=TRUE,overwrite=TRUE)

#create land use change database
#area_lc1<-as.data.frame(levels(landuse1))
#area_lc2<-as.data.frame(levels(landuse2))
#area_zone<-as.data.frame(levels(zone))
#area<-min(sum(area_zone$COUNT), sum(area_lc1$COUNT), sum(area_lc2$COUNT))
#levels(landuse1)<-merge((levels(landuse1)),lookup_l,by="ID")
#levels(landuse2)<-merge((levels(landuse2)),lookup_l,by="ID")
#levels(zone) <- merge(area_zone,lookup_z,by="ID")
#area_lc1<-as.data.frame(levels(landuse1))
#area_lc2<-as.data.frame(levels(landuse2))
#area_zone<-as.data.frame(levels(zone))
#colnames(area_lc1)[2] = "COUNT_LC1"
#colnames(area_lc1)[3] = "CLASS_LC1"
#colnames(area_lc2)[2] = "COUNT_LC2"
#colnames(area_lc2)[3] = "CLASS_LC2"
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
PreQUES_traj_database$Traj_Code<-toupper(abbreviate(PreQUES_traj_database$Traj))
colnames(lookup_lr)[1]="ID"

lookup_l<-lookup_lr; #lu table 
lookup_l[3]<-NULL

cross_temp<-data.frame(Var1=PreQUES_traj_database$ID_LC1, Var2=PreQUES_traj_database$ID_LC2, Var3=PreQUES_traj_database$ZONE, Freq=PreQUES_traj_database$COUNT)

#alpha beta table loop.
for(i in 1:nrow(lookup_z)){
  zone_id<-lookup_z$ZONE[i]
  print(zone_id)
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"<-na.omit(cross_temp[ which(cross_temp$Var3==",zone_id,"),])", sep=''))))
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"$Var1<-as.numeric(as.character(unlist((cross_temp_zone_",zone_id,"$Var1))))", sep=''))))
  
  colnames(lookup_l)[1] = "Var1"
  colnames(lookup_l)[2] = "LC1"
  
  eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l,by='Var1', all=T)", sep=''))))
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"$Var2<-as.numeric(as.character(unlist((cross_temp_zone_",zone_id,"$Var2))))", sep=''))))
  
  colnames(lookup_l)[1] = "Var2"
  colnames(lookup_l)[2] = "LC2"
  eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l,by='Var2', all=T)", sep=''))))
  
  eval(parse(text=( paste("print(head(cross_temp_zone_",zone_id,'))', sep=''))));#matrix output checking per zone
  
  #eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l,by='Var2', all=T)", sep=''))));#exactly the same
  
  eval(parse(text=( paste('cross_temp.melt_', zone_id," <-melt(data = cross_temp_zone_",zone_id,",id.vars=c('Var1','Var2'), measure.vars=c('Freq'))", sep=''))))
  eval(parse(text=( paste('cross_temp.melt.cast_', zone_id," <-dcast(data = cross_temp.melt_",zone_id,", formula = Var1 ~ Var2, fun.aggregate = sum)", sep=''))))
  eval(parse(text=( paste('cross_temp.melt.cast_', zone_id," [1]<-NULL", sep=''))))
  #eval(parse(text=( paste('cross_temp.melt_', zone_id," <-cross_temp.melt_", zone_id,"*",Spat_res, sep=''))))
  
  
  #write transition matrix file
  filename<-paste('transition_matrix_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  eval(parse(text=( paste("write.csv(cross_temp.melt.cast_",zone_id,", filename, row.names=F)",sep=''))))
  
  #Alpha Betha table NORMALIZED
  #calculate alpha
  eval(parse(text=( paste("a.table<-cross_temp.melt.cast_", zone_id ,sep=''))))
  eval(parse(text=( paste("tot.col<-colSums(cross_temp.melt.cast_", zone_id ,")",sep=''))))
  
  
  for(x in 1:ncol(a.table)){
    eval(parse(text=(paste("a.table[", x, "]<-a.table[",x,"]/tot.col[",x,"]", sep=""))))
  }
  a.table[is.na(a.table)] <- 0
  a.table$"NA"<-NULL ;#remove NA column
  a.table<-a.table[-nrow(a.table),];#remove last NA row
  a.table<-do.call(data.frame,lapply(a.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  
  a.val<-rowSums(a.table)
  
  #calculate betha
  eval(parse(text=( paste("b.table<-cross_temp.melt.cast_", zone_id ,sep=''))))
  eval(parse(text=( paste("tot.row<-rowSums(cross_temp.melt.cast_", zone_id ,")",sep=''))))
  
  for(y in 1:ncol(b.table)){
    eval(parse(text=(paste("b.table[", y, ",]<-b.table[",y,",]/tot.row[",y,"]", sep=""))))
  }
  b.table[is.na(b.table)] <- 0
  b.table<-do.call(data.frame,lapply(b.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  b.table$"NA."<-NULL
  b.table <- b.table[-nrow(b.table),];#remove last NA row
  b.val<-colSums(b.table)
  
  
  #Bind alpha and betha
  eval(parse(text=( paste("id<-na.omit(as.integer(colnames(cross_temp.melt.cast_", zone_id ,")))",sep=''))))
  fromto.table<-cbind(na.omit(id),a.val,b.val)
  colnames(fromto.table)<-c("ID","a.val","b.val")
  colnames(lookup_l)[1] = "ID"
  colnames(lookup_l)[2] = "LC"
  fromto.table<-merge(lookup_l,fromto.table,by='ID')
  #write from to table
  dir.name<-paste('Output_tables_and_graph_',location,'_',period1,'_',period2,'_normalized',sep='')#change working directory
  dir.create(file.path(getwd(), dir.name), showWarnings = FALSE)
  setwd(file.path(getwd(), dir.name))
  
  filename<-paste('from_to_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  write.csv(fromto.table, filename, row.names=F)
  
  fromtoplot<-ggplot(fromto.table, aes(x=as.numeric(as.character(fromto.table$a.val)), y=as.numeric(as.character(fromto.table$b.val)),xend=6, yend=6)) + geom_point(shape=1,size=2) + 
    geom_text(size=3, aes(label=fromto.table$LC),hjust=1,vjust=-1, angle =0)+ 
    labs(x = as.character(paste('Alpha',period1,'-', period2)), y=as.character(paste('Betha',period1,'-', period2))) +geom_vline(xintercept = 1)+geom_hline(yintercept=1)+ggtitle(paste(location,period1,'-', period2,', Zone',lookup_z$ZONE[i],'(',lookup_z$Z_NAME[i],')' ))
  
  #fromtoplot
  eval(parse(text=( paste("fromtoplot_zone_",zone_id,'<-fromtoplot', sep=''))));#save plots
  
  filename<-paste('graph_from_to_',location,'_',period1,'_',period2,'_zone',zone_id,'_normalized.png',sep='')
  ggsave(fromtoplot, file=filename, width=20, height=20, units="cm")
  
  setwd(Wdir)
  #---------------------------------------------------------------------------
  #Alpha Betha table NORMAL
  #calculate alpha
  eval(parse(text=( paste("a.table.true<-cross_temp.melt.cast_", zone_id ,sep=''))))
  
  a.table.true[is.na(a.table.true)] <- 0
  a.table.true$"NA"<-NULL ;#remove NA column
  a.table.true<-a.table.true[-nrow(a.table.true),];#remove last NA row
  a.table.true<-do.call(data.frame,lapply(a.table.true, function(x) replace(x, is.infinite(x),0)));#replace Inf
  
  a.val<-rowSums(a.table.true)
  
  #calculate betha
  eval(parse(text=( paste("b.table.true<-cross_temp.melt.cast_", zone_id ,sep=''))))
  
  b.table.true[is.na(b.table.true)] <- 0
  b.table.true<-do.call(data.frame,lapply(b.table.true, function(x) replace(x, is.infinite(x),0)));#replace Inf
  b.table.true$"NA."<-NULL
  b.table.true <- b.table.true[-nrow(b.table.true),];#remove last NA row
  b.val<-colSums(b.table.true)
  
  
  #Bind alpha and betha
  eval(parse(text=( paste("id<-na.omit(as.integer(colnames(cross_temp.melt.cast_", zone_id ,")))",sep=''))))
  fromto.table.true<-cbind(na.omit(id),a.val,b.val)
  colnames(fromto.table.true)<-c("ID","a.val","b.val")
  fromto.table.true<-merge(lookup_l,fromto.table.true,by='ID')
  #write from to table
  dirname<-paste('Output_tables_and_graph_',location,'_',period1,'_',period2,sep='')#change working directory
  dir.create(file.path(getwd(), dirname), showWarnings = FALSE)
  setwd(file.path(getwd(), dirname))
  
  filename<-paste('from_to_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  write.csv(fromto.table.true, filename, row.names=F)
  
  fromtoplot.true<-ggplot(fromto.table.true, aes(x=as.numeric(as.character(fromto.table.true$a.val)), y=as.numeric(as.character(fromto.table.true$b.val)),xend=6, yend=6)) + geom_point(shape=1,size=2) + 
    geom_text(size=3, aes(label=fromto.table.true$LC),hjust=1,vjust=-1, angle =0)+ 
    labs(x = as.character(paste('Initial',period1,'-', period2)), y=as.character(paste('Final',period1,'-', period2))) +ggtitle(paste(location,period1,'-', period2,', Zone',lookup_z$ZONE[i],'(',lookup_z$Z_NAME[i],')' ))
  #+geom_vline(xintercept =remark.tres)+geom_hline(yintercept=remark.tres)
  #fromtoplot.true
  filename<-paste('graph_from_to_',location,'_',period1,'_',period2,'_zone',zone_id,'.png',sep='')
  eval(parse(text=( paste("fromtoplot.true_zone_",zone_id,'<-fromtoplot.true', sep=''))));#save plots
  ggsave(fromtoplot.true, file=filename, width=20, height=20, units="cm")
  setwd(Wdir)
  eval(parse(text=( paste("rm(cross_temp.melt.cast_", zone_id,")" ,sep=''))))
  eval(parse(text=( paste("rm(cross_temp.melt_", zone_id,")" ,sep=''))))
  eval(parse(text=( paste("rm(cross_temp_zone_", zone_id,")" ,sep=''))))
}


#LAND COVER CHAGE TRAJECTORY INDEXES
#filter function
#PREQUES_filter<-function(idlc1, idlc2, filter1, filter2) {
#  eval(parse(text=( paste("PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1",filter1,idlc1,',','ID_L2',filter2,idlc2,')', sep=''))))
  #PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1 == idlc1, ID_L2 == idlc2)
#  PreQUES_traj_database.filtered<-filter(PreQUES_traj_database.filtered, COUNT!=0)
#  PreQUES_traj_database.filtered <- aggregate(COUNT ~  Traj+Z_NAME, data=PreQUES_traj_database.filtered, FUN=sum)
 # return(PreQUES_traj_database.filtered)
#}

#index.forest.degrad<-PREQUES_filter(1,2,'==','==')
#index.deforest<-PREQUES_filter(c(1,2),c(1,2),'==','!=')
#index.agric.exp<-PREQUES_filter(5,5,'!=','==')
#index.urban<-PREQUES_filter(7,7,'!=','==')
#index.agroforestation<-PREQUES_filter(4,4,'!=','==')
#index.estate.exp<-PREQUES_filter(3,3,'!=','==')
#index.degraded<-PREQUES_filter(6,6,'!=','==')
#index.stableforest<-PREQUES_filter(1,1,'==','==')

#Calculate deforestation, degradation, and reforestation rate
PREQUES_filter_2<-function(idlc1, idlc2, filter1, filter2) {
  eval(parse(text=( paste("PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1",filter1,as.characteridlc1,',','ID_L2',filter2,idlc2,')', sep=''))))
  #PreQUES_traj_database.filtered<-filter(PreQUES_traj_database,ID_L1 == idlc1, ID_L2 == idlc2)
  PreQUES_traj_database.filtered<-filter(PreQUES_traj_database.filtered, COUNT!=0)
  PreQUES_traj_database.filtered <- aggregate(COUNT ~  Z_NAME, data=PreQUES_traj_database.filtered, FUN=sum)
  return(PreQUES_traj_database.filtered)
}

#1 Deforestation
#index.deforest<-PREQUES_filter_2(c(1,2),c(1,2),'==','!=')
index.deforest<-filter(PreQUES_traj_database,ID_L1==c(1,2),ID_L2!=c(1,2))
index.deforest<-filter(index.deforest, COUNT!=0)
index.deforest <- aggregate(COUNT ~  Z_NAME, data=index.deforest, FUN=sum)
colnames(index.deforest)<-c('ZONE', 'Deforestasi')
total.deforest<-data.frame(ZONE="TOTAL",Deforestasi=sum(index.deforest[,2]))
index.deforest<-rbind(index.deforest,total.deforest)

#2 Degradasi Hutan
index.forest.degrad<-filter(PreQUES_traj_database,ID_L1==1,ID_L2==c(2))
index.forest.degrad<-filter(index.forest.degrad, COUNT!=0)
index.forest.degrad <- aggregate(COUNT ~  Z_NAME, data=index.forest.degrad, FUN=sum)
colnames(index.forest.degrad)<-c('ZONE', 'Degradasi_Hutan')
total.forest.degrad<-data.frame(ZONE="TOTAL",Degradasi_Hutan=sum(index.forest.degrad[,2]))
index.forest.degrad<-rbind(index.forest.degrad,total.forest.degrad)


tryCatch({
#3 Reforestasi
index.reforest<-filter(PreQUES_traj_database,ID_L1==c(3,4,5,6,7,8),ID_L2==c(1,2))
index.reforest<-filter(index.reforest, COUNT!=0)
index.reforest <- aggregate(COUNT ~  Z_NAME, data=index.reforest, FUN=sum)
colnames(index.reforest)<-c('ZONE', 'Reforestasi')
total.reforest<-data.frame(ZONE="TOTAL",Reforestasi=sum(index.reforest[,2]))
index.reforest<-rbind(index.reforest,total.reforest)
},error=function(e){cat("No reforestation found",conditionMessage(e), "\n")})

#4 Stable Forest
#index.stable.forest<-PREQUES_filter_2(1,1,'==','==')
#colnames(index.stable.forest)<-c('ZONE', 'Tetap_Hutan')
#total.stable.forest<-data.frame(ZONE="TOTAL",Tetap_Hutan=sum(index.stable.forest[,2]))
#index.stable.forest<-rbind(index.stable.forest,total.stable.forest)

#5 Initial Forest Cover
index.init.forest<-filter(PreQUES_traj_database,ID_L1==1)
index.init.forest<-filter(index.init.forest, COUNT!=0)
index.init.forest <- aggregate(COUNT ~  Z_NAME, data=index.init.forest, FUN=sum)
colnames(index.init.forest)<-c('ZONE', 'Forest_T1')
total.init.forest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forest[,2]))
index.init.forest<-rbind(index.init.forest,total.init.forest)

#6 Total forest cover
index.init.forestandlogged<-filter(PreQUES_traj_database,ID_L1==c(1,2))
index.init.forestandlogged<-filter(index.init.forestandlogged, COUNT!=0)
index.init.forestandlogged <- aggregate(COUNT ~  Z_NAME, data=index.init.forestandlogged, FUN=sum)
colnames(index.init.forestandlogged)<-c('ZONE', 'Forest_T1')
total.init.forestandlogged<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.forestandlogged[,2]))
index.init.forestandlogged<-rbind(index.init.forestandlogged,total.init.forestandlogged)

#7 Initial non forest
index.init.nonforest<-filter(PreQUES_traj_database,ID_L1!=c(1,2))
index.init.nonforest<-filter(index.init.nonforest, COUNT!=0)
index.init.nonforest <- aggregate(COUNT ~  Z_NAME, data=index.init.nonforest, FUN=sum)
colnames(index.init.nonforest)<-c('ZONE', 'Forest_T1')
total.init.nonforest<-data.frame(ZONE="TOTAL",Forest_T1=sum(index.init.nonforest[,2]))
index.init.nonforest<-rbind(index.init.nonforest,total.init.nonforest)

#Degradation rate *********
degrad.rate<-merge(index.forest.degrad, index.init.forest, by='ZONE', all=T)
degrad.rate$Degradation_Rate<-(degrad.rate[2]/degrad.rate[3]*100)[,1]
degrad.rate<-cbind(degrad.rate[1], round(degrad.rate[4],2))

#Deforestation rate*********
deforest.rate<-merge(index.deforest, index.init.forestandlogged, by='ZONE', all=T)
deforest.rate$Deforestation_Rate<-(deforest.rate[2]/deforest.rate[3]*100)[,1]
deforest.rate<-cbind(deforest.rate[1], round(deforest.rate[4],2))

tryCatch({
#Reforestation rate***********************
reforest.rate<-merge(index.reforest, index.init.nonforest, by='ZONE', all=T)
reforest.rate$Reforestation_Rate<-(reforest.rate[2]/reforest.rate[3]*100)[,1]
reforest.rate<-cbind(reforest.rate[1], round(reforest.rate[4],2))
},error=function(e){cat("No reforestation found",conditionMessage(e), "\n")})

#SUMMARY FOREST CHANGE IN %
forest.change.rate<-merge(deforest.rate, degrad.rate,by='ZONE',all=T)
if(ncol(reforest.rate)==2){
  forest.change.rate<-merge(forest.change.rate, reforest.rate,by='ZONE',all=T)
  } else {print('no reforestation found')}


#create trajectories map
landuse_tr1<-landuse1
landuse_tr2<-landuse2
landuse_tr1<- reclassify(landuse_tr1, cbind(128,NA))
landuse_tr2<- reclassify(landuse_tr2, cbind(128,NA))
landuse_tr1<-ratify(landuse_tr1, filename='landuse1.grd',count=TRUE,overwrite=TRUE)
landuse_tr2<-ratify(landuse_tr2, filename='landuse2.grd',count=TRUE,overwrite=TRUE)
#lookup_lr<-read.table(lu_reclass, header=TRUE, sep=",")
colnames(lookup_lr)[3]="ID_L1"
levels(landuse_tr1)<-merge((levels(landuse_tr1)),lookup_lr, by="ID")
colnames(lookup_lr)[3]="ID_L2"
levels(landuse_tr2)<-merge((levels(landuse_tr2)),lookup_lr, by="ID")
landuse_tr1<-deratify(landuse_tr1,'ID_L1')
landuse_tr2<-deratify(landuse_tr2,'ID_L2')
lu_trajectories<-overlay(landuse_tr1,landuse_tr2,fun=function(x,y){return((x*10)+y)})
lu_trajectories<-ratify(lu_trajectories, filename='lu_trajectories.grd',count=TRUE,overwrite=TRUE)
colnames(name_traj)[1]="ID"
levels(lu_trajectories)<-merge((levels(lu_trajectories)),name_traj,by='ID')
lu_trajectories_final<-deratify(lu_trajectories,'ID_trf')
lu_trajectories_final<-ratify(lu_trajectories_final, filename='lu_trajectories_final.grd',count=TRUE,overwrite=TRUE)
colnames(leg_traj)[1]="ID"
levels(lu_trajectories_final)<-merge((levels(lu_trajectories_final)),leg_traj,by='ID')

#calculate summary statistics by zone and overall
PreQUES_traj_database.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Traj', 'Traj_Code'), measure.vars=c('COUNT'))
PreQUES_traj_database.zone <- dcast(data = PreQUES_traj_database.melt, formula = Z_NAME ~ Traj_Code, fun.aggregate = sum)
PreQUES_traj_database.overal <- dcast(data = PreQUES_traj_database.melt, formula = Traj ~ ., fun.aggregate = sum)

PreQUES_traj_forest.melt <- melt(data = PreQUES_traj_database, id.vars=c('Z_NAME','Def'), measure.vars=c('COUNT'))
PreQUES_traj_forest.zone <- dcast(data = PreQUES_traj_forest.melt, formula = Z_NAME ~ Def, fun.aggregate = sum)
PreQUES_traj_forest.zone$Other<-NULL
PreQUES_traj_forest.zone$Others<-NULL

PreQUES_traj_forest.overal <- dcast(data = PreQUES_traj_forest.melt, formula = Def ~ ., fun.aggregate = sum)

PreQUES_traj_drive.melt <- melt(data = PreQUES_traj_database, id.vars=c('Traj','Def'), measure.vars=c('COUNT'))
PreQUES_traj_drive.zone <- dcast(data = PreQUES_traj_drive.melt, formula = Traj ~ Def, fun.aggregate = sum)
PreQUES_traj_drive.zone$Other<-NULL
PreQUES_traj_drive.zone$Others<-NULL

#plot trajectories map
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

LU1 <- rasterToPoints(lu_trajectories_final);
LU1 <- as.data.frame(LU1)
colnames(LU1) <- c("X","Y","ID_trf")
LU1<-LU1[which(LU1$ID_trf != 0),]
lu.lab<-name_traj
LU1<-merge(LU1, lu.lab, by="ID_trf")
LU1$ID_trf<-as.factor(LU1$ID_trf)
myColors.lu <- myColors[1:length(unique(LU1$ID_trf))]
names(myColors.lu) <- unique(LU1$Traj)
ColScale.lu<-scale_fill_manual(name="Landuse Change", values = myColors.lu )

plot.LU1  <- ggplot(data=LU1) + geom_raster(aes(x=LU1$X, y=LU1$Y, fill=LU1$Traj)) +
  ColScale.lu + coord_equal() +
  ggtitle(paste("Landuse Trajectories Map of", Area_name, period1, "-", period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

colnames(PreQUES_traj_database.melt)<-c("Zone", "Trajectories","Abbrev", "variable", "Area"); #rename column names
plot_traj<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Zone),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
  theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
  theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)')+coord_flip()+
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

plot_traj_group<-ggplot(data=PreQUES_traj_database.melt,aes(factor(Trajectories),Area,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
  theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
  theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

colnames(PreQUES_traj_forest.melt)<-c("Zone", "Forest_Change","variable", "Area"); #rename column names
plot_def<-ggplot(data=PreQUES_traj_forest.melt,aes(factor(Zone),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
  theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
  theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

colnames(PreQUES_traj_drive.melt)<-c("Trajectories", "Forest_Change","variable", "Area"); #rename column names
plot_drive<-ggplot(data=PreQUES_traj_drive.melt,aes(factor(Trajectories),Area,fill=factor(Forest_Change)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
  theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
  theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))

#for(s in 2:(ncol(PreQUES_traj_database.zone))){
#  print(s)
#  PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_database.zone, id.vars=c('Trajectories'), measure.vars=c(colnames(PreQUES_traj_database.zone)[s]))
#  plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(Trajectories),value,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
#  theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
#  theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
#  theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
#  eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
#}



colnames(PreQUES_traj_database.overal)<-c("Trajectories", "Area (Ha)")
colnames(PreQUES_traj_database.zone)[1]<-c("Trajectories")
colnames(PreQUES_traj_forest.overal)<-c("Forest cover changes", "Area (Ha)")

#export data
Overall_trajectories<-PreQUES_traj_database.overal
Zone_trajectories<-PreQUES_traj_database.zone
write.dbf(PreQUES_traj_database, "PreQUES_traj_database.dbf")
write.dbf(PreQUES_traj_database.overal, "Overall_trajectories.dbf")
write.dbf(PreQUES_traj_database.zone, "Zone_trajectories.dbf")
writeRaster(lu_trajectories_final, filename="lulcc_trajectories_map.tif", format="GTiff", overwrite=TRUE)

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-Pre QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules 2: Land Use Trajectory Analysis \\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", Area_name, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
rtffile <- RTF("LUMENS_Pre-QUES_Trajectory_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Analisa perubahan tutupan lahan dilakukan untuk mengetahui kecenderungan perubahan tutupan lahan di suatu daerah pada satu kurun waktu. Analisa ini dilakukan dengan menggunakan data peta tutupan lahan pada dua periode waktu yang berbeda. Selain itu, dengan memasukkan data unit perencanaan kedalam proses analisa, dapat diketahui kecenderungan perubahan tutupan lahan pada masing-masing kelas unit perencanaan yang ada. Informasi yang dihasilkan melalui analisa ini dapat digunakan dalam proses perencanaan untuk berbagai hal. Diantaranya adalah: menentukan prioritas pembangunan, mengetahui faktor pemicu perubahan penggunaan lahan, merencanakan skenario pembangunan di masa yang akan datang, dan lain sebagainya.")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 ALUR PERUBAHAN PENGGUNAAN LAHAN\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Alur perubahan penggunaan lahan merupakan ringkasan keseluruhan tipe rangkaian perubahan penggunaan lahan yang mungkin terjadi di sebuah daerah. Kategori besar dari alur perubahan lahan dibagi menjadi dua jenis yaitu Loss of tree cover dan recovery of tree cover")
addNewLine(rtffile)

addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot.LU1)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 1. Peta Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj_group)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 2. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, sep=" "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 3. Grafik Kelompok Perubahan Penutupan Lahan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_traj)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 1. Luas Area Kelompok Perubahan Penutupan Lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_database.overal,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 2. Luas Area Kelompok Perubahan Lahan di \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 Tiap Unit Perencanaan \\b0 \\fs20", sep=" "))
addTable(rtffile,PreQUES_traj_database.zone,font.size=8)

#PLOT: TOTAL CHANGES PER TRAJECTORY
for(s in 2:(ncol(PreQUES_traj_database.zone))){
  print(s)
  c<-s-1
  PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_database.zone, id.vars=c('Trajectories'), measure.vars=c(colnames(PreQUES_traj_database.zone)[s]))
  plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(Trajectories),value,fill=factor(Trajectories)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan lahan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Lahan di Berbagai Zona Perencanaan untuk jenis ",colnames(PreQUES_traj_database.zone)[s], "\\b0 \\fs20 di ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
  addNewLine(rtffile)
}


addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs24 PERUBAHAN TUTUPAN HUTAN\\b0 \\fs24", sep=""))
addNewLine(rtffile)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Salah satu bentuk alur perubahan penggunaan lahan yang paling banyak mendapatkan perhatian adalah alur perubahan hutan alam menjadi tipe tutupan lahan lainnya (deforestasi) dan perubahan hutan alam primer menjadi hutan alam sekunder (degradasi). Bagian ini memperlihatkan hasil analisa LUMENS terhadap perubahan tutupan hutan di sebuah daerah")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 4. Grafik Perubahan Tutupan Hutan di Berbagai Zona Perencanaan\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_def)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 3. Luas deforestasi \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_forest.overal,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 4. Luas deforestasi berdasarkan zonasi \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_forest.zone,font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Laju deforestasi berdasarkan zonasi \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,forest.change.rate,font.size=8)
addNewLine(rtffile)


#PLOT: TOTAL CHANGES PER TRAJECTORY
for(s in 2:(ncol(PreQUES_traj_forest.zone))){
  print(s)
  c<-s-1
  PreQUES_traj_database.zone.melt_pertrajek<- melt(data = PreQUES_traj_forest.zone, id.vars=c('Z_NAME'), measure.vars=c(colnames(PreQUES_traj_forest.zone)[s]))
  colnames(PreQUES_traj_database.zone.melt_pertrajek)[1]<-'ZONE'
  plot_per_trajek<-ggplot(data=PreQUES_traj_database.zone.melt_pertrajek,aes(factor(ZONE),value,fill=factor(ZONE)))+geom_bar(stat="identity",position="dodge")+ coord_equal() +
    theme(axis.text.x= element_text(angle=90,hjust=1))+ labs(x = 'Zona', y='Luas area (Ha)')+
    theme(axis.text.x= element_text(angle=360, hjust=1))+ labs(x = 'Jenis perubahan tutupan hutan', y='Luas area (Ha)')+coord_flip()+
    theme( legend.title = element_text(size=8),legend.text = element_text(size = 6))
  #eval(parse(text=( paste("plot.per.trajek_",s,'_',colnames(PreQUES_traj_database.zone)[s],'<-plot_per_trajek', sep=''))));#save plots
  addNewLine(rtffile)
  addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_per_trajek)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs20 Sub Gambar ",c,". Grafik Perubahan Hutan di Berbagai Zona Perencanaan untuk ",colnames(PreQUES_traj_forest.zone)[s], "\b  di \\b0 \\fs20", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
  addNewLine(rtffile)
}


addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=300, plot_drive)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 5. Grafik Perubahan Tutupan Hutan dan alur perubahan yang menyebabkannya\\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep,"\\b \\fs20 berdasarkan Unit Perencanaan \\b0 \\fs20", sep=" "))
addNewLine(rtffile)

addParagraph(rtffile, paste("\\b \\fs20 Tabel 5. Luas deforestasi berdasarkan alur perubahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 Tahun \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addTable(rtffile,PreQUES_traj_drive.zone,font.size=8)
addNewLine(rtffile)
done(rtffile)

#command<-paste("start ", "winword ", Wdir, "/LUMENS_Pre-QUES_Trajectory_report.lpr", sep="" )
#shell(command)

# Stop the clock
elapsed<-proc.time() - ptm
elapsed<-round((elapsed[3]/60),2)
print(paste("processing time:",elapsed,"minute(s)"))
