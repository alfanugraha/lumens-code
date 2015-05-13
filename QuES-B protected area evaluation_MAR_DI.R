ptm <- proc.time()
##[QUES]=group
Wdir="C:/QUES_B_DJB/ques_b_pa_eval/"
#landuse_1="C:/QUES_B_DJB/ques_b_pa_eval/Jambi_170414_UTM/fin_jb_lc901.tif"
#landuse_2="C:/QUES_B_DJB/ques_b_pa_eval/Jambi_170414_UTM/fin_jb_lc001.tif"
#period1=1990
#period2=2000

zone_l="C:/QUES_B_DJB/ques_b_pa_eval/buffer_PA.tif"
location="Merangin"
lookup_lcover="C:/QUES_B_DJB/ques_b_pa_eval/Jambi_170414_UTM/lookup_merangin_alreddi.csv"
lookup_zo="C:/QUES_B_DJB/ques_b_pa_eval/Tabel_zona_PA_Merangin.csv"
raster.nodata=0
remark.tres=10000 ;#in Ha
##luchg=output raster
##proj_prop=output table
##data_merge_sel=output table
##Ov_chg=output table
##cross_temp.melt.cast=output table
##luchg_att=output table
##passfilenames

library(foreign)
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
library(rtf)
library(utils)
library(gridExtra)



#select raster files
file.names<-(data.frame(file_name=choose.files(default = "", caption = "Select files",multi = TRUE)))
file.names<-cbind(file.names, data.frame(year=2000:(2000+(nrow(file.names)-1))))

tryCatch({
if (grepl(".tif", as.character(file.names[1,1]))){
  print("TIF Raster map(s) is/are found")
  
  } else{
    print("No TIF Raster map(s) found")
    stop()
  }
},error=function(e){cat("ERROR: No raster map found ||",conditionMessage(e), "\n")})

file.names<-edit(file.names)


#set project properties
setwd(Wdir)
file.names<-arrange(file.names,year)

#ZONE preparation
zone <- raster(zone_l)
NAvalue(zone)<-raster.nodata
lookup_z <- read.table(lookup_zo, header=TRUE, sep=",",)
colnames(lookup_z)<-c("ID", "DESC")

ptm <- proc.time()
#selecting landuse
for(ai in 1:(nrow(file.names)-1)){
  lu_selector<-c(ai,(ai+1))
  landuse_1<-as.character(file.names[lu_selector[1],1])
  landuse_2<-as.character(file.names[lu_selector[2],1])
  period1<-as.numeric(file.names[lu_selector[1],2])
  period2<-as.numeric(file.names[lu_selector[2],2])
 
#PISAHKAN INPUT ZONA

#load datasets
landuse1 <- raster(landuse_1)
landuse2 <- raster(landuse_2)


NAvalue(landuse1)<-raster.nodata
NAvalue(landuse2)<-raster.nodata

print(landuse1)
print(landuse2)


#projection handling
if (grepl("+units=m", as.character(landuse1@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(landuse1)[1]*res(landuse1)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(landuse1@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(landuse1)[1]*res(landuse1)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  stop("Raster map projection is unknown")
}


#Extent handling and raster resolution land-cover maps
if (as.character(landuse1@crs)==as.character(landuse2@crs)){
  print("Raster map time series 1 and 2 have the same projection")
  if (res(landuse1)[1]==res(landuse2)[1]){
    print("Raster map time series 1 and 2 have the same extent")
  } else{
    print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
    landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
  }
} else{
  print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
  landuse2<-spatial_sync_raster(landuse2, landuse1, method = "ngb")
}

# Extent handling and raster resolution zone map
if (as.character(landuse1@crs)==as.character(zone@crs)){
  print("Raster map time series 1 and zone have the same projection")
  if (res(landuse1)[1]==res(zone)[1]){
    print("Raster map time series 1 and zone have the same resolution")
    if (landuse1@extent==zone@extent){
      print("Raster map time series 1 and zone have the same extent")
    } else {
      print("Raster map time series 1 and zone don't have the same extent, synchronising land-cover map...")
      zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
    }
  } else{
    print("Raster map time series 1 and zone don't have the same resolution, synchronising land-cover map...")
    zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
  }
} else{
  print("Raster map time series 1 and zone don't have the same projection, synchronising land-cover map...")
  zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}


#load look up table
lookup_l<- read.table(lookup_lcover, header=TRUE, sep=",",)
lookup_lc<- read.table(lookup_lcover, header=TRUE, sep=",",)
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")

#create land use change map
cross_temp<-crosstab(stack(landuse1,landuse2, zone))
colnames(cross_temp)[1] = "Var1"
colnames(cross_temp)[2] = "Var2"
colnames(cross_temp)[3] = "Var3"

cross_temp$chkVar1<-as.numeric(is.na(cross_temp$Var1))
cross_temp$chkVar2<-as.numeric(is.na(cross_temp$Var2))
cross_temp$chkVar3<-as.numeric(is.na(cross_temp$Var3))
cross_temp$chkNull<-cross_temp$chkVar1+cross_temp$chkVar2+cross_temp$chkVar3
#cross_temp <- cross_temp[ which(cross_temp$chkNull < 1),]

#data_merge_sel <- cross_temp[ which(cross_temp$Freq > 0),]
cross_temp$chkVar1<-NULL
cross_temp$chkVar2<-NULL
cross_temp$chkVar3<-NULL
cross_temp$chkNull<-NULL

zone_loop_lookup<-as.numeric(levels(cross_temp$Var3));#identify zone id
zone_loop_lookup<-data.frame(ID=1:length(zone_loop_lookup),zone=zone_loop_lookup)
zone_loop_lookup<-merge(zone_loop_lookup, lookup_z, by="ID")

#alpha beta table loop.
for(i in 1:nrow(zone_loop_lookup)){
  zone_id<-zone_loop_lookup$zone[i]
  print(zone_id)
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"<-na.omit(cross_temp[ which(cross_temp$Var3==",zone_id,"),])", sep=''))))
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"$Var1<-as.numeric(as.character(unlist((cross_temp_zone_",zone_id,"$Var1))))", sep=''))))
  
  colnames(lookup_l)[1] = "Var1"
  colnames(lookup_l)[2] = "LC1"
  
  eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l[1],by='Var1', all.y=T)", sep=''))))
  eval(parse(text=( paste('cross_temp_zone_', zone_id,"$Var2<-as.numeric(as.character(unlist((cross_temp_zone_",zone_id,"$Var2))))", sep=''))))
  
  colnames(lookup_l)[1] = "Var2"
  colnames(lookup_l)[2] = "LC2"
  eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l[1],by='Var2', all=T)", sep=''))))
  
  eval(parse(text=( paste("print(head(cross_temp_zone_",zone_id,'))', sep=''))));#matrix output checking per zone
  
  eval(parse(text=( paste('cross_temp_zone_', zone_id," <-merge(cross_temp_zone_",zone_id,",lookup_l[1],by='Var2', all=T)", sep=''))));#exactly the same
  
  eval(parse(text=( paste('cross_temp.melt_', zone_id," <-melt(data = cross_temp_zone_",zone_id,",id.vars=c('Var1','Var2'), measure.vars=c('Freq'))", sep=''))))
  eval(parse(text=( paste('cross_temp.melt.cast_', zone_id," <-dcast(data = cross_temp.melt_",zone_id,", formula = Var1 ~ Var2, fun.aggregate = sum)", sep=''))))
  eval(parse(text=( paste('cross_temp.melt.cast_', zone_id," [1]<-NULL", sep=''))))
  eval(parse(text=( paste('cross_temp.melt_', zone_id," <-cross_temp.melt_", zone_id,"*",Spat_res, sep=''))))
  
  
  #write transition matrix file
  filename<-paste('transition_matrix_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  eval(parse(text=( paste("write.csv(cross_temp.melt.cast_",zone_id,", filename, row.names=F)",sep=''))))
  
   
  
  #matrix sudah benar
  #Lanjutkan dari sini, 17 Maret 2015"
  #selesaikan hingga 1 loop menghasilkan alpha betha table
  
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
  fromto.table<-merge(lookup_lc,fromto.table,by='ID')
  #write from to table
  dirname<-paste('Output_tables_and_graph_',location,'_',period1,'_',period2,'_normalized',sep='')#change working directory
  dir.create(file.path(getwd(), dirname), showWarnings = FALSE)
  setwd(file.path(getwd(), dirname))
  
  filename<-paste('from_to_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  write.csv(fromto.table, filename, row.names=F)
  
  fromtoplot<-ggplot(fromto.table, aes(x=as.numeric(as.character(fromto.table$a.val)), y=as.numeric(as.character(fromto.table$b.val)),xend=6, yend=6)) + geom_point(shape=1,size=2) + 
    geom_text(size=3, aes(label=fromto.table$CLASS),hjust=1,vjust=-1, angle =0)+ 
    labs(x = as.character(paste('Alpha',period1,'-', period2)), y=as.character(paste('Betha',period1,'-', period2))) +geom_vline(xintercept = 1)+geom_hline(yintercept=1)+ggtitle(paste(location,period1,'-', period2,', Zone',zone_loop_lookup$ID[i],'(',zone_loop_lookup$DESC[i],')' ))
  fromtoplot
  
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
  fromto.table.true<-merge(lookup_lc,fromto.table.true,by='ID')
  #write from to table
  dirname<-paste('Output_tables_and_graph_',location,'_',period1,'_',period2,sep='')#change working directory
  dir.create(file.path(getwd(), dirname), showWarnings = FALSE)
  setwd(file.path(getwd(), dirname))
  
  filename<-paste('from_to_',period1,'_',period2,'_zone_',zone_id,'.csv',sep='')
  write.csv(fromto.table.true, filename, row.names=F)
  
  fromtoplot.true<-ggplot(fromto.table.true, aes(x=as.numeric(as.character(fromto.table.true$a.val)), y=as.numeric(as.character(fromto.table.true$b.val)),xend=6, yend=6)) + geom_point(shape=1,size=2) + 
    geom_text(size=3, aes(label=fromto.table.true$CLASS),hjust=1,vjust=-1, angle =0)+ 
    labs(x = as.character(paste('Alpha',period1,'-', period2)), y=as.character(paste('Betha',period1,'-', period2))) +geom_vline(xintercept = remark.tres)+geom_hline(yintercept=remark.tres)+ggtitle(paste(location,period1,'-', period2,', Zone',zone_loop_lookup$ID[i],'(',zone_loop_lookup$DESC[i],')' ))
  fromtoplot.true
  
  filename<-paste('graph_from_to_',location,'_',period1,'_',period2,'_zone',zone_id,'.png',sep='')
  ggsave(fromtoplot.true, file=filename, width=20, height=20, units="cm")
  setwd(Wdir)
  
}
}

# Stop the clock
elapsed<-proc.time() - ptm
elapsed<-round((elapsed[3]/60),2)
print(paste("processing time:",elapsed,"minute(s)"))
