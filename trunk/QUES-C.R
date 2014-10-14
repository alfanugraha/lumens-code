##[QUES]=group
##working_directory=folder
##landuse1=raster
##landuse2=raster
##zone=raster
##periode1=number 2010
##periode2=number 2015
##location=string
##carbon_lookup=file
##zone_lookup=file
##carbontiff1=output raster
##carbontiff2=output raster
##emission=output raster
##sequestration=output raster
##zone_carbon=output table
##fs_table=output table
##data_merge=output table
##passfilenames


library(R2HTML)
library(raster)
library(rgdal)
library(SDMTools)
library(tiff)
library(foreign)
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
library(pander)
library(knitr)
library(markdown)
library(rtf)

##[QUES]=group
working_directory="C:/LUMENS_kal/QUES_C_test"
landuse1="C:/LUMENS_kal/BERAU_QUES_C_L4/kec_berau_lc00_l4ndat.tif"
landuse2="C:/LUMENS_kal/BERAU_QUES_C_L4/kec_berau_lc05_l4_ndat.tif"
zone="C:/LUMENS_kal/BERAU_QUES_C_L4/zone_kec_berau_ndat.tif"
periode1= 2000
periode2= 2005
location="Berau"
carbon_lookup="C:/LUMENS_kal/3_Table/2_Berau/Tabel_cadangan_karbon.csv"
zone_lookup="C:/LUMENS_kal/3_Table/2_Berau/Tabel_zonasi.csv"
nodata=27

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# set working directory
setwd(working_directory)

# load datasets
landuse1 <- raster(landuse1)
landuse2 <- raster(landuse2)
zone <- raster(zone)


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


#Extent handling and raster resolution zone map
if (as.character(landuse1@crs)==as.character(zone@crs)){
  print("Raster map time series 1 and 2 have the same projection")
  if (res(landuse1)[1]==res(zone)[1]){
    print("Raster map time series 1 and 2 have the same extent")
  } else{
    print("Raster map time series 1 and 2 don't have the same extent, synchronising land-cover map...")
    zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
  }
} else{
  print("Raster map time series 1 and 2 don't have the same projection, synchronising land-cover map...")
  zone<-spatial_sync_raster(zone, landuse1, method = "ngb")
}

# load look up tables
lookup_c<- read.table(carbon_lookup, header=TRUE, sep=",",)
lookup_z <- read.table(zone_lookup, header=TRUE, sep=",",)
lookup_lc<-lookup_c

# set proj prop
title=location
tab_title<-as.data.frame(title)
period1=periode1
period2=periode2
period=period2-period1
proj_prop<-as.data.frame(title)
proj_prop$period1<-period1
proj_prop$period2<-period2
proj_prop$period <- do.call(paste, c(proj_prop[c("period1", "period2")], sep = " - "))


#Carbon accounting process
NAvalue(landuse1)<-nodata
NAvalue(landuse2)<-nodata
rcl.m.c1<-as.matrix(lookup_c[,1])
rcl.m.c2<-as.matrix(lookup_c[,3])
rcl.m<-cbind(rcl.m.c1,rcl.m.c2)
carbon1<-reclassify(landuse1, rcl.m)
carbon2<-reclassify(landuse2, rcl.m)
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq
stack<-stack(landuse1,landuse2, zone)
cross<-as.data.frame(crosstab(stack))
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
colnames(lookup_c)[1]="ID_LC1"
colnames(lookup_c)[2]="LC_t1"
colnames(lookup_c)[3]="CARBON_t1"
data_merge <- merge(cross,lookup_c,by="ID_LC1")
colnames(lookup_c)[1]="ID_LC2"
colnames(lookup_c)[2]="LC_t2"
colnames(lookup_c)[3]="CARBON_t2"
data_merge <- as.data.frame(merge(data_merge,lookup_c,by="ID_LC2"))
colnames(lookup_z)[1]="ZONE"
colnames(lookup_z)[2]="Z_NAME"
data_merge <- as.data.frame(merge(data_merge,lookup_z,by="ZONE"))
#modify carbon stock density ecah time series
data_merge$CARBON_t1<-data_merge$CARBON_t1*Spat_res
data_merge$CARBON_t2<-data_merge$CARBON_t2*Spat_res

data_merge$ck_em<-data_merge$CARBON_t1>data_merge$CARBON_t2
data_merge$ck_sq<-data_merge$CARBON_t1<data_merge$CARBON_t2
data_merge$em<-(data_merge$CARBON_t1-data_merge$CARBON_t2)*data_merge$ck_em*data_merge$COUNT*3.67
data_merge$sq<-(data_merge$CARBON_t2-data_merge$CARBON_t1)*data_merge$ck_sq*data_merge$COUNT*3.67
data_merge$LU_CHG <- do.call(paste, c(data_merge[c("LC_t1", "LC_t2")], sep = " to "))
data_merge$null<-0
data_merge$nullCek<-data_merge$em+data_merge$sq



#generate area_zone lookup and calculate min area
area_zone<-levels(ratify(zone, count=T))
colnames(lookup_z)[1]<-"ID"
area_zone<-merge(area_zone, lookup_z, by="ID")
area<-min(sum(area_zone$COUNT), sum(data_merge$COUNT))

#calculate emission for each planning unit
zone_emission <- as.data.frame(zonal((Spat_res*emission),zone,'sum')) #adjust emission by actual raster area
zone_sequestration <- as.data.frame(zonal((Spat_res*sequestration),zone,'sum'))#adjust sequestration by actual raster area
colnames(zone_emission)[1] = "ID"
colnames(zone_emission)[2] = "Em_tot"
colnames(zone_sequestration)[1] = "ID"
colnames(zone_sequestration)[2]="Sq_tot"
zone_emission<-merge(area_zone,zone_emission,by="ID")
zone_carbon<-merge(zone_emission,zone_sequestration,by="ID")
zone_carbon$Net_em<-zone_carbon$Em_tot-zone_carbon$Sq_tot
zone_carbon$Net_em_rate<-round((zone_carbon$Net_em/zone_carbon$COUNT/period), digits=3)
zone_carbon$Sq_tot<-round(zone_carbon$Sq_tot, digits=3)
#zone_carbon[,4:7]<-round(zone_carbon[,4:7], digits=3)

# create final summary of emission calculation at landscape level
fs_id<-c(1,2,3,4,5,6,7)
fs_cat<-c("Period", "Total area", "Total Emission (Ton CO2eq)", "Total Sequestration (Ton CO2eq)", "Net emission (Ton CO2eq)", "Emission rate (Ton CO2/yr)","Emission rate per-unit area (Ton CO2eq/ha.yr)")
fs_em<-sum(zone_carbon$Em_tot)
fs_sq<-sum(zone_carbon$Sq_tot)
fs_Nem<-fs_em-fs_sq
fs_Rem<-fs_Nem/period
fs_ARem<-fs_Rem/area
fs_summary<-c(proj_prop$period, area,round(fs_em, digits=3),round(fs_sq, digits=3),round(fs_Nem, digits=3),round(fs_Rem, digits=3),round(fs_ARem, digits=3))
fs_table<-data.frame(fs_id,fs_cat,fs_summary)
colnames(fs_table)<-c("ID", "Category", "Summary")

#create QUES-C database


#make zonal statistics database
lg<-length(unique(data_merge$ZONE))
zone_lookup<-area_zone
data_zone<-area_zone
data_zone$Z_CODE<-toupper(abbreviate(data_zone$Z_NAME))
for(i in 1:lg){
  data_z<-data_merge[which(data_merge$ZONE == i),]
  data_zone$Avg_C_t1[which(data_zone$ID == i)]<-sum(data_z$CARBON_t1*data_z$COUNT)/sum(data_z$COUNT)
  data_zone$Avg_C_t2[which(data_zone$ID == i)]<-sum(data_z$CARBON_t2*data_z$COUNT)/sum(data_z$COUNT)
  data_zone$Rate_em[which(data_zone$ID == i)]<-sum(data_z$em)/(sum(data_z$COUNT)*period)
  data_zone$Rate_seq[which(data_zone$ID == i)]<-sum(data_z$sq)/(sum(data_z$COUNT)*period)
}
data_zone[,5:8]<-round(data_zone[,5:8],digits=3)

#calculate largest source of emission
data_merge_sel <- data_merge[ which(data_merge$nullCek > data_merge$null),]
order_sq <- as.data.frame(data_merge[order(-data_merge$sq),])
order_em <- as.data.frame(data_merge[order(-data_merge$em),])

#Total Emission
tb_em_total<-as.data.frame(cbind(order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
colnames(tb_em_total)<-c("LU_CHG", "em")
tb_em_total<-aggregate(em~LU_CHG,data=tb_em_total,FUN=sum)
tb_em_total$LU_CODE<-as.factor(toupper(abbreviate(tb_em_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_em_total<-tb_em_total[order(-tb_em_total$em),]
tb_em_total<-tb_em_total[c(3,1,2)]
tb_em_total$Percentage<-as.numeric(format(round((tb_em_total$em / sum(tb_em_total$em) * 100),2), nsmall=2))
tb_em_total_10<-head(tb_em_total,n=10)

#Zonal Emission
tb_em_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tb_em<-as.data.frame(cbind(order_em$ZONE, order_em$LU_CHG, as.data.frame(round(order_em$em, digits=3))))
  colnames(tb_em)<-c("ZONE","LU_CHG", "em")
  tb_em_z<-as.data.frame(tb_em[which(tb_em$ZONE == i),])
  tb_em_z<-aggregate(em~ZONE+LU_CHG,data=tb_em_z,FUN=sum)
  tb_em_z$LU_CODE<-as.factor(toupper(abbreviate(tb_em_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
  tb_em_z<-tb_em_z[order(-tb_em_z$em),]
  tb_em_z<-tb_em_z[c(1,4,2,3)]
  tb_em_z$Percentage<-as.numeric(format(round((tb_em_z$em / sum(tb_em_z$em) * 100),2), nsmall=2))
  tb_em_z_10<-head(tb_em_z,n=10)
  tb_em_zonal<-rbind(tb_em_zonal,tb_em_z_10)
}

#Total Sequestration
tb_seq_total<-as.data.frame(cbind(order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
colnames(tb_seq_total)<-c("LU_CHG", "seq")
tb_seq_total<-aggregate(seq~LU_CHG,data=tb_seq_total,FUN=sum)
tb_seq_total$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_seq_total<-tb_seq_total[order(-tb_seq_total$seq),]
tb_seq_total<-tb_seq_total[c(3,1,2)]
tb_seq_total$Percentage<-as.numeric(format(round((tb_seq_total$seq / sum(tb_seq_total$seq) * 100),2), nsmall=2))
tb_seq_total_10<-head(tb_seq_total,n=10)

#Zonal Sequestration
tb_seq_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tb_seq<-as.data.frame(cbind(order_sq$ZONE, order_sq$LU_CHG, as.data.frame(round(order_sq$sq, digits=3))))
  colnames(tb_seq)<-c("ZONE","LU_CHG", "seq")
  tb_seq_z<-as.data.frame(tb_seq[which(tb_seq$ZONE == i),])
  tb_seq_z<-aggregate(seq~ZONE+LU_CHG,data=tb_seq_z,FUN=sum)
  tb_seq_z$LU_CODE<-as.factor(toupper(abbreviate(tb_seq_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
  tb_seq_z<-tb_seq_z[order(-tb_seq_z$seq),]
  tb_seq_z<-tb_seq_z[c(1,4,2,3)]
  tb_seq_z$Percentage<-as.numeric(format(round((tb_seq_z$seq / sum(tb_seq_z$seq) * 100),2), nsmall=2))
  tb_seq_z_10<-head(tb_seq_z,n=10)
  tb_seq_zonal<-rbind(tb_seq_zonal,tb_seq_z_10)
}

#Total Net Emission Rate
tb_net_total<-as.data.frame(cbind(order_em$LU_CHG, order_em$COUNT, as.data.frame(round(order_em$em, digits=3)), as.data.frame(round(order_em$sq, digits=3))))
colnames(tb_net_total)<-c("LU_CHG","COUNT", "em", "seq")
tb_net_total<-aggregate(. ~LU_CHG,data=tb_net_total,FUN=sum)
tb_net_total$Net_em_rate<-round((tb_net_total$em-tb_net_total$seq)/(tb_net_total$COUNT*period), digits=3)
tb_net_total$Net_em_rate[which(tb_net_total$Net_em_rate == "NaN")]<-0
tb_net_total$COUNT<-tb_net_total$em<-tb_net_total$seq<-NULL
tb_net_total$LU_CODE<-as.factor(toupper(abbreviate(tb_net_total$LU_CHG, minlength=5, strict=FALSE, method="both")))
tb_net_total<-tb_net_total[order(-tb_net_total$Net_em_rate),]
tb_net_total<-tb_net_total[c(3,1,2)]
tb_net_total$Percentage<-as.numeric(format(round((tb_net_total$Net_em_rate / sum(tb_net_total$Net_em_rate) * 100),2), nsmall=2))
tb_net_total_10<-head(tb_net_total,n=10)

#Zonal Net Emission Rate
tb_net_zonal<-as.data.frame(NULL)
for (i in 1:length(zone_lookup$ID)){
  tb_net<-as.data.frame(cbind(order_em$ZONE, order_em$LU_CHG, order_em$COUNT, as.data.frame(round(order_em$em, digits=3)), as.data.frame(round(order_em$sq, digits=3))))
  colnames(tb_net)<-c("ZONE", "LU_CHG","COUNT", "em", "seq")
  tb_net_z<-as.data.frame(tb_net[which(tb_net$ZONE == i),])
  tb_net_z<-aggregate(. ~ZONE+LU_CHG,data=tb_net_z,FUN=sum)
  tb_net_z$Net_em_rate<-round((tb_net_z$em-tb_net_z$seq)/(tb_net_z$COUNT*period), digits=3)
  tb_net_z$Net_em_rate[which(tb_net_z$Net_em_rate == "NaN")]<-0
  tb_net_z$COUNT<-tb_net_z$em<-tb_net_z$seq<-NULL
  tb_net_z$LU_CODE<-as.factor(toupper(abbreviate(tb_net_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
  tb_net_z<-tb_net_z[order(-tb_net_z$Net_em_rate),]
  tb_net_z<-tb_net_z[c(1,4,2,3)]
  tb_net_z$Percentage<-as.numeric(format(round((tb_net_z$Net_em_rate / sum(tb_net_z$Net_em_rate) * 100),2), nsmall=2))
  tb_net_z_10<-head(tb_net_z,n=10)
  tb_net_zonal<-rbind(tb_net_zonal,tb_net_z_10)
}

#Zonal Additional Statistics
name.matrix<-lookup_lc
name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$LC, minlength=4, method="both"))
#Zonal Emission matrix
e.m.z<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
em.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(e.m.z)){
    for (j in 1:ncol(e.m.z)){
      em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      e.m.z[i,j]<-as.numeric(round(sum(em.data$em), 2))
    }
  }
  e.m.z<-as.data.frame(e.m.z)
  e.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,e.m.z))
  e.m.z.c<-cbind(rep(k,nrow(e.m.z)),e.m.z.c)
  em.matrix.zonal<-rbind(em.matrix.zonal,e.m.z.c)
}
colnames(em.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))

#Total Emission matrix
e.m<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
for (i in 1:nrow(e.m)){
  for (j in 1:ncol(e.m)){
    em.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    e.m[i,j]<-round(sum(em.data$em), digits=2)
  }
}
e.m<-as.data.frame(e.m)
em.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,e.m))
colnames(em.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))

#Zonal Sequestration matrix
s.m.z<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
seq.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(s.m.z)){
    for (j in 1:ncol(s.m.z)){
      seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      s.m.z[i,j]<-round(sum(seq.data$sq), digits=2)
    }
  }
  s.m.z<-as.data.frame(s.m.z)
  s.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,s.m.z))
  s.m.z.c<-cbind(rep(k,nrow(s.m.z)),s.m.z.c)
  seq.matrix.zonal<-rbind(seq.matrix.zonal,s.m.z.c)
}
colnames(seq.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))

#Total Sequestration matrix
s.m<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
for (i in 1:nrow(s.m)){
  for (j in 1:ncol(s.m)){
    seq.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    s.m[i,j]<-round(sum(seq.data$sq), digits=2)
  }
}
s.m<-as.data.frame(s.m)
seq.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,s.m))
colnames(seq.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))

#Zonal Net emission matrix
n.m.z<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
net.matrix.zonal<-as.data.frame(NULL)
for (k in 1:length(zone_lookup$ID)){
  for (i in 1:nrow(n.m.z)){
    for (j in 1:ncol(n.m.z)){
      net.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j & data_merge_sel$ZONE==k),]
      n.m.z[i,j]<-round(((sum(net.data$em)-sum(net.data$sq))/(sum(net.data$COUNT)*period)), digits=2)
    }
  }
  n.m.z[n.m.z=="NaN"]<-0
  n.m.z<-as.data.frame(n.m.z)
  n.m.z.c<-as.data.frame(cbind(name.matrix$LC_CODE,n.m.z))
  n.m.z.c<-cbind(rep(k,nrow(n.m.z)),n.m.z.c)
  net.matrix.zonal<-rbind(net.matrix.zonal,n.m.z.c)
}
colnames(net.matrix.zonal)<-c("ZONE","LC_CODE",as.vector(name.matrix$LC_CODE))

#Net emission matrix
n.m<-matrix(0, nrow=length(lookup_lc$ID), ncol=length(lookup_lc$ID))
for (i in 1:nrow(n.m)){
  for (j in 1:ncol(n.m)){
    net.data<-data_merge_sel[which(data_merge_sel$ID_LC1==i & data_merge_sel$ID_LC2==j),]
    n.m[i,j]<-round(((sum(net.data$em)-sum(net.data$sq))/(sum(net.data$COUNT)*period)), digits=2)
  }
}
n.m[n.m=="NaN"]<-0
n.m<-as.data.frame(n.m)
net.matrix.total<-as.data.frame(cbind(name.matrix$LC_CODE,n.m))
colnames(net.matrix.total)<-c("LC_CODE",as.vector(name.matrix$LC_CODE))

#produce chart and map
#par(mfrow=c(3,2))
#plot(landuse1,main='Land Use Map t1')
#plot(landuse2,main='Land Use Map t2')
#plot(carbon1,main='Carbon Density Map t1')
#plot(carbon2, main='Carbon Density Map t2')

work_dir<-paste(working_directory,"/Result", sep="")
dir.create("Result")
setwd(work_dir)

#export analysis result
carbontiff1<-carbon1
carbontiff2<-carbon2
writeRaster(carbon1, filename="carbon1.tif", format="GTiff", overwrite=TRUE)
writeRaster(carbon2, filename="carbon2.tif", format="GTiff", overwrite=TRUE)
writeRaster(emission, filename="emission.tif", format="GTiff", overwrite=TRUE)
writeRaster(sequestration, filename="sequestration.tif", format="GTiff", overwrite=TRUE)
write.dbf(zone_carbon, "emission_by_zone.dbf")
write.dbf(fs_table, "summary_QUES-C.dbf")
write.dbf(data_merge, "QUES-C_database.dbf")
write.dbf(data_zone, "Carbon_Summary.dbf")
write.dbf(em.matrix.total,"Total_Emission_Matrix.dbf ")
write.dbf(seq.matrix.total, "Total_Sequestration_Matrix.dbf")
write.dbf(net.matrix.total, "Total_Rate_Matrix.dbf")
for (i in 1:length(zone_lookup$ID)){
  em_matrix_z<-em.matrix.zonal[which(em.matrix.zonal$ZONE == i),]
  em_matrix_z$ZONE<-NULL
  seq_matrix_z<-seq.matrix.zonal[which(seq.matrix.zonal$ZONE == i),]
  seq_matrix_z$ZONE<-NULL
  net_matrix_z<-net.matrix.zonal[which(net.matrix.zonal$ZONE == i),]
  net_matrix_z$ZONE<-NULL
  write.dbf(em_matrix_z,paste("Emission_Matrix_Zone_",i,sep=""))
  write.dbf(seq_matrix_z,paste("Sequestration_Matrix_Zone_",i,sep=""))
  write.dbf(net_matrix_z,paste("Rate_Matrix_Zone_",i,sep=""))
}

#REARRANGE zone carbon
zone_carbon_pub<-zone_carbon
colnames(zone_carbon_pub) <- c("ID", "Area (Ha)", "Land cover class", "Total emission (Ton CO2/Ha)", "Total sequestration(Ton CO2/Ha)", "Net emission (Ton CO2/Ha)", "Emission rate (Ton CO2/Ha.yr)")

#Create Map for report
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)

#Landuse 1 map
LU1 <- rasterToPoints(landuse1_pl);
LU1 <- as.data.frame(LU1)
colnames(LU1) <- c("X","Y","ID")
LU1<-LU1[which(LU1$ID != 0),]
lu.lab<-lookup_lc
colnames(lu.lab)[1]<-"ID"
LU1<-merge(LU1, lu.lab, by="ID")
LU1$ID<-as.factor(LU1$ID)
myColors.lu <- myColors[1:length(unique(LU1$ID))]
names(myColors.lu) <- unique(LU1$CLASS)
ColScale.lu<-scale_fill_manual(name="Land Use Class", values = myColors.lu )
plot.LU1  <- ggplot(data=LU1) + geom_raster(aes(x=LU1$X, y=LU1$Y, fill=LU1$CLASS)) +
  ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))+coord_equal()

#Landuse 2 map
LU2 <- rasterToPoints(landuse2_pl);
LU2 <- as.data.frame(LU2)
colnames(LU2) <- c("X","Y","ID")
LU2<-LU2[which(LU2$ID != 0),]
LU2<-merge(LU2, lu.lab, by="ID")
LU2$ID<-as.factor(LU2$ID)
myColors.lu <- myColors[1:length(unique(LU2$ID))]
names(myColors.lu) <- unique(LU2$CLASS)
ColScale.lu<-scale_fill_manual(name="Land Use Class", values = myColors.lu )
plot.LU2  <- ggplot(data=LU2) + geom_raster(aes(x=LU2$X, y=LU2$Y, fill=LU2$CLASS)) +
  ColScale.lu +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))+coord_equal()

#zone map
Z <- rasterToPoints(zone_pl);
Z <- as.data.frame(Z)
colnames(Z) <- c("X","Y","ID")
Z<-Z[which(Z$ID != 0),]
Z<-merge(Z, data_zone, by="ID")
Z$ID<-as.factor(Z$ID)
Z[,6:9]<-round(Z[,6:9], digits=3)
myColors.Z <- myColors[1:length(unique(Z$ID))]
names(myColors.Z) <- unique(Z$ZONE)
ColScale.Z<-scale_fill_manual(name="Zone Class", values = myColors.Z )
plot.Z  <- ggplot(data=Z) + geom_raster(aes(x=Z$X, y=Z$Y, fill=Z$ZONE)) +
  ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))+coord_equal()

#Average Zonal Carbon Rate t1
plot.Z.Avg.C.t1  <- ggplot(data=Z) + geom_raster(aes(x=Z$X, y=Z$Y, fill=Z$Avg_C_t1)) +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Carbon Density of", location, period1 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Average Zonal Carbon Rate t2
plot.Z.Avg.C.t2  <- ggplot(data=Z) + geom_raster(aes(x=Z$X, y=Z$Y, fill=Z$Avg_C_t2)) +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Carbon Density of", location, period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Average Zonal Emission Rate
plot.Z.Avg.em  <- ggplot(data=Z) + geom_raster(aes(x=Z$X, y=Z$Y, fill=Z$Rate_em)) +
  scale_fill_gradient(name="Emission Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Emission Rate of", location, period1, "-", period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Average Zonal Sequestration Rate
plot.Z.Avg.seq  <- ggplot(data=Z) + geom_raster(aes(x=Z$X, y=Z$Y, fill=Z$Rate_seq)) +
  scale_fill_gradient(name="Sequestration Level",low = "#FFCC66", high="#003300", guide="colourbar") +
  ggtitle(paste(" Average Sequestration Rate of", location, period1, "-", period2 )) +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()
#Carbon 1 map
C1 <- rasterToPoints(carbon1);
C1 <- as.data.frame(C1)
colnames(C1) <- c("X","Y","CARBON")
y<-ceiling(max(C1$CARBON)/100)
y<-y*100
plot.C1  <- ggplot(data=C1) + geom_raster(aes(x=C1$X, y=C1$Y, fill=C1$CARBON)) +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Carbon 2 map
C2 <- rasterToPoints(carbon2);
C2 <- as.data.frame(C2)
colnames(C2) <- c("X","Y","CARBON")
plot.C2  <- ggplot(data=C2) + geom_raster(aes(x=C2$X, y=C2$Y, fill=C2$CARBON)) +
  scale_fill_gradient(name="Carbon Density Level",low = "#FFCC66", high="#003300",limits=c(0,y), breaks=c(0,10,20,50,100,200,300), guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 7),
         legend.key.height = unit(1.5, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()


#Carbon Emission Map of Bungo 2005-2010
E <- rasterToPoints(emission);
E <- as.data.frame(E)
colnames(E) <- c("X","Y","Emission")
plot.E  <- ggplot(data=E) + geom_raster(aes(x=E$X, y=E$Y, fill=E$Emission)) +
  scale_fill_gradient(name="Emission (TON CO2eq)",low = "#FFCC66", high="#FF0000", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Carbon Sequestration Map of Bungo 2005-2010
S <- rasterToPoints(sequestration);
S <- as.data.frame(S)
colnames(S) <- c("X","Y","Sequestration")
plot.S  <- ggplot(data=S) + geom_raster(aes(x=S$X, y=S$Y, fill=S$Sequestration)) +
  scale_fill_gradient(name="Sequestration (TON CO2eq)",low = "#FFCC66", high="#000033", guide="colourbar") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size = 8),
         legend.key.height = unit(0.375, "cm"),
         legend.key.width = unit(0.375, "cm"))+coord_equal()

#Emission Rate
emissionRate<-ggplot(data=zone_carbon, aes(x=reorder(ZONE, -Net_em_rate), y=(zone_carbon$Net_em_rate))) + geom_bar(stat="identity", fill="Red") +
  geom_text(data=zone_carbon, aes(label=round(Net_em_rate, 1)),size=4) +
  ggtitle(paste("Net Emmission Rate of", location, period1,"-", period2 )) + guides(fill=FALSE) + ylab("CO2eq/ha.yr") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle=20),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#Largest emission
largestEmission<-ggplot(data=tb_em_total_10, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
  geom_text(data=tb_em_total_10, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Largest Source of Emmission in", location )) + guides(fill=FALSE) + ylab("CO2eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#Largest Sequestration
largestSeq<-ggplot(data=tb_seq_total_10, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
  geom_text(data=tb_seq_total_10, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Largest Source of Sequestration in", location )) + guides(fill=FALSE) + ylab("CO2eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#Largest Net Emission rate
largestNet<-ggplot(data=tb_net_total_10, aes(x=reorder(LU_CODE, -Net_em_rate), y=(Net_em_rate))) + geom_bar(stat="identity", fill="dark grey") +
  geom_text(data=tb_net_total_10, aes(x=LU_CODE, y=Net_em_rate, label=round(Net_em_rate, 1)),size=3, vjust=0.1) +
  ggtitle(paste("Largest Net Emission Rate in", location )) + guides(fill=FALSE) + ylab("CO2eq") +
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

#rtf report file
title<-"\\b\\fs32 LUMENS-QUES Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules: Carbon Dynamics Quantification\\b0\\fs20"
test<-as.character(Sys.Date())
date<-paste("Date : ", test, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
chapter1<-"\\b\\fs24 DATA INPUT \\b0\\fs20"
chapter2<-"\\b\\fs24 ANALYSIS AT LANDSCAPE LEVEL \\b0\\fs20"
chapter3<-"\\b\\fs24 ANALYSIS AT PLANNING UNIT LEVEL \\b0\\fs20"
rtffile <- RTF("LUMENS_QUES-C_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)

text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.4, height=4, res=150, plot.LU1 )
rm(LU1)
text <- paste("\\b \\fs20 Peta penutupan lahan \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.LU2 )
rm(LU2)
text <- paste("\\b \\fs20 Peta unit perencanaan \\b0 \\fs20 ", area_name_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z )

addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C1 )
rm(C1)
text <- paste("\\b \\fs20 Peta kerapatan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_2_rep, " \\b \\fs20 (dalam Ton C/Ha)\\b0 \\fs20", sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.C2 )
addNewLine(rtffile, n=1)
rm(C2)
text <- paste("\\b \\fs20 Peta emisi karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.E )
addNewLine(rtffile, n=1)
rm(E)
text <- paste("\\b \\fs20 Peta penyerapan karbon \\b0 \\fs20 ", area_name_rep, "\\b \\fs20 tahun \\b0 \\fs20 ", I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20 ", I_O_period_2_rep, sep="")
addParagraph(rtffile, text)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.S )
rm(S)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, fs_table)
addNewLine(rtffile, n=1)

addParagraph(rtffile, "\\b \\fs20 Intisari perhitungan emisi per unit perencanaan\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, data_zone)
addNewLine(rtffile, n=1)

addNewLine(rtffile, n=1)
addTable(rtffile, zone_carbon)
addParagraph(rtffile, "Note : ")
addParagraph(rtffile, "Em_tot = Total Emission in ton CO2eq ")
addParagraph(rtffile, "Sq_tot = Total Sequestration in ton CO2eq ")
addParagraph(rtffile, "Net_em = Total Emission - Total Sequestration in ton CO2eq ")
addParagraph(rtffile, "Net_em_rate = (Total Emission - Total Sequestration) / (area * period) in ton CO2eq/ha.year ")
addNewLine(rtffile, n=1)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, emissionRate )
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t1 )
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.C.t2 )
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.em  )
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=4, res=150, plot.Z.Avg.seq )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, "\\b \\fs20 Largest Sources of Emission\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, tb_em_total_10)
addNewLine(rtffile, n=1)
rm(Z)
addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestEmission )
addNewLine(rtffile, n=1)

addParagraph(rtffile, "\\b \\fs20 Largest Sources of Sequestration\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, tb_seq_total_10)
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestSeq )
addNewLine(rtffile, n=1)

addParagraph(rtffile, "\\b \\fs20 Largest Sources of Net Emission Rate\\b0 \\fs20")
addNewLine(rtffile, n=1)
addTable(rtffile, tb_net_total_10)
addNewLine(rtffile, n=1)

addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestNet )
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addNewLine(rtffile, n=1)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)

for(i in 1:length(zone_lookup$ID)){
  zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
  zona_nm<-paste("\\b", "\\fs20", data_zone$ZONE[i], "\\b0","\\fs20")
  zona_ab<-paste("\\b", "\\fs20", data_zone$Z_CODE[i], "\\b0","\\fs20")
  addParagraph(rtffile, "\\b \\fs20 Largest Sources of Emission in Zone \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
  addNewLine(rtffile, n=1)
  
  tb_em_zon<-tb_em_zonal[which(tb_em_zonal$ZONE == i),]
  tb_em_zon$ZONE<-NULL
  addTable(rtffile, tb_em_zon)
  addNewLine(rtffile, n=1)
  
  #Largest emission
  largestE.Z<-ggplot(data=tb_em_zon, aes(x=reorder(LU_CODE, -em), y=(em))) + geom_bar(stat="identity", fill="blue") +
    geom_text(data=tb_em_zon, aes(x=LU_CODE, y=em, label=round(em, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Largest Emission in Zone",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2eq") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestE.Z )
  addNewLine(rtffile, n=1)
  
  addParagraph(rtffile, "\\b \\fs20 Largest Sources of Sequestration in Zone \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
  addNewLine(rtffile, n=1)
  
  tb_seq_zon<-tb_seq_zonal[which(tb_seq_zonal$ZONE == i),]
  tb_seq_zon$ZONE<-NULL
  addTable(rtffile, tb_seq_zon)
  addNewLine(rtffile, n=1)
  
  #Largest Sequestration
  largestS.Z<-ggplot(data=tb_seq_zon, aes(x=reorder(LU_CODE, -seq), y=(seq))) + geom_bar(stat="identity", fill="green") +
    geom_text(data=tb_seq_zon, aes(x=LU_CODE, y=seq, label=round(seq, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Largest Sequestration in Zone",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2eq") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestS.Z )
  addNewLine(rtffile, n=1)
  
  addParagraph(rtffile, "\\b \\fs20 Largest Sources of Net Emission Rate in Zone \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
  addNewLine(rtffile, n=1)
  
  tb_net_zon<-tb_net_zonal[which(tb_net_zonal$ZONE == i),]
  tb_net_zon$ZONE<-NULL
  addTable(rtffile, tb_net_zon)
  addNewLine(rtffile, n=1)
  
  #Largest Net Emission Rate
  largestN.Z<-ggplot(data=tb_net_zon, aes(x=reorder(LU_CODE, -Net_em_rate), y=(Net_em_rate))) + geom_bar(stat="identity", fill="dark grey") +
    geom_text(data=tb_net_zon, aes(x=LU_CODE, y=Net_em_rate, label=round(Net_em_rate, 1)),size=3, vjust=0.1) +
    ggtitle(paste("Largest Net Emission Rate in Zone",i, "-", data_zone$Z_CODE[i] )) + guides(fill=FALSE) + ylab("CO2eq") +
    theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  
  addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, largestN.Z )
  addNewLine(rtffile, n=1)
  
}
done(rtffile)

reports<-paste("
Land Use Planning for Multiple Environmental Services
========================================================
***
               
# Lembar hasil analisis QUES-C:
# Perhitungan dinamika karbon di tingkat bentang lahan
               
***
               
***
# Peta penutupan lahan `r location` tahun `r periode1`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(landuse1_pl, col.regions=rainbow)
```
               
***
               
# Peta penutupan lahan `r location` tahun `r periode2`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(landuse2_pl, col.regions=rainbow)
```
***
               
# Peta unit perencanaan `r location`
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(zone_pl, col.regions=rainbow)
```
               
***
               
               
# Peta kerapatan karbon `r location` tahun `r periode1`
*dalam Ton C/Ha*
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(carbon1, col.regions= function(x)rev(terrain.colors(x)))
```
               
***
               
# Peta kerapatan karbon `r location` tahun `r periode2`
*dalam Ton C/Ha*
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(carbon2, col.regions= function(x)rev(terrain.colors(x)))
```
               
***
               
# Peta emisi karbon `r location` tahun `r periode1` - `r periode2`
*dalam Ton CO2/Ha*
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(emission, col.regions= function(x)rev(heat.colors(x)))
```
               
***
# Peta penyerapan karbon `r location` tahun `r periode1` - `r periode2`
*dalam Ton CO2/Ha*
```{r fig.width=10, fig.height=9, echo=FALSE}
levelplot(sequestration, col.regions= function(x)rev(terrain.colors(x)))
               
```
***
# Intisari perhitungan emisi
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(fs_table)
               
```
***
               
# Intisari perhitungan emisi per unit perencanaan
```{r fig.width=20, fig.height=9, echo=FALSE}
pandoc.table(zone_carbon_pub)
               
```
***
               
```{r fig.width=10, fig.height=9, echo=FALSE}
barplot (zone_carbon$Rate_em, names.arg=zone_carbon$CLASS,col='red' ,main='Laju emisi per Unit Perencanaan',ylim=c(0,signif(max(zone_carbon$Rate_em), 1)), ylab='CO2eq/ha.yr',xaxt='n', space=1)
end_point = 0.5 + nrow(zone_carbon) + nrow(zone_carbon)-1 ;#this is the line which does the trick (together with barplot 'space = 1' parameter)
text(seq(1.5,end_point,by=2), par('usr')[3]-0.25, srt = 60, adj= 1, xpd = TRUE,labels = zone_carbon$ZONE, cex=0.8)
               
               
```
***
               
```{r fig.width=10, fig.height=9, echo=FALSE}
barplot (tb_em$em, names.arg=tb_em$LU_CHG,col='red',main='Jenis perubahan penutupan lahan penyumbang emisi terbesar', ylim=c(0,signif(max(tb_em$em),1)), ylab='CO2eq',xaxt='n', space=1)
end_point = 0.5 + nrow(tb_em) + nrow(tb_em)-1 ;#this is the line which does the trick (together with barplot 'space = 1' parameter)
text(seq(1.5,end_point,by=2), par('usr')[3]-0.25, srt = 30, adj= 1, xpd = TRUE,labels = tb_em$LU_CHG, cex=0.8)
               
```
***
")


#WRITE REPORT
write(reports,file="reporthtml.Rmd")

knit2html("reporthtml.Rmd", options=c("use_xhml"))
