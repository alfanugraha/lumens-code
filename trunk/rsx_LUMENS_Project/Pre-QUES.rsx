##[LUMENS]=group
##project=file
##result_dir=folder
##raster.nodata=number 0
##passfilenames


#project="C:/Users/dindiarto/lumens-code/APR 2015/Pre_QUES_update_Belitung/lumens_april_2015.lpj"
#result_dir="C:/Users/dindiarto/lumens-code/APR 2015/Pre_QUES_update_Belitung/preques_merauke_1990_2000/"
#raster.nodata=0

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#====load library====
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


#====CREATE RESAVE FUNCTION====
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

#====READ PROJECT DESCRIPTION FILE AND IDENTIFY RDATA====
proj_desc<-read.table(project, header = TRUE, sep = "")
working_directory<-as.character(proj_desc[1,])
setwd(working_directory)
lumens_database<-list.files (path=working_directory, pattern="lpd")
load(lumens_database)

#====READ LANDUSE DATA FROM LUMENS DATABASE====
data<-as.data.frame(as.character(ls(pattern="landuse_t")))

per<-as.data.frame(ls(pattern="freq_landuse"))
n<-nrow(per)

data<-as.data.frame(data[(n+1):(n+n),1])
n<-nrow(data)
command1<-NULL
command2<-NULL
for(i in 1:n) {
  if (i!=n){
    command1<-paste(command1,"period", i, ",", sep="")
    command2<-paste(command2,"landuse_t", i, ",", sep="")
  } else {
    command1<-paste(command1,"period", i, sep="")
    command2<-paste(command2,"landuse_t", i, sep="")
  }
}

data2<-as.data.frame(as.character(ls(pattern="pu_pu")))
n<-nrow(data2)
command3<-NULL
for(i in 1:n) {
  if (i!=n){
    command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
    command3<-c(command3,command3a)
  } else {
    command3a<-eval(parse(text=(paste( "names(pu_pu", i, ")", sep=""))))
    command3<-c(command3,command3a)
  }
}



#READ PLANNING UNIT DATA FROM LUMENS DATABASE
#planning.unit<-as.data.frame(as.character(ls(pattern="pu_pu")))
#colnames(planning.unit)[1]<-"data"
#planning.unit$data<-as.character(planning.unit$data)
#planning.unit$usage<-0
#planning.unit<-edit(planning.unit)
#planning.unit <- planning.unit[which(planning.unit$usage==1),]
#pu<-as.character(planning.unit[1,1])

#====SELECT DATA TO BE ANALYZED====
eval(parse(text=(paste("year<-c(", command1, ")", sep=""))))
data<-as.data.frame(cbind(data,year))
data$t1<-0
data$t2<-0
colnames(data)[1]<-"data"
data$data<-as.character(data$data)
data3<-data
a<-nrow(data3)
repeat{
  data<-edit(data)
  if((sum(data$t1)+sum(data$t2))==2){
    break
  }
}
data$sum<-data$t1+data$t2
data <- data[which(data$sum==1),]

data$t1<-NULL
data$t2<-NULL
data$sum<-NULL

n<-nrow(data)
command1<-NULL
T1<-data[1,2]
T2<-data[2,2]

#====SELECT PLANNING UNIT TO BE ANALYZED====
data2<-as.data.frame(cbind(data2,command3))
data2$usage<-0
colnames(data2)[1]<-"data"
colnames(data2)[2]<-"sources"
data2$data<-as.character(data2$data)

repeat{
  data2<-edit(data2)
  if(sum(data2$usage)==1){
    break
  }
}

data2 <- data2[which(data2$usage==1),]
data2$usage<-NULL
pu<-as.character(data2[1,1])


#====PROJECTION HANDLING====
for(j in 1:n) {
  input <- as.character(data[j,1])
  eval(parse(text=(paste(input,"[",input, "==", raster.nodata, "]<-NA", sep=""))))
  command1<-paste(command1,input, ",", sep="")
}

#====projection handling====
if (grepl("+units=m", as.character(r.brick@crs))){
  print("Raster maps have projection in meter unit")
  Spat_res<-res(r.brick)[1]*res(r.brick)[2]/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else if (grepl("+proj=longlat", as.character(r.brick@crs))){
  print("Raster maps have projection in degree unit")
  Spat_res<-res(r.brick)[1]*res(r.brick)[2]*(111319.9^2)/10000
  paste("Raster maps have ", Spat_res, " Ha spatial resolution, QuES-C will automatically generate data in Ha unit")
} else{
  stop("Raster map projection is unknown")
}

#====CREATE LANDUSE CHANGE DATABASE====
command1<-paste(command1,pu, sep="")
eval(parse(text=(paste("r.brick<-brick(stack(", command1, "))", sep=""))))
lu.db<-crosstab(r.brick,long=TRUE,useNA=FALSE,progress='-')
sub1.lu.db<-lu.db[n-1]
sub2.lu.db<-lu.db[n]
sub3.lu.db<-lu.db[n+1]
sub4.lu.db<-lu.db[n+2]
cross<-cbind(sub1.lu.db,sub2.lu.db,sub3.lu.db,sub4.lu.db)
cross$Freq<-cross$Freq*Spat_res

#====CREATE OVERALL SUMMARY OF LAND USE CHANGE====
colnames(lut.lc)[1]<-"value"
colnames(lut.lc)[2]<-"class"

eval(parse(text=(paste("lu.summary<-na.omit(merge(lut.lc, freq_", data3[1,1], ', by="value"))', sep=""))))
colnames(lu.summary)[3]<-paste(data3[1,2], "_ha", sep="")

for (q in 2:a) {
  eval(parse(text=(paste("lu.summary<-na.omit(merge(lu.summary, freq_", data3[q,1], ', by="value"))', sep=""))))
  colnames(lu.summary)[q+2]<-paste(data3[q,2], "_ha", sep="")
}

#colnames(lut.lc)[1]<-"value"
#colnames(lut.lc)[2]<-"class"
#eval(parse(text=(paste("lu.summary<-na.omit(merge(lut.lc, freq_", data[1,1], ', by="value"))', sep=""))))
#eval(parse(text=(paste("lu.summary<-na.omit(merge(lu.summary, freq_", data[2,1], ', by="value"))', sep=""))))
#colnames(lu.summary)[3]<-paste(data[1,2], "_ha", sep="")
#colnames(lu.summary)[4]<-paste(data[2,2], "_ha", sep="")


#lu.summary<-na.omit(merge(lut.lc, freq_landuse_t4, by="value"))
#lu.summary<-na.omit(merge(lu.summary, freq_landuse_t5, by="value"))

#lu.summary<-freq(r.brick, merge=TRUE)
#lu.summary[n+2]<-NULL
#colnames(lut.lc)[1]<-"value"
#colnames(lut.lc)[2]<-"class"
#lu.summary<-merge(lut.lc, lu.summary, by="value")

#====load look up table=====
lookup_lc2<-lut.lc
lookup_l<- lut.lc
lookup_z <- lut.pu
lookup_lc<- lut.lc
colnames(lookup_l)<-c("ID", "CLASS")
colnames(lookup_lc)<-c("ID", "CLASS")
colnames(lookup_z)<-c("ID", "ZONE")

#====CREATE INDIVIDUAL TABLE FOR EACH LANDUSE MAP====

eval(parse(text=(paste("area_lc1<-freq_", data[1,1], sep=""))))
eval(parse(text=(paste("area_lc2<-freq_", data[2,1], sep=""))))
colnames(area_lc1)[1] = "ID"
colnames(area_lc1)[2] = "COUNT_LC1"
colnames(area_lc2)[1] = "ID"
colnames(area_lc2)[2] = "COUNT_LC2"
area_lc1<-merge(area_lc1,lookup_l,by="ID")
area_lc2<-merge(area_lc2,lookup_l,by="ID")
colnames(area_lc1)[3] = "CLASS_LC1"
colnames(area_lc2)[3] = "CLASS_LC2"


eval(parse(text=(paste("area_zone<-as.data.frame(freq(", pu,"))", sep=""))))
colnames(area_zone)[1] = "ID"
colnames(area_zone)[2] = "COUNT_ZONE"
area_zone<-merge(area_zone,lookup_z,by="ID")

area<-min(sum(area_zone[,2]), sum(area_lc1[,2]), sum(area_lc2[,2]))

#Cross
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
lg_chg$ID1<-as.numeric(as.character((lg_chg$ID_LC1)))
lg_chg$ID2<-as.numeric(as.character((lg_chg$ID_LC2)))
lg_chg$IDC<-lg_chg$ID1-lg_chg$ID2
lg_chg<-lg_chg[ which(lg_chg$IDC!=0),]
lg_chg <- as.data.frame(lg_chg[order(-lg_chg$COUNT),])
lg_chg$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg$LU_CHG, minlength=5, strict=FALSE, method="both")))
lg_chg$ID1<-lg_chg$ID2<-lg_chg$IDC<-NULL
lg_chg_top<-head(lg_chg, n=10)
lg_chg_top$LC_t1<-lg_chg_top$LC_t2<-NULL

#====Landuse Dominant Change====
chg_only<-aggregate(COUNT~LU_CHG,data=lg_chg,FUN=sum)
chg_only$CHG_CODE<-as.factor(toupper(abbreviate(chg_only$LU_CHG, minlength=5, strict=FALSE, method="both")))
chg_only<-chg_only[order(-chg_only$COUNT),]
chg_only<-chg_only[c(1,3,2)]
chg_only_top<-head(chg_only, n=10)

#====Zonal Dominant Change====
lg_chg_zonal<-as.data.frame(NULL)
for (l in 1:length(area_zone$ID)){
  tryCatch({
    a<-(area_zone$ID)[l]
    lg_chg_z<-lg_chg
    lg_chg_z<-as.data.frame(lg_chg_z[which(lg_chg_z$ZONE == a),])
    lg_chg_z<-aggregate(COUNT~ZONE+LU_CHG,data=lg_chg_z,FUN=sum)
    lg_chg_z$CHG_CODE<-as.factor(toupper(abbreviate(lg_chg_z$LU_CHG, minlength=5, strict=FALSE, method="both")))
    lg_chg_z<-lg_chg_z[order(-lg_chg_z$COUNT),]
    lg_chg_z<-lg_chg_z[c(1,2,4,3)]
    lg_chg_z_10<-head(lg_chg_z,n=10)
    lg_chg_zonal<-rbind(lg_chg_zonal,lg_chg_z_10)
  },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#====calculate basic statistic====
for (z in 3:ncol(lu.summary)){
  lu.summary[z]<-lu.summary[z]*Spat_res
}
Ov_chg<-lu.summary
colnames(Ov_chg)[2]="Land_use_type"
Ov_chg$LU_CODE<-as.factor(toupper(abbreviate(Ov_chg$Land_use_type, minlength=4, strict=FALSE, method="both")))

c.name<-NULL
a<-nrow(data3)

for(m in 2:a){
  if (m!=a) {
    eval(parse(text=(paste("Period<-period", m,"-period", m-1, sep=""))))
    sub<-eval(parse(text=(paste("period", m, sep=""))))
    sub<-paste(sub,"_ha", sep="")
    eval(parse(text=(paste("colnames(Ov_chg)[", m+2, ']="', sub, '"', sep=""))))
    c.name<-c(c.name,sub)
  }
}

Ov_chg.ha<-Ov_chg[1:2]
Ov_chg.rate<-Ov_chg.ha
count.period<-a-1
p.name<-NULL
p.name2<-NULL

for(o in 1:count.period){
  name<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_ha", sep="")
  name2<-paste((eval(parse(text=(paste("period",o,sep=""))))), "-", eval(parse(text=(paste("period",o+1,sep="")))),"_%/yrs", sep="")
  eval(parse(text=(paste("p.chg", "<-Ov_chg[", o, "+2+1]-Ov_chg[", o, "+2]", sep=""))))
  p.rate<-round(((p.chg/(Ov_chg[3]*Period))*100), 2)
  colnames(p.chg)[1]<-name
  colnames(p.rate)[1]<-name2
  Ov_chg.ha<-cbind(Ov_chg.ha,p.chg)
  Ov_chg.rate<-cbind(Ov_chg.rate,p.rate)
  p.name<-c(p.name,name)
  p.name2<-c(p.name2,name2)
}

#WARNING: OUTPUT FILE UP To THIS POINT ARE Ov_chg, Ov_chg.ha, Ov_chg.rate, lu.db!!!

#====CREATE OVERALL CHANGE TABLE AND GRAPH====
Ov_chg.melt <- melt(data = Ov_chg, id.vars=c('Land_use_type','LU_CODE'))
Ov_chg.melt<-Ov_chg.melt[which(Ov_chg.melt$variable!="value"),]
colnames(Ov_chg.melt)<-c("Land_use_type","LU_CODE", "Year", "Area")

ov.change.plot.2<-ggplot(Ov_chg.melt,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=10),legend.text = element_text(size = 10), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())

#====CREATE OVERALL CHANGE TABLE AND GRAPH IN RATE====
#Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:4]),(Ov_chg[6])) ##SOURCE OF ERROR STILL IN HARDCODE
eval(parse(text=(paste("Ov_chg.merge<-cbind(Ov_chg.ha,(Ov_chg.rate[3:",2+count.period,"]),(Ov_chg[", 2+a+1, "]))", sep=""))))

Ov_chg.melt2 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name)
colnames(Ov_chg.melt2)<-c("Land_use_type","LU_CODE", "Year", "Area")
ov.change.plot.3<-ggplot(Ov_chg.melt2,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())+coord_flip()

Ov_chg.melt3 <- melt(data = Ov_chg.merge, id.vars=c('Land_use_type','LU_CODE'), measure.vars=p.name2)
colnames(Ov_chg.melt3)<-c("Land_use_type","LU_CODE", "Year", "Area")
ov.change.plot.4<-ggplot(Ov_chg.melt3,aes(x=reorder(LU_CODE, -Area),y=Area,fill=Year))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x= element_text(angle=45,hjust=1))+ ylab("Area (Ha)") + xlab("Land use type") +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8), axis.text.x = element_text(size = 10),
         axis.title.x=element_blank())+coord_flip()+ylim (c(-100, 100))

#Create Database
setwd(result_dir)
write.dbf(Ov_chg, "Overall_change.dbf")

#====ALPHA BETA TABLE AND CHART====
alphabeta<-function(cross_temp_all, lookup_cover, t1, t2, area.analysis) {
  
  #select LU changes for creating blank diagonal values matrix
  cross_temp.blank_diag<-cross_temp_all
  cross_temp.blank_diag$check<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
  #cross_temp.blank_diag<-filter(cross_temp.blank_diag, check!=0)
  cross_temp.blank_diag<-cross_temp.blank_diag[which(cross_temp.blank_diag$check!=0),]
  
  #create land use transition matrix
  cross_temp.blank_diag$ID_LC1<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC1))))
  colnames(lookup_cover)[1] = "ID_LC1"
  colnames(lookup_cover)[2] = "LC1"
  cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC1', all=T)
  
  cross_temp.blank_diag$ID_LC2<-as.numeric(as.character(unlist((cross_temp.blank_diag$ID_LC2))))
  colnames(lookup_cover)[1] = "ID_LC2"
  colnames(lookup_cover)[2] = "LC2"
  cross_temp.blank_diag<-merge(cross_temp.blank_diag,lookup_cover[],by='ID_LC2', all=T)
  
  cross_temp.melt <- melt(data = cross_temp.blank_diag, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
  cross_temp.melt.cast <- dcast(data = cross_temp.melt, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
  cross_temp.melt.cast[1]<-NULL
  cross_temp.melt.cast<-cross_temp.melt.cast[-nrow(cross_temp.melt.cast),]; #remove last row
  cross_temp.melt.cast<-cross_temp.melt.cast[,-ncol(cross_temp.melt.cast)]; #remove last column
  
  a.table<-cross_temp.melt.cast
  tot.col<-colSums(cross_temp.melt.cast)
  
  
  for(x in 1:ncol(a.table)){
    eval(parse(text=(paste("a.table[", x, "]<-a.table[",x,"]/tot.col[",x,"]", sep=""))))
  }
  a.table[is.na(a.table)] <- 0
  a.table$"NA"<-NULL ;#remove NA column
  a.table<-a.table[-nrow(a.table),];#remove last NA row
  a.table<-do.call(data.frame,lapply(a.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  
  a.val<-rowSums(a.table)
  
  #calculate beta
  b.table<-cross_temp.melt.cast
  tot.row<-rowSums(cross_temp.melt.cast)
  
  for(y in 1:ncol(b.table)){
    eval(parse(text=(paste("b.table[", y, ",]<-b.table[",y,",]/tot.row[",y,"]", sep=""))))
  }
  b.table[is.na(b.table)] <- 0
  b.table<-do.call(data.frame,lapply(b.table, function(x) replace(x, is.infinite(x),0)));#replace Inf
  b.table$"NA."<-NULL
  b.table <- b.table[-nrow(b.table),];#remove last NA row
  b.val<-colSums(b.table)
  
  
  #Bind alpha and beta
  eval(parse(text=( paste("id<-na.omit(as.integer(colnames(cross_temp.melt.cast)))",sep=''))))
  fromto.table<-cbind(na.omit(id),a.val,b.val)
  
  colnames(lookup_cover)[1] = "ID"
  colnames(lookup_cover)[2] = "LC"
  colnames(fromto.table)<-c("ID","a.val","b.val")
  fromto.table<-merge(lookup_cover,fromto.table, by='ID', all=T); #<- ALPHA beta TABLE
  
  #============================
  #LULC summary
  
  area.lu1<-ddply(cross_temp_all, c("ID_LC1"), summarise,LU1=sum(COUNT))
  colnames(area.lu1)[1]<-"ID"
  area.lu2<-ddply(cross_temp_all, c("ID_LC2"), summarise,LU2=sum(COUNT))
  colnames(area.lu2)[1]<-"ID"
  LULC_summary<-merge(area.lu1, area.lu2, by="ID", ALL=T)
  LULC_summary[is.na(LULC_summary)] <- 0
  LULC_summary <- LULC_summary[-nrow(LULC_summary),]
  
  
  LULC_summary$Change<-as.numeric(LULC_summary$LU2)-as.numeric(LULC_summary$LU1)
  LULC_summary$Total_change<-abs(LULC_summary$Change)
  fromto.table_LULC<-na.omit(merge(fromto.table, LULC_summary, by='ID', all=T))
  value<-data.frame(stringsAsFactors=FALSE)
  
  #NEgative Change or positive change
  for(d in 1:nrow(fromto.table_LULC)){
    if(fromto.table_LULC[d,7]>0){
      nval<-1
      value<-rbind(value,nval)
    } else {
      nval<-0
      value<-rbind(value,nval)
    }}
  
  value[value==0]<-"decreasing"
  value[value==1]<-"increasing"
  colnames(value)<-"Trend"
  fromto.table_LULC<-cbind(fromto.table_LULC,value)
  fromto.table_LULC<-arrange(fromto.table_LULC, -Total_change)
  
  #Rate of Change
  fromto.table_LULC$rate<-(abs(fromto.table_LULC$Total_change/fromto.table_LULC$LU1)*100)
  fromto.table_LULC$rate[is.na(fromto.table_LULC$rate)] <- 0
  
  fromto.table_LULC$area<-area.analysis
  fromto.table_LULC$lc_abr<-toupper(abbreviate(fromto.table_LULC$LC))
  
  print(fromto.table_LULC)
  return(fromto.table_LULC)
}

alphabeta.plot<-function(alphabeta_table, t1, t2, area.analysis){
  alphabeta_table[alphabeta_table$a.val<0.5&alphabeta_table$b.val<0.5, 1:10]<-NA ;#IGNORED TRANSTITION LESS THAN 1
  ab.plot<-ggplot(alphabeta_table, aes(x=a.val, y=b.val,xend=6, yend=6)) +
    geom_point(data=alphabeta_table,aes(x=a.val, y=b.val,size =Total_change, colour =Trend), alpha=.5)+
    geom_text(size=3, aes(label=lc_abr),hjust=0.5,vjust=-1, angle =0)+
    scale_size(range = c(1,50)) + labs(x = paste("Alpha", t1), y= paste("Beta", t2))+ ggtitle(paste(area.analysis, t1, '-', t2))
  #+theme_bw()
  
  return(ab.plot)}

#ALPHA BETA AT LANDSCAPE LEVEL
landscape.alphabeta<-alphabeta(cross, lookup_lc, T1, T2, paste("Keseluruhan", location))
landscape.alphabeta.plot<-alphabeta.plot(landscape.alphabeta, T1, T2,paste("Keseluruhan", location))

#ALPHA BETA AT PLANNING UNIT LEVEL
for(i in 1:nrow(lookup_z)){
  if (i==1){
    zone_id<-lookup_z$ZONE[i]
    print(zone_id)
    eval(parse(text=( paste("cross_temp_zone<-na.omit(cross[ which(cross$ZONE==",zone_id,"),])", sep=''))))
    zona<-paste(lookup_z$Z_NAME[i])
    eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
    #eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(",cross_temp_zone,",", lookup_l,",", T1, ",",T2,",", zona,")", sep=''))))
    eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
    #plot.name<-paste("alpha_beta_",location,"_",T1,"_",T2,"_zona_", zona,".png", sep='')
    #eval(parse(text=( paste('ggsave(alphabeta_plot_zone_', zone_id,",file= plot.name,width=20,height=20, units='cm')", sep='')))) 
    alpha_beta_database<-rbind(landscape.alphabeta,alphabeta_zone_1)
  } else {
    zone_id<-lookup_z$ZONE[i]
    print(zone_id)
    eval(parse(text=( paste("cross_temp_zone<-na.omit(cross[ which(cross$ZONE==",zone_id,"),])", sep=''))))
    zona<-paste(lookup_z$Z_NAME[i])
    eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(cross_temp_zone, lookup_l, T1,T2, zona)", sep=''))))
    #eval(parse(text=( paste('alphabeta_zone_', zone_id,"<-alphabeta(",cross_temp_zone,",", lookup_l,",", T1, ",",T2,",", zona,")", sep=''))))
    eval(parse(text=( paste('alphabeta_plot_zone_', zone_id,"<-alphabeta.plot(alphabeta_zone_", zone_id,", T1, T2, zona)", sep=''))))
    eval(parse(text=( paste("alpha_beta_database<-rbind(alpha_beta_database, alphabeta_zone_", zone_id,")", sep=''))))
    
  }}

write.dbf(alpha_beta_database, "Pre_QUES_IO_table.dbf")


#====Create Map for report====
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- rev(brewer.pal(11, "RdYlGn"))
myColors8 <- "#000000"
myColors9 <- brewer.pal(12, "Set3")

if (0 %in% area_zone$ID){
  myColors  <-c(myColors8, myColors5,myColors1, myColors2, myColors7, myColors4, myColors5, myColors6)
} else {
  myColors  <-c(myColors5,myColors1, myColors2, myColors3, myColors4, myColors7, myColors6)
}

#zone map
eval(parse(text=(paste("zone<-", pu, sep=""))))
myColors.Z <- myColors[1:length(unique(area_zone$ID))]
ColScale.Z<-scale_fill_manual(name="Unit perencanaan", breaks=area_zone$ID, labels=area_zone$ZONE, values=myColors.Z)
plot.Z<-gplot(zone, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
  coord_equal() + ColScale.Z +
  theme(plot.title = element_text(lineheight= 5, face="bold")) +
  theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
         panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
         legend.title = element_text(size=8),
         legend.text = element_text(size=6),
         legend.key.height = unit(0.25, "cm"),
         legend.key.width = unit(0.25, "cm"))

#rm(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6, myColors9)

#Largest Source of Change in Landuse
colnames(chg_only_top)[3]<-"COUNT"
Largest.chg<- ggplot(data=chg_only_top, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
  geom_text(data=chg_only_top, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
  ggtitle(paste("10 Perubahan Tutupan Lahan Dominan di", location, T1,"-",T2 )) +
  labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
  theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())

setwd(result_dir)
#====WRITE REPORT====
title1<-"{\\colortbl;\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red146\\green208\\blue80;\\red0\\green176\\blue240;\\red140\\green175\\blue71;\\red0\\green112\\blue192;\\red79\\green98\\blue40;} \\pard\\qr\\b\\fs70\\cf2 L\\cf3U\\cf4M\\cf5E\\cf6N\\cf7S \\cf1REPORT \\par\\b0\\fs20\\ql\\cf1"
title2<-paste("\\pard\\qr\\b\\fs40\\cf1 PreQUES-Land Use Change Analysis ", "for ", location, ", ", province, ", ", country, "\\par\\b0\\fs20\\ql\\cf1", sep="")
sub_title<-"\\cf2\\b\\fs32 ANALISA PERUBAHAN PENGGUNAAN LAHAN\\cf1\\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Proses dimulai : ", time_start, sep="")
time_end<-paste("Proses selesai : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
area_name_rep<-paste("\\b", "\\fs20", location, "\\b0","\\fs20")
I_O_period_1_rep<-paste("\\b","\\fs20", T1)
I_O_period_2_rep<-paste("\\b","\\fs20", T2)
chapter1<-"\\cf2\\b\\fs28 DATA YANG DIGUNAKAN \\cf1\\b0\\fs20"
chapter2<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT BENTANG LAHAN \\cf1\\b0\\fs20"
chapter3<-"\\cf2\\b\\fs28 HASIL ANALISA PADA TINGKAT UNIT PERENCANAAN \\cf1\\b0\\fs20"
rtffile <- RTF("LUMENS_Pre-QUES_change_report.lpr", font.size=9)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, title1)
addParagraph(rtffile, title2)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
width<-as.vector(c(1.34,3.1))
addTable(rtffile,proj_descr,font.size=8,col.widths=width)
addPageBreak(rtffile)
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
addParagraph(rtffile, chapter1)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Data yang digunakan dalam analisa ini adalah data peta penggunaan lahan dan data peta unit perencanaan daerah. Data pendukung yang digunakan adalah peta acuan tipe penggunaan lahan dan data acuan kelas unit perencanaan")
addNewLine(rtffile)
#addTable(rtffile,test3,font.size=8)
#addNewLine(rtffile)
for(q in 1:n){
  tryCatch({
    eval(parse(text=(paste("landusemap<-landuse_t", q, sep=""))))
    eval(parse(text=(paste("area_lcmap<-as.data.frame(freq(landuse_t", q,"))", sep=""))))
    colnames(area_lcmap)[1] = "ID"
    colnames(area_lcmap)[2] = "COUNT_LC1"
    colnames(lookup_l)[1]<-"ID"
    area_lcmap<-merge(area_lcmap,lookup_l,by="ID")
    colnames(area_lcmap)[3] = "CLASS_LC1"
    if (0 %in% area_lcmap$ID){
      myColors  <-c(myColors8, myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    } else {
      myColors  <-c(myColors7,myColors1, myColors2, myColors3, myColors4, myColors5, myColors6)
    }
    myColors.lu <- myColors[1:length(unique(area_lcmap$ID))]
    periodmap<-eval(parse(text=(paste("T",q, sep="" ))))
    addParagraph(rtffile, paste("\\b \\fs20 Peta Tutupan Lahan ", location, " tahun ", periodmap," \\b0 \\fs20", sep=""))
    addNewLine(rtffile, n=1)
    #Land cover map
    ColScale.lu<-scale_fill_manual(name="Jenis tutupan lahan", breaks=area_lcmap$ID, labels=area_lcmap$CLASS_LC1, values=myColors.lu)
    plot.LU<-gplot(landusemap, maxpixels=100000) + geom_raster(aes(fill=as.factor(value))) +
      coord_equal() + ColScale.lu +
      theme(plot.title = element_text(lineheight= 5, face="bold")) +
      theme( axis.title.x=element_blank(),axis.title.y=element_blank(),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title = element_text(size=8),
             legend.text = element_text(size = 6),
             legend.key.height = unit(0.25, "cm"),
             legend.key.width = unit(0.25, "cm"))
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, plot.LU )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Peta Unit Perencanaan "," \\b0 \\fs20", sep=""))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, plot.Z)
addNewLine(rtffile)
addParagraph(rtffile, chapter2)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk keseluruhan bentang lahan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada bentang lahan yang dianalisa")
addNewLine(rtffile)
addParagraph(rtffile, "Tabel Intisari Perubahan Tutupan Lahan menyajikan perbandingan luasan tipe-tipe tutupan lahan di sebuah bentnag lahan pada kurun waktu tertentu. Kolom Overall Change menunjukkan perubahan luasan dalam satuan hektar. Notasi negatif pada kolom ini menunjukkan pengurangan luasan sebaliknya notasi positif menunjukkan penambahan luasan. Kolom Rate menunjukkan laju perubahan luasan tutupan lahan dalam satuan %/tahun. Kolom ini dihitung dengan mengurangi luasan pada t2-t1 kemudian dibagi dengan perkalian luasan pada t1 dan kurun waktu analisa")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Intisari Perubahan Tutupan Lahan di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,0.69,0.69,0.69,0.69))
addTable(rtffile,Ov_chg,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.2)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Luasan Perubahan Tutupan Lahan (ha) di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,0.69,0.69))
addTable(rtffile,Ov_chg.ha,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.3)

addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Rerata Luasan Perubahan Tutupan Lahan (%/tahun) di", location, "\\b \\fs20", sep=" "))
#width<-as.vector(c(0.44,2,1,1))
addTable(rtffile,Ov_chg.rate,font.size=8)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=4,res=150,  ov.change.plot.4)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Sepuluh Perubahan Lahan Dominan di\\b0 \\fs20 ",location, "\\b0 \\fs20", sep=" "))
addNewLine(rtffile, n=1)
colnames(chg_only_top)[3]<-"Luas(ha)"
addTable(rtffile, chg_only_top)
addNewLine(rtffile, n=1)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150,Largest.chg)

addNewLine(rtffile)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Grafik IO: dinamika perubahan lahan di\\b0 \\fs20 ", area_name_rep, I_O_period_1_rep, "\\b \\fs20 - \\b0 \\fs20", I_O_period_2_rep, sep=" "))
addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150,  landscape.alphabeta.plot)
addNewLine(rtffile)
addNewLine(rtffile)

addParagraph(rtffile, chapter3)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, "Pada bagian ini disajikan hasil analisa perubahan penggunaan lahan untuk masing-masing kelas unit perencanaan yang dianalisa. Beberapa bentuk analisa yang dilakukan antara lain: perbandingan luasan tutupan lahan pada periode analisa dan tipe perubahan lahan dominan pada unit perencanaan yang dianalisa")
addNewLine(rtffile)
for(i in 1:length(area_zone$ID)){
  tryCatch({
    zonal.db<-area_zone
    zonal.db$Z_CODE<-toupper(abbreviate(zonal.db$ZONE))
    zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
    zona_nm<-paste("\\b", "\\fs20", zonal.db$ZONE[i], "\\b0","\\fs20")
    zona_ab<-paste("\\b", "\\fs20", zonal.db$Z_CODE[i], "\\b0","\\fs20")
    addParagraph(rtffile, "\\b \\fs20 Sepuluh Tipe Perubahan Tutupan Lahan Dominan di Unit Perencanaan \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
    addNewLine(rtffile, n=1)
    lg_chg_zon<-lg_chg_zonal[which(lg_chg_zonal$ZONE == i),]
    lg_chg_zon$ZONE<-NULL
    lg_chg_plot<-lg_chg_zon
    colnames(lg_chg_zon)[3]<-"Luas (ha)"
    addTable(rtffile, lg_chg_zon)
    addNewLine(rtffile, n=1)
    #Largest Source of Change in Planning Unit Level
    Largest.chg.z<- ggplot(data=lg_chg_plot, aes(x=reorder(CHG_CODE, -COUNT),y=COUNT, fill=CHG_CODE))+geom_bar(stat='identity',position='dodge')+
      geom_text(data=lg_chg_plot, aes(x=CHG_CODE, y=COUNT, label=round(COUNT, 1)),size=3, vjust=0.1) +
      ggtitle(paste("10 Perubahan Dominan pada Unit Perencanaan",i, "-", zonal.db$Z_CODE[i] )) + guides(fill=FALSE) +
      labs(x = 'Jenis perubahan penutupan lahan', y='Luas area (Ha)') + guides(fill=FALSE)+
      theme(plot.title = element_text(lineheight= 5, face="bold")) + scale_y_continuous() +
      theme(axis.title.x=element_blank(), axis.text.x = element_text(size=8),
            panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
    addPlot.RTF(rtffile, plot.fun=plot, width=6.7, height=3, res=150, Largest.chg.z )
    addNewLine(rtffile, n=1)
  },error=function(e){cat("Nice try pal! ~ please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}

for(i in 1:nrow(lookup_z)){
  tryCatch({
    zona<-paste("\\b", "\\fs20", i, "\\b0","\\fs20")
    zona_nm<-paste("\\b", "\\fs20", lookup_z$ZONE[i], "\\b0","\\fs20")
    zona_ab<-paste("\\b", "\\fs20", lookup_z$Z_NAME[i], "\\b0","\\fs20")
    addParagraph(rtffile, "\\b \\fs20 Grafik IO: dinamika perubahan lahan di \\b0 \\fs20", zona,"\\b \\fs20 - \\b0 \\fs20", zona_nm, "\\b \\fs20 (\\b0 \\fs20", zona_ab, "\\b \\fs20)\\b0 \\fs20" )
    
    addNewLine(rtffile)
    eval(parse(text=( paste("addPlot(rtffile,plot.fun=print, width=6.7,height=5,res=150, alphabeta_plot_zone_",i,")", sep=''))))
    
    addNewLine(rtffile)
    
  },error=function(e){cat("please re-check your input data :",conditionMessage(e), "\n"); addParagraph(rtffile, "no data");addNewLine(rtffile)})
}


addNewLine(rtffile)
done(rtffile)

#====database export====
PreQUES.index=PreQUES.index+1
eval(parse(text=(paste("PreQUES_data", PreQUES.index, "<-data", sep=""   ))))
newPre<-paste("PreQUES_data", PreQUES.index, sep="")
eval(parse(text=(paste( "lu.db", PreQUES.index, "<-lu.db", sep=""))))
newludb<-paste( "lu.db", PreQUES.index,sep="")


list.landuse<-ls(pattern="landuse_t")
command<-paste("resave(PreQUES.index,r.brick, Ov_chg,Ov_chg.ha,Ov_chg.rate,", newPre, ",", newludb, ",",  sep="")

#for (q in 1:n) {
#  command<-paste(command,list.landuse[q],",", sep="")
#}
#command<-paste(command,'file="lumens_banyuasin_ed.lpd")', sep="")
command<-paste(command,'file="', lumens_database, '")', sep="")

setwd(working_directory)
eval(parse(text=(command)))


command2<-paste("start ", "winword ", result_dir, "/LUMENS_Pre-QUES_change_report.lpr", sep="" )
shell(command2)


#CLEAN ENVIRONMENT
#rm(list=ls(all.names=TRUE))
