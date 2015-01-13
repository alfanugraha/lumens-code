##[SCIENDO]=group
##workingDirectory=folder
##carbonData=file
##period1=number 2005
##period2=number 2010
##iteration=number 5


#====1_load_library====
library(pander)
library(knitr)
library(markdown)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(foreign)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

#====2_workingDirectory====
setwd(workingDirectory)

#====3_load_datasets====
LUTMDatabase <- read.dbf(carbonData)
period <- period2-period1
total <- sum(LUTMDatabase$COUNT)

LUTMDatabaseMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))

areaLandCover1 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC1 ~ ., fun.aggregate = sum)
colnames(areaLandCover1)[2] ="COUNT"

areaLandCover2 <- dcast(data = LUTMDatabaseMelt, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(areaLandCover2)[2] ="COUNT"

LUTMDatabaseMelt <- melt(data = LUTMDatabase, id.vars=c('ZONE'), measure.vars=c('COUNT'))
areaZone <- dcast(data = LUTMDatabaseMelt, formula = ZONE ~ variable, fun.aggregate = sum)

#====4_calculate_transition_probability_1st_iteration====
colnames(areaZone)[2] = "Z_AREA"
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaZone,by="ZONE"))

LUTMDatabase$ID_LC1_FR <- LUTMDatabase$ID_LC1
LUTMDatabase$ID_LC2_TO <- LUTMDatabase$ID_LC2
LUTMDatabase$ID_LC2_FR <- LUTMDatabase$ID_LC1

colnames(LUTMDatabase)[4] = "COUNT1_2"

colnames(areaLandCover1)[1] ="ID_LC1_FR"
colnames(areaLandCover1)[2] ="OVCOUNT_T1FR"

colnames(areaLandCover2)[1] ="ID_LC2_TO"
colnames(areaLandCover2)[2] ="OVCOUNT_T2TO"

LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover1,by="ID_LC1_FR"))
LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_TO"))

colnames(areaLandCover2)[1] ="ID_LC2_FR"
colnames(areaLandCover2)[2] ="OVCOUNT_T2FR"

LUTMDatabase <- as.data.frame(merge(LUTMDatabase,areaLandCover2,by="ID_LC2_FR"))

LUTMDatabase$TPM <- LUTMDatabase$COUNT1_2 / LUTMDatabase$OVCOUNT_T1FR     #hitung proporsi for all zone: tpm = count of class1to2/sumOfClass1

LUTMDatabase$COUNT2_3 <- LUTMDatabase$TPM * LUTMDatabase$OVCOUNT_T2FR     #hitung nilai perubahan baru: newCount = tpm*sumOfClass2

LUTMDatabase$LUTMLandscape <- LUTMDatabase$COUNT2_3 / total

LUTMDatabase$LUTMZone <- LUTMDatabase$COUNT2_3 / LUTMDatabase$Z_AREA

LUTMDatabase$CEK_EM <- LUTMDatabase$CARBON_t1 > LUTMDatabase$CARBON_t2
LUTMDatabase$CEK_SQ <- LUTMDatabase$CARBON_t1 < LUTMDatabase$CARBON_t2

LUTMDatabase$EM0 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT1_2 * 3.67
LUTMDatabase$SQ0 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT1_2 * 3.67

LUTMDatabase$EM1 <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT2_3 * 3.67
LUTMDatabase$SQ1 <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT2_3 * 3.67

LUTM1<-LUTMDatabase

#====5_calculate_transition_probability_for_the_next_n-iter====
for(i in 2:iteration){
eval(parse(text=(paste( "LUTMDatabase$ID_LC", i+1, "_TO <- LUTMDatabase$ID_LC2", sep="" ))))
eval(parse(text=(paste( "LUTMDatabase$ID_LC", i+1, "_FR <- LUTMDatabase$ID_LC1", sep="" ))))

eval(parse(text=(paste( "LUTMDatabaseMelt <- melt(data = LUTMDatabase, id.vars=c('ID_LC", i+1, "_TO'), measure.vars=c('COUNT", i, "_", i+1, "'))", sep="" ))))

eval(parse(text=(paste( "areaLandCover", i+1, " <- dcast(data = LUTMDatabaseMelt, formula = ID_LC", i+1, "_TO ~ ., fun.aggregate = sum)", sep="" ))))
eval(parse(text=(paste( "colnames(areaLandCover", i+1, ")[1] ='ID_LC", i+1, "_FR'", sep="" ))))
eval(parse(text=(paste( "colnames(areaLandCover", i+1, ")[2] ='OVCOUNT_T", i+1, "FR'", sep="" ))))

eval(parse(text=(paste( "LUTMDatabase <- as.data.frame(merge(LUTMDatabase, areaLandCover", i+1, ", by='ID_LC", i+1, "_FR'))", sep="" ))))
eval(parse(text=(paste( "LUTMDatabase$COUNT", i+1, "_", i+2, " <- LUTMDatabase$TPM * LUTMDatabase$OVCOUNT_T", i+1, "FR", sep="" ))))

eval(parse(text=(paste( "LUTMDatabase$EM", i, " <- (LUTMDatabase$CARBON_t1 - LUTMDatabase$CARBON_t2) * LUTMDatabase$CEK_EM * LUTMDatabase$COUNT", i+1, "_", i+2, " * 3.67", sep="" ))))
eval(parse(text=(paste( "LUTMDatabase$SQ", i, " <- (LUTMDatabase$CARBON_t2 - LUTMDatabase$CARBON_t1) * LUTMDatabase$CEK_SQ * LUTMDatabase$COUNT", i+1, "_", i+2, " * 3.67", sep="" ))))
eval(parse(text=(paste( "LUTM", i, " <- LUTMDatabase", sep="" ))))
}

#====summary====
Parameters <- c("Total emission (CO2 eq)", "Total sequestration (CO2 eq)", "Net emission (CO2 eq)", "Emission rate (CO2 eq/(ha.yr))", "Cumulative emission (CO2 eq/(ha.yr))")

sum_em0 <- sum(LUTMDatabase$EM0)
sum_sq0 <- sum(LUTMDatabase$SQ0)
net_em0 <- sum(sum_em0-sum_sq0)
rate_em0 <- net_em0/(total*period)
cum0 <- 0
Base <- c(sum_em0,sum_sq0,net_em0,rate_em0,cum0)

for(i in 1:iteration){
eval(parse(text=(paste( "sum_em", i, " <- sum(LUTMDatabase$EM", i, ")", sep="" ))))
eval(parse(text=(paste( "sum_sq", i, " <- sum(LUTMDatabase$SQ", i, ")", sep="" ))))
eval(parse(text=(paste( "net_em", i, " <- sum(sum_em", i, " - sum_sq", i, ")", sep="" ))))
eval(parse(text=(paste( "rate_em", i, " <- net_em", i, " / (total*period)", sep="" ))))
if(i==1){
eval(parse(text=(paste( "cum1 <- rate_em0 + rate_em1", sep="" ))))
} else {
eval(parse(text=(paste( "cum", i, " <- cum", i-1, " + rate_em", i, sep="" ))))
}
eval(parse(text=(paste( "Iteration", i, " <- c(sum_em", i, ", sum_sq", i, ", net_em", i, ", rate_em", i, ", cum", i, ")", sep="" ))))
if(i==1){
eval(parse(text=(paste( "summary_SCIENDO_iteration1 <- data.frame(Parameters, Base, Iteration1)", sep="" ))))
} else {
eval(parse(text=(paste( "summary_SCIENDO_iteration", i, " <- data.frame(summary_SCIENDO_iteration", i-1, ", Iteration", i, ")", sep="" ))))
}
}

#====save_SCIENDO-LUWES_Database====
eval(parse(text=(paste( "SCIENDO_LUWES <- LUTM", iteration, sep="" ))))

#====save_SCIENDO-LUWES Summary====
eval(parse(text=(paste( "SCIENDO_LUWES_summary <- summary_SCIENDO_iteration", iteration, sep="" ))))

#====remove_unnecessary_column====
SCIENDO_LUWES$ID_LC6_FR<-SCIENDO_LUWES$ID_LC5_FR<-SCIENDO_LUWES$ID_LC4_FR<-SCIENDO_LUWES$ID_LC3_FR<-SCIENDO_LUWES$ID_LC2_FR<-SCIENDO_LUWES$ID_LC2_TO<-SCIENDO_LUWES$ID_LC1_FR<-NULL
SCIENDO_LUWES$em<-SCIENDO_LUWES$sq<-SCIENDO_LUWES$null<-SCIENDO_LUWES$nullCek<-SCIENDO_LUWES$ZONE_ID<-SCIENDO_LUWES$COUNT<-SCIENDO_LUWES$OVCOUNT_T1FR<-SCIENDO_LUWES$OVCOUNT_T2TO<-NULL
SCIENDO_LUWES$OVCOUNT_T2FR<-SCIENDO_LUWES$ID_LC3_TO<-SCIENDO_LUWES$OVCOUNT_T3FR<-SCIENDO_LUWES$ID_LC4_TO<-SCIENDO_LUWES$OVCOUNT_T4FR<-SCIENDO_LUWES$ID_LC5_TO<-SCIENDO_LUWES$OVCOUNT_T5FR<-SCIENDO_LUWES$ID_LC6_TO<-SCIENDO_LUWES$OVCOUNT_T6FR<-NULL

#====SCIENDO-LUWES Overall LUTM====
LUTMOverall<-as.data.frame(as.numeric(SCIENDO_LUWES$ID_LC1))
colnames(LUTMOverall)[1] ="ID_LC1"
LUTMOverall$ID_LC2 <- as.numeric(SCIENDO_LUWES$ID_LC2)
LUTMOverall$landcover_t1 <- SCIENDO_LUWES$LC_t1
LUTMOverall$landcover_t2 <- SCIENDO_LUWES$LC_t2
LUTMOverall$ZONE <- SCIENDO_LUWES$Z_NAME
LUTMOverall$TPM <- SCIENDO_LUWES$TPM
LUTMOverall$LUTM <- SCIENDO_LUWES$LUTMLandscape

LUTMOverallMelt <- melt(data = LUTMOverall, id.vars=c('ID_LC1','ID_LC2','landcover_t1','landcover_t2'), measure.vars=c('TPM'))
TPMatrix <- dcast(data = LUTMOverallMelt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)

LUTMOverallMelt <- melt(data = LUTMOverall, id.vars=c('ID_LC1','ID_LC2','landcover_t1'), measure.vars=c('LUTM'))
LUTMatrix <- dcast(data = LUTMOverallMelt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)

#====SCIENDO-LUWES Zones LUTM====
LUTMZones<-SCIENDO_LUWES
LUTMZones$LUTM_Z <- LUTMZones$COUNT1_2 / LUTMZones$Z_AREA

LUTMZonesMelt <- melt(data = LUTMZones, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('LUTM_Z'))
LUZoneP <- dcast(data = LUTMZonesMelt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum)
colnames(LUZoneP)[3] ="LUZONE"
LUZoneP$key <- do.call(paste, c(LUZoneP[c("LC_t1", "Z_NAME")], sep = " in "))
LUZoneP$LC_t2 <- LUZoneP$Z_NAME<-NULL
LUTMZones$key <- do.call(paste, c(LUTMZones[c("LC_t1", "Z_NAME")], sep = " in "))
LUTMZones <- merge(LUTMZones,LUZoneP,by="key")

LUTMZones$TPM_Z <- LUTMZones$LUTM_Z / LUTMZones$LUZONE

LUTMZones$ZONE<-LUTMZones$CARBON_t1<-LUTMZones$CARBON_t2<-LUTMZones$ck_em<-LUTMZones$ck_sq<-LUTMZones$LUCHG<-LUTMZones$ID_LC1<-LUTMZones$ID_LC2<-NULL
LUTMZones$TPM<-LUTMZones$COUNT2_3<-LUTMZones$LUTMLandscape<-LUTMZones$EM0<-LUTMZones$SQ0<-LUTMZones$EM1<-LUTMZones$SQ1<-LUTMZones$COUNT3_4<-LUTMZones$EM2<-LUTMZones$SQ2<-LUTMZones$COUNT4_5<-LUTMZones$EM3<-LUTMZones$SQ3<-LUTMZones$COUNT5_6<-LUTMZones$EM4<-LUTMZones$SQ4<-LUTMZones$COUNT6_7<-LUTMZones$EM5<-LUTMZones$SQ5<-NULL
LUTMZones$LU_CHG<-LUTMZones$key<-LUTMZones$LUTMZone<-LUTMZones$tpmx<-LUTMZones$A_lczone<-NULL
LUTMZones$Z_AREA<-LUTMZones$COUNT1_2<-LUTMZones$LC_t1.y<-LUTMZones$LUZONE<-NULL

#Plot data
#subdat <- SCIENDO_LUWES_summary[5,]
#subdat_l <- data.frame(Value = unlist(subdat))
#subdat_l=subdat_l[-1,]
#subdat_l<-subdat_l[-c(1,1)]
#baseline<-barplot(subdat_l)

#conduct analysis on the dataset
SCIENDO_LUWES_summary[,2:ncol(SCIENDO_LUWES_summary)]<-round(SCIENDO_LUWES_summary[,2:ncol(SCIENDO_LUWES_summary)],digits=2)
SL_overall<-SCIENDO_LUWES_summary
SL_analysis<-SCIENDO_LUWES
SL_overall.melt <- melt(data = SL_overall)
SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (CO2 eq/(ha.yr))"))
SL_overall_data<- melt(data = SL_overall.melt.cast)
SL_overall_data<-SL_overall_data[-c(1),]

t_1<-period2
t_2<-t_1+period
Period.db<-as.data.frame(NULL)
Periode<-as.data.frame(NULL)
for ( i in 1:nrow(SL_overall_data)){
period.int<-paste(t_1,"-",t_2, sep="")
Period.db<-c(Period.db,period.int)
t_1<-t_1+period
t_2<-t_1+period
}
Period.db<-as.character(Period.db)
t_1<-period1
t_2<-t_1+period
for ( i in 1:(nrow(SL_overall_data)+1)){
period.int<-paste(t_1,"-",t_2, sep="")
Periode<-c(Periode,period.int)
t_1<-t_1+period
t_2<-t_1+period
}
Periode<-as.character(Periode)

m.var<-'EM0'
for(i in 1:iteration){
var<-paste('EM',i,sep="")
m.var<-c(m.var,var)
}


#write output to file
write.dbf(SCIENDO_LUWES,"SCIENDO-LUWES_database.dbf")
#write.dbf(SCIENDO_LUWES_summary,"SCIENDO-LUWES_summary.dbf")
#write.dbf(LUTMZones,"SCIENDO-LUWES_zones_tpm.dbf")
#write.dbf(LUTMZones,"SCIENDO-LUWES_zones_tpm_model.dbf")
#write.dbf(LUTMOverall,"SCIENDO-LUWES_overall_lutm.dbf")
#write.dbf(TPMatrix,"SCIENDO-LUWES_overall_tpm_matrix.dbf")
#write.dbf(LUTMZones,"lutm_z.dbf")

#THIS PART IS INITIALLY IN ABACUS PROJECTION SCRIPT

# SELECT QUES-C DATABASE
t1=period1
t2=period2
period<-abs(t2-t1)
data_merge<-read.dbf(carbonData)
data_merge2<-read.dbf(paste(workingDirectory,"SCIENDO-LUWES_database.dbf", sep="/"))

pu <- melt(data = data_merge, id.vars=c('ZONE','Z_NAME'), measure.vars=c('COUNT'))
pu <- dcast(data = pu, formula = Z_NAME + ZONE ~ variable, fun.aggregate = sum )
pu$percentage<-(pu$COUNT/sum(pu$COUNT))
test<-as.character(pu$Z_NAME)
data_lookup<-melt(data=data_merge, id.vars=c('ID_LC1','LC_t1'))
data_lookup$variable<-data_lookup$value<-NULL
data_lookup<-unique(data_lookup)
colnames(data_lookup)<-c("ID","CLASS")

name.matrix<-data_lookup
name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$CLASS, minlength=4, method="both"))
name.matrix$order<-name.matrix$ID
name.matrix$order<-as.numeric(levels(name.matrix$order))[name.matrix$order]
name.matrix<- as.data.frame(name.matrix[order(name.matrix$order, decreasing=FALSE),])
name.matrix$order<-NULL
data_selec <- melt(data = data_merge, id.vars=c('LC_t1','LC_t2','Z_NAME'), measure.vars=c('COUNT'))

#Creating SCIENDO-Emission Baseline Database
iteration<-(ncol(data_merge2[,21:ncol(data_merge2)])/3)
col.select<-as.character(c('EM0','SQ0'))
for(i in 1:iteration){
EM.slc<-paste('EM',i,sep="")
col.select<-c(col.select,EM.slc)
SQ.slc<-paste('SQ',i,sep="")
col.select<-c(col.select,SQ.slc)
}
Baseline.db.1<-data_merge2[,1:12]
Baseline.db.2<-data_merge2[c(col.select)]
Baseline.db<-as.data.frame(cbind(Baseline.db.1,Baseline.db.2))
rm(Baseline.db.1)
rm(Baseline.db.2)

options(scipen=999)
Scenario_name<-gsub(" ","","Historical baseline")

#CREATING ABACUS PROJECT FILE
Gnrl.info.1<-c("file_version", "title","description", "numberofzones","total_area","time", "include_bg","include_modif", "using_bg_factor","using_modif_factor", "model_iteration")
Gnrl.info.2<-c("1.1.0", "SCIENDO", "Project description",length(unique(pu$ZONE)),sum(data_merge$COUNT), iteration, "false", "false", "true", "true", 2)
Gnrl.info<-paste(Gnrl.info.1,Gnrl.info.2,sep="=")

#General Information
fileConn<-file(paste(workingDirectory,"/",Scenario_name,".txt",sep=""))
text0<-"#GENERAL"
write(text0, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(Gnrl.info, paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")
text<-"\n#ZONE"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

#Zone information
zone<-pu[c('Z_NAME','percentage')]
log.val<-rep('true',length(pu$ZONE))
zone<-cbind(zone, log.val)
write.table(zone, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")

#Landuse Information
text<-"\n#LANDCOVER"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(name.matrix$CLASS, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t")

#Eligibility
egb<-matrix('true',ncol=nrow(name.matrix), nrow=nrow(name.matrix))
egb<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),egb))
colnames(egb)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#ELIGIBILITY"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(egb, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#Cost Benefit Unit
text<-"\n#COSTBENEFIT_UNIT"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
text<-"Private\tNet return received by the land-use operator, farmers"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

#Carbon Stock
text<-"\n#CARBONSTOCK"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
carbon<-matrix(ncol=nrow(name.matrix),nrow=nrow(name.matrix),0)
data_merge.melt <- melt(data = data_merge, id.vars=c('LC_t2','Z_NAME'), measure.vars=c('CARBON_t2'))
carbon <- dcast(data = data_merge.melt, formula = LC_t2 ~ Z_NAME, fun.aggregate = mean)
#for(i in 1:nrow(name.matrix)){
#for(j in 1:nrow(name.matrix)){
#carbon[i,j]<-round(unique(data_merge$CARBON_t1[which(data_merge$ID_LC1==i & data_merge$ID_LC2==j)]),digits=2)
#}
#}
#carbon<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),carbon))
#carbon[,2:23][carbon[,2:23]==0]<-format(carbon[,2:23][carbon[,2:23]==0], nsmall=1, digits=2)
#colnames(carbon)<-(c('//LandCover', as.character(name.matrix$CLASS)))
write.table(carbon, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#NPV Private
NPV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
NPV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),NPV))
#NPV[,2:23]<-format(NPV[,2:23],nsmall=1,digits=2)
colnames(NPV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#NPV_Private"
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(NPV, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")

#COST Benefit CONVERSION Private
for (i in 1:nrow(zone)){
CBCV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
CBCV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),CBCV))
#CBCV[,2:23]<-format(CBCV[,2:23], nsmall=1,digits=2)
colnames(CBCV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#COSTBENEFIT_CONVERSION_Private\tZONE=",zone$Z_NAME[i], sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(NPV, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#LANDCOVER CHANGE
write("", paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
LC_chg<-melt(data_merge2, id.vars=c('ZONE','Z_NAME','ID_LC1','ID_LC2'), measure.vars=c('LUTMZone'))
LC_chg$order1<-LC_chg$ID_LC1
LC_chg$order1<-as.numeric(levels(LC_chg$order1))[LC_chg$order1]
LC_chg$order2<-LC_chg$ID_LC2
LC_chg$order2<-as.numeric(levels(LC_chg$order2))[LC_chg$order2]
for(i in 1:nrow(zone)){
LC_chg_Z<-LC_chg[which(LC_chg$ZONE==i),]
LC_chg_Z_M<-dcast(LC_chg_Z, order1~order2, fun.aggregate=mean, value.var='value')
LC_chg_Z_M$order1<-NULL
LC_chg_Z_M<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),LC_chg_Z_M))
#LC_chg_Z_M[,2:23][LC_chg_Z_M[,2:23]==0]<-format(LC_chg_Z_M[,2:23][LC_chg_Z_M[,2:23]==0], nsmall=1, digits=2)
colnames(LC_chg_Z_M)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#LANDCOVER_CHANGE\tZONE=",unique(LC_chg_Z$Z_NAME[which(LC_chg_Z$ZONE==i)]), sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(LC_chg_Z_M, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#BelowGround Emission
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
BGE<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
BGE<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGE))
#BGE[,2:23]<-format(BGE[,2:23],nsmall=1,digits=2)
colnames(BGE)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#BELOWGROUND_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(BGE, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#BelowGround Emission Factor
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
BGEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
BGEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGEF))
#BGEF[,2:23]<-format(BGEF[,2:23],nsmall=1,digits=2)
colnames(BGEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#BELOWGROUND_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(BGEF, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Modif Emission
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
ME<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
ME<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),ME))
#ME[,2:23]<-format(ME[,2:23],nsmall=1,digits=2)
colnames(ME)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#MODIF_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(ME, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Modif Emission Factor
write("", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
MEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
MEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),MEF))
#MEF[,2:23]<-format(MEF[,2:23],nsmall=1,digits=2)
colnames(MEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#MODIF_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(MEF, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Transition Probability Matrix Iteration 0
write("", paste(workingDirectory,"/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
TPM<-melt(data_merge2, id.vars=c('ZONE','Z_NAME','ID_LC1','ID_LC2'), measure.vars=c('TPM'))
TPM$order1<-TPM$ID_LC1
TPM$order1<-as.numeric(levels(TPM$order1))[TPM$order1]
TPM$order2<-TPM$ID_LC2
TPM$order2<-as.numeric(levels(TPM$order2))[TPM$order2]
for(i in 1:nrow(zone)){
TPM_Z<-TPM[which(TPM$ZONE==i),]
TPM_Z_M<-dcast(TPM_Z, order1~order2, fun.aggregate=mean, value.var='value')
TPM_Z_M$order1<-NULL
TPM_Z_M<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),TPM_Z_M))
#TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0]<-format(TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0], nsmall=1, digits=2)
colnames(TPM_Z_M)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#TRANSITION_PROBABILITY_MATRIX\tITERATION=0\tZONE=",unique(TPM_Z$Z_NAME[which(TPM_Z$ZONE==i)]), sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(TPM_Z_M, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}

#Transition Probability Matrix Iteration 1
TPM<-melt(data_merge2, id.vars=c('ZONE','Z_NAME','ID_LC1','ID_LC2'), measure.vars=c('TPM'))
TPM$order1<-TPM$ID_LC1
TPM$order1<-as.numeric(levels(TPM$order1))[TPM$order1]
TPM$order2<-TPM$ID_LC2
TPM$order2<-as.numeric(levels(TPM$order2))[TPM$order2]
for(i in 1:nrow(zone)){
TPM_Z<-TPM[which(TPM$ZONE==i),]
TPM_Z_M<-dcast(TPM_Z, order1~order2, fun.aggregate=mean, value.var='value')
TPM_Z_M$order1<-NULL
TPM_Z_M<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),TPM_Z_M))
#TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0]<-format(TPM_Z_M[,2:23][TPM_Z_M[,2:23]==0], nsmall=1, digits=2)
colnames(TPM_Z_M)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-paste("\n#TRANSITION_PROBABILITY_MATRIX\tITERATION=1\tZONE=",unique(TPM_Z$Z_NAME[which(TPM_Z$ZONE==i)]), sep="")
write(text, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")
write.table(TPM_Z_M, paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t")
}
write("\n", paste(workingDirectory, "/",Scenario_name,".car",sep=""),append=TRUE, sep="\t")

Abacus_Project_File = paste(workingDirectory, "/",Scenario_name,".car",sep="") #work with car file and also supported text file with abacus project format
#Original_Project_File = paste(workingDirectory, "/","Original_data.car",sep="")
#file.copy(Abacus_Project_File,Original_Project_File)

if (file.exists("C:/Program Files (x86)/LUMENS/AbacusScenario")){
abacusExecutable = "C:/Progra~2/LUMENS/AbacusScenario/abacus "
} else{
abacusExecutable = "C:/Progra~1/LUMENS/AbacusScenario/abacus "
}
systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)


# THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE

Set_Working_Directory<-workingDirectory
#setwd(Set_Working_Directory)
#New_Abacus_Project_db<-as.data.frame(readLines(paste(Set_Working_Directory,  "/Paling_Baru.car",sep="")))
New_Abacus_Project_file<-readLines(paste(Set_Working_Directory,"/Historicalbaseline.car",sep=""))
zone_number<-as.character(New_Abacus_Project_file[5])
zone_number<-as.data.frame(strsplit(zone_number, "="))
zone_number<-as.numeric(as.character(zone_number[2,]))
iteration_number<-as.character(New_Abacus_Project_file[12])
iteration_number<-as.data.frame(strsplit(iteration_number, "="))
iteration_number<-as.numeric(as.character(iteration_number[2,]))
baris<-as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris<-baris+4

#Net Emission Per-Ha (ton CO2-eq/ha.year)
#NE.ha<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.ha, paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.ha<-read.table(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),sep="\t")
#NE.ha$V8<-NULL
#NE.ha[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.ha[,2:7]))))
#colnames(NE.ha)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""))
baris<-baris+zone_number+3

#Net Emission (ton CO2-eq/year)
#NE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE, paste(Set_Working_Directory,  "/NE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE<-read.table(paste(Set_Working_Directory,  "/NE.txt",sep=""),sep="\t")
#NE$V8<-NULL
#NE[,2:7]<-as.numeric(as.character(as.factor(unlist(NE[,2:7]))))
#colnames(NE)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.txt",sep=""))
baris<-baris+zone_number+3

#Emission Per-Ha Area (ton CO2-eq/ha.year)
#NE.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(NE.Ha.A, paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#NE.Ha.A<-read.table(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),sep="\t")
#NE.Ha.A$V8<-NULL
#NE.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.Ha.A[,2:7]))))
#colnames(NE.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE, paste(Set_Working_Directory,  "/TE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
TE<-read.table(paste(Set_Working_Directory,  "/TE.txt",sep=""),sep="\t")
eval(parse(text=(paste( "TE$V" ,iteration+3, "<-NULL", sep="" ))))
TE[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(TE[,2:(iteration+2)]))))
#colnames(TE)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/TE.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Per-Ha Area (ton CO2-eq/ha.year)
#Se.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
#write.table(Se.Ha.A, paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
#Se.Ha.A<-read.table(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),sep="\t")
#Se.Ha.A$V8<-NULL
#Se.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(Se.Ha.A[,2:7]))))
#colnames(Se.Ha.A)<-c("Zone","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory,  "/ST.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ")
ST<-read.table(paste(Set_Working_Directory,  "/ST.txt",sep=""),sep="\t")
eval(parse(text=(paste( "ST$V" ,iteration+3, "<-NULL", sep="" ))))
ST[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(ST[,2:(iteration+2)]))))
#colnames(ST)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/ST.txt",sep=""))
baris<-baris+zone_number+3

#Land Use System  Emission Per-Ha Area (ton CO2-eq/ha.year)
baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
#LUS.EM<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM, paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM<-read.table(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),sep="\t")
#LUS.EM$V10<-NULL
#LUS.EM[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM[,4:9]))))
#colnames(LUS.EM)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""))
baris<-baris2+2

#Land Use System  Emission Total  (ton CO2-eq/year)
baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
#LUS.EM.T<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
#write.table(LUS.EM.T, paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#LUS.EM.T<-read.table(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),sep="\t")
#LUS.EM.T$V10<-NULL
#LUS.EM.T[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM.T[,4:9]))))
#colnames(LUS.EM.T)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
#file.remove(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""))
baris<-baris2+4

#Cost Threshold ($/ton CO2-eq)  5.0
#CT<-as.data.frame(New_Abacus_Project_file[baris:(baris+1+iteration_number)])
#write.table(CT, paste(Set_Working_Directory,  "/CT.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
#CT<-read.table(paste(Set_Working_Directory,  "/CT.txt",sep=""),sep="\t")
#CT[,2]<-as.numeric(as.character(as.factor(unlist(CT[,2]))))
#colnames(CT)<-c("Iteration","Private")
#file.remove(paste(Set_Working_Directory,  "/CT.txt",sep=""))
baris<-baris+iteration_number+4

#Total
baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary<-as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory,  "/Summary.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t")
Summary<-read.table(paste(Set_Working_Directory,  "/Summary.txt",sep=""),sep="\t")
eval(parse(text=(paste( "Summary$V" ,iteration+3, "<-NULL", sep="" ))))
Summary[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(Summary[,2:(iteration+2)]))))
#colnames(Summary)<-c("Summary","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/Summary.txt",sep=""))

row.number<-nrow(SL_overall_data)
SL_overall_data[(row.number+1),]<-SL_overall_data[1,]
NewData<-Summary[1,2:(iteration+2)]
NewData<-cumsum(t(NewData))
SL_overall_data[3]<-NewData
NewID<-seq(1,(iteration+1))
SL_overall_data[2]<-NewID
NewYear<-paste(as.character(period1),as.character(period2),sep="-")
Period.db<-append(NewYear,Period.db)

plot1<-ggplot(SL_overall_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red")+
geom_point(colour="red", size=4, shape=21, fill="white") +
geom_text(data=SL_overall_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
scale_x_discrete(breaks=SL_overall_data$variable ,labels=Period.db) +
xlab('Year') +  ylab('Cum.CO2eq/ha.yr') +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

TableSum<-round((Summary[(1:12),(2:(iteration+2))]),digits=2)
TableSum<-cbind((Summary[1]),TableSum)
CName<-append("Parameters", Period.db)
colnames(TableSum) <- c(CName)
TableSum = TableSum[-2,]
TableSum = TableSum[-3,]
TableSum = TableSum[-4,]
TableSum = TableSum[-5,]
TableSum = TableSum[-6,]
TableSum = TableSum[-7,]

SL_overall_data$value<-round(SL_overall_data$value,digits=2)
Cum.em<-as.data.frame(cbind(Period.db,SL_overall_data$value))
colnames(Cum.em)[1]<-"Periode"
colnames(Cum.em)[2]<-"Cumulative emission rate (CO2eq/ha.yr)"

TE1<-round((TE[,(2:(iteration+2))]),digits=2)
ST1<-round((ST[,(2:(iteration+2))]),digits=2)
TE<-cbind((TE[1]),TE1)
ST<-cbind((ST[1]),ST1)

colnames(TE) <- c(CName)
colnames(ST) <- c(CName)
TE$Total<-rowSums(TE[,2:(iteration+2)])
ST$Total<-rowSums(ST[,2:(iteration+2)])
TE.total<-sum(TE$Total)
ST.total<-sum(ST$Total)
TE$Percentage<-round(((TE$Total/TE.total)*100),digits=2)
ST$Percentage<-round(((ST$Total/ST.total)*100),digits=2)

TE <- TE[order(-TE$Percentage),]
ST <- ST[order(-ST$Percentage),]


# WRITE REPORT
title<-"\\b\\fs32 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Proyeksi Baseline Emisi Historis \\b0\\fs20"
date<-paste("Date : ", as.character(Sys.Date()), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", period1)
I_O_period_2_rep<-paste("\\b","\\fs20", period2)
rtffile <- RTF("LUMENS_SCIENDO-PHB_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot1)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi emisi kumulatif dengan baseline historis\\b0 \\fs20 "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Emisi kumulatif (ton Co2eq/ha.yr) \\b0 \\fs20"))
addTable(rtffile,Cum.em)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Intisari perhitungan emisi historis \\b0 \\fs20"))
addTable(rtffile,TableSum, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi emisi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,TE, font.size=8)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Prediksi sequestrasi pada unit perencanaan (ton CO2eq/yr) \\b0 \\fs20"))
addTable(rtffile,ST, font.size=8)
addNewLine(rtffile)
done(rtffile)
