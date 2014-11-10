##[SCIENDO]=group
Set_Working_Directory<-"R://Work/SCIENDO/SCIENDO"
period1=2005
period2=2010
location="Bungo"
carbon_database="QUES-C_database.dbf"
SCIENDO_database="SCIENDO-LUWES_database.dbf"
##Abacus_Project_File = file
##SCIENDO_LUWES=output table
##SCIENDO_LUWES_summary=output table
##lutm_z=output table
##lutm_o=output table
##tpm_matrix=output table
##report

library(foreign)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# SET WORKING DIRECTORY
setwd(Set_Working_Directory)

# SELECT QUES-C DATABASE
t1=period1
t2=period2
period<-abs(t2-t1)
data_merge<-read.dbf(carbon_database)
data_merge2<-read.dbf(SCIENDO_database)

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

#CREATING ABACUS PROJECT FILE
Gnrl.info.1<-c("file_version", "title","description", "numberofzones","total_area","time", "include_bg","include_modif", "using_bg_factor","using_modif_factor", "model_iteration")
Gnrl.info.2<-c("1.1.0", "SCIENDO", "Project description",length(unique(pu$ZONE)),sum(data_merge$COUNT), iteration, "false", "false", "true", "true", 2)
Gnrl.info<-paste(Gnrl.info.1,Gnrl.info.2,sep="=")

#General Information
fileConn<-file("SCIENDO.car")
writeLines("#GENERAL", fileConn)
close(fileConn)
write.table(Gnrl.info, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t") 
text<-"\n#ZONE"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t") 

#Zone information
zone<-pu[c('Z_NAME','percentage')]
log.val<-rep('true',length(pu$ZONE))
zone<-cbind(zone, log.val)
write.table(zone, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t") 

#Landuse Information
text<-"\n#LANDCOVER"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t") 
write.table(name.matrix$CLASS, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=FALSE,row.names=FALSE, sep="\t") 

#Eligibility
egb<-matrix('true',ncol=nrow(name.matrix), nrow=nrow(name.matrix))
egb<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),egb))
colnames(egb)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#ELIGIBILITY"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
write.table(egb, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 

#Cost Benefit Unit
text<-"\n#COSTBENEFIT_UNIT"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
text<-"Private\tNet return received by the land-use operator, farmers"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")

#Carbon Stock
text<-"\n#CARBONSTOCK"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
carbon<-matrix(ncol=nrow(name.matrix),nrow=nrow(name.matrix),0)
for(i in 1:nrow(name.matrix)){
  for(j in 1:nrow(name.matrix)){
      carbon[i,j]<-round(unique(data_merge$CARBON_t1[which(data_merge$ID_LC1==i & data_merge$ID_LC2==j)]),digits=2)

  }
}
carbon<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),carbon))
#carbon[,2:23][carbon[,2:23]==0]<-format(carbon[,2:23][carbon[,2:23]==0], nsmall=1, digits=2)
colnames(carbon)<-(c('//LandCover', as.character(name.matrix$CLASS)))
write.table(carbon, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 

#NPV Private
NPV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
NPV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),NPV))
#NPV[,2:23]<-format(NPV[,2:23],nsmall=1,digits=2)
colnames(NPV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
text<-"\n#NPV_Private"
write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
write.table(NPV, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 

#COST Benefit CONVERSION Private
for (i in 1:nrow(zone)){
  CBCV<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  CBCV<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),CBCV))
  #CBCV[,2:23]<-format(CBCV[,2:23], nsmall=1,digits=2)
  colnames(CBCV)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#COSTBENEFIT_CONVERSION_Private\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(NPV, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#LANDCOVER CHANGE
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
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
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(LC_chg_Z_M, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#BelowGround Emission
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  BGE<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  BGE<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGE))
  #BGE[,2:23]<-format(BGE[,2:23],nsmall=1,digits=2)
  colnames(BGE)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#BELOWGROUND_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(BGE, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#BelowGround Emission Factor
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  BGEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  BGEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),BGEF))
  #BGEF[,2:23]<-format(BGEF[,2:23],nsmall=1,digits=2)
  colnames(BGEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#BELOWGROUND_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(BGEF, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#Modif Emission 
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  ME<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  ME<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),ME))
  #ME[,2:23]<-format(ME[,2:23],nsmall=1,digits=2)
  colnames(ME)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#MODIF_EMISSION\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(ME, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#Modif Emission Factor
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
for (i in 1:nrow(zone)){
  MEF<-matrix(0,ncol=nrow(name.matrix), nrow=nrow(name.matrix))
  MEF<-as.data.frame(cbind(as.data.frame(name.matrix$CLASS),MEF))
  #MEF[,2:23]<-format(MEF[,2:23],nsmall=1,digits=2)
  colnames(MEF)<-(c('//LandCover', as.character(name.matrix$CLASS)))
  text<-paste("\n#MODIF_E_FACTOR\tZONE=",zone$Z_NAME[i], sep="")
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(MEF, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}

#Transition Probability Matrix Iteration 0
write("", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
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
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(TPM_Z_M, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
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
  write(text, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")
  write.table(TPM_Z_M, paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE,quote=FALSE, col.names=TRUE,row.names=FALSE, sep="\t") 
}
write("\n", paste(Set_Working_Directory, "/SCIENDO.car",sep=""),append=TRUE, sep="\t")

Abacus_Project_File = "R://Work/SCIENDO/SCIENDO/SCIENDO.car" #work with car file and also supported text file with abacus project format

abacusExecutable = "C:/Progra~2/LUMENS/Abacus/abacus " #the default directory for abacus executable 
systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)

#you can modify anything from this line, for example, 
#I already added some line to read car file after editing
#and saving Abacus project file to your own working directory 
test <- paste("test.car", sep="")
abacusCar = readLines(test) 
