##[SCIENDO]=group
working_directory<-"R://Work/SCIENDO/SCIENDO"
periode1=2005
periode2=2010
location="Bungo"
carbon_database="QUES-C_database.dbf"
SCIENDO_database="SCIENDO-LUWES_database.dbf"
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
library(reshapeGUI)
library(tcltk2)
library(RGtk2Extras)
library(RGtk2)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# SET WORKING DIRECTORY
working_directory<-"R://Work/SCIENDO/SCIENDO"
setwd(working_directory)
# SELECT QUES-C DATABASE
t1=periode1
t2=periode2
period<-abs(t2-t1)
data_merge<-read.dbf(carbon_database)
data_merge2<-read.dbf(SCIENDO_database)

pu <- melt(data = data_merge, id.vars=c('ZONE','Z_NAME'), measure.vars=c('COUNT'))
pu <- dcast(data = pu, formula = Z_NAME + ZONE ~ variable, fun.aggregate = mean )
pu[,3]<-NULL
test<-as.character(pu$Z_NAME)
data_lookup<-melt(data=data_merge, id.vars=c('ID_LC1','LC_t1'))
data_lookup$variable<-data_lookup$value<-NULL
data_lookup<-unique(data_lookup)
colnames(data_lookup)<-c("ID","CLASS")

name.matrix<-data_lookup
name.matrix$LC_CODE<-toupper(abbreviate(name.matrix$CLASS, minlength=4, method="both"))
data_selec <- melt(data = data_merge, id.vars=c('LC_t1','LC_t2','Z_NAME'), measure.vars=c('COUNT'))

#Creating SCIENDO-Emission Baseline Database
Baseline.db.1<-data_merge2[,1:12]
Baseline.db.2<-data_merge2[c('em0','sq0','em1','sq1','em2','sq2','em3','sq3','em4','sq4','em5','sq5')]
Baseline.db<-as.data.frame(cbind(Baseline.db.1,Baseline.db.2))
rm(Baseline.db.1)
rm(Baseline.db.2)

#SCIENDO-LUWES Baseline SUMMARY
Parameters<-c("Total emission (CO2 eq)", "Cumulative emission (CO2 eq/(ha.yr))")
sum_em0<-sum(Baseline.db$em0)
sum_em1<-sum(Baseline.db$em1)
sum_em2<-sum(Baseline.db$em2) 
sum_em3<-sum(Baseline.db$em3) 
sum_em4<-sum(Baseline.db$em4)
sum_em5<-sum(Baseline.db$em5) 
cum_em0<-sum_em0
cum_em1<-sum_em0+sum_em1
cum_em2<-sum_em0+sum_em1+sum_em2
cum_em3<-sum_em0+sum_em1+sum_em2+sum_em3
cum_em4<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4
cum_em5<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4+sum_em5
Scenario<-rep("Baseline",2)
Base<-c(sum_em0,cum_em0)
Iteration1<-c(sum_em1,cum_em1)
Iteration2<-c(sum_em2,cum_em2)
Iteration3<-c(sum_em3,cum_em3)
Iteration4<-c(sum_em4,cum_em4)
Iteration5<-c(sum_em5,cum_em5)
SCIENDO.Baseline.Summary<-data.frame(Scenario, Parameters, Base,Iteration1,Iteration2,Iteration3,Iteration4,Iteration5)

i=1
scenario.list<-as.character(NULL)
SCIENDO.Scenario.Summary<-SCIENDO.Baseline.Summary
userinput<-as.data.frame(c("0","0","0","create new scenario"))
while(userinput[4,] !="finish building scenario" | userinput[4,] != 0 ){
  PU_ID <-(function(sc,x,z,o){c(sc,x,z,o)})
  PU_ID.dialog = list(
    title="Scenario builder",
    sc.stringItem="(type your scenario name)", label="Scenario Name",
    BREAK=TRUE,
    x.choiceItem=c(test),
    label="Select your planning unit",
    tooltip="Select your planning unit",
    z.radiobuttonItem=c(value="hectares", "fraction"), item.labels = c("hectares", "fraction"), label = "Select unit",
    o.choiceItem=c("create new scenario","finish building scenario", "edit existing scenario"),
    label="What do you want to do next?",
    tooltip="choose command"
  )
  PU_ID2 <-(function(cs){(cs)})
  PU_ID2.dialog = list(
    title="Scenario builder",
    cs.choiceItem=c(scenario.list),
    label="Choose Scenario to Edit",
    tooltip="choose command"
  )
  tt<-run.dialog(PU_ID)
  userinput<-as.data.frame(as.character(PU_ID_output))
  
  #eval(parse(text=paste("tt<-run.dialog(PU_ID",i,")",sep="")))
  #eval(parse(text=paste("userinput<-as.data.frame(as.character(PU_ID",i,"_output))",sep="")))
  if (userinput[4,] =="finish building scenario" | userinput[4,] == 0 ){
    break
  }
  else if (userinput[4,]=="edit existing scenario"){
    run.dialog(PU_ID2)
    Sc.input<-as.character(PU_ID2_output)
    sc.num<-as.numeric(match(Sc.input,scenario.list))
    eval(parse(text=paste("userinput<-",Sc.input,"_prop",sep="")))
    eval(parse(text=paste("Scenario",sc.num,"<-dfedit(Scenario",sc.num,")",sep="")))
    eval(parse(text=paste("matrix1<-Scenario",sc.num,sep="")))
    lutm.matrix1.t0<-matrix1[,2:23]
    for(a in 0:5){
      if(a < 5){
        eval(parse(text=paste("tpm.matrix1.t",a,"<-apply(lutm.matrix1.t",a,",2,function(x){x/rowSums(lutm.matrix1.t",a,")})",sep="")))
        #eval(parse(text=paste("tpm.matrix1.t",i,"<-round(tpm.matrix1.t",i,",digits=2)",sep="")))
        eval(parse(text=paste("tpm.matrix1.t",a,"[which(tpm.matrix1.t",a,"=='NaN')]<-0",sep="")))
        eval(parse(text=paste("lutm.matrix1.t",a+1,"<-apply(tpm.matrix1.t",a,",2,function(x){x*colSums(lutm.matrix1.t",a,")})",sep="")))
      }
      else{
        eval(parse(text=paste("tpm.matrix1.t",a,"<-apply(lutm.matrix1.t",a,",2,function(x){x/rowSums(lutm.matrix1.t",a,")})",sep="")))
        #eval(parse(text=paste("tpm.matrix1.t",i+1,"<-round(tpm.matrix1.t",i+1,",digits=2)",sep="")))
        eval(parse(text=paste("tpm.matrix1.t",a,"[which(tpm.matrix1.t",a,"=='NaN')]<-0",sep="")))
      }
    }
    
    zona<-as.character(userinput[2,])
    input<-pu$ZONE[which(pu$Z_NAME == zona)]
    input<-as.numeric(levels(input)[input])
    
    #Creating New Database
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-data_merge2[which(data_merge2$ZONE == input),]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-SCIENDO.Scenario.",sc.num,".db[,1:12]",sep="")))
    
    #LUTM 0
    lutm.matrix.0<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t0))
    lutm.matrix.0.melt<-melt(data=lutm.matrix.0)
    colnames(lutm.matrix.0.melt)[1]<-'CLASS'
    lutm.matrix.0.melt<-merge(lutm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.0.melt)<-c('LC_t1','CLASS','COUNT.0','ID_LC1')
    lutm.matrix.0.melt<-merge(lutm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.0.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.0.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 0
    tpm.matrix.0<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t0))
    tpm.matrix.0.melt<-melt(data=tpm.matrix.0)
    colnames(tpm.matrix.0.melt)[1]<-'CLASS'
    tpm.matrix.0.melt<-merge(tpm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.0.melt)<-c('LC_t1','CLASS','TPM.0','ID_LC1')
    tpm.matrix.0.melt<-merge(tpm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.0.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.0.melt)[5]<-'ID_LC2'
    
    #Creating Emission Database for Scenario
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.0.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.0.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em0<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.0*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq0<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.0*3.67",sep="")))
    
    #LUTM 1
    lutm.matrix.1<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t1))
    lutm.matrix.1.melt<-melt(data=lutm.matrix.1)
    colnames(lutm.matrix.1.melt)[1]<-'CLASS'
    lutm.matrix.1.melt<-merge(lutm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.1.melt)<-c('LC_t1','CLASS','COUNT.1','ID_LC1')
    lutm.matrix.1.melt<-merge(lutm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.1.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.1.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 1
    tpm.matrix.1<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t1))
    tpm.matrix.1.melt<-melt(data=tpm.matrix.1)
    colnames(tpm.matrix.1.melt)[1]<-'CLASS'
    tpm.matrix.1.melt<-merge(tpm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.1.melt)<-c('LC_t1','CLASS','TPM.1','ID_LC1')
    tpm.matrix.1.melt<-merge(tpm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.1.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.1.melt)[5]<-'ID_LC2'
    
    #Updating Database
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.1.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.1.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em1<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.1*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq1<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.1*3.67",sep="")))
    
    #LUTM 2
    lutm.matrix.2<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t2))
    lutm.matrix.2.melt<-melt(data=lutm.matrix.2)
    colnames(lutm.matrix.2.melt)[1]<-'CLASS'
    lutm.matrix.2.melt<-merge(lutm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.2.melt)<-c('LC_t1','CLASS','COUNT.2','ID_LC1')
    lutm.matrix.2.melt<-merge(lutm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.2.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.2.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 2
    tpm.matrix.2<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t2))
    tpm.matrix.2.melt<-melt(data=tpm.matrix.2)
    colnames(tpm.matrix.2.melt)[1]<-'CLASS'
    tpm.matrix.2.melt<-merge(tpm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.2.melt)<-c('LC_t1','CLASS','TPM.2','ID_LC1')
    tpm.matrix.2.melt<-merge(tpm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.2.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.2.melt)[5]<-'ID_LC2'
    
    #Updating Database 2
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.2.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.2.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em2<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.2*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq2<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.2*3.67",sep="")))
    
    #Transition Probability Matrix 3
    tpm.matrix.3<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t3))
    tpm.matrix.3.melt<-melt(data=tpm.matrix.3)
    colnames(tpm.matrix.3.melt)[1]<-'CLASS'
    tpm.matrix.3.melt<-merge(tpm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.3.melt)<-c('LC_t1','CLASS','TPM.3','ID_LC1')
    tpm.matrix.3.melt<-merge(tpm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.3.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.3.melt)[5]<-'ID_LC2'
    
    #LUTM 3
    lutm.matrix.3<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t3))
    lutm.matrix.3.melt<-melt(data=lutm.matrix.3)
    colnames(lutm.matrix.3.melt)[1]<-'CLASS'
    lutm.matrix.3.melt<-merge(lutm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.3.melt)<-c('LC_t1','CLASS','COUNT.3','ID_LC1')
    lutm.matrix.3.melt<-merge(lutm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.3.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.3.melt)[5]<-'ID_LC2'
    
    #Updating Database 3
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.3.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.3.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em3<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.3*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq3<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.3*3.67",sep="")))
    
    #Transition Probability Matrix 4
    tpm.matrix.4<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t4))
    tpm.matrix.4.melt<-melt(data=tpm.matrix.4)
    colnames(tpm.matrix.4.melt)[1]<-'CLASS'
    tpm.matrix.4.melt<-merge(tpm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.4.melt)<-c('LC_t1','CLASS','TPM.4','ID_LC1')
    tpm.matrix.4.melt<-merge(tpm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.4.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.4.melt)[5]<-'ID_LC2'
    
    #LUTM 4
    lutm.matrix.4<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t4))
    lutm.matrix.4.melt<-melt(data=lutm.matrix.4)
    colnames(lutm.matrix.4.melt)[1]<-'CLASS'
    lutm.matrix.4.melt<-merge(lutm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.4.melt)<-c('LC_t1','CLASS','COUNT.4','ID_LC1')
    lutm.matrix.4.melt<-merge(lutm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.4.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.4.melt)[5]<-'ID_LC2'
    
    #Updating Database 4
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.4.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.4.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em4<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.4*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq4<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.4*3.67",sep="")))
    
    #Transition Probability Matrix 5
    tpm.matrix.5<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t5))
    tpm.matrix.5.melt<-melt(data=tpm.matrix.5)
    colnames(tpm.matrix.5.melt)[1]<-'CLASS'
    tpm.matrix.5.melt<-merge(tpm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.5.melt)<-c('LC_t1','CLASS','TPM.5','ID_LC1')
    tpm.matrix.5.melt<-merge(tpm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.5.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.5.melt)[5]<-'ID_LC2'
    
    #LUTM 5
    lutm.matrix.5<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t5))
    lutm.matrix.5.melt<-melt(data=lutm.matrix.5)
    colnames(lutm.matrix.5.melt)[1]<-'CLASS'
    lutm.matrix.5.melt<-merge(lutm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.5.melt)<-c('LC_t1','CLASS','COUNT.5','ID_LC1')
    lutm.matrix.5.melt<-merge(lutm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.5.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.5.melt)[5]<-'ID_LC2'
    
    #Updating Database 5
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, tpm.matrix.5.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-merge(SCIENDO.Scenario.",sc.num,".db, lutm.matrix.5.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$em5<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_em*SCIENDO.Scenario.",sc.num,".db$COUNT.5*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db$sq5<-(SCIENDO.Scenario.",sc.num,".db$CARBON_t1-SCIENDO.Scenario.",sc.num,".db$CARBON_t2)*SCIENDO.Scenario.",sc.num,".db$ck_sq*SCIENDO.Scenario.",sc.num,".db$COUNT.5*3.67",sep="")))
    
    #Finalizing New Database
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db.1<-SCIENDO.Scenario.",sc.num,".db[,1:12]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db.2<-SCIENDO.Scenario.",sc.num,".db[c('em0','sq0','em1','sq1','em2','sq2','em3','sq3','em4','sq4','em5','sq5')]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".db<-cbind(SCIENDO.Scenario.",sc.num,".db.1,SCIENDO.Scenario.",sc.num,".db.2)",sep="")))
    eval(parse(text=paste("rm(SCIENDO.Scenario.",sc.num,".db.1)",sep="")))
    eval(parse(text=paste("rm(SCIENDO.Scenario.",sc.num,".db.2)",sep="")))
    
    #Scenario-Emission Database
    eval(parse(text=paste("Scenario.",sc.num,".em.db.1<-data_merge2[,1:12]",sep="")))
    eval(parse(text=paste("Scenario.",sc.num,".em.db.2<-data_merge2[c('em0','sq0','em1','sq1','em2','sq2','em3','sq3','em4','sq4','em5','sq5')]",sep="")))
    eval(parse(text=paste("Scenario.",sc.num,".em.db<-as.data.frame(cbind(Scenario.",sc.num,".em.db.1,Scenario.",sc.num,".em.db.2))",sep="")))
    eval(parse(text=paste("rm(Scenario.",sc.num,".em.db.1)",sep="")))
    eval(parse(text=paste("rm(Scenario.",sc.num,".em.db.2)",sep="")))
    
    #Merging database
    eval(parse(text=paste("Scenario.",sc.num,".em.db<-Scenario.",sc.num,".em.db[which(Scenario.",sc.num,".em.db$ZONE!=input),]",sep="")))
    eval(parse(text=paste("Scenario.",sc.num,".em.db<-rbind(Scenario.",sc.num,".em.db, SCIENDO.Scenario.",sc.num,".db)",sep="")))
    
    #SCIENDO-LUWES SUMMARY
    Parameters<-c("Total emission (CO2 eq)", "Cumulative emission (CO2 eq/(ha.yr))")
    sum_em0<-sum(Baseline.db$em0)
    eval(parse(text=paste(" sum_em1<-sum(Scenario.",sc.num,".em.db$em1)",sep="")))
    eval(parse(text=paste(" sum_em2<-sum(Scenario.",sc.num,".em.db$em2)",sep="")))
    eval(parse(text=paste(" sum_em3<-sum(Scenario.",sc.num,".em.db$em3)",sep="")))
    eval(parse(text=paste(" sum_em4<-sum(Scenario.",sc.num,".em.db$em4)",sep="")))
    eval(parse(text=paste(" sum_em5<-sum(Scenario.",sc.num,".em.db$em5)",sep="")))
    cum_em0<-sum_em0
    cum_em1<-sum_em0+sum_em1
    cum_em2<-sum_em0+sum_em1+sum_em2
    cum_em3<-sum_em0+sum_em1+sum_em2+sum_em3
    cum_em4<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4
    cum_em5<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4+sum_em5
    Scenario<-rep(Sc.input,2)
    Base<-c(sum_em0,cum_em0)
    Iteration1<-c(sum_em1,cum_em1)
    Iteration2<-c(sum_em2,cum_em2)
    Iteration3<-c(sum_em3,cum_em3)
    Iteration4<-c(sum_em4,cum_em4)
    Iteration5<-c(sum_em5,cum_em5)
    eval(parse(text=paste("SCIENDO.Scenario.",sc.num,".Summary<-data.frame(Scenario, Parameters, Base,Iteration1,Iteration2,Iteration3,Iteration4,Iteration5)",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.Summary[which(SCIENDO.Scenario.Summary$Scenario == Sc.input),]<-SCIENDO.Scenario.",sc.num,".Summary",sep="")))
    
    #Exporting Scenario Database
    work_dir<-paste(working_directory,"/",Sc.input, sep="")
    setwd(work_dir)
    eval(parse(text=paste("write.dbf(Scenario",sc.num,",'SCIENDO_Scenario",sc.num,"_LUTM.dbf')",sep="")))
    eval(parse(text=paste("write.dbf(SCIENDO.Scenario.",sc.num,".db,'Scenario",sc.num,"_PU_Emission.dbf')",sep="")))
    eval(parse(text=paste("write.dbf(Scenario.",sc.num,".em.db,'Scenario",sc.num,"_LU_Emission.dbf')",sep="")))
    #eval(parse(text=paste("write.dbf(Scenario",i,"_prop,'Scenario",i,"_properties.dbf')",sep="")))
    
    i=i-1
  }
  else{
    selection<-as.character(userinput[2,])
    selection2<-as.character(gsub(" ","",userinput[1,]))
    scenario.list<-as.character(c(scenario.list,selection2))
    matrix1 <- dcast(data = data_selec, formula = LC_t1 ~ LC_t2, fun.aggregate = sum, subset = .(Z_NAME==selection))
    eval(parse(text=paste("Scenario",i,"<-dfedit(matrix1)",sep="")))
    eval(parse(text=paste("matrix1<-Scenario",i,sep="")))
    lutm.matrix1.t0<-matrix1[,2:23]
    for(a in 0:5){
      if(a < 5){
        eval(parse(text=paste("tpm.matrix1.t",a,"<-apply(lutm.matrix1.t",a,",2,function(x){x/rowSums(lutm.matrix1.t",a,")})",sep="")))
        #eval(parse(text=paste("tpm.matrix1.t",i,"<-round(tpm.matrix1.t",i,",digits=2)",sep="")))
        eval(parse(text=paste("tpm.matrix1.t",a,"[which(tpm.matrix1.t",a,"=='NaN')]<-0",sep="")))
        eval(parse(text=paste("lutm.matrix1.t",a+1,"<-apply(tpm.matrix1.t",a,",2,function(x){x*colSums(lutm.matrix1.t",a,")})",sep="")))
      }
      else{
        eval(parse(text=paste("tpm.matrix1.t",a,"<-apply(lutm.matrix1.t",a,",2,function(x){x/rowSums(lutm.matrix1.t",a,")})",sep="")))
        #eval(parse(text=paste("tpm.matrix1.t",i+1,"<-round(tpm.matrix1.t",i+1,",digits=2)",sep="")))
        eval(parse(text=paste("tpm.matrix1.t",a,"[which(tpm.matrix1.t",a,"=='NaN')]<-0",sep="")))
      }
    }
    
    zona<-as.character(userinput[2,])
    input<-pu$ZONE[which(pu$Z_NAME == zona)]
    input<-as.numeric(levels(input)[input])
    
    #Creating New Database
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-data_merge2[which(data_merge2$ZONE == input),]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-SCIENDO.Scenario.",i,".db[,1:12]",sep="")))
    
    #LUTM 0
    lutm.matrix.0<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t0))
    lutm.matrix.0.melt<-melt(data=lutm.matrix.0)
    colnames(lutm.matrix.0.melt)[1]<-'CLASS'
    lutm.matrix.0.melt<-merge(lutm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.0.melt)<-c('LC_t1','CLASS','COUNT.0','ID_LC1')
    lutm.matrix.0.melt<-merge(lutm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.0.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.0.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 0
    tpm.matrix.0<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t0))
    tpm.matrix.0.melt<-melt(data=tpm.matrix.0)
    colnames(tpm.matrix.0.melt)[1]<-'CLASS'
    tpm.matrix.0.melt<-merge(tpm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.0.melt)<-c('LC_t1','CLASS','TPM.0','ID_LC1')
    tpm.matrix.0.melt<-merge(tpm.matrix.0.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.0.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.0.melt)[5]<-'ID_LC2'
    
    #Creating Emission Database for Scenario
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.0.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.0.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em0<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.0*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq0<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.0*3.67",sep="")))
    
    #LUTM 1
    lutm.matrix.1<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t1))
    lutm.matrix.1.melt<-melt(data=lutm.matrix.1)
    colnames(lutm.matrix.1.melt)[1]<-'CLASS'
    lutm.matrix.1.melt<-merge(lutm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.1.melt)<-c('LC_t1','CLASS','COUNT.1','ID_LC1')
    lutm.matrix.1.melt<-merge(lutm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.1.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.1.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 1
    tpm.matrix.1<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t1))
    tpm.matrix.1.melt<-melt(data=tpm.matrix.1)
    colnames(tpm.matrix.1.melt)[1]<-'CLASS'
    tpm.matrix.1.melt<-merge(tpm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.1.melt)<-c('LC_t1','CLASS','TPM.1','ID_LC1')
    tpm.matrix.1.melt<-merge(tpm.matrix.1.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.1.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.1.melt)[5]<-'ID_LC2'
    
    #Updating Database
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.1.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.1.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em1<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.1*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq1<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.1*3.67",sep="")))
    
    #LUTM 2
    lutm.matrix.2<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t2))
    lutm.matrix.2.melt<-melt(data=lutm.matrix.2)
    colnames(lutm.matrix.2.melt)[1]<-'CLASS'
    lutm.matrix.2.melt<-merge(lutm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.2.melt)<-c('LC_t1','CLASS','COUNT.2','ID_LC1')
    lutm.matrix.2.melt<-merge(lutm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.2.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.2.melt)[5]<-'ID_LC2'
    
    #Transition Probability Matrix 2
    tpm.matrix.2<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t2))
    tpm.matrix.2.melt<-melt(data=tpm.matrix.2)
    colnames(tpm.matrix.2.melt)[1]<-'CLASS'
    tpm.matrix.2.melt<-merge(tpm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.2.melt)<-c('LC_t1','CLASS','TPM.2','ID_LC1')
    tpm.matrix.2.melt<-merge(tpm.matrix.2.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.2.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.2.melt)[5]<-'ID_LC2'
    
    #Updating Database 2
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.2.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.2.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em2<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.2*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq2<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.2*3.67",sep="")))
    
    #Transition Probability Matrix 3
    tpm.matrix.3<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t3))
    tpm.matrix.3.melt<-melt(data=tpm.matrix.3)
    colnames(tpm.matrix.3.melt)[1]<-'CLASS'
    tpm.matrix.3.melt<-merge(tpm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.3.melt)<-c('LC_t1','CLASS','TPM.3','ID_LC1')
    tpm.matrix.3.melt<-merge(tpm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.3.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.3.melt)[5]<-'ID_LC2'
    
    #LUTM 3
    lutm.matrix.3<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t3))
    lutm.matrix.3.melt<-melt(data=lutm.matrix.3)
    colnames(lutm.matrix.3.melt)[1]<-'CLASS'
    lutm.matrix.3.melt<-merge(lutm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.3.melt)<-c('LC_t1','CLASS','COUNT.3','ID_LC1')
    lutm.matrix.3.melt<-merge(lutm.matrix.3.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.3.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.3.melt)[5]<-'ID_LC2'
    
    #Updating Database 3
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.3.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.3.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em3<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.3*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq3<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.3*3.67",sep="")))
    
    #Transition Probability Matrix 4
    tpm.matrix.4<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t4))
    tpm.matrix.4.melt<-melt(data=tpm.matrix.4)
    colnames(tpm.matrix.4.melt)[1]<-'CLASS'
    tpm.matrix.4.melt<-merge(tpm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.4.melt)<-c('LC_t1','CLASS','TPM.4','ID_LC1')
    tpm.matrix.4.melt<-merge(tpm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.4.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.4.melt)[5]<-'ID_LC2'
    
    #LUTM 4
    lutm.matrix.4<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t4))
    lutm.matrix.4.melt<-melt(data=lutm.matrix.4)
    colnames(lutm.matrix.4.melt)[1]<-'CLASS'
    lutm.matrix.4.melt<-merge(lutm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.4.melt)<-c('LC_t1','CLASS','COUNT.4','ID_LC1')
    lutm.matrix.4.melt<-merge(lutm.matrix.4.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.4.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.4.melt)[5]<-'ID_LC2'
    
    #Updating Database 4
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.4.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.4.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em4<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.4*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq4<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.4*3.67",sep="")))
    
    #Transition Probability Matrix 5
    tpm.matrix.5<-cbind(as.data.frame(matrix1[,1]),as.data.frame(tpm.matrix1.t5))
    tpm.matrix.5.melt<-melt(data=tpm.matrix.5)
    colnames(tpm.matrix.5.melt)[1]<-'CLASS'
    tpm.matrix.5.melt<-merge(tpm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.5.melt)<-c('LC_t1','CLASS','TPM.5','ID_LC1')
    tpm.matrix.5.melt<-merge(tpm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(tpm.matrix.5.melt)[1]<-'LC_t2'
    colnames(tpm.matrix.5.melt)[5]<-'ID_LC2'
    
    #LUTM 5
    lutm.matrix.5<-cbind(as.data.frame(matrix1[,1]),as.data.frame(lutm.matrix1.t5))
    lutm.matrix.5.melt<-melt(data=lutm.matrix.5)
    colnames(lutm.matrix.5.melt)[1]<-'CLASS'
    lutm.matrix.5.melt<-merge(lutm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.5.melt)<-c('LC_t1','CLASS','COUNT.5','ID_LC1')
    lutm.matrix.5.melt<-merge(lutm.matrix.5.melt,data_lookup,by='CLASS')
    colnames(lutm.matrix.5.melt)[1]<-'LC_t2'
    colnames(lutm.matrix.5.melt)[5]<-'ID_LC2'
    
    #Updating Database 5
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, tpm.matrix.5.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-merge(SCIENDO.Scenario.",i,".db, lutm.matrix.5.melt, by=c('ID_LC1','LC_t1','ID_LC2','LC_t2'))",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$em5<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_em*SCIENDO.Scenario.",i,".db$COUNT.5*3.67",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db$sq5<-(SCIENDO.Scenario.",i,".db$CARBON_t1-SCIENDO.Scenario.",i,".db$CARBON_t2)*SCIENDO.Scenario.",i,".db$ck_sq*SCIENDO.Scenario.",i,".db$COUNT.5*3.67",sep="")))
    
    #Finalizing New Database
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db.1<-SCIENDO.Scenario.",i,".db[,1:12]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db.2<-SCIENDO.Scenario.",i,".db[c('em0','sq0','em1','sq1','em2','sq2','em3','sq3','em4','sq4','em5','sq5')]",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.",i,".db<-cbind(SCIENDO.Scenario.",i,".db.1,SCIENDO.Scenario.",i,".db.2)",sep="")))
    eval(parse(text=paste("rm(SCIENDO.Scenario.",i,".db.1)",sep="")))
    eval(parse(text=paste("rm(SCIENDO.Scenario.",i,".db.2)",sep="")))
    
    #Scenario-Emission Database
    eval(parse(text=paste("Scenario.",i,".em.db.1<-data_merge2[,1:12]",sep="")))
    eval(parse(text=paste("Scenario.",i,".em.db.2<-data_merge2[c('em0','sq0','em1','sq1','em2','sq2','em3','sq3','em4','sq4','em5','sq5')]",sep="")))
    eval(parse(text=paste("Scenario.",i,".em.db<-as.data.frame(cbind(Scenario.",i,".em.db.1,Scenario.",i,".em.db.2))",sep="")))
    eval(parse(text=paste("rm(Scenario.",i,".em.db.1)",sep="")))
    eval(parse(text=paste("rm(Scenario.",i,".em.db.2)",sep="")))
    
    #Merging database
    eval(parse(text=paste("Scenario.",i,".em.db<-Scenario.",i,".em.db[which(Scenario.",i,".em.db$ZONE!=input),]",sep="")))
    eval(parse(text=paste("Scenario.",i,".em.db<-rbind(Scenario.",i,".em.db, SCIENDO.Scenario.",i,".db)",sep="")))
    
    #SCIENDO-LUWES SUMMARY
    Parameters<-c("Total emission (CO2 eq)", "Cumulative emission (CO2 eq/(ha.yr))")
    sum_em0<-sum(Baseline.db$em0)
    eval(parse(text=paste(" sum_em1<-sum(Scenario.",i,".em.db$em1)",sep="")))
    eval(parse(text=paste(" sum_em2<-sum(Scenario.",i,".em.db$em2)",sep="")))
    eval(parse(text=paste(" sum_em3<-sum(Scenario.",i,".em.db$em3)",sep="")))
    eval(parse(text=paste(" sum_em4<-sum(Scenario.",i,".em.db$em4)",sep="")))
    eval(parse(text=paste(" sum_em5<-sum(Scenario.",i,".em.db$em5)",sep="")))
    cum_em0<-sum_em0
    cum_em1<-sum_em0+sum_em1
    cum_em2<-sum_em0+sum_em1+sum_em2
    cum_em3<-sum_em0+sum_em1+sum_em2+sum_em3
    cum_em4<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4
    cum_em5<-sum_em0+sum_em1+sum_em2+sum_em3+sum_em4+sum_em5
    Scenario<-rep(selection2,2)
    Base<-c(sum_em0,cum_em0)
    Iteration1<-c(sum_em1,cum_em1)
    Iteration2<-c(sum_em2,cum_em2)
    Iteration3<-c(sum_em3,cum_em3)
    Iteration4<-c(sum_em4,cum_em4)
    Iteration5<-c(sum_em5,cum_em5)
    eval(parse(text=paste("SCIENDO.Scenario.",i,".Summary<-data.frame(Scenario, Parameters, Base,Iteration1,Iteration2,Iteration3,Iteration4,Iteration5)",sep="")))
    eval(parse(text=paste("SCIENDO.Scenario.Summary<-rbind(SCIENDO.Scenario.Summary,SCIENDO.Scenario.",i,".Summary )",sep="")))
    eval(parse(text=(paste(selection2,"_prop<-userinput", sep=""))))
    #eval(parse(text=(paste("save(",selection2,',file="SCIENDO_scenario.RData")',sep=""))))
    
    #Exporting Scenario Database
    work_dir<-paste(working_directory,"/",selection2, sep="")
    dir.create(selection2)
    setwd(work_dir)
    eval(parse(text=paste("write.dbf(Scenario",i,",'SCIENDO_Scenario",i,"_LUTM.dbf')",sep="")))
    eval(parse(text=paste("write.dbf(SCIENDO.Scenario.",i,".db,'Scenario",i,"_PU_Emission.dbf')",sep="")))
    eval(parse(text=paste("write.dbf(Scenario.",i,".em.db,'Scenario",i,"_LU_Emission.dbf')",sep="")))
    #eval(parse(text=paste("write.dbf(Scenario",i,"_prop,'Scenario",i,"_properties.dbf')",sep="")))
    
  }
  setwd(working_directory)
  i=i+1
}

setwd(working_directory)
#Exporting Database
write.dbf(Baseline.db,"Baseline_LU_Emission.dbf")
write.dbf(SCIENDO.Scenario.Summary,"SCIENDO_Scenario_Summary.dbf")

t_1<-t1
t_2<-t_1+period
Periode<-as.data.frame(NULL)
for ( a in 1:(ncol(SCIENDO.Scenario.Summary[,3:8]))+1){
  period.int<-paste(t_1,"-",t_2, sep="")
  Periode<-c(Periode,period.int)
  t_1<-t_1+period
  t_2<-t_1+period
}
Periode<-as.character(Periode)

#Creating Plot
myColors1 <- brewer.pal(9,"Set1")
myColors2 <- brewer.pal(8,"Accent")
myColors3 <- brewer.pal(12,"Paired")
myColors4 <- brewer.pal(9, "Pastel1")
myColors5 <- brewer.pal(8, "Set2")
myColors6 <- brewer.pal(8, "Dark2")
myColors7 <- brewer.pal(11, "Spectral")
myColors  <-c(myColors1, myColors7, myColors2, myColors3, myColors4, myColors5, myColors6)
myColors.plot <- myColors[1:length(unique(SCIENDO.Scenario.Summary$Scenario))]
names(myColors.plot) <- unique(SCIENDO.Scenario.Summary$Scenario)

#Total Emission Plot
sum.em<-SCIENDO.Scenario.Summary[which(SCIENDO.Scenario.Summary$Parameters=='Total emission (CO2 eq)'),]
sum.em.melt<-melt(sum.em)

sum.em.plot<-ggplot(sum.em.melt,aes(x=variable,y=value,group=Scenario,fill=Scenario))+ 
  geom_line(data=sum.em.melt, aes(x=variable, y=value, group=Scenario, fill=Scenario, colour=Scenario)) + 
  geom_point(data=sum.em.melt, aes(x=variable, y=value, colour=Scenario), size=3) +
  scale_x_discrete(breaks=unique(sum.em.melt$variable) ,labels=Periode) +
  xlab('Year') +  ylab('CO2eq/ha.yr') + scale_colour_manual(values = myColors.plot) +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

#Barchart Total Emission Plot
sum.em.plot.bar<-ggplot(sum.em.melt,aes(variable,value,fill=Scenario))+geom_bar(stat="identity",position="dodge")+
  xlab('Year') +  ylab('CO2eq/ha.yr') + scale_x_discrete(breaks=unique(sum.em.melt$variable) ,labels=Periode) +
  scale_fill_manual(values = myColors.plot) +
  theme(axis.text.x= element_text(angle=0,hjust=1),
        legend.title = element_text(size=8),legend.text = element_text(size = 8))

#Cumulative Emission Plot
cum.em<-SCIENDO.Scenario.Summary[which(SCIENDO.Scenario.Summary$Parameters=='Cumulative emission (CO2 eq/(ha.yr))'),]
cum.em.melt<-melt(cum.em)
cum.em.plot<-ggplot(cum.em.melt,aes(x=variable,y=value,group=Scenario,fill=Scenario))+ 
  geom_line(data=cum.em.melt, aes(x=variable, y=value, group=Scenario, fill=Scenario, colour=Scenario)) + 
  geom_point(data=cum.em.melt, aes(x=variable, y=value, colour=Scenario), size=3) +
  scale_x_discrete(breaks=unique(cum.em.melt$variable) ,labels=Periode) +
  xlab('Year') +  ylab('Cum.CO2eq/ha.yr') + scale_colour_manual(values = myColors.plot) +
  theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

#Barchart Cumulative Emission Plot
cum.em.plot.bar<-ggplot(cum.em.melt,aes(variable,value,fill=Scenario))+geom_bar(stat="identity",position="dodge")+
  xlab('Year') +  ylab('Cum.CO2eq/ha.yr') + 
  scale_x_discrete(breaks=unique(cum.em.melt$variable) ,labels=Periode) +
  scale_fill_manual(values = myColors.plot) +
  theme(axis.text.x= element_text(angle=0,hjust=1), 
        legend.title = element_text(size=8),legend.text = element_text(size = 8))

colnames(sum.em)[3:8]<-Periode
sum.em[,3:8]<-round(sum.em[,3:8])
sum.em[,2]<-NULL
colnames(cum.em)[3:8]<-Periode
cum.em[,3:8]<-round(cum.em[,3:8])
cum.em[,2]<-NULL

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Projection on Scenario \\b0\\fs20"
date<-paste("Date : ",date(), sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", t1)
I_O_period_2_rep<-paste("\\b","\\fs20", t2)
rtffile <- RTF("LUMENS_SCIENDO-SCN_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 1. Summary of SCIENDO-LUWES Total Emission Result \\b0 \\fs20"))
addTable(rtffile,sum.em)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, sum.em.plot)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, sum.em.plot.bar)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 1.Predicted Total emission (CO2 eq)\\b0 \\fs20 "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 2. Summary of SCIENDO-LUWES Cumulative Emission Result\\b0 \\fs20"))
addTable(rtffile,cum.em)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, cum.em.plot)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=150, cum.em.plot.bar)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 2.Predicted Cumulative Emission (CO2 eq/(ha.yr)) \\b0 \\fs20 "))
addNewLine(rtffile)
done(rtffile)

#conduct analysis on the dataset
#SCIENDO_LUWES_Summary[,2:7]<-round(SCIENDO_LUWES_Summary[,2:7],digits=2)
#SL_overall<-SCIENDO_LUWES_Summary
#SL_analysis<-sub.data
#SL_overall.melt <- melt(data = SL_overall)
#SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (CO2 eq/(ha.yr))"))
#SL_overall_data<- melt(data = SL_overall.melt.cast)
#SL_overall_data<-SL_overall_data[-c(1),]


#SL_analysis.melt <- melt(data = SL_analysis, id.vars=c('Z_NAME'), measure.vars=c('em0','em1','em2','em3','em4','em5'))
#em_by_zone<-dcast(data = SL_analysis.melt, formula = Z_NAME ~ variable, fun.aggregate = sum)
#em_by_zone$Z_CODE<-toupper(abbreviate(em_by_zone$Z_NAME))
#em_by_zone<-em_by_zone[c(1,8,2,3,4,5,6,7)]
#em_by_zone[,3:8]<-round(em_by_zone[,3:8],digits=2)

#Cumulative emission
#cum_em<-em_by_zone
#n.col<-ncol(cum_em)
#for (i in 1:(n.col-3)){
#cum_em[,i+3]<-rowSums(em_by_zone[,(3):(i+3)])
#}
#cum_em.melt<-melt(data = cum_em, id.vars=c('Z_NAME','Z_CODE'), measure.vars=c('em0','em1','em2','em3','em4','em5'))

#plot1<-ggplot(SL_overall_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red") + 
#geom_point(colour="red", size=4, shape=21, fill="white") +
#geom_text(data=SL_overall_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
#scale_x_discrete(breaks=SL_overall_data$variable ,labels=Period.db) +
#xlab('Year') +  ylab('Cum.CO2eq/ha.yr') + 
#theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))
#plot2<-ggplot(cum_em.melt,aes(Z_NAME,value,fill=variable))+geom_bar(stat="identity",position="dodge")+
#xlab('Zone Code') +  ylab('Cum.CO2eq/ha.yr') + scale_x_discrete(breaks=SL_analysis.melt$Z_NAME ,labels=em_by_zone$Z_CODE) +
#theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Cumulative \nEmission (CO2eq)", labels=Periode)
#plot3<-ggplot(cum_em.melt,aes(variable,value,fill=Z_CODE))+ geom_line(data=cum_em.melt, aes(x=variable, y=value, group=Z_CODE, fill=Z_CODE, colour=Z_CODE), stat="identity") + 
#geom_point(data=cum_em.melt,aes(colour=Z_CODE), size=3) +
#scale_x_discrete(breaks=unique(cum_em.melt$variable) ,labels=Periode) +
#xlab('Year') +  ylab('Cum.CO2eq/ha.yr') + 
#theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))
