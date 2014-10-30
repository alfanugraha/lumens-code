##[SCIENDO]=group
##workingDirectory=folder
##carbonData=file
##period1=number 2005
##period2=number 2010
##iteration=number 5 
##SCIENDO_LUWES=output table
##SCIENDO_LUWES_summary=output table
##lutm_z=output table
##LUTMOverall=output table
##TPMatrix=output table
##report

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


#====2_set_working_directory====
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
SL_overall<-SCIENDO_LUWES_summary
SL_analysis<-SCIENDO_LUWES
SL_overall.melt <- melt(data = SL_overall)
SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (CO2 eq/(ha.yr))"))
SL_overall_data<- melt(data = SL_overall.melt.cast)
SL_overall_data<-SL_overall_data[-c(1),]
plot1<-ggplot(SL_overall_data,aes(variable,value,fill=Parameters))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Emission (CO2eq)")+theme(legend.position="bottom")
SL_analysis.melt <- melt(data = SL_analysis, id.vars=c('Z_NAME'), measure.vars=c('EM0','EM1','EM2','EM3','EM4','EM5'))
plot2<-ggplot(SL_analysis.melt,aes(variable,value,fill=Z_NAME))+geom_bar(stat="identity",position="dodge")+theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Emission (CO2eq)")
em_by_zone <- dcast(data = SL_analysis.melt, formula = Z_NAME ~ variable, fun.aggregate = sum)

#write output to file
write.dbf(SCIENDO_LUWES,"SCIENDO-LUWES_database.dbf")
write.dbf(SCIENDO_LUWES_summary,"SCIENDO-LUWES_summary.dbf")
write.dbf(LUTMZones,"SCIENDO-LUWES_zones_tpm.dbf")
write.dbf(LUTMZones,"SCIENDO-LUWES_zones_tpm_model.dbf")
write.dbf(LUTMOverall,"SCIENDO-LUWES_overall_lutm.dbf")
write.dbf(TPMatrix,"SCIENDO-LUWES_overall_tpm_matrix.dbf")
write.dbf(LUTMZones,"lutm_z.dbf")

reports<-paste("
Land Use Planning for Multiple Environmental Services
========================================================
***

# Lembar hasil analisis SCIENDO-Scenario Development and Simulation:
# Prediksi emisi di masa yang akan datang berdasarkan berbagai skenario

***

***
# Predicted emission
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plot1)
```
***
# Summary of SCIENDO-LUWES Result
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(SCIENDO_LUWES_summary)

```
***

# Predicted emission by zone
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(plot2)
```
***
# Summary of SCIENDO-LUWES Result by planning unit
```{r fig.width=10, fig.height=9, echo=FALSE}
pandoc.table(em_by_zone)

```
***
")


#WRITE REPORT
write(reports,file="reporthtml.Rmd")
knit2html("reporthtml.Rmd", options=c("use_xhml"))
