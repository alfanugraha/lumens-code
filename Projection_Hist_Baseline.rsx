##[SCIENDO]=group
##wd=folder
##carbon_data=file
##period1=number 2005
##period2=number 2010
##SCIENDO_LUWES=output table
##SCIENDO_LUWES_summary=output table
##lutm_z=output table
##lutm_o=output table
##tpm_matrix=output table

#example to undo a commit

library(foreign)
library(reshape2)
library(plyr)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(hexbin)
library(grid)
library(ggplot2)
library(R2wd)
library(rtf)

time_start<-paste(eval(parse(text=(paste("Sys.time ()")))), sep="")

# set working directory
setwd(wd)
# load datasets
data_merge<-read.dbf(carbon_data)
t1=period1
t2=period2
period<-t2-t1
iteration=5
total<-sum(data_merge$COUNT)
data_merge.melt <- melt(data = data_merge, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
area_lc1 <- dcast(data = data_merge.melt, formula = ID_LC1 ~ ., fun.aggregate = sum)
colnames(area_lc1)[2] ="COUNT"
area_lc2<- dcast(data = data_merge.melt, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(area_lc2)[2] ="COUNT"
data_merge.melt <- melt(data = data_merge, id.vars=c('ZONE'), measure.vars=c('COUNT'))
area_zone <- dcast(data = data_merge.melt, formula = ZONE ~ variable, fun.aggregate = sum)

#calculate transition probability 1st iteration
x<-as.data.frame(data_merge)
y<-as.data.frame(area_lc1)
z<-as.data.frame(area_lc2)
v<-as.data.frame(area_zone)
colnames(v)[2] ="Z_AREA"
data_merge <- as.data.frame(merge(data_merge,v,by="ZONE"))
ID_LC1_fr<-x$ID_LC1
ID_LC2_to<-x$ID_LC2
ID_LC2_fr<-x$ID_LC1
ID_LC3_to<-x$ID_LC2
ID_LC3_fr<-x$ID_LC1
ID_LC4_to<-x$ID_LC2
ID_LC4_fr<-x$ID_LC1
ID_LC5_to<-x$ID_LC2
ID_LC5_fr<-x$ID_LC1
ID_LC6_to<-x$ID_LC2
ID_LC6_fr<-x$ID_LC1
ID_LC7_to<-x$ID_LC2
ID_LC7_fr<-x$ID_LC1

lutm_database<-data_merge
lutm_database$ID_LC1_fr<-ID_LC1_fr
lutm_database$ID_LC2_to<-ID_LC2_to
lutm_database$ID_LC2_fr<-ID_LC2_fr
lutm_database$COUNT1_2<-x$COUNT
colnames(y)[1] ="ID_LC1_fr"
colnames(y)[2] ="ovcount_t1fr"
colnames(z)[1] ="ID_LC2_to"
colnames(z)[2] ="ovcount_t2to"
lutm_database <- as.data.frame(merge(lutm_database,y,by="ID_LC1_fr"))
lutm_database <- as.data.frame(merge(lutm_database,z,by="ID_LC2_to"))
colnames(z)[1] ="ID_LC2_fr"
colnames(z)[2] ="ovcount_t2fr"
lutm_database <- as.data.frame(merge(lutm_database,z,by="ID_LC2_fr"))
lutm_database$tpm<-lutm_database$COUNT/lutm_database$ovcount_t1fr
lutm_database$COUNT2_3<-lutm_database$tpm*lutm_database$ovcount_t2fr
lutm_database$lutm_landscape<-lutm_database$COUNT2_3/total
lutm_database$lutm_zone<-lutm_database$COUNT2_3/lutm_database$Z_AREA
lutm_database$ck_em<-lutm_database$CARBON_t1>lutm_database$CARBON_t2
lutm_database$ck_sq<-lutm_database$CARBON_t1<lutm_database$CARBON_t2
lutm_database$em0<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT1_2*3.67
lutm_database$sq0<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT1_2*3.67
lutm_database$em1<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT2_3*3.67
lutm_database$sq1<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT2_3*3.67
lutm_1<-lutm_database

#calculate transition probability 2nd iteration
lutm_database$ID_LC3_to<-lutm_database$ID_LC2
lutm_database$ID_LC3_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC3_to'), measure.vars=c('COUNT2_3'))
area_lc3 <- dcast(data = lutm_database.melt, formula = ID_LC3_to ~ ., fun.aggregate = sum)
colnames(area_lc3)[1] ="ID_LC3_fr"
colnames(area_lc3)[2] ="ovcount_t3fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc3,by="ID_LC3_fr"))
lutm_database$COUNT3_4<-lutm_database$tpm*lutm_database$ovcount_t3fr
lutm_database$em2<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT3_4*3.67
lutm_database$sq2<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT3_4*3.67
lutm_2<-lutm_database

#calculate transition probability 3rd iteration
lutm_database$ID_LC4_to<-lutm_database$ID_LC2
lutm_database$ID_LC4_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC4_to'), measure.vars=c('COUNT3_4'))
area_lc4 <- dcast(data = lutm_database.melt, formula = ID_LC4_to ~ ., fun.aggregate = sum)
colnames(area_lc4)[1] ="ID_LC4_fr"
colnames(area_lc4)[2] ="ovcount_t4fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc4,by="ID_LC4_fr"))
lutm_database$COUNT4_5<-lutm_database$tpm*lutm_database$ovcount_t4fr
lutm_database$em3<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT4_5*3.67
lutm_database$sq3<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT4_5*3.67
lutm_3<-lutm_database

#calculate transition probability 4th iteration
lutm_database$ID_LC5_to<-lutm_database$ID_LC2
lutm_database$ID_LC5_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC5_to'), measure.vars=c('COUNT4_5'))
area_lc5 <- dcast(data = lutm_database.melt, formula = ID_LC5_to ~ ., fun.aggregate = sum)
colnames(area_lc5)[1] ="ID_LC5_fr"
colnames(area_lc5)[2] ="ovcount_t5fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc5,by="ID_LC5_fr"))
lutm_database$COUNT5_6<-lutm_database$tpm*lutm_database$ovcount_t5fr
lutm_database$em4<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT5_6*3.67
lutm_database$sq4<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT5_6*3.67
lutm_4<-lutm_database

#calculate transition probability 5th iteration (this will be maximum iteration)
lutm_database$ID_LC6_to<-lutm_database$ID_LC2
lutm_database$ID_LC6_fr<-lutm_database$ID_LC1
lutm_database.melt <- melt(data = lutm_database, id.vars=c('ID_LC6_to'), measure.vars=c('COUNT5_6'))
area_lc6 <- dcast(data = lutm_database.melt, formula = ID_LC6_to ~ ., fun.aggregate = sum)
colnames(area_lc6)[1] ="ID_LC6_fr"
colnames(area_lc6)[2] ="ovcount_t6fr"
lutm_database <- as.data.frame(merge(lutm_database,area_lc6,by="ID_LC6_fr"))
lutm_database$COUNT6_7<-lutm_database$tpm*lutm_database$ovcount_t6fr
lutm_database$em5<-(lutm_database$CARBON_t1-lutm_database$CARBON_t2)*lutm_database$ck_em*lutm_database$COUNT6_7*3.67
lutm_database$sq5<-(lutm_database$CARBON_t2-lutm_database$CARBON_t1)*lutm_database$ck_sq*lutm_database$COUNT6_7*3.67
lutm_5<-lutm_database

#make summary

Parameters<-c("Total emission (CO2 eq)", "Total sequestration (CO2 eq)", "Net emission (CO2 eq)", "Emission rate (CO2 eq/(ha.yr))", "Cumulative emission (CO2 eq/(ha.yr))")

sum_em0<-sum(lutm_database$em0)
sum_sq0<-sum(lutm_database$sq0)
net_em0<-sum(sum_em0-sum_sq0)
rate_em0<-net_em0/(total*period)
cum0<-0
sum_em1<-sum(lutm_database$em1)
sum_sq1<-sum(lutm_database$sq1)
net_em1<-sum(sum_em1-sum_sq1)
rate_em1<-net_em1/(total*period)
cum1<-rate_em0+rate_em1
Base<-c(sum_em0,sum_sq0,net_em0,rate_em0,cum0)
Iteration1<-c(sum_em1,sum_sq1,net_em1,rate_em1,cum1)
summary_SCIENDO_iteration1<-data.frame(Parameters,Base,Iteration1)

sum_em2<-sum(lutm_database$em2)
sum_sq2<-sum(lutm_database$sq2)
net_em2<-sum(sum_em2-sum_sq2)
rate_em2<-net_em2/(total*period)
cum2<-cum1+rate_em2
Iteration2<-c(sum_em2,sum_sq2,net_em2,rate_em2,cum2)
summary_SCIENDO_iteration2<-data.frame(summary_SCIENDO_iteration1,Iteration2)

sum_em3<-sum(lutm_database$em3)
sum_sq3<-sum(lutm_database$sq3)
net_em3<-sum(sum_em3-sum_sq3)
rate_em3<-net_em3/(total*period)
cum3<-cum2+rate_em3
Iteration3<-c(sum_em3,sum_sq3,net_em3,rate_em3,cum3)
summary_SCIENDO_iteration3<-data.frame(summary_SCIENDO_iteration2,Iteration3)

sum_em4<-sum(lutm_database$em4)
sum_sq4<-sum(lutm_database$sq4)
net_em4<-sum(sum_em4-sum_sq4)
rate_em4<-net_em4/(total*period)
cum4<-cum3+rate_em4
Iteration4<-c(sum_em4,sum_sq4,net_em4,rate_em4,cum4)
summary_SCIENDO_iteration4<-data.frame(summary_SCIENDO_iteration3,Iteration4)

sum_em5<-sum(lutm_database$em5)
sum_sq5<-sum(lutm_database$sq5)
net_em5<-sum(sum_em5-sum_sq5)
rate_em5<-net_em5/(total*period)
cum5<-cum4+rate_em5
Iteration5<-c(sum_em5,sum_sq5,net_em5,rate_em5,cum5)
summary_SCIENDO_iteration5<-data.frame(summary_SCIENDO_iteration4,Iteration5)


#save SCIENDO-LUWES Database
SCIENDO_LUWES<-if(iteration==1){
lutm_1
}else if(iteration==2){
lutm_2
}else if(iteration==3){
lutm_3
}else if(iteration==4){
lutm_4
}else if(iteration==5){
lutm_5
}

#save SCIENDO-LUWES Summary
SCIENDO_LUWES_summary<-if(iteration==1){
summary_SCIENDO_iteration1
}else if(iteration==2){
summary_SCIENDO_iteration2
}else if(iteration==3){
summary_SCIENDO_iteration3
}else if(iteration==4){
summary_SCIENDO_iteration4
}else if(iteration==5){
summary_SCIENDO_iteration5
}

#Remove unnecessary colum
SCIENDO_LUWES$ID_LC6_fr<-SCIENDO_LUWES$ID_LC5_fr<-SCIENDO_LUWES$ID_LC4_fr<-SCIENDO_LUWES$ID_LC3_fr<-SCIENDO_LUWES$ID_LC2_fr<-SCIENDO_LUWES$ID_LC2_to<-SCIENDO_LUWES$ID_LC1_fr<-NULL
SCIENDO_LUWES$em<-SCIENDO_LUWES$sq<-SCIENDO_LUWES$null<-SCIENDO_LUWES$nullCek<-SCIENDO_LUWES$ZONE_ID<-SCIENDO_LUWES$COUNT<-SCIENDO_LUWES$ovcount_t1fr<-SCIENDO_LUWES$ovcount_t2to<-NULL
SCIENDO_LUWES$ovcount_t2fr<-SCIENDO_LUWES$ID_LC3_to<-SCIENDO_LUWES$ovcount_t3fr<-SCIENDO_LUWES$ID_LC4_to<-SCIENDO_LUWES$ovcount_t4fr<-SCIENDO_LUWES$ID_LC5_to<-SCIENDO_LUWES$ovcount_t5fr<-SCIENDO_LUWES$ID_LC6_to<-SCIENDO_LUWES$ovcount_t6fr<-NULL

#SCIENDO-LUWES Overall LUTM
lutm_o<-as.data.frame(as.numeric(SCIENDO_LUWES$ID_LC1))
colnames(lutm_o)[1] ="ID_LC1"
lutm_o$ID_LC2<-as.numeric(SCIENDO_LUWES$ID_LC2)
lutm_o$landcover_t1<-SCIENDO_LUWES$LC_t1
lutm_o$landcover_t2<-SCIENDO_LUWES$LC_t2
lutm_o$zone<-SCIENDO_LUWES$Z_NAME
lutm_o$TPM<-SCIENDO_LUWES$tpm
lutm_o$LUTM<-SCIENDO_LUWES$lutm_landscape
lutm_o.melt <- melt(data = lutm_o, id.vars=c('ID_LC1','ID_LC2','landcover_t1','landcover_t2'), measure.vars=c('TPM'))
tpm_matrix <- dcast(data = lutm_o.melt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)
lutm_o.melt <- melt(data = lutm_o, id.vars=c('ID_LC1','ID_LC2','landcover_t1'), measure.vars=c('LUTM'))
lutm_matrix <- dcast(data = lutm_o.melt, formula = ID_LC1 + landcover_t1 ~ ID_LC2, fun.aggregate = sum)

#SCIENDO-LUWES Zones LUTM
lutm_z<-SCIENDO_LUWES
lutm_z$lutm_z<-lutm_z$COUNT1_2/lutm_z$Z_AREA
lutm_z.melt <- melt(data = lutm_z, id.vars=c('LC_t1','Z_NAME'), measure.vars=c('lutm_z'))
luzone_p <- dcast(data = lutm_z.melt, formula = LC_t1 + Z_NAME ~ ., fun.aggregate = sum)
colnames(luzone_p)[3] ="luzone"
luzone_p$key <- do.call(paste, c(luzone_p[c("LC_t1", "Z_NAME")], sep = " in "))
luzone_p$LC_t2<-luzone_p$Z_NAME<-NULL
lutm_z$key <- do.call(paste, c(lutm_z[c("LC_t1", "Z_NAME")], sep = " in "))
lutm_z<-merge(lutm_z,luzone_p,by="key")
lutm_z$tpm_z<-lutm_z$lutm_z/lutm_z$luzone
lutm_z$ZONE<-lutm_z$CARBON_t1<-lutm_z$CARBON_t2<-lutm_z$ck_em<-lutm_z$ck_sq<-lutm_z$LUCHG<-lutm_z$ID_LC1<-lutm_z$ID_LC2<-NULL
lutm_z$tpm<-lutm_z$COUNT2_3<-lutm_z$lutm_landscape<-lutm_z$em0<-lutm_z$sq0<-lutm_z$em1<-lutm_z$sq1<-lutm_z$COUNT3_4<-lutm_z$em2<-lutm_z$sq2<-lutm_z$COUNT4_5<-lutm_z$em3<-lutm_z$sq3<-lutm_z$COUNT5_6<-lutm_z$em4<-lutm_z$sq4<-lutm_z$COUNT6_7<-lutm_z$em5<-lutm_z$sq5<-NULL
lutm_z$LU_CHG<-lutm_z$key<-lutm_z$lutm_zone<-lutm_z$tpmx<-lutm_z$A_lczone<-NULL
lutm_z$Z_AREA<-lutm_z$COUNT1_2<-lutm_z$LC_t1.y<-lutm_z$luzone<-NULL

#Plot data
#subdat <- SCIENDO_LUWES_summary[5,]
#subdat_l <- data.frame(Value = unlist(subdat))
#subdat_l=subdat_l[-1,]
#subdat_l<-subdat_l[-c(1,1)]
#baseline<-barplot(subdat_l)

#conduct analysis on the dataset
SCIENDO_LUWES_summary[,2:7]<-round(SCIENDO_LUWES_summary[,2:7],digits=2)
SL_overall<-SCIENDO_LUWES_summary
SL_analysis<-SCIENDO_LUWES
SL_overall.melt <- melt(data = SL_overall)
SL_overall.melt.cast <- dcast(data = SL_overall.melt, formula = Parameters ~ variable, fun.aggregate = sum, subset = .(Parameters=="Cumulative emission (CO2 eq/(ha.yr))"))
SL_overall_data<- melt(data = SL_overall.melt.cast)
SL_overall_data<-SL_overall_data[-c(1),]

t_1<-t1
t_2<-t_1+period
Period.db<-as.data.frame(NULL)
Periode<-as.data.frame(NULL)
for ( i in 1:nrow(SL_overall_data)){
period.int<-paste(t_1,"-",t_2, sep="")
Period.db<-c(Period.db,period.int)
t_1<-t_1+period
t_2<-t_1+period
}
t_1<-t1
t_2<-t_1+period
Period.db<-as.character(Period.db)
for ( i in 1:(nrow(SL_overall_data)+1)){
period.int<-paste(t_1,"-",t_2, sep="")
Periode<-c(Periode,period.int)
t_1<-t_1+period
t_2<-t_1+period
}
Periode<-as.character(Periode)


SL_analysis.melt <- melt(data = SL_analysis, id.vars=c('Z_NAME'), measure.vars=c('em0','em1','em2','em3','em4','em5'))
em_by_zone<-dcast(data = SL_analysis.melt, formula = Z_NAME ~ variable, fun.aggregate = sum)
em_by_zone$Z_CODE<-toupper(abbreviate(em_by_zone$Z_NAME))
em_by_zone<-em_by_zone[c(1,8,2,3,4,5,6,7)]
em_by_zone[,3:8]<-round(em_by_zone[,3:8],digits=2)

#Cumulative emission
cum_em<-em_by_zone
n.col<-ncol(cum_em)
for (i in 1:(n.col-3)){
cum_em[,i+3]<-rowSums(em_by_zone[,(3):(i+3)])
}
cum_em.melt<-melt(data = cum_em, id.vars=c('Z_NAME','Z_CODE'), measure.vars=c('em0','em1','em2','em3','em4','em5'))

plot1<-ggplot(SL_overall_data,aes(variable,value,group=1,fill=Parameters))+ geom_line(colour="red") +
geom_point(colour="red", size=4, shape=21, fill="white") +
geom_text(data=SL_overall_data, aes(x=variable, y=value, label=round(value, 1)),size=3, hjust=1.5,vjust=-0.5) +
scale_x_discrete(breaks=SL_overall_data$variable ,labels=Period.db) +
xlab('Year') +  ylab('Cum.CO2eq/ha.yr') +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))
plot2<-ggplot(cum_em.melt,aes(Z_NAME,value,fill=variable))+geom_bar(stat="identity",position="dodge")+
xlab('Zone Code') +  ylab('Cum.CO2eq/ha.yr') + scale_x_discrete(breaks=SL_analysis.melt$Z_NAME ,labels=em_by_zone$Z_CODE) +
theme(axis.text.x= element_text(angle=0,hjust=1))+ scale_fill_discrete(name="Cumulative \nEmission (CO2eq)", labels=Periode)
plot3<-ggplot(cum_em.melt,aes(variable,value,fill=Z_CODE))+ geom_line(data=cum_em.melt, aes(x=variable, y=value, group=Z_CODE, fill=Z_CODE, colour=Z_CODE), stat="identity") +
geom_point(data=cum_em.melt,aes(colour=Z_CODE), size=3) +
scale_x_discrete(breaks=unique(cum_em.melt$variable) ,labels=Periode) +
xlab('Year') +  ylab('Cum.CO2eq/ha.yr') +
theme( legend.title = element_text(size=8),legend.text = element_text(size = 8))

#write output to file
write.dbf(SCIENDO_LUWES,"SCIENDO-LUWES_database.dbf")
write.dbf(SCIENDO_LUWES_summary,"SCIENDO-LUWES_summary.dbf")
write.dbf(lutm_z,"SCIENDO-LUWES_zones_tpm.dbf")
write.dbf(lutm_z,"SCIENDO-LUWES_zones_tpm_model.dbf")
write.dbf(lutm_o,"SCIENDO-LUWES_overall_lutm.dbf")
write.dbf(tpm_matrix,"SCIENDO-LUWES_overall_tpm_matrix.dbf")
write.dbf(lutm_z,"lutm_z.dbf")

#WRITE REPORT
title<-"\\b\\fs32 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs28 Sub-modules : Projection on Historical Baseline \\b0\\fs20"
#date<-paste("Date : ", date, sep="")
time_start<-paste("Processing started : ", time_start, sep="")
time_end<-paste("Processing ended : ", eval(parse(text=(paste("Sys.time ()")))), sep="")
line<-paste("------------------------------------------------------------------------------------------------------------------------------------------------")
I_O_period_1_rep<-paste("\\b","\\fs20", t1)
I_O_period_2_rep<-paste("\\b","\\fs20", t2)
rtffile <- RTF("LUMENS_SCIENDO-PHB_report.lpr", font.size=9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
#addParagraph(rtffile, date)
addParagraph(rtffile, time_start)
addParagraph(rtffile, time_end)
addParagraph(rtffile, line)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot1)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 1.Predicted Emission\\b0 \\fs20 "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 1. Summary of SCIENDO-LUWES Result \\b0 \\fs20"))
addTable(rtffile,SCIENDO_LUWES_summary)
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot2)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 2.Predicted Cumulative Emission by Zone\\b0 \\fs20 "))
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, plot3)
addParagraph(rtffile, paste("\\b \\fs20 Gambar 3.Predicted Cumulative Emission \\b0 \\fs20 "))
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs20 Table 2. Summary of SCIENDO-LUWES Result by Planning Unit \\b0 \\fs20"))
addTable(rtffile,cum_em)
addNewLine(rtffile)
done(rtffile)


