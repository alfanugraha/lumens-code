##[SCIENDO-EMRED]=group
Set_Working_Directory="C:/LUMENS_MURA/EM_RED"
period1=2000
period2=2005
iteration=3
Scenario_Directory="C:/LUMENS_MURA/EM_RED/"
Abacus_Project_File="C:/LUMENS_MURA/Historicalbaseline.car"

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

time_start <- paste(eval(parse(text=(paste("Sys.time ()")))), sep="")
setwd(Set_Working_Directory)
cars <- list.files(Scenario_Directory, full.names=TRUE, pattern="\\.car$")

numOfCars <- length(cars)
for(i in 1:numOfCars) {
  eval(parse(text=(paste("carsLocation", i, " <- basename(as.character(cars[", i, "]))", sep=""))))  #carsLocation1 <- basename(as.character(cars[1]))
  eval(parse(text=(paste("carsName", i, " <- substr(basename(carsLocation", i, "), 1, nchar(basename(carsLocation", i, ")) - 4)", sep="")))) #carsName1 <- substr(basename(carsLocation1), 1, nchar(basename(carsLocation1)) - 4)

  eval(parse(text=(paste("New_Abacus_Project_file <- readLines(carsLocation", i, ")", sep="")))) #New_Abacus_Project_file<-readLines(file.name)
  zone_number <- as.character(New_Abacus_Project_file[5])
  zone_number <- as.data.frame(strsplit(zone_number, "="))
  zone_number <- as.numeric(as.character(zone_number[2,]))
  iteration_number <- as.character(New_Abacus_Project_file[12])
  iteration_number <- as.data.frame(strsplit(iteration_number, "="))
  iteration_number <- as.numeric(as.character(iteration_number[2,]))
                               
  baris <- as.numeric(pmatch('Summary', New_Abacus_Project_file))
  baris <- baris+4
  baris <- baris+zone_number+3
  baris <- baris+zone_number+3
  baris <- baris+zone_number+3
   
  #Emission Total  (ton CO2-eq/year)
  TE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
  write.table(TE, paste(Set_Working_Directory,  "/TE.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
  eval(parse(text=(paste("TE", i, " <- read.table(paste(Set_Working_Directory, '/TE.txt',sep=''), sep='\t')", sep="")))) #TE1<-read.table(paste(Set_Working_Directory,  "/TE.txt",sep=""),sep="\t")
  eval(parse(text=(paste("TE", i, "$V", iteration+3, " <- NULL", sep="" ))))
  eval(parse(text=(paste("TE", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(TE", i, "[,2:(iteration+2)]))))", sep="" )))) #TE1[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(TE1[,2:(iteration+2)]))))
  file.remove(paste(Set_Working_Directory,  "/TE.txt", sep=""))
   
  baris<-baris+zone_number+3
  baris<-baris+zone_number+3
  
  #Sequestration Total (ton CO2-eq/year)
  ST<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
  write.table(ST, paste(Set_Working_Directory, "/ST.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
  eval(parse(text=(paste("ST", i, " <- read.table(paste(Set_Working_Directory, '/ST.txt', sep=''), sep='\t')", sep=""))))  
  eval(parse(text=(paste("ST", i, "$V", iteration+3, " <- NULL", sep=""))))
  eval(parse(text=(paste("ST", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(ST", i, "[,2:(iteration+2)]))))", sep=""))))
  file.remove(paste(Set_Working_Directory,  "/ST.txt", sep=""))
  
  baris<-baris+zone_number+3
  baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
  baris<-baris2+2
  baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
  baris<-baris2+4
  baris<-baris+iteration_number+4
  
  #Total
  baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
  Summary<-as.data.frame(New_Abacus_Project_file[baris:baris2])
  write.table(Summary, paste(Set_Working_Directory,  "/Summary.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
  eval(parse(text=(paste("Summary", i, " <- read.table(paste(Set_Working_Directory, '/Summary.txt', sep=''), sep='\t')", sep="")))) #Summary<-read.table(paste(Set_Working_Directory,  "/Summary.txt",sep=""),sep="\t")
  eval(parse(text=(paste("Summary", i, "$V", iteration+3, " <- NULL", sep="" ))))
  eval(parse(text=(paste("Summary", i, "[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(Summary",  i, "[,2:(iteration+2)]))))", sep="")))) #Summary[,2:(iteration+2)]<-as.numeric(as.character(as.factor(unlist(Summary[,2:(iteration+2)]))))
  file.remove(paste(Set_Working_Directory,  "/Summary.txt", sep=""))  
}

# SCENARIO COMPARISON
# THIS PART IS ORIGINALLY WROTE TO ACCESS ABACUS .CAR FILE
#====Historical Baseline====

New_Abacus_Project_file <- readLines(Abacus_Project_File)
zone_number <- as.character(New_Abacus_Project_file[5])
zone_number <- as.data.frame(strsplit(zone_number, "="))
zone_number <- as.numeric(as.character(zone_number[2,]))
iteration_number <- as.character(New_Abacus_Project_file[12])
iteration_number <- as.data.frame(strsplit(iteration_number, "="))
iteration_number <- as.numeric(as.character(iteration_number[2,]))

baris <- as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris <- baris+4
baris <- baris+zone_number+3
baris <- baris+zone_number+3
baris <- baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE <- as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE, paste(Set_Working_Directory, "/TEHist.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
TEHist <- read.table(paste(Set_Working_Directory, "/TEHist.txt", sep=""), sep="\t")
eval(parse(text=(paste("TEHist$V", iteration+3, " <- NULL", sep="" ))))
TEHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(TEHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/TEHist.txt", sep=""))

baris <- baris+zone_number+3
baris <- baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST <- as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory, "/STHist.txt",sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
STHist <- read.table(paste(Set_Working_Directory, "/STHist.txt",sep=""), sep="\t")
eval(parse(text=(paste( "STHist$V" ,iteration+3, " <- NULL", sep="" ))))
STHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(STHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/STHist.txt", sep=""))

baris <- baris+zone_number+3
baris2 <- as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
baris <- baris2+2
baris2 <- as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
baris <- baris2+4
baris <- baris+iteration_number+4

#Total
baris2 <- as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary <- as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory, "/SummaryHist.txt", sep=""), append=TRUE, quote=FALSE, col.names=FALSE, row.names=FALSE, sep="\t")
SummaryHist <- read.table(paste(Set_Working_Directory, "/SummaryHist.txt", sep=""), sep="\t")
eval(parse(text=(paste( "SummaryHist$V", iteration+3, " <- NULL", sep="" ))))
SummaryHist[,2:(iteration+2)] <- as.numeric(as.character(as.factor(unlist(SummaryHist[,2:(iteration+2)]))))
file.remove(paste(Set_Working_Directory, "/SummaryHist.txt",sep=""))

#BASELINE RESULT
TableSumHist <- round((SummaryHist[(1:12),(2:(iteration+2))]),digits=2)
TableSumHist <- cbind((SummaryHist[1]),TableSumHist)
TableSumHist = TableSumHist[-2,]
TableSumHist = TableSumHist[-3,]
TableSumHist = TableSumHist[-4,]
TableSumHist = TableSumHist[-5,]
TableSumHist = TableSumHist[-6,]
TableSumHist = TableSumHist[-7,]
TableSum.Baseline<-TableSumHist
TEtemp <- round((TEHist[,(2:(iteration+2))]),digits=2)
STtemp <- round((STHist[,(2:(iteration+2))]),digits=2)
TEHist <- cbind((TEHist[1]), TEtemp)
STHist <- cbind((STHist[1]), STtemp)
TEHist$Total <- rowSums(TEHist[,2:(iteration+2)])
STHist$Total <- rowSums(STHist[,2:(iteration+2)])
TEHist.total <- sum(TEHist$Total)
STHist.total <- sum(STHist$Total)
TEHist$Percentage <- round(((TEHist$Total/TEHist.total)*100),digits=2)
STHist$Percentage <- round(((STHist$Total/STHist.total)*100),digits=2)
TEHist <- TEHist[order(-TEHist$Percentage),]
STHist <- STHist[order(-STHist$Percentage),]
TE.Baseline <- TEHist
ST.Baseline <- STHist

#SCENARIO RESULT
for(i in 1:numOfCars){ 
  eval(parse(text=(paste("TableSum <- round((Summary", i, "[(1:12), (2:(iteration+2))]), digits=2)", sep="")))) #TableSum <- round((Summary[(1:12), (2:(iteration+2))]), digits=2)
  eval(parse(text=(paste("TableSum <- cbind((Summary", i, "[1]), TableSum)", sep="")))) #TableSum <- cbind((Summary[1]), TableSum)
  TableSum = TableSum[-2,]
  TableSum = TableSum[-3,]
  TableSum = TableSum[-4,]
  TableSum = TableSum[-5,]
  TableSum = TableSum[-6,]
  TableSum = TableSum[-7,]
  eval(parse(text=(paste("TableSum.Scenario", i, " <- TableSum", sep="")))) #TableSum.Scenario <- TableSum
  
  eval(parse(text=(paste("TEtemp <- round((TE", i, "[,(2:(iteration+2))]),digits=2)", sep="")))) #TEtemp <- round((TE[,(2:(iteration+2))]),digits=2)
  eval(parse(text=(paste("TE", i, " <- cbind((TE", i, "[1]),TEtemp)", sep="")))) #TE <- cbind((TE[1]),TEtemp)
  eval(parse(text=(paste("TE", i, "$Total <- rowSums(TE", i, "[,2:(iteration+2)])", sep="")))) #TE$Total <- rowSums(TE[,2:(iteration+2)])
  eval(parse(text=(paste("TE.total", i, " <- sum(TE", i, "$Total)", sep="")))) #TE.total <- sum(TE$Total)
  eval(parse(text=(paste("TE", i, "$Percentage <- round(((TE", i, "$Total/TE.total", i, ")*100),digits=2)", sep="")))) #TE$Percentage <- round(((TE$Total/TE.total)*100),digits=2)
  eval(parse(text=(paste("TE", i, " <- TE", i, "[order(-TE", i, "$Percentage),]", sep="")))) #TE <- TE[order(-TE$Percentage),]
  eval(parse(text=(paste("TE.Scenario", i, " <- TE", i, sep="")))) #TE.Scenario <- TE
  
  eval(parse(text=(paste("STtemp <- round((ST", i, "[,(2:(iteration+2))]),digits=2)", sep="")))) #STtemp <- round((ST[,(2:(iteration+2))]),digits=2)  
  eval(parse(text=(paste("ST", i, " <- cbind((ST", i, "[1]),TEtemp)", sep="")))) #ST <- cbind((ST[1]),STtemp)
  eval(parse(text=(paste("ST", i, "$Total <- rowSums(ST", i, "[,2:(iteration+2)])", sep="")))) #ST$Total <- rowSums(ST[,2:(iteration+2)]) 
  eval(parse(text=(paste("ST.total", i, " <- sum(ST", i, "$Total)", sep="")))) #ST.total <- sum(ST$Total) 
  eval(parse(text=(paste("ST", i, "$Percentage <- round(((ST", i, "$Total/ST.total", i, ")*100),digits=2)", sep="")))) #ST$Percentage <- round(((ST$Total/ST.total)*100),digits=2)  
  eval(parse(text=(paste("ST", i, " <- ST", i, "[order(-ST", i, "$Percentage),]", sep="")))) #ST <- ST[order(-ST$Percentage),]    
  eval(parse(text=(paste("ST.Scenario", i, " <- ST", i, sep="")))) #TE.Scenario <- TE
}

#====CREATE CUMULATIVE SUMMARY====
Cum.summary<-as.data.frame(cumsum(t(TableSum.Baseline[1,2:(iteration+2)])))
for(i in 1:numOfCars){
  eval(parse(text=(paste("Cum.summary.sc", i, "<-as.data.frame(cumsum(t(TableSum.Scenario", i, "[1,2:(iteration+2)])))", sep="")))) #Cum.summary.sc<-as.data.frame(cumsum(t(TableSum.Scenario[1,2:(iteration+2)]))) 
}

#====CREATE PERIODE OF SIMULATION====
interval<-period2-period1
t_1<-period1
t_2<-t_1+interval
Period.db<-as.data.frame(NULL)
Periode<-as.data.frame(NULL)
for ( i in 1:(ncol(Cum.summary)-1)){
  period.int<-paste(t_1,"-",t_2, sep="")
  Period.db<-c(Period.db,period.int)
  t_1<-t_1+interval
  t_2<-t_1+interval
}
Period.db<-as.character(Period.db)
t_1<-period1
t_2<-t_1+interval
for (i in 1:(nrow(Cum.summary))){
  period.int<-paste(t_1,"-",t_2, sep="")
  Periode<-c(Periode,period.int)
  t_1<-t_1+interval
  t_2<-t_1+interval
}
Periode<-as.character(Periode)

#FIX COLUMN NAME
Colname.ST.TE<-append("Planning unit", Periode)
Colname.ST.TE<-append(Colname.ST.TE, "Total")
Colname.ST.TE<-append(Colname.ST.TE, "Percentage")

colnames(TE.Baseline) <- c(Colname.ST.TE)
colnames(ST.Baseline) <- c(Colname.ST.TE)

for(i in 1:numOfCars){
  eval(parse(text=(paste("colnames(TE.Scenario", i, ") <- c(Colname.ST.TE)", sep="")))) #colnames(TE.Scenario) <- c(Colname.ST.TE)
  eval(parse(text=(paste("colnames(ST.Scenario", i, ") <- c(Colname.ST.TE)", sep="")))) #colnames(ST.Scenario) <- c(Colname.ST.TE)
}

Colname.Summary<-append("Parameters", Periode)
colnames(TableSum.Baseline) <- c(Colname.Summary)

for(i in 1:numOfCars){
  eval(parse(text=(paste("colnames(TableSum.Scenario", i, ") <- c(Colname.Summary)", sep="")))) #colnames(TableSum.Scenario) <- c(Colname.Summary)
}

#====CUMULATIVE TABLE====
Cumulative.table <- cbind(as.data.frame(Periode), Cum.summary)
colnames(Cumulative.table)[2] <- "BASELINE Cum.em (tonCo2/ha.yr)"

for(i in 1:numOfCars){
  eval(parse(text=(paste("Cumulative.table <- cbind(Cumulative.table, Cum.summary.sc", i, ")", sep="")))) #Cumulative.table<-cbind(Cumulative.table, Cum.summary.sc)
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  b <- paste(a, " Cum.em (tonCO2/ha.yr)", sep="")
  eval(parse(text=(paste("colnames(Cumulative.table)[", 3+i-1, "] <- b", sep="")))) #colnames(Cumulative.table)[3]<-'SCENARIO Cum.em (tonCo2/ha.yr)'
}

#====PREPARE TABLE FOR GRAPH AND CREATE GRAPH====
Cum.summary.temp<-Cum.summary
colnames(Cum.summary.temp)[1]<-"Cum.em"

class1<-"BASELINE"
Cum.summary.temp<-cbind(class1, Cum.summary.temp)
Cum.summary.temp<-cbind(as.data.frame(Periode), Cum.summary.temp)

for(i in 1:numOfCars){
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- Cum.summary.sc", i, sep="")))) #Cum.summary.temp.sc<-Cum.summary.sc1
  eval(parse(text=(paste("colnames(Cum.summary.temp.sc", i, ")[1] <- 'Cum.em'", sep="")))) #colnames(Cum.summary.temp.sc)[1] <- 'Cum.em'
  eval(parse(text=(paste("class1 <- carsName", i, sep="")))) #class1<-"SCENARIO"
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- cbind(class1,Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(class1,Cum.summary.temp.sc)
  eval(parse(text=(paste("Cum.summary.temp.sc", i, " <- cbind(as.data.frame(Periode), Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.temp.sc <- cbind(as.data.frame(Periode), Cum.summary.temp.sc)
  if(i==1){
    eval(parse(text=(paste("Cum.summary.tab <- rbind(Cum.summary.temp, Cum.summary.temp.sc", i, ")", sep="")))) #Cum.summary.tab <- rbind(Cum.summary.temp,Cum.summary.temp.sc)
  } else {
    eval(parse(text=(paste("Cum.summary.tab <- rbind(Cum.summary.tab, Cum.summary.temp.sc", i, ")", sep=""))))
  }
}
PLOT1 <- ggplot(data=Cum.summary.tab, aes(x=Periode, y=Cum.em, group=class1, colour=class1)) + ylab("Emisi kumulatif (tonCO2/ha.yr)") + geom_line() + geom_point()+ geom_point(size=3, fill="white")

#====CALCULATE EMISSION REDUCTION====
for(i in 1:numOfCars){
  em.reduction1 <- Cumulative.table[nrow(Cumulative.table),2]
  eval(parse(text=(paste("em.reduction2 <- Cumulative.table[nrow(Cumulative.table),", 3+i-1, "]", sep="")))) #em.reduction2<-Cumulative.table[nrow(Cumulative.table),3]
  eval(parse(text=(paste("em.reduction.sc", i, " <- ((em.reduction1-em.reduction2)/em.reduction1)*100", sep="")))) #em.reduction.sc1<-((em.reduction1-em.reduction2)/em.reduction1)*100
  eval(parse(text=(paste("em.reduction.sc", i, " <- round(em.reduction.sc", i, ", digits=2)", sep="")))) #em.reduction<-round(em.reduction, digits=2)
  
  #kalimat<-paste("Persentase penurunan emisi terhadap baseline adalah " , em.reduction, "%", sep="")
  a<-"Persentase skenario penurunan emisi "
  eval(parse(text=(paste("b <- carsName", i, sep=""))))
  c<-" terhadap baseline adalah "
  d<-" %"
  eval(parse(text=(paste("kalimat", i, "<-paste(a, b, c, em.reduction.sc", i, ", d, sep='')", sep=""))))
}

#====PREPARE COMPARISON OF PLANNING UNIT====
for(i in 1:numOfCars){
  eval(parse(text=(paste("compare.pu", i, " <- merge(TE.Baseline, TE.Scenario", i, ", by='Planning unit')", sep="")))) #compare.pu<-merge(TE.Baseline, TE.Scenario, by="Planning unit")
  eval(parse(text=(paste("compare.pu.fin", i, " <- compare.pu", i, "[1]", sep="")))) #compare.pu.fin<-compare.pu[1]
  eval(parse(text=(paste("compare.pu.fin", i, "$em.reduction <- compare.pu", i, "$Total.x-compare.pu", i, "$Total.y", sep="")))) #compare.pu.fin$em.reduction<-compare.pu$Total.x-compare.pu$Total.y
  eval(parse(text=(paste("sum1 <- sum(compare.pu.fin", i, "$em.reduction)", sep="")))) #sum1<-sum(compare.pu.fin$em.reduction)
  eval(parse(text=(paste("compare.pu.fin", i, "$percentage <- (compare.pu.fin", i, "$em.reduction/sum1)*100", sep="")))) #compare.pu.fin$percentage<-(compare.pu.fin$em.reduction/sum1)*100
  eval(parse(text=(paste("compare.pu.fin", i, "$percentage <- round(compare.pu.fin", i, "$percentage, digits=2)", sep="")))) #compare.pu.fin$percentage<-round(compare.pu.fin$percentage, digits=2)
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[1] <- 'Planning_unit'", sep="")))) #colnames(compare.pu.fin)[1]<-'Planning_unit'
  eval(parse(text=(paste("PLOT2_", i, " <- ggplot(data=compare.pu.fin", i, ", aes(x=Planning_unit, y=percentage)) + xlab('Planning unit') + ylab('Persentase') +  geom_bar(stat='identity')+coord_flip()", sep="")))) #PLOT2<-ggplot(data=compare.pu.fin, aes(x=Planning_unit, y=percentage)) + geom_bar(stat="identity")+coord_flip()
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[1] <- 'Planning unit'", sep="")))) 
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[2] <- 'Penurunan emisi'", sep="")))) 
  eval(parse(text=(paste("colnames(compare.pu.fin", i, ")[3] <- 'Persentase'", sep="")))) 
}

#====WRITE REPORT====
title<-"\\b\\fs40 LUMENS-SCIENDO Project Report\\b0\\fs20"
sub_title<-"\\b\\fs32 PERBANDINGAN PROYEKSI EMISI TERHADAP BASELINE \\b0\\fs20"
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
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 1. INTISARI PERHITUNGAN EMISI DARI SCENARIO\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perhitungan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu. Informasi yang dihasilkan meliputi jumlah emisi, jumlah sequestrasi dan jumlah emisi mulatif dalam periode analisa")
addNewLine(rtffile)

for(i in 1:numOfCars){
  eval(parse(text=(paste("a <- carsName", i, sep=""))))
  addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah emisi",a,"\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,TableSum.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,TableSum.Scenario, font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah emisi",a,"untuk setiap unit perencanaan\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,TE.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,TE.Scenario, font.size=8)
  addNewLine(rtffile)
  addParagraph(rtffile, paste("\\b \\fs32 Tabel risalah sequestrasi",a,"untuk setiap unit perencanaan\\b0 \\fs20"))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,ST.Scenario", 1, ", font.size=8)", sep=""))))
  #addTable(rtffile,ST.Scenario, font.size=8)
  addNewLine(rtffile)
}

addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 2. PERBANDINGAN TERHADAP BASELINE\\b0 \\fs20"))
addNewLine(rtffile)
addParagraph(rtffile, "Bagian ini menjelaskan hasil perbandingan emisi dari skenario perencanaan penggunaan lahan yang disusun dalam periode tertentu terhadap baseline atau kondisi yang diperkirakan terjadi jika tidak ada intervensi terhadap kondisi saat ini. Informasi yang dihasilkan meliputi jumlah emisi, jumlah sequestrasi dan jumlah emisi mulatif dalam periode analisa")
addNewLine(rtffile)
addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT1)
addParagraph(rtffile,"Grafik perbandingan emisi scenario terhadap baseline")
addNewLine(rtffile)
addParagraph(rtffile, paste("\\b \\fs32 Tabel perbandingan emisi kumulatif\\b0 \\fs20"))
addNewLine(rtffile)
addTable(rtffile,Cumulative.table, font.size=8)
addNewLine(rtffile)

for(i in 1:numOfCars){
  eval(parse(text=(paste("kalimat <- kalimat", i, sep=""))))
  addParagraph(rtffile, paste("\\b \\fs24", kalimat,"\\b0 \\fs20", sep=""))
  addNewLine(rtffile)
  eval(parse(text=(paste("addTable(rtffile,compare.pu.fin", i, ", font.size=8)", sep=""))))
  #addTable(rtffile,compare.pu.fin, font.size=8)
  addNewLine(rtffile)
  eval(parse(text=(paste("addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2_", i, ")", sep=""))))
  #addPlot(rtffile,plot.fun=print, width=6.7,height=3,res=300, PLOT2)
  addNewLine(rtffile)
}

done(rtffile)
