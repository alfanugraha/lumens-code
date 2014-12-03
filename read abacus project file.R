Set_Working_Directory<-"R://Work/SCIENDO/SCIENDO"
setwd(Set_Working_Directory)
#New_Abacus_Project_db<-as.data.frame(readLines(paste(Set_Working_Directory,  "/Paling_Baru.car",sep="")))
New_Abacus_Project_file<-readLines(paste(Set_Working_Directory,  "/Paling_Baru.car",sep=""))
zone_number<-as.character(New_Abacus_Project_file[5])
zone_number<-as.data.frame(strsplit(zone_number, "="))
zone_number<-as.numeric(as.character(zone_number[2,]))
iteration_number<-as.character(New_Abacus_Project_file[12])
iteration_number<-as.data.frame(strsplit(iteration_number, "="))
iteration_number<-as.numeric(as.character(iteration_number[2,]))
baris<-as.numeric(pmatch('Summary', New_Abacus_Project_file))
baris<-baris+4

#Net Emission Per-Ha (ton CO2-eq/ha.year)
NE.ha<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(NE.ha, paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
NE.ha<-read.table(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""),sep="\t")
NE.ha$V8<-NULL
NE.ha[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.ha[,2:7]))))
colnames(NE.ha)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/NE.ha.txt",sep=""))
baris<-baris+zone_number+3

#Net Emission (ton CO2-eq/year)
NE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(NE, paste(Set_Working_Directory,  "/NE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
NE<-read.table(paste(Set_Working_Directory,  "/NE.txt",sep=""),sep="\t")
NE$V8<-NULL
NE[,2:7]<-as.numeric(as.character(as.factor(unlist(NE[,2:7]))))
colnames(NE)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/NE.txt",sep=""))
baris<-baris+zone_number+3

#Emission Per-Ha Area (ton CO2-eq/ha.year)
NE.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(NE.Ha.A, paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
NE.Ha.A<-read.table(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""),sep="\t")
NE.Ha.A$V8<-NULL
NE.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(NE.Ha.A[,2:7]))))
colnames(NE.Ha.A)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/NE.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Emission Total  (ton CO2-eq/year)
TE<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(TE, paste(Set_Working_Directory,  "/TE.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
TE<-read.table(paste(Set_Working_Directory,  "/TE.txt",sep=""),sep="\t")
TE$V8<-NULL
TE[,2:7]<-as.numeric(as.character(as.factor(unlist(TE[,2:7]))))
colnames(TE)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/TE.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Per-Ha Area (ton CO2-eq/ha.year)
Se.Ha.A<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(Se.Ha.A, paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
Se.Ha.A<-read.table(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""),sep="\t")
Se.Ha.A$V8<-NULL
Se.Ha.A[,2:7]<-as.numeric(as.character(as.factor(unlist(Se.Ha.A[,2:7]))))
colnames(Se.Ha.A)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/Se.Ha.A.txt",sep=""))
baris<-baris+zone_number+3

#Sequestration Total (ton CO2-eq/year)
ST<-as.data.frame(New_Abacus_Project_file[baris:(baris+zone_number)])
write.table(ST, paste(Set_Working_Directory,  "/ST.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep=" ") 
ST<-read.table(paste(Set_Working_Directory,  "/ST.txt",sep=""),sep="\t")
ST$V8<-NULL
ST[,2:7]<-as.numeric(as.character(as.factor(unlist(ST[,2:7]))))
colnames(ST)<-c("Zone","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/ST.txt",sep=""))
baris<-baris+zone_number+3

#Land Use System  Emission Per-Ha Area (ton CO2-eq/ha.year)
baris2<-as.numeric(pmatch('Land Use System\tEmission Total  (ton CO2-eq/year)', New_Abacus_Project_file))
LUS.EM<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
write.table(LUS.EM, paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t") 
LUS.EM<-read.table(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""),sep="\t")
LUS.EM$V10<-NULL
LUS.EM[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM[,4:9]))))
colnames(LUS.EM)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/LUS.EM.txt",sep=""))
baris<-baris2+2

#Land Use System  Emission Total  (ton CO2-eq/year)
baris2<-as.numeric(pmatch('Emissions Associated with Benefit Less Than Threshold ', New_Abacus_Project_file))
LUS.EM.T<-as.data.frame(New_Abacus_Project_file[baris:(baris2-2)])
write.table(LUS.EM.T, paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t") 
LUS.EM.T<-read.table(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""),sep="\t")
LUS.EM.T$V10<-NULL
LUS.EM.T[,4:9]<-as.numeric(as.character(as.factor(unlist(LUS.EM.T[,4:9]))))
colnames(LUS.EM.T)<-c("Zone","Original.LUS","New.LUS","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/LUS.EM.T.txt",sep=""))
baris<-baris2+4

#Cost Threshold ($/ton CO2-eq)  5.0
CT<-as.data.frame(New_Abacus_Project_file[baris:(baris+1+iteration_number)])
write.table(CT, paste(Set_Working_Directory,  "/CT.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t") 
CT<-read.table(paste(Set_Working_Directory,  "/CT.txt",sep=""),sep="\t")
CT[,2]<-as.numeric(as.character(as.factor(unlist(CT[,2]))))
colnames(CT)<-c("Iteration","Private")
file.remove(paste(Set_Working_Directory,  "/CT.txt",sep=""))
baris<-baris+iteration_number+4

#Total
baris2<-as.numeric(nrow(as.data.frame(New_Abacus_Project_file)))
Summary<-as.data.frame(New_Abacus_Project_file[baris:baris2])
write.table(Summary, paste(Set_Working_Directory,  "/Summary.txt",sep=""),append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE, sep="\t") 
Summary<-read.table(paste(Set_Working_Directory,  "/Summary.txt",sep=""),sep="\t")
Summary$V8<-NULL
Summary[,2:7]<-as.numeric(as.character(as.factor(unlist(Summary[,2:7]))))
colnames(Summary)<-c("Summary","0","1","2","3","4","5")
file.remove(paste(Set_Working_Directory,  "/Summary.txt",sep=""))
