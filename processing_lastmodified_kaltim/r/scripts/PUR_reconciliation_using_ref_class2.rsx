##[PUR]=group
##WDir=folder
##data=file
##ref=file
##loc=string
##PUR_dbfinal=output table
##report

library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(PBSmapping)
library(gridExtra)
library(raster)
library(knitr)
library(markdown)
library(foreign)

wd <- WDir
location <- loc
PUR_data <- data
ref_data <- ref
setwd (wd)

# PREPARE REFERENCE DATA
datalist_ref <- read.table(ref_data, header=FALSE, sep=",")
file_ref <- datalist_ref[1]
alias_ref <- datalist_ref [2]
attribute_ref <- datalist_ref [3]
LUT_ref<- datalist_ref[4]
ref_data<-as.character(file_ref[1,])

st_area_file<- substr(basename(ref_data), 1, nchar(basename(ref_data)) - 4)
st_area_file
sa<-readOGR(".",st_area_file)
xlim_sa <- sa@bbox[1, ]
ylim_sa <- sa@bbox[2, ]
ext<-extent (sa)
xy_sa <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n<-100  # in meter, need to be modified if in degree 0.0009
r <- raster(ext, ncol=xy_sa[1]/n, nrow=xy_sa[2]/n)
ref <-rasterize(sa, r)
ref[is.na(ref)]<-0
#plot (ref)


# PREPARE PLANNING UNIT FILE

datalist <- read.table(PUR_data, header=FALSE, sep=",")
file <- datalist[1]
alias <- datalist [2]
attribute <- datalist [3]
LUT<- datalist[4]
file.number <-nrow(file)
command1<-paste()
command2<-paste()
command3<-paste()
for (i in 1:file.number) {
    test<-basename (as.character(file[i,]))
    input <-substr(basename(test), 1, nchar(basename(test)) - 4)
    eval(parse(text=(paste("pu_v",i,'<-readOGR(".","', input,'")', sep=""))))
    eval(parse(text=(paste("pu",i,'<-','rasterize(pu_v',i,',r)',sep=""))))
    eval(parse(text=(paste("pu", i, "<-deratify(pu", i, ",'", as.character(attribute[i,]), "')", sep=""))))
#eval(parse(text=(paste("PUR<-stack(PUR,","pu",i,")", sep=""))))
    eval(parse(text=(paste("pu", i, "_attribute<-levels(pu",i,")", sep=""))))
    eval(parse(text=(paste("pu", i, "_attribute<-as.data.frame(pu", i, "_attribute)", sep=""))))
#eval(parse(text=(paste("pu", i,"_attribute<-pu", i, '_attribute[,c("ID",(as.character(attribute[',i, ",])))]", sep=""))))
    eval(parse(text=(paste("colnames(pu",i,'_attribute)<-c((paste("Var",i,sep="")),(as.character(alias[', i, ",])))", sep=""))))
    eval(parse(text=(paste("pu", i, "_LUT<-read.table(as.character(LUT[", i, ',]), header=TRUE, sep=",")', sep=""))))
    eval(parse(text=(paste("pu", i, "[is.na(pu", i, ")]<-0", sep=""))))
    if (i!=file.number) {
        command1<-paste(command1,"pu", i, ",", sep="")
        command2<-paste(command2,"pu",i,"[]",",",sep="")
        command3<-paste(command3,"Var",i, ",", sep="")
        } else {
            command1<-paste(command1,"pu", i, sep="")
            command2<-paste(command2,"pu",i,"[]",sep="")
            command3<-paste(command3,"Var",i, sep="")
            }
    }

# COMBINE PLANNING UNIT FILE AND REFERENCE FILE
ref_attribute<-as.data.frame(levels(ref))
ref.number<-file.number+1
colnames(ref_attribute)<-c((paste("Var",ref.number,sep="")),(as.character(alias_ref[1,])))
PUR<-ref
command1<-paste(command1, ",ref", sep="")
command2<-paste(command2, ",ref[]", sep="")
command3<-paste(command3, ",Var", as.character(ref.number), sep="")
eval(parse(text=(paste("PUR[]<-as.integer(interaction(", command2, "))", sep=""))))
PUR<-ratify (PUR, filename='PUR.grd', count=TRUE, overwrite=TRUE)
eval(parse(text=(paste("PUR_stack<-stack(", command1, ")", sep=""))))
PUR_db<-crosstab(PUR_stack)
eval(parse(text=(paste( "PUR_db<-transform(PUR_db, unique_id=as.integer(interaction(", command3, ", drop=TRUE)))", sep=""))))
PUR_db<-transform(PUR_db, unique_id=as.integer(interaction(Var1, Var2, Var3, Var4, Var5, drop=TRUE)))
PUR_db<-PUR_db[ which(PUR_db$Freq > 0),]

#plot(PUR)

# PREPARE ATTRIBUTE DATA TO BE MERGE
central_attr<-ref_attribute
colnames(central_attr)[1] = "c1"
colnames(central_attr)[2] = "c2"
add1<-c(0)
add2<-c("none")
add<-data.frame(add1,add2)

# CREATE CENTRAL ATTRIBUTE
for (j in 1:file.number) {
    eval(parse(text=(paste("temp<-pu",j,"_attribute", sep=""))))
    colnames(temp)[1] = "c1"
    colnames(temp)[2] = "c2"
    central_attr<-rbind(central_attr,temp)
    }
add1<-c(0)
add2<-c("none")
add<-data.frame(add1,add2)
colnames(add)[1]= "c1"
colnames(add)[2]= "c2"
central_attr<-rbind(central_attr,add)
c_nrow<-nrow(central_attr)
seq_id<-sequence(c_nrow)
seq<-as.data.frame(seq_id)
central_attr<-cbind(central_attr,seq)
central_attr[1]<-NULL

# EDIT PU ATTRIBUTE BY ADDING NEW ROW FOR 0 VALUES
for (k in 1:file.number) {
    eval(parse(text=(paste('colnames(add)[1]= "Var',k,'"', sep=""))))
    eval(parse(text=(paste('colnames(add)[2]="', as.character(alias[k,]), '"',sep=""))))
    eval(parse(text=(paste("pu", k,"_attribute<-rbind(pu", k, "_attribute,add)", sep=""))))
    }

for (q in 1:file.number) {
    x2<-as.character(alias[q,])
    x3<-paste("seq", q, sep="")
    x4<-paste("pu", q, "_attribute", sep="")
    colnames(central_attr)[1]<-x2
    colnames(central_attr)[2]<-x3
    eval(parse(text=(paste("pu", q, "_attribute<-merge(pu", q, "_attribute,central_attr, by=x2)", sep=""))))
    }

# MERGING ATTRIBUTE DATA TO PUR_DATABASE
PUR_dbmod<-PUR_db
for (l in 1:file.number) {
    eval(parse(text=(paste("PUR_dbmod<-merge(PUR_dbmod, pu", l, "_attribute, by='Var", l, "')", sep=""))))
    }

colnames(add)[1]= paste("Var", as.character(ref.number), sep="")
colnames(add)[2]= as.character(alias_ref[1,])
ref_attribute<-rbind(ref_attribute,add)
PUR_dbmod<-merge(PUR_dbmod, ref_attribute, by=(paste("Var", ref.number, sep="")))

# PREPARE RECLASSIFICATION FILE OF PLANNING UNIT AND MERGE RECLASSIFICATION FILE INTO PUR DATABASE
add3<- c("none")
add4<- c("none")
add_11<- data.frame(add3,add4)

rec_id1<-c("Conservation", "Production", "Other", "none")
rec_id2<-c(1,2,3,100)
rec_idref<-data.frame(rec_id1,rec_id2)



for (m in 1:file.number) {
    eval(parse(text=(paste("att<-as.character(alias[", m, ",])", sep=""))))
    eval(parse(text=(paste("att2<-as.character(LUT[", m, ",])", sep=""))))
    look_pu<- read.table(att2, header=TRUE, sep=",")
    colnames(look_pu)[1] = att
    colnames(look_pu)[2] = paste(att,"_rec", sep="")
    colnames(add_11)[1] = att
    colnames(add_11)[2] = paste(att,"_rec", sep="")
    colnames(rec_idref)[1]= paste(att,"_rec", sep="")
    colnames(rec_idref)[2]= paste("rec_id", m, sep="")
    look_pu<-rbind(look_pu,add_11)
    x1<-paste(att,"_rec", sep="")
    look_pu<-merge(look_pu, rec_idref, by=x1)
    eval(parse(text=(paste("look_pu", m, "<-look_pu", sep=""))))
    PUR_dbmod<-merge(PUR_dbmod, look_pu, by=att)
    }
att3<-as.character(LUT_ref[1,])
att4<-as.character(alias_ref[1,])
look_ref<- read.table(att3, header=TRUE, sep=",")
colnames(look_ref)[1] = att4
colnames(look_ref)[2] = "REF"
colnames(add_11)[1] = att4
colnames(add_11)[2] = "REF"
colnames(rec_idref)[1] = "REF"
colnames(rec_idref)[2] = "REF_id"
look_ref<-rbind(look_ref,add_11)
look_ref<-merge(look_ref,rec_idref, by='REF')
PUR_dbmod<-merge(PUR_dbmod, look_ref, by=att4)


# CONDUCT RECONCILIATION PHASE 1
for (n in 1:file.number) {
    eval(parse(text=(paste("PUR_dbmod<-within(PUR_dbmod,{cek", n, "<-as.numeric(rec_id", n, "==REF_id)})", sep=""))))
    }
command4<-paste()
for (p in 1:file.number) {
    if (p!=file.number) {
        eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '+', '")', sep=""))))
                            } else {
                                eval(parse(text=(paste("command4<-paste(command4,", '"cek', p, '")',                    sep=""))))
                                    }
                            }
PUR_dbmod<-within(PUR_dbmod, {reconcile1<-eval(parse(text=(command4)))})
PUR_dbmod<-within(PUR_dbmod, {reconcile_attr<-ifelse(reconcile1==0,as.character(Tata_ruang), "next")})

command5<-paste()
for (r in 1:file.number) {
if (r!=file.number) {
    eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , '"seq",' ,r, ', ")+", sep="")', sep="" ))))
    } 
    else {
        eval(parse(text=(paste("command5<-paste(command5, ", '"(cek",', r,',"*",' , '"seq",' ,r, ', ")", sep="")', sep="" ))))
        }
    }
    PUR_dbmod<-within(PUR_dbmod, {reconcile_attr2<-ifelse(reconcile1==1, reconcile_attr2<-eval(parse(text=(command5))),100)
    })

central_attrmod<-central_attr
colnames(central_attrmod)[1]="Rec_phase1"
colnames(central_attrmod)[2]="reconcile_attr2"
add5<- c("none")
add6<- c(100)
add_22<- data.frame(add5,add6)
colnames(add_22)[1]="Rec_phase1"
colnames(add_22)[2]="reconcile_attr2"
central_attrmod<-rbind(central_attrmod, add_22)

PUR_dbfinal<-merge(PUR_dbmod,central_attrmod, by='reconcile_attr2')

PUR_dbfinal<-within(PUR_dbfinal, {
Rec_phase1<-ifelse(Rec_phase1=="none", as.character(reconcile_attr), as.character(Rec_phase1))})
PUR_dbfinal2<-PUR_dbfinal[,c('unique_id','Rec_phase1')]
colnames(PUR_dbfinal2)[1]= "ID"
levels(PUR)<-merge((levels(PUR)),PUR_dbfinal2,by="ID")
PUR_rec1 <- deratify(PUR,'Rec_phase1')
PUR_rec2<-ratify(PUR_rec1, filename='PUR_rec1.grd',count=TRUE,overwrite=TRUE)
levels(PUR_rec1)<-merge((levels(PUR_rec1)),levels(PUR_rec2),by="ID")
PUR_rec3<-stack(PUR, PUR_rec1)

area_rec1<-as.data.frame(levels(PUR_rec1))
write.dbf(PUR_dbfinal, "PUR_reconciliation_table")


#WRITE REPORT
report<-paste("Land Use Planning for Multiple Environmental Services
========================================================
***

# Lembar hasil analisis PUR:
# Rekonsiliasi menggunakan kelas acuan

***

***
# Peta `r location` unit perencanaan sebelum rekonsiliasi
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(PUR_stack)
```

***


# Peta hasil rekonsiliasi fase 1
```{r fig.width=10, fig.height=9, echo=FALSE}
plot(PUR, main='Overlapping Map')
plot(PUR_rec1, main='Reconciliation Map')
```

***

# Luasan unit perencanaan fase 1
```{r fig.width=10, fig.height=9, echo=FALSE}
barplot(area_rec1$COUNT, names.arg=area_rec1$Rec_phase1,col=c(2,3,4), ylab='Luas (ha)', xlab='Unit Perencanaan fase 1')
```

***")

write(report,file="reporthtml.Rmd")

knit2html("reporthtml.Rmd", options=c("use_xhml"))
