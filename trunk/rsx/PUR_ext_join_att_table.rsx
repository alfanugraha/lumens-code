##[PUR]=group
##wd=folder
##input_poly=vector
##input_table=table
##merged_shp=output vector

library(sp)
library(rgdal)
library(foreign)

setwd (wd)

# PREPARE REFERENCE DATA
#input_poly<- substr(basename(input_poly), 1, nchar(basename(input_poly)) - 4)
#sa<-readOGR(dsn=wd,input_poly)
sa<-input_poly

#read and modify PUR table
input_table <- read.dbf(input_table)
shp.tab.input<-as.data.frame(input_table)
colnames(shp.tab.input)<-c("DN","reconciled","area")
merged_shp<-merge(sa, shp.tab.input, by="DN")


#write joined shapefile
writeOGR(merged_shp, wd, "PUR_reconciled_planning_unit",driver="ESRI Shapefile", overwrite=TRUE)
