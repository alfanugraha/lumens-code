#=================================DATA_DATA_DATA==========================================
#di jambi terdapat sekitar 34 landcover class, sedangkan merangin ada 21 landcover class (included NoData)
#berdasarkan data yang digunakan untuk artikel ETFRN: merangin study case
#
#persiapan data
#1. landcover map merangin di masking dari landcover map jambi sesuai planning unit merangin dengan jumlah 
#   10 planning unit --> mer1990
#2. output masking menghasilkan 29 class, kemudian di reclass menjadi 21 untuk menghilangkan landcover class
#   swamp dan peat (tidak ada swamp dan di peat di sana) --> mer90_rec1
#3. output reclass kemudian disederhanakan kembali menjadi 9 class --> mer90_rec2
#4. untuk data contoh, output disederhakan dengan mereclass waterbody - NoData --> mer90_rec3
#5. planning unit map ditransform dari polygon ke raster --> zone
#6. kemudian outputnya di reclass (zona transmigrasi dan sungai) menjadi 8 class --> zone_rec
#7. convert seluruh landcover dan planning unit menjadi tif dan ascii
#
#luas init dan final raster beda dengan zone karena planning unit sungai dijadiin no data

library(parallel)
library(raster)
library(reshape2)
library(doSNOW)
library(foreach)
library(iterators)
library(snow)
library(foreign)
library(neuralnet)
library(nnet)
library(RSNNS)
library(monmlp)
library(rgdal)

#setwd("C:/Users/ANugraha/Documents/Merangin/LUC/workdir/")
setwd("~/Desktop/spinboxdelegate/")

#set parameter parallel processing (if used)
numOfCores<-detectCores() 
cl <- makeCluster(numOfCores)
registerDoSNOW(cl)

#====================================================STEP_ONE============================================================
#==============_1_DataPreparation=======================
Inital_Land_Use <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/tif/mer2005.tif"
Final_Land_Use <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/tif/mer2010.tif"
Land_Use_LUT <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/land_use2.csv"

Planning_Unit <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/tif/zone.tif"
Planning_Unit_LUT <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/zone.csv"

initLandUse <- raster(Inital_Land_Use)
finalLandUse <- raster(Final_Land_Use)
zone <- raster(Planning_Unit)
landUseLUT <- read.table(Land_Use_LUT, header=TRUE, sep=",")
zoneLUT <- read.table(Planning_Unit_LUT, header=TRUE, sep=",")

#==============_2_CreateCategoricalRaster================
initLandUse <- ratify(initLandUse, filename='landuse1.grd', count=FALSE, overwrite=TRUE)
finalLandUse <- ratify(finalLandUse, filename='landuse2.grd', count=FALSE, overwrite=TRUE)
zone <- ratify(zone, filename='zone.grd', count=TRUE, overwrite=TRUE)

#==============_3_ReadRATnReclassifyMap==================
landUseID <- data.frame(levels(initLandUse))
numOfClass <- length(landUseID$ID)
landUseID$ID2 <- 1:numOfClass
landUseID2 <- data.frame(landUseID$ID2)
landUseID2$originalID <- landUseID$ID
newClassMatrix <- data.matrix(landUseID) ## LUT to reclassify land use map to be processed    
oldClassMatrix <- data.matrix(landUseID2) ## LUT to reclassify back processing result to original ID

initLandUse <- reclassify(initLandUse, newClassMatrix)
initLandUse <- ratify(initLandUse, filename='landuse1.grd', count=TRUE, overwrite=TRUE)

finalLandUse <- reclassify(finalLandUse, newClassMatrix)
finalLandUse <- ratify(finalLandUse, filename='landuse2.grd', count=TRUE, overwrite=TRUE)

levels(initLandUse) <- merge(levels(initLandUse), landUseLUT, by="ID")
levels(finalLandUse) <- merge(levels(finalLandUse), landUseLUT, by="ID")
levels(zone) <- merge(levels(zone), zoneLUT, by="ID")

areaInitLandUse <- as.data.frame(levels(initLandUse))
areaFinalLandUse <- as.data.frame(levels(finalLandUse))
areaZone <- as.data.frame(levels(zone))
area <- sum(areaInitLandUse$COUNT) 

#==============_4_AddnMergeAttributeByIDFromLUT==========
colnames(areaInitLandUse)[2] = "COUNT_LC1"
colnames(areaInitLandUse)[3] = "LC_t1"
colnames(areaFinalLandUse)[2] = "COUNT_LC2"
colnames(areaFinalLandUse)[3] = "LC_t2"

#==============__4_1_withoutPU_optional===========================
crossTemp <- crosstab(initLandUse,finalLandUse)
crossTemp <- subset(crossTemp, Var1!="<NA>")
crossTemp <- subset(crossTemp, Var2!="<NA>")

crossTemp$Var1Numeric <- as.numeric(crossTemp$Var1)
crossTemp$Var2Numeric <- as.numeric(crossTemp$Var2)
crossTemp$ID <- as.factor(crossTemp$Var1Numeric + (crossTemp$Var2Numeric*100))

colnames(crossTemp)[1] = "ID_LC1"
colnames(crossTemp)[2] = "ID_LC2"
colnames(crossTemp)[3] = "COUNT"
colnames(landUseLUT)[1] = "ID_LC1"
colnames(landUseLUT)[2] = "LC_t1"

LUCmatrixNonPU <- merge(crossTemp, landUseLUT, by="ID_LC1")

colnames(landUseLUT)[1] = "ID_LC2"
colnames(landUseLUT)[2] = "LC_t2"

LUCmatrixNonPU <- merge(LUCmatrixNonPU, landUseLUT, by="ID_LC2")

LUCMapNonPU <- overlay(initLandUse, finalLandUse, fun=function(x,y){ return(x + ( y * 100 )) })
LUCMapNonPU <- ratify(LUCMapNonPU, filename='luchg.grd', count=TRUE, overwrite=TRUE)
levels(LUCMapNonPU) <- merge((levels(LUCMapNonPU)), LUCmatrixNonPU, by="ID", all=TRUE) #don't use parameter 'all=T' to exclude <NA> changes 
LUCAttributeNonPU <- as.data.frame(levels(LUCMapNonPU))
LUCAttributeNonPU$COUNT.x <- NULL #same with LUCmatrixNonPU

#==============__4_2_withPU===============================
cross <- as.data.frame(crosstab((stack(initLandUse,finalLandUse, zone)))) #new class for NA
cross <- subset(cross, Var1!="<NA>")
cross <- subset(cross, Var2!="<NA>")
cross <- subset(cross, Var3!="<NA>") #missing count value in this step
#areaZone = 737325 + areaInitLandUse = 741536 = 1478861
#totalArea = 1505504
#harus di cross check ulang, terutama pada id yang dikelaskan menjadi NA 

cross$Var1Numeric <- as.numeric(cross$Var1)
cross$Var2Numeric <- as.numeric(cross$Var2)
cross$ID <- as.factor(cross$Var1Numeric + (cross$Var2Numeric*100))

colnames(cross)[1] = "ID_LC1"
colnames(cross)[2] = "ID_LC2"
colnames(cross)[3] = "ZONE"
colnames(cross)[4] = "COUNT"
colnames(landUseLUT)[1] = "ID_LC1"
colnames(landUseLUT)[2] = "LC_t1"

LUCmatrix <- merge(cross, landUseLUT, by="ID_LC1")

colnames(landUseLUT)[1] = "ID_LC2"
colnames(landUseLUT)[2] = "LC_t2"

LUCmatrix <- merge(LUCmatrix, landUseLUT, by="ID_LC2")

colnames(zoneLUT)[1] = "ZONE"
colnames(zoneLUT)[2] = "Z_NAME"

LUCmatrix <- merge(LUCmatrix, zoneLUT, by="ZONE")

#==============_5_CreateLUCDBnMap========================= 
LUCmatrix$LU_CHG <- do.call(paste, c(LUCmatrix[c("LC_t1", "LC_t2")], sep = " to "))
LUMapOverlay <- overlay(initLandUse, finalLandUse, fun=function(x,y){ return(x + ( y * 100 )) })
LUMapOverlay <- ratify(LUMapOverlay, filename='luchg.grd', count=TRUE, overwrite=TRUE)

LUCMap <- LUMapOverlay
levels(LUCMap) <- merge((levels(LUCMap)), LUCmatrix, by="ID")
LUCAttribute <- as.data.frame(levels(LUCMap))

write.dbf(LUCAttribute, "LandUseChanges_Database.dbf")
writeRaster(LUCMap, filename="LandUseChanges_Map.tif", format="GTiff", overwrite=TRUE)

#==============_6_CreateLUCDBnTMnMapPerZone================
numOfZone <- length(zoneLUT$ZONE)
foreach(i=1:numOfZone) %do% {
  #create transition matrix LUT per zone
  #eval(parse(text=(paste("LUCmatrix", i, "<- LUCmatrix[ which(LUCmatrix$ZONE == ", i, " ),]", sep=""))))
  eval(parse(text=(paste("LUCmatrixPU", i, " <- subset(LUCmatrix, ZONE==", i, ")", sep=""))))                                                           #LUCmatrixPU1 <- subset(LUCmatrix, ZONE==1)
  eval(parse(text=(paste("LUCmatrixPUmelt", i, " <- melt(data = LUCmatrixPU", i, ", id.vars=c('LC_t1','LC_t2'), measure.vars=c('COUNT'))", sep=""))))   #LUCmatrixPUmelt1 <- melt(data=LUCmatrixPU1, id.vars=('LC_t1','LC_t2'), measure.vars=c('COUNT'))
  eval(parse(text=(paste("LUCmatrixPUmeltcast", i, " <- dcast(data = LUCmatrixPUmelt", i, ", formula = LC_t1 ~ LC_t2, fun.aggregate = sum)", sep="")))) #LUCmatrixPUmeltcast1 <- dcast(data = LUCmatrixPUmelt1, formula = LC_t1 ~ LC_t2, fun.aggregate = sum)
  eval(parse(text=(paste("write.dbf(LUCmatrixPUmeltcast", i, ",", "'TransitionMatrix_Zone", i, ".dbf')", sep=""))))                                     #write.dbf(LUCmatrixPUmeltcast1, 'TransitionMatrix_Zone1.dbf')
  
  #create changes map per zone
  eval(parse(text=(paste("zone", i, " <- zone==", i, sep=""))))                                                                             #zone1 <- zone==1
  eval(parse(text=(paste("zone", i, " <- ratify(zone", i, ", 'PU", i, ".grd', count=TRUE, overwrite=TRUE)",  sep=""))))                     #zone1 <- ratify(zone1, 'PU1.grd', count=TRUE, overwrite=TRUE)
  eval(parse(text=(paste("LUCMapPU", i, " <- LUMapOverlay*zone", i, sep=""))))                                                              #LUCMapPU1 <- LUMapOverlay*zone
  eval(parse(text=(paste("writeRaster(LUCMapPU", i, ", 'LandUseChanges_Map_Zone", i, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))   #writeRaster(LUCMapPU1, 'LandUseChanges_Map_Zone1.tif', format='GTiff', overwrite=TRUE)
  eval(parse(text=(paste("LUCMapPU", i, " <- ratify(LUCMapPU", i, ", 'LUCperPU", i, ".grd', count=TRUE, overwrite=TRUE)",  sep=""))))       #LUCMapPU1 <- ratify(LUCMapPU1, 'LUCperPU1.grd', count=TRUE, overwrite=TRUE)
  
  #create initial map per zone
  eval(parse(text=(paste("initMapPU", i, " <- initLandUse*zone", i, sep=""))))                                                            #initMapPU1 <- initLandUse*zone1
  eval(parse(text=(paste("writeRaster(initMapPU", i, ", 'Initial_Map_Zone", i, ".tif', format='GTiff', overwrite=TRUE)", sep=""))))       #writeRaster(initMapPU1, 'Initial_Map_Zone1.tif', format='GTiff', overwrite=TRUE)
  eval(parse(text=(paste("initMapPU", i, " <- ratify(initMapPU", i, ", 'initPerPU", i, ".grd', count=TRUE, overwrite=TRUE)",  sep=""))))  #initMapPU1 <- ratify(initMapPU1, 'initPerPU1.grd', count=TRUE, overwrite=TRUE)
}



#====================================================STEP_TWO==============================================================
#==============_1_LoadFactors============================
factorDir <- "/media/System/Users/ANugraha/Documents/Merangin/LUC/tif/factors/"
factors <- list.files(factorDir, full.names=TRUE, pattern="\\.tif$")
numOfFactors <- length(factors)
foreach(i=1:numOfFactors) %do% {
  assign(paste("mapFactor", i, sep=""),(factors[c(i)]))                                                                               #mapFactor[i] <- factors[c(i)]
  eval(parse(text=(paste("mapFactor", i, " <- raster(mapFactor", i, ")", sep=""))))                                                   #mapFactor1 <- raster(mapFactor1)
  eval(parse(text=(paste("mapFactor", i, " <- ratify(mapFactor", i, ", 'factor", i, ".grd', count=TRUE, overwrite=TRUE)", sep=""))))  #mapFactor1 <- ratify(mapFactor1, 'factor1.grd', count=TRUE, overwrite=TRUE)
}

#==============_2_CreateDummyVariablesForLandUseClass============================
#for example:
#  number of class = 3
#  number of dummy variables = n-1 = 3-1 = 2
#               V1   V2
#    class[0]   1   0
#    class[1]   0   1
#    class[2]   0   0
numOfDummy <- as.integer(numOfClass - 2)
dummyVars <- landUseLUT  
colnames(dummyVars)[1] = "ID"
colnames(dummyVars)[2] = "CLASS"
for(i in 0:numOfDummy){
  eval(parse(text=(paste("dummyVars$STATE", i, " <- 0", sep=""))))            #dummyVars$STATE0 <- 0
  eval(parse(text=(paste("dummyVars$STATE", i, "[", i+1, "] <- 1", sep="")))) #dummyVars$STATE0[0+1] <- 1
}

#==============_3_CreateSamples=================================================
planningUnitTh = 1  #execute 1st planning_unit 
samples = 10000
sampleCount = 1
#sampleMode = c("ALL", "RANDOM", "STRATIFIED") 
eval(parse(text=(paste("numOfRow <- nrow(initMapPU", planningUnitTh, ")", sep=""))))      #numOfRow <- nrow(initMapPU1)
eval(parse(text=(paste("numOfCol <- ncol(initMapPU", planningUnitTh, ")", sep=""))))      #numOfCol <- ncol(initMapPU1)

#function to generate sample if ALL mode selected
samples <- length(LUCMapPU1[values(LUCMapPU1)!="NA"]) #I think its unused 

#faster than get id and generate sample simultaneously
rasterDetail<-NULL
for(r in 1:numOfRow){
  for(c in 1:numOfCol){
    #id<-initMapPU1[r,c]
    
    if (is.na(id)) {
      next
    } else if (id==0) {
      next
    } else {
      h<-data.frame(r,c,id)
    }
    
    if(is.null(rasterDetail)){ 
      rasterDetail <- h
    } else {
      rasterDetail <- rbind(rasterDetail, h)
    }
  }
}
#end

#function to generate sample if RANDOM mode selected
#   user   system  elapsed 
#   6730.117  223.782 6967.583 
#for(i in sampleCount:samples) {
system.time( 
  while(sampleCount<=samples) { 
    ROW <- as.integer(runif(1, min=1, max=numOfRow))
    COL <- as.integer(runif(1, min=1, max=numOfCol))
    eval(parse(text=(paste("x <- xFromCol(initMapPU", planningUnitTh, ", COL)", sep=""))))  #x <- xFromCol(initMapPU1, COL) ===> coordinate-x
    eval(parse(text=(paste("y <- yFromRow(initMapPU", planningUnitTh, ", ROW)", sep=""))))  #y <- yFromRow(initMapPU1, ROW) ===> coordinate-y
      
    #get CLASS ID from coords
    eval(parse(text=(paste("ID <- as.integer(getValues(initMapPU", planningUnitTh, ", ROW, numOfRow)[COL])", sep=""))))  #ID <- as.integer(getValues(initMapPU1, ROW, numOfRow)[COL])
      
    #remove ID="NA" (1)
    if (is.na(ID)) {
      next
    } else if(ID==0){
      next
    } else {
      val <- data.frame(x, y, ROW, COL, ID)
    }
      
    #get FACTOR ID from coords
    for(j in 1:numOfFactors) {
      eval(parse(text=(paste("FACTOR", j, " <- getValues(mapFactor", j, ", ROW, numOfRow)[COL]", sep=""))))  #FACTOR1 <- getValues(mapFactor1, ROW, numOfRow)[COL]
      eval(parse(text=(paste("val <- cbind(val, FACTOR", j, ")", sep=""))))                                  #val <- cbind(val, FACTOR1)
    }
      
    #get CHANGES ID from coords
    eval(parse(text=(paste("OUTPUT <- getValues(LUCMapPU", planningUnitTh, ", ROW, numOfRow)[COL]", sep="")))) #OUTPUT <- getValues(LUCMapPU1, ROW, numOfRow)[COL]
      
    val <- cbind(val, OUTPUT)
    if(sampleCount != 1){
      sampleLUT <- rbind(sampleLUT, val)
    } else {
      sampleLUT <- val
    }
    
    sampleCount=sampleCount+1
  }
)
#end

#function to generate sample if STRATIFIED mode selected
samples = round(samples / numOfClass)
#select row and col where ID-i == category-i
for(i in 1:numOfClass){
  index <- subset(rasterDetail, rasterDetail$id==i)
  row.names(index) <- NULL
  
  counter=1
  while(counter<=samples){
    ROW <- index[counter,]$r
    COL <- index[counter,]$c
    ID <- index[counter,]$id
    eval(parse(text=(paste("x <- xFromCol(initMapPU", planningUnitTh, ", COL)", sep=""))))  #x <- xFromCol(initMapPU1, COL) ===> coordinate-x
    eval(parse(text=(paste("y <- yFromRow(initMapPU", planningUnitTh, ", ROW)", sep=""))))  #y <- yFromRow(initMapPU1, ROW) ===> coordinate-y
    
    val <- data.frame(x, y, ROW, COL, ID)
  
    #get FACTOR ID from coords
    for(j in 1:numOfFactors) {
      eval(parse(text=(paste("FACTOR", j, " <- getValues(mapFactor", j, ", ROW, numOfRow)[COL]", sep=""))))  #FACTOR1 <- getValues(mapFactor1, ROW, numOfRow)[COL]
      eval(parse(text=(paste("val <- cbind(val, FACTOR", j, ")", sep=""))))                                  #val <- cbind(val, FACTOR1)
    }
    
    #get CHANGES ID from coords
    eval(parse(text=(paste("OUTPUT <- getValues(LUCMapPU", planningUnitTh, ", ROW, numOfRow)[COL]", sep="")))) #OUTPUT <- getValues(LUCMapPU1, ROW, numOfRow)[COL]
    
    val <- cbind(val, OUTPUT)
    if(sampleCount != 1){
      sampleLUT <- rbind(sampleLUT, val)
    } else {
      sampleLUT <- val
    }
    
    counter=counter+1
  }
}

#==============_4_ReadSamples=================================================
sampleShp <- "sample_shp_grass.shp"
sample <- readOGR(sampleShp, layer="sample_shp_grass")

#==============_4_NormalizationFactorsSample====================================
#(optional) select unique LU_CHG and merge with sample
changesLUT <- LUCmatrix[which(LUCmatrix$ZONE==1),]
changesLUT <- subset(changesLUT, select=c(ID, LU_CHG))
colnames(changesLUT)[1] <- "OUTPUT"
colnames(changesLUT)[2] <- "LU_CHG"
changesLUT$OUTPUT_RECLASS <- c(1:81)
row.names(changesLUT) <- NULL
numOfChanges <- length(changesLUT$OUTPUT_RECLASS)

sampleLUT <- merge(sampleLUT, changesLUT, by="OUTPUT") #remove OUTPUT="NA" (2)
sampleLUT <- merge(sampleLUT, dummyVars, by="ID") #remove ID=0 or not in a category list from dummy vars

#RSNNS::normalizeData()
normalize <- function(x){ 
  a <- min(x) 
  b <- max(x) 
  (x - a)/(b - a) #[0,1]
}

for(j in 1:numOfFactors){
  eval(parse(text=(paste("sampleLUT$FACTOR", j, " <- normalize(sampleLUT$FACTOR", j, ")", sep="")))) #sampleLUT$FACTOR1 <- normalize(sampleLUT$FACTOR1)
}

#==============_5_SampleVisualization====================
coords <- data.frame(sampleLUT$x, sampleLUT$y)
sampleData <- subset(sampleLUT, select=-c(x, y, ROW, COL, OUTPUT_RECLASS))
eval(parse(text=(paste("proj <- projection(LUCMapPU", planningUnitTh, ")", sep="")))) #proj <- projection(LUCMapPU1)

spatialSample<-SpatialPointsDataFrame(coords, sampleData)
crs(spatialSample)<-proj

writeOGR(spatialSample, getwd(), "samples_10rb", driver = "ESRI Shapefile")

plot(LUCMapPU1)
plot(spatialSample, add=T)



#====================================================STEP_THREE============================================================
#==============_1_CreateFormula====================================
f <- "OUTPUT_RECLASS~"
for(i in 0:numOfDummy){
  f<-paste(f, "STATE", i, "+", sep="")         
}
f<-paste(f, "FACTOR1", sep="") #minimal one FACTOR
for(i in 2:numOfFactors){
  f<-paste(f, "+FACTOR", i, sep="")         
}
formulaInput <- as.formula(f)

#==============_2_TrainNeuralNetwork================================
#1. OUTPUT HARUS DIRECLASS
#2. SAMPLE DIBAGI RATA SESUAI BANYAKNYA TRANSISI PERUBAHAN DI SETIAP PLANNING UNIT
hiddenLayer = 14
iteration = 1000

validationSampleCount = samples * 20/100 #20% data testing
trainingSampleCount = samples -validationSampleCount #80% data training 

#random training & validation sample
randomValidationSample <- as.integer(runif(validationSampleCount, min=1, max=samples))
validationSample <- sampleLUT[c(randomValidationSample),] #opt2: validationSample<-trainingSampleIO[8001:10000,]
trainingSample <- sampleLUT[-c(randomValidationSample),] #opt2: trainingSample<-trainingSampleIO[1:8000,]

row.names(validationSample) <- NULL
row.names(trainingSample) <- NULL

trainingSampleIO <- subset(trainingSample, select=-c(ID, x, y, ROW, COL, LU_CHG, CLASS, OUTPUT))
trainingSampleI <- subset(trainingSample, select=-c(ID, x, y, ROW, COL, LU_CHG, CLASS, OUTPUT, OUTPUT_RECLASS))
trainingSampleO <- subset(trainingSample, select=c(OUTPUT_RECLASS))
validationSampleI <- subset(validationSample, select=-c(ID, x, y, ROW, COL, LU_CHG, CLASS, OUTPUT, OUTPUT_RECLASS))

#neuralnet packages
# networkSimulation1$covariate == input 
# networkSimulation1$response == output
# networkSimulation1$data == output+input
# networkSimulation1$net.result == output of the neural network

networkSimulation1 <- neuralnet(formulaInput, trainingSampleIO, hidden=hiddenLayer, stepmax=iteration, threshold=0.01, learningrate=0.1, algorithm='backprop', err.fct='sse', linear.output=FALSE) #error if linear.output set to TRUE
#kasus ini semua hasil network 1 karena output linier

networkSimulation1 <- neuralnet(formulaInput, trainingSampleIO, hidden=hiddenLayer, stepmax=iteration, threshold=0.01, learningrate=0.1, err.fct='sse', linear.output=TRUE) 
#kasus ini semua output terkelaskan jauh dari kelas sebelumnya, karena hidden disesuaikan hanya dengan jumlah factor bukan state

networkSimulation1 <- neuralnet(formulaInput, trainingSampleIO, hidden=14, stepmax=iteration, threshold=0.01, learningrate=0.1, err.fct='sse', linear.output=TRUE) 
#sama

networkSimulation1 <- neuralnet(formulaInput, trainingSampleIO, hidden=6, stepmax=iteration, linear.output=TRUE) #error if linear.output set to TRUE
#sama

networkResult <- compute(networkSimulation1, validationSampleI) #for computation of a given neural network for given covariate vectors
predictionResult <- prediction(networkSimulation1) #for a summary of the output of the neural network
plot(networkSimulation1)
plot.nnet(networkSimulation1)

#nnet packages
networkSimulation2<- nnet(formulaInput, data=trainingSampleIO, size=hiddenLayer, linout=TRUE, maxit = iteration)
networkResult <- predict(networkSimulation2, validationSampleI) #from stats package = linear result
plot.nnet(networkSimulation2)

#RSNNS packages
networkSimulation3<- mlp(trainingSampleI, trainingSampleO, size=hiddenLayer, maxit=iteration, learnFunc="Std_Backpropagation", learnFuncParams=c(0.1))
networkResult <- predict(networkSimulation3, validationSampleI) #linear
plot.nnet(networkSimulation3)

#monmlp packages
matrixTrainingSampleI <- as.matrix(trainingSampleI)
matrixTrainingSampleO <- as.matrix(trainingSampleO)
matrixValidationSampleI <- as.matrix(validationSampleI)
networkSimulation4 <- monmlp.fit(x = matrixTrainingSampleI, y = matrixTrainingSampleO, hidden1 = hiddenLayer, iter.max=iteration, iter.stopped=10, bag=TRUE)
out <- capture.output(networkSimulation4)
write.table(out, "net.txt")
mlpPredict <- monmlp.predict(matrixValidationSampleI,networkSimulation4)

#==============_2_Prediction================================
predictedMap <- raster(ncol=numOfCol, nrow=numOfRow)
crs(predictedMap) <- crs(initLandUse)
extent(predictedMap) <- extent(initLandUse)
res(predictedMap) <- res(initLandUse)
predictedMap[] <- NA

confidenceMap <- raster(ncol=numOfCol, nrow=numOfRow)
crs(confidenceMap) <- crs(initLandUse)
extent(confidenceMap) <- extent(initLandUse)
res(confidenceMap) <- res(initLandUse)
confidenceMap[] <- NA

for(i in 1:numOfChanges){
  eval(parse(text=(paste("transitionPotentials", i, " <- raster(ncol=numOfCol, nrow=numOfRow)", sep=""))))
  eval(parse(text=(paste("crs(transitionPotentials", i, ") <- crs(initLandUse)", sep=""))))
  eval(parse(text=(paste("extent(transitionPotentials", i, ") <- extent(initLandUse)", sep=""))))
  eval(parse(text=(paste("res(transitionPotentials", i, ") <- res(initLandUse)", sep=""))))
  eval(parse(text=(paste("transitionPotentials", i, "[] <- NA", sep=""))))
}

#    user   system  elapsed 
#8146.637   23.533 8189.904 

#Parallel Timing stopped at: 944.915   58.516 4704.350 
foreach(i=1:numOfRow) %do% {
  foreach(j=1:numOfCol, .packages=c('raster','monmlp')) %dopar% {
    input <- subset(sampleLUT, ROW==i & COL==j) #cari baris dan kolom yang sudah dikasih sample dan ditrain
    if(nrow(input) != 0){
      input <- as.matrix(subset(input, select=-c(ID, x, y, ROW, COL, LU_CHG, CLASS, OUTPUT, OUTPUT_RECLASS)))
      predictResult <- round(monmlp.predict(input, networkSimulation4))
      predictResult <- round(predictResult)
      
      predictedMap[i, j] <- predictResult
    }
      
  }
}
