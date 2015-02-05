library(parallel)
library(raster)
library(reshape2)
library(doSNOW)
library(foreach)
library(iterators)
library(snow)

#set working directory
workDir <- "C:/Users/ANugraha/Documents/Merangin/SCIENDO/"
setwd(workDir)

dir.create("tmp") #temporary folder
secondWorkDir <- paste(workDir,"tmp/", sep="")
setwd(secondWorkDir)

dir.create ("res") #result files
resultWorkDir <- paste(secondWorkDir, "res", sep="")
setwd(workDir)
# rasterOptions (tolerance=0)

#set parameter parallel processing
numOfCores<-detectCores() 
cl <- makeCluster(numOfCores)
registerDoSNOW(cl)

#load datasets
landuse1 <-"C:/Users/ANugraha/Documents/Merangin/ltm_data/lc2005.asc"
landuse2 <-"C:/Users/ANugraha/Documents/Merangin/ltm_data/lc2010.asc"
pu <- "C:/Users/ANugraha/Documents/Merangin/ltm_data/pu.asc"
exclusion <-"C:/Users/ANugraha/Documents/Merangin/ltm_data/studyarea.asc"
workFactor <- "C:/Users/ANugraha/Documents/Merangin/ltm_data/factors/"

initLandUse <- raster(landuse1)
finalLandUse <- raster(landuse2)
zone <- raster(pu)
exclusionData<- raster(exclusion)

iter <- 1000
sample <- 50000
neuron <- 13 #number of ANN hidden layer

#calculate fraction number of sample
totalCell <- ncell(initLandUse)
sample <- round(totalCell/sample)

#prepare extent
#setInitialExtent <- function(left, right, files) {
#  left = readLines(files)
#  foreach(i=1:7){
#    eval(parse(text=(paste(left, "[", i, "]<-", right, i, sep=""))))
#  }
#}
extBatch  = readLines(landuse1)
extBatch1 = extBatch[1] #ncols
extBatch2 = extBatch[2] #nrows
extBatch3 = extBatch[3] #xllcorner
extBatch4 = extBatch[4] #yllcorner
extBatch5 = extBatch[5] #cellsize
extBatch6 = extBatch[6] #NODATA_value
extBatch7 = extBatch[7] #value


#CREATE SEPARATE RASTER FOR EACH VALUE
#create categorical raster
initLandUse <- ratify(initLandUse, filename='landuse1.grd', count=FALSE, overwrite=TRUE)
finalLandUse <- ratify(finalLandUse, filename='landuse2.grd', count=FALSE, overwrite=TRUE)
zone <- ratify(zone, filename='zone.grd', count=FALSE, overwrite=TRUE)

#read raster attribute/class ID and reclassify land use map into new raster with sequential ID
landUseID <- data.frame(levels(initLandUse))
landUseID <- landUseID$ID
numOfClass <- length(landUseID)

reclass <- data.frame(a=levels(initLandUse))
reclass$ID2 <- 1:numOfClass

newClass <- data.frame(reclass$ID2)
newClass$ID_ori <- reclass$ID

newClassMatrix <- data.matrix(reclass) ## LUT to reclassify land use map to be processed    
oldClassMatrix <- data.matrix(newClass) ## LUT to reclassify back processing result to original ID

initLandUse <- reclassify (initLandUse, newClassMatrix)
initLandUse <- ratify (initLandUse, filename='landuse1.grd', count=TRUE, overwrite=TRUE)

finalLandUse <- reclassify (finalLandUse, newClassMatrix)
finalLandUse <- ratify (finalLandUse, filename='landuse2.grd', count=TRUE, overwrite=TRUE)

#create land use change database (taken from Pre-QUES script)
areaInitLandUse <- as.data.frame(levels(initLandUse))
areaFinalLandUse <- as.data.frame(levels(finalLandUse))
areaZone <- as.data.frame(levels(zone))
area <- sum(areaInitLandUse$COUNT)
colnames(areaInitLandUse)[2] = "COUNT_LC1"
colnames(areaFinalLandUse)[2] = "COUNT_LC2"
cross <- as.data.frame(crosstab((stack(initLandUse,finalLandUse))))
cross <- subset(cross, Var1!="<NA>")
cross <- subset(cross, Var2!="<NA>")
colnames(cross)[1] ="ID_LC1"
colnames(cross)[2] = "ID_LC2"
#colnames(cross)[3] = "ZONE"
colnames(cross)[3] = "COUNT"

#calculate number of change pixel from initial and final land use data, IF USING HISTORICAL APPROACH
foreach(i=1:numOfClass) %do%  {
  data.frame(assign(paste("changePixel", i, sep=""),(subset(cross,ID_LC2==i)))) #assign changePixel[1]..changePixel[i] ---> if ID_LC2 == i in cross
  eval(parse(text=(paste("changePixel", i, "$ID_LC1<-NULL", sep="")))) #assign changePixel[i]$ID_LC1<-NULL
  eval(parse(text=(paste("changePixel", i, "$ID_LC2<-NULL", sep="")))) #assign changePixel[i]$ID_LC2<-NULL
  foreach(j=1:numOfClass) %do% {
    eval(parse(text=(paste("changePixel", i, "from", j,"<- changePixel", i, "[", j, ",]", sep="")))) # changePixel[i]from[j]<-changePixeli[j,]
  }
  eval(parse(text=(paste("changePixel", i, "<-changePixel", i, "[-c(", i, "),]",sep="")))) # changePixel[i]<-changePixel[i][-c(i),]
  eval(parse(text=(paste("changePixel", i, "<-sum(changePixel", i, ")", sep="")))) # changePixel[i]<-sum(changePixel[i])
}

#Read factor data from workFactor folder
factors <- list.files(workFactor, full.names=TRUE)
numOfFactors <- length(factors)
foreach(i=1:numOfFactors) %do% {
  assign(paste("mapFactor", i, sep=""),(factors[c(i)])) # mapFactor[i] <- factors[c(i)]
}

#unnecessary
#foreach(i=1:numOfClass) %dopar% {
#  assign(paste("initial", i, sep=""), (initLandUse == i))
#  assign(paste("final", i, sep=""), (finalLandUse == i))
#}

#RUNNING LAND TRANSFORMATION MODEL-TRAINING STAGE
foreach(i=1:numOfClass, .packages='raster') %dopar% {
  setwd(secondWorkDir)
  terWorkDir <- paste(i,"/", sep="")
  dir.create(terWorkDir)
  setwd(paste(secondWorkDir,terWorkDir, sep=""))
  
  writeRaster((initLandUse==i), (paste("initial", i, ".asc", sep="")), format="ascii", NAflag=-9999, overwrite=TRUE) 
  writeRaster(((initLandUse==i)*7), (paste("test_mask", i, ".asc", sep="")), format="ascii", NAflag=-9999, overwrite=TRUE)
  writeRaster((finalLandUse==i), (paste("final", i, ".asc", sep="")), format="ascii", NAflag=-9999, overwrite=TRUE)
  writeRaster(((finalLandUse==i)*7), (paste("forecast_mask", i, ".asc", sep="")), format="ascii", NAflag=-9999, overwrite=TRUE) 
  
  asciiInit <- paste("initial", i, ".asc", sep="")
  asciiInitBatch    = readLines(asciiInit) 
  asciiInitBatch[1] = extBatch1
  asciiInitBatch[2] = extBatch2
  asciiInitBatch[3] = extBatch3
  asciiInitBatch[4] = extBatch4
  asciiInitBatch[5] = extBatch5
  asciiInitBatch[6] = extBatch6
  asciiInitBatch[7] = extBatch7
  writeLines(asciiInitBatch, asciiInit)
  
  asciiFinal <- paste("final", i, ".asc", sep="")
  asciiFinalBatch    = readLines(asciiFinal)
  asciiFinalBatch[1] = extBatch1
  asciiFinalBatch[2] = extBatch2
  asciiFinalBatch[3] = extBatch3
  asciiFinalBatch[4] = extBatch4
  asciiFinalBatch[5] = extBatch5
  asciiFinalBatch[6] = extBatch6
  asciiFinalBatch[7] = extBatch7
  writeLines(asciiFinalBatch, asciiFinal) 
  
  asciiTestMask <- paste("test_mask", i, ".asc", sep="")
  asciiTestMaskBatch    = readLines(asciiTestMask)
  asciiTestMaskBatch[1] = extBatch1
  asciiTestMaskBatch[2] = extBatch2
  asciiTestMaskBatch[3] = extBatch3
  asciiTestMaskBatch[4] = extBatch4
  asciiTestMaskBatch[5] = extBatch5
  asciiTestMaskBatch[6] = extBatch6
  asciiTestMaskBatch[7] = extBatch7
  writeLines(asciiTestMaskBatch, asciiTestMask)
  
  asciiForecastMask <- paste("forecast_mask", i, ".asc", sep="")
  asciiForecastMaskBatch    = readLines(asciiForecastMask) 
  asciiForecastMaskBatch[1] = extBatch1
  asciiForecastMaskBatch[2] = extBatch2
  asciiForecastMaskBatch[3] = extBatch3
  asciiForecastMaskBatch[4] = extBatch4
  asciiForecastMaskBatch[5] = extBatch5
  asciiForecastMaskBatch[6] = extBatch6
  asciiForecastMaskBatch[7] = extBatch7
  writeLines(asciiForecastMaskBatch, asciiForecastMask)
  
  #copying necessary file in working folder
  src.dir <- "C:/Users/ANugraha/Documents/LUMENS/D_Development/Simulation/LTM/LTM_app/"
  LTMfiles <- dir(src.dir)
  #sapply(x, function(x) {
  file.copy(paste(src.dir, LTMfiles, sep=""), paste(secondWorkDir, terWorkDir, LTMfiles, sep="")) 
  #})
  
  for(z in 1:numOfFactors) {
    fileConn<-file("LTM_testing.txt")
    writeLines(c("2", "0", mapFactor1, mapFactor2, numOfFactors, factors[c(1:z)], "0", 
                 paste(secondWorkDir,terWorkDir,"test_mask", i, ".asc"," 7", sep=""),
                 paste(secondWorkDir,terWorkDir,"initial", i, ".asc", sep="") ,
                 paste(secondWorkDir,terWorkDir,"final", i, ".asc", sep="") , 
                 "1", sample, "1", "1"), fileConn) #jadinya cuma 1 faktor
    close(fileConn)
    fileConn<-file("LTM_forecasting.txt")
    writeLines(c("2", "1", mapFactor1, mapFactor2, numOfFactors, factors[c(1:z)], "0", 
                 paste(secondWorkDir,terWorkDir,"forecast_mask", i, ".asc"," 7", sep=""), 
                 paste(secondWorkDir,terWorkDir,"initial", i, ".asc", sep=""),
                 paste(secondWorkDir,terWorkDir,"final", i, ".asc", sep=""),
                 "1", "1", "1", "1"), fileConn)
    close(fileConn)
  }
  
}

foreach(i=1:4) %dopar% {
  setwd(secondWorkDir)
  terWorkDir <- paste(i,"/", sep="")
  setwd(paste(secondWorkDir,terWorkDir, sep=""))
  
  # LTM-running, (1) create batch process for TESTING (2) create NN network file and (3) execute training
  com1 <- paste("createnet", numOfFactors, neuron, 1, "ltm.net", sep=" ")
  com2 <- paste("CreatePattern.6.5", " LTM_testing.txt", " v", sep="")
  com3 <- paste("while MSE > 0.0 and CYCLES <", iter, sep="")
  com3 <- paste(com3,"do", sep=" ")
  
  LTM_batch = readLines("train.bat")
  LTM_batch[6]=com3
  writeLines(LTM_batch,"train.bat")
  
  LTM_batch2 = readLines("LTM_testing.bat")
  LTM_batch2[2]=com1
  LTM_batch2[3]=com2
  writeLines(LTM_batch2,"LTM_testing.bat")
  
  if (i %% (numOfCores) != 0) { 
    system("LTM_testing.bat", ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  } else {
    system("LTM_testing.bat", ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)
  }
  
  eval(parse(text=(paste("test_mask", i, "<-raster('test_mask", i, ".asc') ", sep=""))))
  eval(parse(text=(paste("forecast_mask", i, "<-raster('forecast_mask", i, ".asc') ", sep=""))))
}
stopCluster(cl)

#Sys.sleep(90)

#RUNNING LAND TRANSFORMATION MODEL-TESTING STAGE
for (i in 1:n_class) {
  setwd (secondWorkDir)
  terWorkDir2 <- paste (i,"/", sep="")
  setwd (paste (secondWorkDir,terWorkDir2, sep=""))
  # LTM-running, (1) create NN network file FOR FORECASTING (2) execute forecasting
  com4 <-paste("CreatePattern.6.5", " LTM_forecasting.txt", " v", sep="")
  com5 <-paste('landusebase = "', 'initial', i, '.asc"', sep="")      
  eval(parse(text=(paste("trans", "<-changePixel", i,sep="")))) #habis ini cek kalo ada trans == 0, kasih handling
  com6 <-paste("trans           =", trans, sep="")
  LTM_batch3 = readLines("LTM_forecasting.bat")
  LTM_batch3[2]=com4
  writeLines(LTM_batch3,"LTM_forecasting.bat")
  LTM_batch4 = readLines("batch-tests.bat")
  LTM_batch4[3]=com5
  LTM_batch4[7]=com6
  writeLines(LTM_batch4,"batch-tests.bat")
  system("LTM_forecasting.bat", ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)
  testing <-file.exists ("ts_1000.asc")
  if (testing==1) {
    eval(parse(text=(paste("forecast", i, "<-raster('ts_", Iteration, ".asc') ", sep=""))))
    eval(parse(text=(paste("forecast", i, "<-forecast", i, "*", i, sep=""))))
  } else {
    eval(parse(text=(paste("forecast", i, "<-final", i, sep=""))))
  }
  testing2 <-file.exists ("res_1000.asc")
  if (testing2==1) {
    eval(parse(text=(paste("probability_", i, '<-raster("res_', Iteration, '.asc")', sep=""))))
    eval(parse(text=(paste("writeRaster(", "probability_", i, ",", (paste('"',secondWorkDir, "res/probability_", i, '.tif", format="GTiff", overwrite=TRUE)', sep="")), sep=""))))
  }
}

#Sys.sleep(90) (NEED TO TEST IF THIS IS NECESSARY AT THIS STAGE)

#merge all forecast together as one predicted map
forecast<-forecast1
for (i in 1:n_class) {
  if (i<n_class) {
    eval(parse(text=(paste("values(forecast)<-ifelse (values(forecast)==0, values(forecast", i, "),","values(forecast))", sep=""))))
  } else {
    eval(parse(text=(paste("values(forecast)<-ifelse (values(forecast)==0," ,"values(finalLandUse),", "values(forecast))",  sep=""))))
  }
}

testing_lu <-forecast
plot (testing_lu)

#create exclusion layer for forecasting stage
for (m in 1:n_class) {
  eval(parse(text=(paste("values(forecast_mask", m, ")<-ifelse (values(forecast_mask", m, ")==0, 7,0)", sep=""))))
}


#RUNNING LAND TRANSFORMATION MODEL-FORECASTING STAGE

forecasted_mask_list<-list()
forecasted_mask<-forecast_mask1
values(forecasted_mask)<-0

#for (a in 1:n_class) {
#  setwd (secondWorkDir)
#  terWorkDir3 <- paste (a,"/", sep="")
#  setwd (paste (secondWorkDir,terWorkDir3, sep=""))
#  for (b in 1:n_class) {
#    #create exclusion layer for each transition
#    if (b!=a) {
#      eval(parse(text=(paste("writeRaster(","forecast_mask", b, ' ,"forecast_mask', b, '.asc"', ' ,format="ascii", NAflag=-9999, overwrite=TRUE)', sep=""))))
#      asc_forecast_mask2 <- paste("forecast_mask", b, ".asc", sep="")
#      asc_final_batch = readLines(asc_forecast_mask2)
#      asc_final_batch[1]=extBatch1
#      asc_final_batch[2]=extBatch2
#      asc_final_batch[3]=extBatch3
#      asc_final_batch[4]=extBatch4
#      asc_final_batch[5]=extBatch5
#      asc_final_batch[6]=extBatch6
#      asc_final_batch[7]=extBatch7
#      writeLines(asc_final_batch,asc_forecast_mask2)
#    } 
#  }    
#}    

for (o in 1:n_class) {
  setwd (secondWorkDir)
  terWorkDir3 <- paste (o,"/", sep="")
  setwd (paste (secondWorkDir,terWorkDir3, sep=""))
  
  for (b in 1:n_class) {
    #create exclusion layer for each transition
    if (b!=o) {
      eval(parse(text=(paste('forecast_mask',b,'<-forecast_mask',b,'*forecasted_mask', sep=""))))
      eval(parse(text=(paste("writeRaster(","forecast_mask", b, ' ,"forecast_mask', b, '.asc"', ' ,format="ascii", NAflag=-9999, overwrite=TRUE)', sep=""))))
      asc_forecast_mask2 <- paste("forecast_mask", b, ".asc", sep="")
      asc_final_batch = readLines(asc_forecast_mask2)
      asc_final_batch[1]=extBatch1
      asc_final_batch[2]=extBatch2
      asc_final_batch[3]=extBatch3
      asc_final_batch[4]=extBatch4
      asc_final_batch[5]=extBatch5
      asc_final_batch[6]=extBatch6
      asc_final_batch[7]=extBatch7
      writeLines(asc_final_batch,asc_forecast_mask2)
    } 
  }  
  
  for (p in 1:n_class) {  
    # execute LTM with modified transition pixel number
    if (p!=o) {
      eval(parse(text=(paste("testing3<-changePixel", o,"from", p, "!=0", sep=""))))
      if (testing3==1) {
        fileConn<-file("LTM_forecasting.txt")
        writeLines(c("2", "1", mapFactor1, mapFactor2, numOfFactors, Factors[c(1:numOfFactors)], "0", 
                     paste(secondWorkDir,terWorkDir3,"forecast_mask", p, ".asc"," 7", sep=""), 
                     paste(secondWorkDir,terWorkDir3,"initial", o, ".asc", sep=""), 
                     paste(secondWorkDir,terWorkDir3,"final", o, ".asc", sep=""), "1", "1", "1", "1"), fileConn)
        close(fileConn)
        com8 <-paste("CreatePattern.6.5", " LTM_forecasting.txt", sep="")
        com9 <-paste("AsciiTs2.3 fullReference.txt res_", Iteration," final", o, ".asc", 
                     " simulated_", o, "_from_", p, ".asc", " 1 ", 
                     (eval(parse(text=(paste("changePixel", o, "from", p, sep=""))))), sep="")
        LTM_batch5 = readLines("LTM_forecasting2.bat")
        LTM_batch5[2]=com8
        LTM_batch5[3]=com9
        writeLines(LTM_batch5,"LTM_forecasting2.bat")
        system("LTM_forecasting2.bat", ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)
        eval(parse(text=(paste("testing4 <-file.exists (", "'simulated_", o, "_from_", p, ".asc')", sep="" ))))
        if(testing4==1){
          eval(parse(text=(paste("forecasted", o, "from", p, "<-raster(", "'simulated_", o, "_from_", p, ".asc')", sep=""))))
          eval(parse(text=(paste("writeRaster(", "forecasted", o, "from", p, ",", (paste('"',WorkDir, "res/simulated", o, "from", p, '.tif", format="GTiff", overwrite=TRUE)', sep="")), sep=""))))
        }
      
        eval(parse(text=(paste("forecasted_mask_list[", p, "]=c(" , "forecasted", o, "from", p, ")", sep=""))))
        #eval(parse(text=(paste('forecasted_mask<-Reduce("+",forecasted_mask_list)', sep=""))))
        eval(parse(text=(paste('forecasted_mask<-forecasted_mask*7', sep=""))))
        #if (testing4==1) {
        #  eval(parse(text=(paste("forecasted", o, "from", p, '<-raster("ts_',o,"from", p, "_", Iteration, '.asc")', sep=""))))
        #}
        #testing3 <-file.exists ("ts_1000.asc")
        #if (testing3==1) {
        #  eval(parse(text=(paste("forecasted", o, "from", p, '<-raster("ts_', Iteration, '.asc")', sep=""))))
        #}
      }
    }
    # editing forecast.asc to include cell that already forecasted
  }
}
