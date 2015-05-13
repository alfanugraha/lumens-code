#PREQUES data handling
if (grepl(".ldbase", as.character(ldabase.preques))){
  print(paste("loading Pre-QuES database from", ldabase.preques,sep=' '))
  #QuES-B and Pre-QuES integration
  #load final database
  load(ldabase.preques)
  #rename initial variables
  prqs.proj.prop<-proj_prop
  prqgs.lucdb<-data_merge_sel
  prqs.ov.chg<-Ov_chg
  prqs.lutm<-cross_temp.melt.dbf
  prqs.luchg<-luchg
  prqs.luchg.att<-luchg_att
  prqs.year.init<-period1
  prqs.year.final<-period2
  remove(proj_prop,data_merge_sel, Ov_chg, cross_temp.melt.dbf, luchg, luchg_att, period1, period2)
  
  #identify contributing land use change to focal area degradation and loss
  habitat.degradation.bol<-habitat.degradation/habitat.degradation; #create boolean degradation map
  
  if (as.character(prqs.luchg@crs)==as.character(habitat.degradation.bol@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.degradation.bol)[1]){
      print("change map has the same extent with the habitat degradation map")
    } else{
      print("change map doesn't have the same extent with the habitat degradation map, synchronising habitat degradation map...")
      habitat.degradation.bol<-spatial_sync_raster(habitat.degradation.bol, prqs.luchg, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat degradation map, synchronising habitat degradation map...")
    habitat.degradation.bol<-spatial_sync_raster(habitat.degradation.bol, prqs.luchg, method = "ngb")
  }
  
  luchg.degradation<-prqs.luchg*habitat.degradation.bol; #focal area degradation LUC
  luchg.degradation.att<-na.omit(as.data.frame(freq(luchg.degradation)))
  colnames(luchg.degradation.att)<-c("ID","CHANGE")
  luchg.degradation.att<-merge(luchg.degradation.att,prqs.luchg.att,by="ID")
  luchg.degradation.att<-as.data.frame(cbind(luchg.degradation.att[1],luchg.degradation.att[2],luchg.degradation.att[4],luchg.degradation.att[5],luchg.degradation.att[12],luchg.degradation.att[13], luchg.degradation.att[14]))
  luchg.degradation.att<-luchg.degradation.att[ order(-luchg.degradation.att[,2]), ]
  tryCatch({
    luchg.degradation.db.filename<-paste("LUCHG_degradation_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(luchg.degradation.att, luchg.degradation.db.filename)
  },error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})
  
  #top 10 habitat degradation
  luchg.degradation.10<-luchg.degradation.att[1:10,]
  
  #Habitat degradation of focal area due to neighboring changes
  tryCatch({
    foc.area.row<-lookup_bh[ which(as.character(lookup_bh$BIODIV)==as.character(1)),]
    foc.area.luchg.id<-paste(as.character(foc.area.row[1]),'0',as.character(foc.area.row[1]), sep='')
    luchg.degradation.focal.area<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID)==foc.area.luchg.id),]
    luchg.degradation.focal.area$LU_CHG<-paste("Stable", luchg.degradation.focal.area$LC_t1, sep=' ')
    luchg.degradation.focal.area<-as.data.frame(cbind(luchg.degradation.focal.area[1],luchg.degradation.focal.area[2],luchg.degradation.focal.area[7]))
  },error=function(e){cat("Habitat degradation of focal area due to neighboring changes :",conditionMessage(e), "\n")})
  
  #Habitat degradation due to LULCC in situ 
  tryCatch({
    luchg.degradation.10.with.change<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID_LC2)!=as.character(luchg.degradation.10$ID_LC1)),];#habitat degradation with landuse/landcover change
    luchg.degradation.10.with.change<-as.data.frame(cbind(luchg.degradation.10.with.change[1],luchg.degradation.10.with.change[2],luchg.degradation.10.with.change[7]))
    colnames(luchg.degradation.10.with.change)<-c("ID", "AREA", "LULC")
  },error=function(e){cat("No Habitat degradation due to LULCC in situ :",conditionMessage(e), "\n")})
  
  
  #Habitat degradation due to neighboring focal area changes
  tryCatch({
    luchg.degradation.10.no.change<-luchg.degradation.10[ which(as.character(luchg.degradation.10$ID_LC2)==as.character(luchg.degradation.10$ID_LC1)),];#degradation in persistent landuse/landcover
    luchg.degradation.10.no.change<-as.data.frame(cbind(luchg.degradation.10.no.change[1],luchg.degradation.10.no.change[2],luchg.degradation.10.no.change[5]))
    colnames(luchg.degradation.10.no.change)<-c("ID", "AREA", "LULC")
    luchg.degradation.10.no.change$LULC<-paste("Stable", luchg.degradation.10.no.change$LULC, sep=' ')
    luchg.degradation.10.no.change<-luchg.degradation.10.no.change[!(luchg.degradation.10.no.change$ID==foc.area.luchg.id),]
  },error=function(e){cat("No Habitat degradation due to LULCC in situ :",conditionMessage(e), "\n")})
  
  #important variables in habitat degradation: luchg.degradation.focal.area, luchg.degradation.10.with.change, luchg.degradation.10.no.change
  
  #HABITAT LOSS ANALYSIS
  #identify contributing land use change to focal area loss
  if (as.character(prqs.luchg@crs)==as.character(habitat.loss.NA@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.loss.NA)[1]){
      print("change map has the same extent with the habitat loss map")
    } else{
      print("change map doesn't have the same extent with the habitat loss map, synchronising habitat loss map...")
      prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.loss.NA, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat loss map, synchronising habitat loss map...")
    prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.loss.NA,  method = "ngb")
  }
  tryCatch({
    luchg.loss<-lu2*habitat.loss.NA; #focal area loss LUC
    luchg.loss.att<-na.omit(as.data.frame(freq(luchg.loss)))
    colnames(luchg.loss.att)<-c("ID","CHANGE")
    luchg.loss.att<-merge(lookup_lc,luchg.loss.att,by="ID")
    #luchg.loss.att<-as.data.frame(cbind(luchg.loss.att[1],luchg.loss.att[2],luchg.loss.att[4],luchg.loss.att[5],luchg.loss.att[12],luchg.loss.att[13], luchg.loss.att[14]))
    luchg.loss.att<-luchg.loss.att[ order(-luchg.loss.att[,3]), ]
    luchg.loss.db.filename<-paste("LUCHG_loss_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(luchg.loss.att, luchg.loss.db.filename)
    #top 10 subsequent LULC related to habitat loss
    luchg.loss.10<-luchg.loss.att[1:10,]
    luchg.loss.10$Proportion<-((luchg.loss.10$CHANGE)/(sum(luchg.loss.10$CHANGE)))*Spat_res*100
    luchg.loss.10.prop<-as.data.frame(cbind(' ','TOTAL',(sum(luchg.loss.10$CHANGE)), (sum(luchg.loss.10$Proportion))))
    luchg.loss.10$Proportion<-round(luchg.loss.10$Proportion,digits=2)
    colnames(luchg.loss.10.prop)<-c('ID','CLASS','CHANGE','Proportion')
    luchg.loss.10<-rbind(luchg.loss.10,luchg.loss.10.prop)
  },error=function(e){cat("Skipping identify contributing land use change to focal area loss :",conditionMessage(e), "\n")})
  
  if (as.character(habitat.loss.NA@crs)==as.character(zone@crs)){
    print("Final land use/cover map has the same projection")
    if (res(habitat.loss.NA)[1]==res(zone)[1]){
      print("zone has the same extent with the habitat map")
    } else{
      print("zone doesn't have the same extent with the habitat map, synchronising zone map...")
      zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
    }
  } else{
    print("zone doesn't have the same projection with the habitat map, synchronising zone map...")
    zone<-spatial_sync_raster(zone, habitat.loss.NA, method = "ngb")
  }
  
  
  #zonal stat for habitat loss
  tryCatch({
    habitat.loss.NA.0<-reclassify(habitat.loss.NA, cbind(NA, 0))
    zstat.habitat.loss.NA<-ZonalStat(habitat.loss.NA.0, zone, FUN = "sum")
    colnames(zstat.habitat.loss.NA)[1] ="ZONE"
    zstat.habitat.loss.NA<-merge(lookup_z, zstat.habitat.loss.NA, by="ZONE")
    zstat.habitat.loss.NA$Proportion<-((zstat.habitat.loss.NA$sum)/(sum(zstat.habitat.loss.NA$sum)))*Spat_res*100
    zstat.habitat.loss.NA.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.loss.NA$sum)), (sum(zstat.habitat.loss.NA$Proportion))))
    zstat.habitat.loss.NA$Proportion<-round(zstat.habitat.loss.NA$Proportion,digits=2)
    colnames(zstat.habitat.loss.NA.prop)<-c('ZONE','Z_NAME','sum','Proportion')
    zstat.habitat.loss.NA<-zstat.habitat.loss.NA[ order(as.numeric(zstat.habitat.loss.NA$sum), decreasing=TRUE), ]
    zstat.habitat.loss.NA<-rbind(zstat.habitat.loss.NA,zstat.habitat.loss.NA.prop)
    colnames(zstat.habitat.loss.NA)<-c('ID','ZONE','total.area','Proportion')
   
  },error=function(e){cat("Skipping zonal stat for habitat loss :",conditionMessage(e), "\n")})
  
  #write zonal stats table for habitat loss
  tryCatch({
    zstat.loss.loss.filename<-paste("Habitat_loss_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(zstat.habitat.loss.NA, zstat.loss.loss.filename)
  },error=function(e){cat("write zonal stats table for habitat loss :",conditionMessage(e), "\n")})
  
  #important variables in habitat loss: luchg.loss.10, zstat.habitat.loss.NA
  
  #identify contributing land use change to focal area recovery and gain
  #HABITAT RECOVERY
  habitat.recovery.bol<-habitat.recovery/habitat.recovery; #create boolean recovery map
  
  if (as.character(prqs.luchg@crs)==as.character(habitat.recovery.bol@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.recovery.bol)[1]){
      print("change map has the same extent with the habitat recovery map")
    } else{
      print("change map doesn't have the same extent with the habitat recovery map, synchronising habitat recovery map...")
      habitat.recovery.bol<-spatial_sync_raster(habitat.recovery.bol, prqs.luchg, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat recovery map, synchronising habitat recovery map...")
    habitat.recovery.bol<-spatial_sync_raster(habitat.recovery.bol, prqs.luchg, method = "ngb")
  }
  
  luchg.recovery<-prqs.luchg*habitat.recovery.bol; #focal area recovery LUC
  luchg.recovery.att<-na.omit(as.data.frame(freq(luchg.recovery)))
  colnames(luchg.recovery.att)<-c("ID","CHANGE")
  luchg.recovery.att<-merge(luchg.recovery.att,prqs.luchg.att,by="ID")
  luchg.recovery.att<-as.data.frame(cbind(luchg.recovery.att[1],luchg.recovery.att[2],luchg.recovery.att[4],luchg.recovery.att[5],luchg.recovery.att[12],luchg.recovery.att[13], luchg.recovery.att[14]))
  luchg.recovery.att<-luchg.recovery.att[ order(-luchg.recovery.att[,2]), ]
  tryCatch({
    luchg.recovery.db.filename<-paste("LUCHG_recovery_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(luchg.recovery.att, luchg.recovery.db.filename)
  },error=function(e){cat("Skipping database export process :",conditionMessage(e), "\n")})
  
  #top 10 habitat recovery
  luchg.recovery.10<-luchg.recovery.att[1:10,]
  
  #Habitat recovery of focal area due to neighboring changes
  tryCatch({
    foc.area.row<-lookup_bh[ which(as.character(lookup_bh$BIODIV)==as.character(1)),]
    foc.area.luchg.id<-paste(as.character(foc.area.row[1]),'0',as.character(foc.area.row[1]), sep='')
    luchg.recovery.focal.area<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID)==foc.area.luchg.id),]
    luchg.recovery.focal.area$LU_CHG<-paste("Stable", luchg.recovery.focal.area$LC_t1, sep=' ')
    luchg.recovery.focal.area<-as.data.frame(cbind(luchg.recovery.focal.area[1],luchg.recovery.focal.area[2],luchg.recovery.focal.area[7]))
  },error=function(e){cat("Habitat recovery of focal area due to neighboring changes :",conditionMessage(e), "\n")})
  
  #Habitat recovery due to LULCC in situ 
  tryCatch({
    luchg.recovery.10.with.change<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID_LC2)!=as.character(luchg.recovery.10$ID_LC1)),];#habitat recovery with landuse/landcover change
    luchg.recovery.10.with.change<-as.data.frame(cbind(luchg.recovery.10.with.change[1],luchg.recovery.10.with.change[2],luchg.recovery.10.with.change[7]))
    colnames(luchg.recovery.10.with.change)<-c("ID", "AREA", "LULC")
  },error=function(e){cat("No Habitat recovery due to LULCC in situ :",conditionMessage(e), "\n")})
  
  
  #Habitat recovery due to neighboring focal area changes
  tryCatch({
    luchg.recovery.10.no.change<-luchg.recovery.10[ which(as.character(luchg.recovery.10$ID_LC2)==as.character(luchg.recovery.10$ID_LC1)),];#recovery in persistent landuse/landcover
    luchg.recovery.10.no.change<-as.data.frame(cbind(luchg.recovery.10.no.change[1],luchg.recovery.10.no.change[2],luchg.recovery.10.no.change[5]))
    colnames(luchg.recovery.10.no.change)<-c("ID", "AREA", "LULC")
    luchg.recovery.10.no.change$LULC<-paste("Stable", luchg.recovery.10.no.change$LULC, sep=' ')
    luchg.recovery.10.no.change<-luchg.recovery.10.no.change[!(luchg.recovery.10.no.change$ID==foc.area.luchg.id),]
  },error=function(e){cat("No Habitat recovery due to LULCC in situ :",conditionMessage(e), "\n")})
  
  #important variables in habitat recovery: luchg.recovery.focal.area, luchg.recovery.10.with.change, luchg.recovery.10.no.change
  
  #HABITAT GAIN ANALYSIS
  #identify contributing land use change to focal area gain
  if (as.character(prqs.luchg@crs)==as.character(habitat.gain.NA@crs)){
    print("Final land use/cover map has the same projection")
    if (res(prqs.luchg)[1]==res(habitat.gain.NA)[1]){
      print("change map has the same extent with the habitat gain map")
    } else{
      print("change map doesn't have the same extent with the habitat gain map, synchronising habitat gain map...")
      prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.gain.NA, method = "ngb")
    }
  } else{
    print("change map doesn't have the same projection with the habitat gain map, synchronising habitat gain map...")
    prqs.luchg<-spatial_sync_raster(prqs.luchg,habitat.gain.NA,  method = "ngb")
  }
  tryCatch({
    luchg.gain<-lu2*habitat.gain.NA; #focal area gain LUC
    luchg.gain.att<-na.omit(as.data.frame(freq(luchg.gain)))
    colnames(luchg.gain.att)<-c("ID","CHANGE")
    luchg.gain.att<-merge(lookup_lc,luchg.gain.att,by="ID")
    #luchg.gain.att<-as.data.frame(cbind(luchg.gain.att[1],luchg.gain.att[2],luchg.gain.att[4],luchg.gain.att[5],luchg.gain.att[12],luchg.gain.att[13], luchg.gain.att[14]))
    luchg.gain.att<-luchg.gain.att[ order(-luchg.gain.att[,3]), ]
    luchg.gain.db.filename<-paste("LUCHG_gain_database",prqs.location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(luchg.gain.att, luchg.gain.db.filename)
    #top 10 subsequent LULC related to habitat gain
    luchg.gain.10<-luchg.gain.att[1:10,]
    luchg.gain.10$Proportion<-((luchg.gain.10$CHANGE)/(sum(luchg.gain.10$CHANGE)))*Spat_res*100
    luchg.gain.10.prop<-as.data.frame(cbind(' ','TOTAL',(sum(luchg.gain.10$CHANGE)), (sum(luchg.gain.10$Proportion))))
    luchg.gain.10$Proportion<-round(luchg.gain.10$Proportion,digits=2)
    colnames(luchg.gain.10.prop)<-c('ID','CLASS','CHANGE','Proportion')
    luchg.gain.10<-rbind(luchg.gain.10,luchg.gain.10.prop)
  },error=function(e){cat("Skipping identify contributing land use change to focal area gain :",conditionMessage(e), "\n")})
  
  if (as.character(habitat.gain.NA@crs)==as.character(zone@crs)){
    print("Final land use/cover map has the same projection")
    if (res(habitat.gain.NA)[1]==res(zone)[1]){
      print("zone has the same extent with the habitat map")
    } else{
      print("zone doesn't have the same extent with the habitat map, synchronising zone map...")
      zone<-spatial_sync_raster(zone, habitat.gain.NA, method = "ngb")
    }
  } else{
    print("zone doesn't have the same projection with the habitat map, synchronising zone map...")
    zone<-spatial_sync_raster(zone, habitat.gain.NA, method = "ngb")
  }
  
  
  #zonal stat for habitat gain
  tryCatch({
    habitat.gain.NA.0<-reclassify(habitat.gain.NA, cbind(NA, 0))
    zstat.habitat.gain.NA<-ZonalStat(habitat.gain.NA.0, zone, FUN = "sum")
    colnames(zstat.habitat.gain.NA)[1] ="ZONE"
    zstat.habitat.gain.NA<-merge(lookup_z, zstat.habitat.gain.NA, by="ZONE")
    zstat.habitat.gain.NA$Proportion<-((zstat.habitat.gain.NA$sum)/(sum(zstat.habitat.gain.NA$sum)))*Spat_res*100
    zstat.habitat.gain.NA.prop<-as.data.frame(cbind(' ','TOTAL',(sum(zstat.habitat.gain.NA$sum)), (sum(zstat.habitat.gain.NA$Proportion))))
    zstat.habitat.gain.NA$Proportion<-round(zstat.habitat.gain.NA$Proportion,digits=2)
    colnames(zstat.habitat.gain.NA.prop)<-c('ZONE','Z_NAME','sum','Proportion')
    zstat.habitat.gain.NA<-zstat.habitat.gain.NA[ order(as.numeric(zstat.habitat.gain.NA$sum), decreasing=TRUE), ]
    zstat.habitat.gain.NA<-rbind(zstat.habitat.gain.NA,zstat.habitat.gain.NA.prop)
    colnames(zstat.habitat.gain.NA)<-c('ID','ZONE','total.area','Proportion')
  },error=function(e){cat("Skipping zonal stat for habitat gain :",conditionMessage(e), "\n")})
  
  #write zonal stats table for habitat gain
  tryCatch({
    zstat.gain.gain.filename<-paste("Habitat_gain_zonal_stat_",location,'_',year1,'_',year2,'.dbf', sep='')
    write.dbf(zstat.habitat.gain.NA, zstat.gain.gain.filename)
  },error=function(e){cat("write zonal stats table for habitat gain :",conditionMessage(e), "\n")})
  
  #important variables in habitat gain: luchg.gain.10, zstat.habitat.gain.NA
  
  
} else{
  print("No previous Pre-QuES database loaded")
}