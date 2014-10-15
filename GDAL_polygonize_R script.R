#Polygonize function
osego_bat<-readLines("C:/OSGeo4W/OSGeo4W.bat")
rast_dir<-paste('"C:\\Users\\dindiarto\\AppData\\Local\\Temp\\Rtmp0Mh3pM\\file203c68ad1190.asc"')
data_format<-paste('"ESRI Shapefile"')
file_out<-paste('"berau_2000_L4.shp"')
osgeo_comm<-paste("python bin\\gdal_polygonize.py", rast_dir, "-f", data_format, file_out, sep=" ")
osego_bat[18]<-osgeo_comm
fileConn<-file("C:/OSGeo4W/OSGeo4W.bat")
writeLines(osego_bat, fileConn)
close(fileConn)

setwd(wd1)
system2("OSGeo4W.bat")
  

#rasterize function
shp_dir<-shQuote("C:/LUMENS_kal/1_PUR/1_Prop_Kaltim/kaltim_penunjukankws_nop2012_utm50n.shp")
file_out<-shQuote("C:/LUMENS_kal/2_Raster/01_Kaltim/kaltim_penunjukan_kawasan.tif")
kolom_data<-paste('ID')
opsi<-1
res<-100
osgeo_comm<-paste("C:\\OSGeo4W\\bin\\gdal_rasterize.exe",shp_dir, file_out,"-a",kolom_data, "-tr", res, res, "-a_nodata 65535", sep=" ")
wd<-"C:\\OSGeo4W\\bin"
shell(osgeo_comm)

setwd(wd)
system2(osgeo_comm)

