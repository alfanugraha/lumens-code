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
  

