library("lidR")
library("rgdal")

# Set working dirctory
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton/"
#workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza/"
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto/"
setwd(workingdirectory)

#Import 
#areaofintfile="w_point_balaton.shp"
#areaofintfile="tisza_full.shp"
areaofintfile="w_point.shp"
areaofint=readOGR(dsn=areaofintfile)

# Extract
ctg = catalog(workingdirectory)

for (i in seq(1,length(areaofint$OBJNAME),1)){ 

  areaofint_sel=areaofint[areaofint$OBJNAME==areaofint$OBJNAME[i],]
  
  print(paste("OBJNAME",areaofint_sel$OBJNAME,sep=""))
  
  clipped_25=lasclipCircle(ctg,areaofint_sel@coords[1],areaofint_sel@coords[2],25)
  
  if (clipped_25@header@PHB[["Number of point records"]]>0) {
    #writeLAS(clipped_25,paste("Balaton_OBJNAME",areaofint_sel$OBJNAME,"_25mrad.laz",sep=""))
    #writeLAS(clipped_25,paste("Tisza_OBJNAME",areaofint_sel$OBJNAME,"_25mrad.laz",sep=""))
    writeLAS(clipped_25,paste("Ferto_OBJNAME",areaofint_sel$OBJNAME,"_25mrad.laz",sep=""))
  }
}