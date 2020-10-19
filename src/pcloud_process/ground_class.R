library(dplyr)
library(lidR)
library(rgdal)
library(raster)
library(sp)

library(e1071)

# Set working dirctory
#workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto_25mrad/"
#workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_leafon/"
#workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad/"
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad/"
setwd(workingdirectory)

fieldfile="w_point_balaton.shp"
#fieldfile="tisza_full.shp"
#fieldfile="w_point.shp"

#Import
fieldsp = readOGR(dsn=fieldfile)
fieldsp_df=fieldsp@data

objname=fieldsp_df$OBJNAME

name="Balaton_OBJNAME"
#name="Tisza_OBJNAME"
#name="Ferto_OBJNAME"

for (i in objname) {
  
  print(i)
  
  if (file.exists(paste(name,i,"_25mrad.laz",sep=""))) {
    las=readLAS(paste(name,i,"_25mrad.laz",sep=""))
    
    field_df_sel=fieldsp_df[fieldsp_df$OBJNAME==i,]
    areaofint_sel=fieldsp[fieldsp$OBJNAME==i,]
    
    #reclassify
    
    las2=las
    
    las2@data$Classification<-1
    
    las2 <- lasground(las2, algorithm = pmf(ws = 5, th = 0.25))
    #plot(las2,color="Classification",colorPalette=random.colors(26),size=4)
    writeLAS(las2,paste("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad_reclass/",
                        name,i,"_25mrad_reclass.laz",sep=""))
    
  }

}