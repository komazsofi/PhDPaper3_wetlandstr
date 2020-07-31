library(dplyr)
library(lidR)
library(rgdal)
library(raster)
library(sp)

library(e1071)

# Set working dirctory
workingdirectory="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad/"
setwd(workingdirectory)

fieldfile="w_point_balaton.shp"

#Import
fieldsp = readOGR(dsn=fieldfile)
fieldsp_df=fieldsp@data

objname=fieldsp_df$OBJNAME

dpcloudfea_exp_df <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("OBJNAME", "point_ID","point_name", "H_max")
colnames(dpcloudfea_exp_df) <- x

#objname[1]

for (i in objname) {
  
  print(i)
  
  if (file.exists(paste("Balaton_OBJNAME",i,"_25mrad.laz",sep=""))) {
    
    las=readLAS(paste("Balaton_OBJNAME",i,"_25mrad.laz",sep=""))
    
    field_df_sel=fieldsp_df[fieldsp_df$OBJNAME==i,]
    areaofint_sel=fieldsp[fieldsp$OBJNAME==i,]
    
    if (nrow(las@data[las@data$Classification==2,])>10) {
      las_norm=lasnormalize(las, knnidw(k=10,p=2))
      
      las_clip=lasclipCircle(las_norm,areaofint_sel@coords[1],areaofint_sel@coords[2],5)
      
      H_max= max(las_clip@data$Z)
      
      newline <- data.frame(t(c(OBJNAME=i,point_ID=areaofint_sel$point_ID,point_name=areaofint_sel$point_name,
                                H_max=H_max)))
      
      dpcloudfea_exp_df <- rbind(dpcloudfea_exp_df, newline)
    }
  }
}

write.csv(dpcloudfea_exp_df,"Balaton_lidarmetrics.csv")
