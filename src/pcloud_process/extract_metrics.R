library(dplyr)
library(lidR)
library(rgdal)
library(raster)
library(sp)

library(e1071)

# Set working dirctory
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad_reclass/"
#workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_reclass/"
#workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_leafon_reclass/"
#workingdirectory="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto_25mrad_reclass/"
setwd(workingdirectory)

fieldfile="w_point_balaton.shp"
#fieldfile="tisza_full.shp"
#fieldfile="w_point.shp"

#Import
fieldsp = readOGR(dsn=fieldfile)
fieldsp_df=fieldsp@data

objname=fieldsp_df$OBJNAME

dpcloudfea_exp_df <- data.frame(matrix(ncol = 13, nrow = 0))
x <- c("OBJNAME", "point_ID","point_name", "H_p99","H_mean","H_median",
       "V_std","V_var","C_ppr","A_std","A_cover","nofveg","nofallp")
colnames(dpcloudfea_exp_df) <- x

rad=2.5
name="Balaton_OBJNAME"
#name="Tisza_OBJNAME"
#name="Ferto_OBJNAME"

for (i in objname) {
  
  print(i)
  
  if (file.exists(paste(name,i,"_25mrad_reclass.laz",sep=""))) {
    
    las=readLAS(paste(name,i,"_25mrad_reclass.laz",sep=""))
    
    field_df_sel=fieldsp_df[fieldsp_df$OBJNAME==i,]
    areaofint_sel=fieldsp[fieldsp$OBJNAME==i,]
    
    if (nrow(las@data[las@data$Classification==2,])>10) {
      las_norm=lasnormalize(las, knnidw(k=35,p=1))
      
      las_clip=lasclipCircle(las_norm,areaofint_sel@coords[1],areaofint_sel@coords[2],rad)
      
      las_norm_veg=lasfilter(las_clip,(Classification!=2L))
      
      H_p99= quantile(las_norm_veg@data$Z, 0.99)
      H_mean= mean(las_norm_veg@data$Z)
      H_median=median(las_norm_veg@data$Z)
      
      V_std=sd(las_clip@data$Z)
      V_var=var(las_clip@data$Z)
      
      C_ppr=(nrow(las_clip@data[las_clip@data$Classification==2L])/length(las_clip@data$Z))*100
      
      A_std=sd(scale(las_clip@data$Intensity,center = FALSE))
      A_cover=sum(las_norm_veg@data$Intensity)/sum(las_clip@data$Intensity)
      
      nofallp=length(las_clip@data$Z)
      nofveg=length(las_norm_veg@data$Z)
      
      newline <- data.frame(t(c(OBJNAME=i,point_ID=areaofint_sel$point_ID,point_name=as.character(areaofint_sel$point_name),
                                H_p99=H_p99,
                                H_mean=H_mean,
                                H_median=H_median,
                                V_std=V_std,
                                V_var=V_var,
                                C_ppr=C_ppr,
                                A_std=A_std,
                                A_cover=A_cover,
                                nofveg=nofveg,
                                nofallp=nofallp
                                )))
      
      dpcloudfea_exp_df <- rbind(dpcloudfea_exp_df, newline)
    }
  }
}

dpcloudfea_exp_df_c=dpcloudfea_exp_df[complete.cases(dpcloudfea_exp_df), ]

write.csv(dpcloudfea_exp_df,paste("Balaton_lidarmetrics_",rad,"_reclass3.csv",sep=""))
#write.csv(dpcloudfea_exp_df_c,paste("Tisza_lidarmetrics_",rad,"_reclass2.csv",sep=""))
#write.csv(dpcloudfea_exp_df_c,paste("Tisza_lidarmetrics_",rad,"_leafon_reclass2.csv",sep=""))
#write.csv(dpcloudfea_exp_df,paste("Ferto_lidarmetrics_",rad,"_reclass2.csv",sep=""))
