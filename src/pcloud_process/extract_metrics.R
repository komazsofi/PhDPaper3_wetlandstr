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

dpcloudfea_exp_df <- data.frame(matrix(ncol = 21, nrow = 0))
x <- c("OBJNAME", "point_ID","point_name", "H_max","H_mean","H_median","H_q25","H_q75","H_q95",
       "V_std","V_var","V_cr","V_vdr","V_sk","V_ku","V_coefv","C_ppr","C_b2","C_vegd","A_std","A_mean")
colnames(dpcloudfea_exp_df) <- x

rad=5

for (i in objname) {
  
  print(i)
  
  if (file.exists(paste("Balaton_OBJNAME",i,"_25mrad.laz",sep=""))) {
    
    las=readLAS(paste("Balaton_OBJNAME",i,"_25mrad.laz",sep=""))
    
    field_df_sel=fieldsp_df[fieldsp_df$OBJNAME==i,]
    areaofint_sel=fieldsp[fieldsp$OBJNAME==i,]
    
    if (nrow(las@data[las@data$Classification==2,])>10) {
      las_norm=lasnormalize(las, knnidw(k=10,p=2))
      
      las_clip=lasclipCircle(las_norm,areaofint_sel@coords[1],areaofint_sel@coords[2],rad)
      las_norm_veg=lasfilter(las_clip,(Classification!=2L))
      
      H_max= max(las_norm_veg@data$Z)
      H_mean= mean(las_norm_veg@data$Z)
      H_median=median(las_norm_veg@data$Z)
      H_q25 = quantile(las_norm_veg@data$Z, 0.25)
      H_q75 = quantile(las_norm_veg@data$Z, 0.75)
      H_q95 = quantile(las_norm_veg@data$Z, 0.95)
      
      V_std=sd(las_clip@data$Z)
      V_var=var(las_clip@data$Z)
      V_cr=(mean(las_clip@data$Z)-min(las_clip@data$Z))/max(las_clip@data$Z)-min(las_clip@data$Z)
      V_vdr=(max(las_clip@data$Z)-median(las_clip@data$Z))/max(las_clip@data$Z)
      V_sk=skewness(las_clip@data$Z)
      V_ku=kurtosis(las_clip@data$Z)
      V_coefv=sd(las_clip@data$Z)/mean(las_clip@data$Z)
      
      C_ppr=(nrow(las_clip@data[las_clip@data$Classification==2L])/length(las_clip@data$Z))*100
      C_b2=(nrow(las_norm_veg@data[(las_norm_veg@data$Z<2)])/length(las_clip@data$Z))*100
      C_vegd=length(las_norm_veg@data$Z)*1
      
      A_std=sd(las_clip@data$Intensity)
      A_mean=mean(las_clip@data$Intensity)
      
      newline <- data.frame(t(c(OBJNAME=i,point_ID=areaofint_sel$point_ID,point_name=as.character(areaofint_sel$point_name),
                                H_max=H_max,
                                H_mean=H_mean,
                                H_median=H_median,
                                H_q25=H_q25,
                                H_q75=H_q75,
                                H_q95=H_q95,
                                V_std=V_std,
                                V_var=V_var,
                                V_cr=V_cr,
                                V_vdr=V_vdr,
                                V_sk=V_sk,
                                V_ku=V_ku,
                                V_coefv=V_coefv,
                                C_ppr=C_ppr,
                                C_b2=C_b2,
                                C_vegd=C_vegd,
                                A_std=A_std,
                                A_mean=A_mean
                                )))
      
      dpcloudfea_exp_df <- rbind(dpcloudfea_exp_df, newline)
    }
  }
}

write.csv(dpcloudfea_exp_df,paste("Balaton_lidarmetrics_",rad,".csv",sep=""))
