library(rgdal)
library(raster)
library(sp)
library(sf)

# Set working dirctory
workingdirectory="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Field_data_process/"
setwd(workingdirectory)

## Import shapefiles per lake

tisza_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/tisza_full.shp")
ferto_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/w_point.shp")
balaton_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/w_point_balaton.shp")

# standardize structure
tisza_pole_df=as.data.frame(tisza_pole)
ferto_pole_df=as.data.frame(ferto_pole)
balaton_pole_df=as.data.frame(balaton_pole)

tisza_pole_df$point_id<-NA

ferto_pole_df <- subset(ferto_pole_df, select = -c(8, 19))
balaton_pole_df <- subset(balaton_pole_df, select = -c(8, 19))

tisza_pole_df_org<- subset(tisza_pole_df, select = c(1,2,23,3:19,21,22,20))
names(tisza_pole_df_org)<-names(ferto_pole_df)

# calculate veg. str. parameters

calc_str_pole= function(data) {
  
  pole_data=subset(data,select=c(9:17))
  
  # FHD
  data$fhd_pole<-NA
  
  for (i in seq(1,length(data$OBJNAME))) {
    
    v=as.numeric(pole_data[i,])
    
    p<- table(v)
    p <- p/sum(p)
    entropy=sum(-p*log(p))
    
    data$fhd_pole[i] <- entropy
    
  }
  
  # sum contact
  data$sum_pole_contact=rowSums(pole_data)
  
  # height
  polecontact=pole_data
  
  polecontact[polecontact>0] <- 1
  polecontact[,1]=polecontact[,1]*0.5
  polecontact[,2]=polecontact[,2]*1
  polecontact[,3]=polecontact[,3]*1.5
  polecontact[,4]=polecontact[,4]*2
  polecontact[,5]=polecontact[,5]*2.5
  polecontact[,6]=polecontact[,6]*3
  polecontact[,7]=polecontact[,7]*3.5
  polecontact[,8]=polecontact[,8]*4
  polecontact[,9]=polecontact[,9]*4.5
  
  data$pole_height=apply(polecontact, 1, max)
  
  return(data)
}

tisza_pole_df_org_wstr=calc_str_pole(tisza_pole_df_org)
ferto_pole_df_wstr=calc_str_pole(ferto_pole_df)
balaton_pole_df_wstr=calc_str_pole(balaton_pole_df)

# clean up only necesary data export

tisza_pole_df_org_wstr_org<- subset(tisza_pole_df_org_wstr, select = c(1:4,20,26,25,18,24,21,22))
ferto_pole_df_wstr_org<- subset(ferto_pole_df_wstr, select = c(1:4,20,26,25,18,24,21,22))
balaton_pole_df_wstr_org<- subset(balaton_pole_df_wstr, select = c(1:4,20,26,25,18,24,21,22))

CreateShape = function(data) {
  
  library(sp)
  
  data$X_obs=data$coords.x1
  data$Y_obs=data$coords.x2
  
  shp=data
  coordinates(shp)=~X_obs+Y_obs
  
  return(shp)
  
}

tisza_pole_df_org_wstr_org_shp=CreateShape(tisza_pole_df_org_wstr_org)
ferto_pole_df_wstr_org_shp=CreateShape(ferto_pole_df_wstr_org)
balaton_pole_df_wstr_org_shp=CreateShape(balaton_pole_df_wstr_org)

raster::shapefile(tisza_pole_df_org_wstr_org_shp, "tisza_pole_df_org_wstr_org.shp",overwrite=TRUE)
raster::shapefile(ferto_pole_df_wstr_org_shp, "ferto_pole_df_wstr_org.shp",overwrite=TRUE)
raster::shapefile(balaton_pole_df_wstr_org_shp, "balaton_pole_df_wstr_org.shp",overwrite=TRUE)