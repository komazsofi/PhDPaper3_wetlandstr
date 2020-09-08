library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis6/"
setwd(workdir)

# Import

balaton_m=read.csv("Balaton_lidarmetrics_5_reclass2.csv")
tisza_m=read.csv("Tisza_lidarmetrics_5_reclass2.csv")
ferto_m=read.csv("Ferto_lidarmetrics_5_reclass2.csv")

plotdata=read.csv("data_quadtrat_tolidar_forarticle.csv")

fieldsp = readOGR(dsn="tisza_full.shp")
fieldsp_df=fieldsp@data

fieldsp_df_sel=fieldsp_df[,c(1,2)]

tisza_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/tisza_full.shp")
ferto_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/w_point.shp")
balaton_pole = readOGR(dsn="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/field_data/w_point_balaton.shp")

tisza_pole_df=as.data.frame(tisza_pole)
ferto_pole_df=as.data.frame(ferto_pole)
balaton_pole_df=as.data.frame(balaton_pole)

tisza_pole_df_min=tisza_pole_df[c(1,21,22)]
ferto_pole_df_min=ferto_pole_df[c(1,23,24)]
balaton_pole_df_min=balaton_pole_df[c(1,23,24)]

# cleaining

balaton_m_c=balaton_m[complete.cases(balaton_m), ]
tisza_m_c=tisza_m[complete.cases(tisza_m), ]
ferto_m_c=ferto_m[complete.cases(ferto_m), ]

# merge

balaton_plot=merge(balaton_m_c,plotdata, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))
ferto_plot=merge(ferto_m_c,plotdata, by.x=c('point_ID','point_name'), by.y=c('point_ID','point_name'))

tisza_1=merge(tisza_m_c,fieldsp_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot=merge(tisza_1,plotdata, by.x=c('pont_nm'), by.y=c('point_name'))

tisza_plot=tisza_plot[tisza_plot$location!="sajkod",]
tisza_plot=tisza_plot[tisza_plot$location!="hegyko island",]
tisza_plot=tisza_plot[tisza_plot$location!="máriafürdo",]
tisza_plot=tisza_plot[tisza_plot$location!="kenese",]

balaton_plot=balaton_plot[c(4,5:18,24,25,26,27,28)]
ferto_plot=ferto_plot[c(4,5:18,24,25,26,27,28)]
tisza_plot=tisza_plot[c(2,4:17,24,25,26,27,28)]

# add coords
balaton_plot_coord=merge(balaton_plot,balaton_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot_coord=merge(ferto_plot,ferto_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot_coord=merge(tisza_plot,tisza_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot_coord$lake="Lake Balaton"
ferto_plot_coord$lake="Lake Ferto"
tisza_plot_coord$lake="Lake Tisza"

merged=rbind(balaton_plot_coord,ferto_plot_coord,tisza_plot_coord)

merged=merged[merged$veg_type_2!="schoenoplectus",]
merged=merged[merged$veg_type_2!="scirpus",]

write.csv(merged,"Plot_db_5.csv")
