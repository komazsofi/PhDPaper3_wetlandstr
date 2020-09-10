library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)
rad=0.5

# Import

balaton_m=read.csv(paste("Balaton_lidarmetrics_",rad,"_reclass2.csv",sep=""))
tisza_m=read.csv(paste("Tisza_lidarmetrics_",rad,"_reclass2.csv",sep=""))
ferto_m=read.csv(paste("Ferto_lidarmetrics_",rad,"_reclass2.csv",sep=""))

plotdata=read.csv("data_quadtrat_tolidar_forarticle.csv")

fieldsp = readOGR(dsn="tisza_full.shp")
fieldsp_df=fieldsp@data

fieldsp_df_sel=fieldsp_df[,c(1,2)]

tisza_pole = readOGR(dsn="tisza_full.shp")
ferto_pole = readOGR(dsn="w_point.shp")
balaton_pole = readOGR(dsn="w_point_balaton.shp")

tisza_pole_df=as.data.frame(tisza_pole)
ferto_pole_df=as.data.frame(ferto_pole)
balaton_pole_df=as.data.frame(balaton_pole)

tisza_pole_df_min=tisza_pole_df[c(1,21,22)]
ferto_pole_df_min=ferto_pole_df[c(1,23,24)]
balaton_pole_df_min=balaton_pole_df[c(1,23,24)]

tisza_2m_r=read.csv("tisza_2m_r_v3.csv")
ferto_2m_r=read.csv("ferto_2m_r_v3.csv")

tisza_2m_r_min=tisza_2m_r[c(3,20)]
names(tisza_2m_r_min)<-c("OBJNAME","W_echw")
ferto_2m_r_min=ferto_2m_r[c(3,20)]
names(ferto_2m_r_min)<-c("OBJNAME","W_echw")

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

balaton_plot=balaton_plot[c(4,5:20,26,27,28,29,30)]
ferto_plot=ferto_plot[c(4,5:20,26,27,28,29,30)]
tisza_plot=tisza_plot[c(2,4:19,26,27,28,29,30)]

# add echowidth
ferto_plot2=merge(ferto_plot,ferto_2m_r_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot2=merge(tisza_plot,tisza_2m_r_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
balaton_plot$W_echw<-0

# add coords
balaton_plot_coord=merge(balaton_plot,balaton_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot_coord=merge(ferto_plot2,ferto_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot_coord=merge(tisza_plot2,tisza_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot_coord$lake="Lake Balaton"
ferto_plot_coord$lake="Lake Ferto"
tisza_plot_coord$lake="Lake Tisza"

merged=rbind(balaton_plot_coord,ferto_plot_coord,tisza_plot_coord)

merged=merged[merged$veg_type_2!="schoenoplectus",]
merged=merged[merged$veg_type_2!="scirpus",]

write.csv(merged,paste("Plot_db_",rad,".csv",sep=""))

# only 0.5

merged_filt05<-subset(merged, OBJNAME %in% c(120,123,124,160,204,209,131,163,170,198,200,203,251,255,317,321,325,118,122,186,187,188))
write.csv(merged_filt05,paste("Plot_db_",rad,"_filt.csv",sep=""))
