library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis5/"
setwd(workdir)

# Import

balaton_m=read.csv("Balaton_lidarmetrics_0.5.csv")
tisza_m=read.csv("Tisza_lidarmetrics_0.5.csv")
ferto_m=read.csv("Ferto_lidarmetrics_0.5.csv")

plotdata=read.csv("data_quadtrat_tolidar_2.csv")

fieldsp = readOGR(dsn="tisza_full.shp")
fieldsp_df=fieldsp@data

fieldsp_df_sel=fieldsp_df[,c(1,2)]

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

balaton_plot=balaton_plot[c(5:35)]
ferto_plot=ferto_plot[c(5:35)]
tisza_plot=tisza_plot[c(4:27,29:35)]

balaton_plot$lake="Lake Balaton"
ferto_plot$lake="Lake Ferto"
tisza_plot$lake="Lake Tisza"

merged=rbind(balaton_plot,ferto_plot,tisza_plot)

merged=merged[merged$veg_type_2!="schoenoplectus",]
merged=merged[merged$veg_type_2!="scirpus",]
merged=merged[merged$X.y!=11,]

# vegetation height

lm=lm(veg_height_m~H_max,data=merged)
summary(lm)

ggplot(data=merged,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=X.y),hjust=0, vjust=0)

# biomass

ggplot(data=merged,aes(x=A_std,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=X.y),hjust=0, vjust=0)