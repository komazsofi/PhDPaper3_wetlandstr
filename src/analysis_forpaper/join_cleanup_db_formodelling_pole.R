library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)
library(tidyverse)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
setwd(workdir)
rad=5

# Import

balaton_m=read.csv(paste("Balaton_lidarmetrics_",rad,"_reclass4.csv",sep=""))
tisza_m=read.csv(paste("Tisza_lidarmetrics_",rad,"_reclass4.csv",sep=""))
ferto_m=read.csv(paste("Ferto_lidarmetrics_",rad,"_reclass4.csv",sep=""))
tisza_m2=read.csv(paste("Tisza_lidarmetrics_",rad,"_leafon_reclass4.csv",sep=""))

tisza_pole = readOGR(dsn="tisza_full.shp")
ferto_pole = readOGR(dsn="w_point.shp")
balaton_pole = readOGR(dsn="w_point_balaton.shp")

tisza_pole_df=as.data.frame(tisza_pole)
ferto_pole_df=as.data.frame(ferto_pole)
balaton_pole_df=as.data.frame(balaton_pole)

tisza_pole_df_min=tisza_pole_df[c(1,17,21,22,19)]
ferto_pole_df_min=ferto_pole_df[c(1,20,23,24,22)]
balaton_pole_df_min=balaton_pole_df[c(1,20,23,24,22)]

# cleaining

balaton_m_c=balaton_m[complete.cases(balaton_m), ]
tisza_m_c=tisza_m[complete.cases(tisza_m), ]
ferto_m_c=ferto_m[complete.cases(ferto_m), ]
tisza_m_c2=tisza_m[complete.cases(tisza_m2), ]

# merge

balaton_plot=merge(balaton_m_c,balaton_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot=merge(tisza_m_c,tisza_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot=merge(ferto_m_c,ferto_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot_leafon=merge(tisza_m_c2,tisza_pole_df_min, by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot3=balaton_plot[c(1,2,5:18)]
ferto_plot3=ferto_plot[c(1,2,5:18)]

balaton_plot3$lake="Lake Balaton"
ferto_plot3$lake="Lake Ferto"
tisza_plot$lake="Lake Tisza"
tisza_plot_leafon$lake="Lake Tisza"

balaton_plot3$season="leaf-off"
ferto_plot3$season="leaf-off"
tisza_plot$season="leaf-off"
tisza_plot_leafon$season="leaf-on"

merged=rbind(balaton_plot3,ferto_plot3,tisza_plot,tisza_plot_leafon)

# clean

merged=merged[merged$class!="water",]
merged=merged[merged$class!="tree",]
merged=merged[merged$class!="land",]
merged=merged[merged$class!="grassland",]
merged=merged[merged$class!="scirpus",]
merged=merged[merged$class!="mudflat",]
merged=merged[merged$class!="artificial",]
merged=merged[merged$class!="shrub",]
merged=merged[merged$class!="not_known",]
merged=merged[merged$class!="flat",]
merged=merged[merged$class!="shrubs",]
merged=merged[merged$class!="trees",]

merged=merged[complete.cases(merged), ]
merged$class=str_replace(merged$class,"reed","phragmites")

merged_clean=merged %>% distinct()

write.csv(merged_clean,paste("Pole_db_",rad,"_filt.csv",sep=""))



