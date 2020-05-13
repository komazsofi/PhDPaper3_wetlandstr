library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis3/"
setwd(workdir)

# Import

tisza_2m_r=read.csv("tisza_2m_r_v2.csv")
ferto_2m_r=read.csv("ferto_2m_r_v2.csv")
balaton_2m_r=read.csv("balaton_2m_r_v2.csv")

plotdata=read.csv("data_quadtrat_tolidar.csv")

# add lidar to plot

balaton_plot=merge(balaton_2m_r,plotdata, by.x=c('pont_ID','pont_nm'), by.y=c('point_ID','point_name'))
ferto_plot=merge(ferto_2m_r,plotdata, by.x=c('pont_ID','pont_nm'), by.y=c('point_ID','point_name'))
tisza_plot=merge(tisza_2m_r,plotdata, by.x=c('pont_nm'), by.y=c('point_name'))

tisza_plot=tisza_plot[tisza_plot$location!="sajkod",]
tisza_plot=tisza_plot[tisza_plot$location!="hegyko island",]
tisza_plot=tisza_plot[tisza_plot$location!="máriafürdo",]
tisza_plot=tisza_plot[tisza_plot$location!="kenese",]

# export
write.csv(balaton_plot,"balaton_plot.csv")
write.csv(ferto_plot,"ferto_plot.csv")
write.csv(tisza_plot,"tisza_plot.csv")