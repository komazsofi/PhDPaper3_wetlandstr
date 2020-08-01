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

balaton_m=read.csv("Balaton_lidarmetrics_1.csv")
tisza_m=read.csv("Tisza_lidarmetrics_1.csv")
ferto_m=read.csv("Ferto_lidarmetrics_1.csv")

fieldsp_tisza = readOGR(dsn="tisza_full.shp")
fieldsp_tisza_df=fieldsp_tisza@data
fieldsp_tisza_df_sel=fieldsp_tisza_df[,c(1,2,17,19)]

fieldsp_ferto = readOGR(dsn="w_point.shp")
fieldsp_ferto_df=fieldsp_ferto@data
fieldsp_ferto_df_sel=fieldsp_ferto_df[,c(1,2,20,22)]

fieldsp_balaton = readOGR(dsn="w_point_balaton.shp")
fieldsp_balaton_df=fieldsp_balaton@data
fieldsp_balaton_df_sel=fieldsp_balaton_df[,c(1,2,20,22)]

# cleaining

balaton_m_c=balaton_m[complete.cases(balaton_m), ]
tisza_m_c=tisza_m[complete.cases(tisza_m), ]
ferto_m_c=ferto_m[complete.cases(ferto_m), ]

# merge

balaton_plot=merge(balaton_m_c,fieldsp_balaton_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot=merge(tisza_m_c,fieldsp_tisza_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot=merge(ferto_m_c,fieldsp_ferto_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot=balaton_plot[c(5:23,25,26)]
ferto_plot=ferto_plot[c(5:23,25,26)]
tisza_plot=tisza_plot[c(3:21,23,24)]

balaton_plot$lake="Lake Balaton"
ferto_plot$lake="Lake Ferto"
tisza_plot$lake="Lake Tisza"

merged=rbind(balaton_plot,ferto_plot,tisza_plot)

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

# biomass

round(cor(merged[,c(1,7:19)], method="spearman"),2) # 

# linear regression across all lakes

lm_lai<-lm(gct_lai~V_std+V_cr+C_ppr2+A_mean,data=merged[merged$lake=="Lake Tisza",])
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

# visualization

ggplot(data=merged[merged$lake=="Lake Tisza",],aes(x=C_ppr2,y=gct_lai))+geom_point(aes(color=lake,shape=class),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)
