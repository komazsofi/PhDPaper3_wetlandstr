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

tisza_2m_r=read.csv("tisza_2m_r_v3.csv")
ferto_2m_r=read.csv("ferto_2m_r_v3.csv")
balaton_2m_r=read.csv("balaton_2m_r_v3.csv")

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
balaton_plot2=merge(balaton_plot,balaton_2m_r[c(3,19,25,17,22)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))
balaton_plot2$EchoWidth2EchoWidthmedian.grd<-0

tisza_plot=merge(tisza_m_c,fieldsp_tisza_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot2=merge(tisza_plot,tisza_2m_r[c(3,19,26,17,23,20)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))

ferto_plot=merge(ferto_m_c,fieldsp_ferto_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot2=merge(ferto_plot,ferto_2m_r[c(3,19,26,17,23,20)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot3=balaton_plot2[c(5:23,25:31)]
ferto_plot3=ferto_plot2[c(5:23,25:31)]
tisza_plot3=tisza_plot2[c(3:21,23:29)]

balaton_plot3$lake="Lake Balaton"
ferto_plot3$lake="Lake Ferto"
tisza_plot3$lake="Lake Tisza"

merged=rbind(balaton_plot3,ferto_plot3,tisza_plot3)

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

lm_lai<-lm(gct_lai~C_ppr2,data=merged)
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

# visualization
merged$logopp=log(merged$NormalizedZP95negOpennessdz.dz.grd)
merged=merged[complete.cases(merged), ]

ggplot(data=merged,aes(x=C_ppr2,y=gct_lai))+geom_point(aes(color=lake,shape=class),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)
