library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

library(corrplot)
library(usdm)

library(olsrr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import

pole_0.5=read.csv("Pole_db_5_filt.csv")
pole_0.5_filt=pole_0.5[pole_0.5$nofveg>2,]

# Modelling

#all
lm_all_lai=lm(gct_lai~H_max+H_q25.25.+V_ku+A_cover,data=pole_0.5_filt)
summary(lm_all_lai)

ols_step_forward_aic(lm_all_lai,details = TRUE)

#high pdens fwf
lm_all_lai=lm(gct_lai~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=pole_0.5_filt[pole_0.5_filt$lake=="Lake Tisza",])
summary(lm_all_lai)

ols_step_forward_aic(lm_all_lai,details = TRUE)

# Visualization

ggplot(data=pole_0.5_filt[pole_0.5_filt$lake=="Lake Tisza",],aes(x=H_max,y=gct_lai))+geom_point(aes(color=lake,shape=class,size=nofveg))+theme_minimal(base_size=12)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=3)+
  xlab("H_max (LiDAR)")+ylab("LAI (field)")+
  ggtitle("Estimation of leaf area")

ggplot(data=pole_0.5_filt[pole_0.5_filt$lake=="Lake Tisza",],aes(x=W_echw,y=gct_lai))+geom_point(aes(color=lake,shape=class,size=nofveg))+theme_minimal(base_size=12)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=3)+
  xlab("W_echw (LiDAR)")+ylab("LAI (field)")+
  ggtitle("Estimation of leaf area")