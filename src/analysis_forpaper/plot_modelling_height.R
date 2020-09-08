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

plot_0.5=read.csv("Plot_db_0.5_filt.csv")

# Height

# Visualize
ggplot(data=plot_0.5,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

ggplot(data=plot_0.5[plot_0.5$lake!="Lake Balaton",],aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

# Model
lm=lm(veg_height_m~H_max,data=plot_0.5)
summary(lm)

# Filters

lm=lm(veg_height_m~H_max,data=plot_0.5[plot_0.5$OBJNAME!=160,])
summary(lm)

lm=lm(veg_height_m~H_max,data=plot_0.5[plot_0.5$lake!="Lake Balaton",])
summary(lm)