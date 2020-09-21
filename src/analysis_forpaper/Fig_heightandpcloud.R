library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

plot_data05=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))
plot_data05=plot_data05[(plot_data05$OBJNAME!=209 & plot_data05$OBJNAME!=160 & plot_data05$OBJNAME!=120),]

##### Visualization

ggplot(data=plot_data05,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_classic(base_size=20)+
  xlab("H_max (LiDAR, res.=0.5 m)")+ylab("Vegetation height [m] (field)")+
  ggtitle("Most important metric for estimating vegetation height")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))
