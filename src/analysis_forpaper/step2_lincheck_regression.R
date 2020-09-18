library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot
  
plot_data05=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))
plot_data2=read.csv(paste("Plot_noncorr",2,".csv",sep=""))
plot_data5=read.csv(paste("Plot_noncorr",5,".csv",sep=""))

##### Visualization

# 0.5 m

plot_data_c05=plot_data05[c(2:7,11,13,15,18,10,8)]
plot_data_c05[plot_data_c05$W_echw==0,6]<- NA
plot_data_vis05=plot_data_c05 %>% gather(-c(veg_height_m,total.weight,lake,veg_type_2,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=plot_data_vis05, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating vegetation height (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data_vis05, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating biomass (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

# 2 m

plot_data_c2=plot_data2[c(2:6,7,9,10,12,14,17)]
plot_data_c2[plot_data_c2$W_echw==0,5]<- NA
plot_data_vis2=plot_data_c2 %>% gather(-c(veg_height_m,total.weight,lake,veg_type_2,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=plot_data_vis2, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating vegetation height (2 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data_vis2, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating biomass (2 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

# 5 m

plot_data_c5=plot_data5[c(2:7,8,10,11,13,15,18)]
plot_data_c5[plot_data_c5$W_echw==0,6]<- NA
plot_data_vis5=plot_data_c5 %>% gather(-c(veg_height_m,total.weight,lake,veg_type_2,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=plot_data_vis5, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating vegetation height (5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data_vis5, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating biomass (5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

