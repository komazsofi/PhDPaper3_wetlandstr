library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis8/"
setwd(workdir)

####################################### Visualization

pdf("plot_lidarmetrics_vis_0.5m.pdf")

plot_data05=read.csv(paste("Plot_db_",0.5,".csv",sep=""))
plot_data05$total.weight=plot_data05$total.weight/10000

plot_data_vis05=plot_data05 %>% gather(-c(X,veg_height_m,total.weight,lake,veg_type_2,
                                          nofveg,nofallp,OBJNAME,water_dept...8,sum_leaf_weight,coords.x1,coords.x2),key = "var", value = "value")

ggplot(data=plot_data_vis05, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("LiDAR metrics for estimating vegetation height (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data_vis05, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("LiDAR metrics for estimating biomass (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

plot_data05_scaled=scale(plot_data05[,c(3:10)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")

plot_data05_wscaled=cbind(plot_data05[,c(1,2,11,12,13,14,15,16,17,18,19,20)],plot_data05_scaled)

plot_data_vis05_scaled=plot_data05_wscaled %>% gather(-c(X,veg_height_m,total.weight,lake,veg_type_2,
                                                  nofveg,nofallp,OBJNAME,water_dept...8,sum_leaf_weight,coords.x1,coords.x2),key = "var", value = "value")

ggplot(data=plot_data_vis05_scaled, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Scaled LiDAR metrics for estimating vegetation height (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data_vis05_scaled, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Scaled LiDAR metrics for estimating biomass (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

dev.off()
