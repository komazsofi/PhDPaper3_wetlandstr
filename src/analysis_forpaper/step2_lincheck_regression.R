library(ggplot2)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import
  
plot_data=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))

plot_data$grpoints=plot_data$nofallp-plot_data$nofveg
plot_data_filt=plot_data[plot_data$nofveg>2,]

##### Visualization  

ggplot(data=plot_data_filt,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=grpoints))+theme_minimal(base_size=12)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=3)+
  xlab("H_max (LiDAR)")+ylab("Vegetation height (field)")+
  ggtitle("Estimation of vegetation height")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

##### Modelling
# vegetation height

model_all=lm(veg_height_m ~ Scaled_H_max+Scaled_H_q25.25.+Scaled_V_ku+Scaled_A_std+Scaled_A_cover, data = plot_data_filt)
summary(model_all) 

#AIC model selection (step)
model_all_step<-step(model_all)
summary(model_all_step)

# check linearity
par(mfrow = c(2, 2))
plot(model_all_step)
