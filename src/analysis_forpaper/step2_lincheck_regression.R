library(ggplot2)
library(dplyr)
library(tidyr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import
  
plot_data=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))

plot_data$grpoints=plot_data$nofallp-plot_data$nofveg
plot_data_filt=plot_data[plot_data$nofveg>2,]

##### Visualization  

plot_data_filt_c=plot_data_filt[c(20:24,11,15,18,9)]
plot_data_filt_c_vis=plot_data_filt_c %>% gather(-c(total.weight,lake,veg_type_2,nofveg),key = "var", value = "value")

ggplot(data=plot_data_filt_c_vis, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2)) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics") 

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

# Import

pole_data=read.csv(paste("Pole_noncorr",5,".csv",sep=""))

pole_data_filt=pole_data[pole_data$nofveg>2,]

##### Visualization  

pole_data_filt_c=pole_data_filt[c(15:19,8,10,13,14)]
pole_data_filt_c_vis=pole_data_filt_c %>% gather(-c(gct_lai,lake,class,nofveg),key = "var", value = "value")

ggplot(data=pole_data_filt_c_vis, aes(x=value , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=class)) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LAI (field)") +
  xlab("LiDAR metrics") 
