library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)
library(lme4)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

# Import
  
plot_data=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))

##### Visualization  

plot_data_c=plot_data[c(2:7,11,13,15,18,9)]
plot_data_vis=plot_data_c %>% gather(-c(veg_height_m,total.weight,lake,veg_type_2,nofveg),key = "var", value = "value")

ggplot(data=plot_data_vis, aes(x=value , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2)) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Vegetation height (field)") +
  xlab("LiDAR metrics") 

ggplot(data=plot_data_vis, aes(x=value , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2)) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Biomass (field)") +
  xlab("LiDAR metrics") 

##### Modelling
# vegetation height

# all
model_all=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data)
summary(model_all) 

#AIC model selection (step)
model_all_step<-step(model_all,direction = "backward")
summary(model_all_step)

# FWF
model_fwf=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw, data = plot_data[plot_data$lake!="Lake Balaton",])
summary(model_fwf) 

#AIC model selection (step)
model_fwf_step<-step(model_fwf,direction = "backward")
summary(model_fwf_step)

# nonFWF
model_nfwf=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data[plot_data$lake=="Lake Balaton",])
summary(model_nfwf) 

stargazer(model_all_step, model_fwf_step,title="Vegetation height", align=TRUE,type="text",column.labels=c("all","fwf"))

# dependence

plot_data$pdens_cat[plot_data$nofveg < 5] <- "low"
plot_data$pdens_cat[plot_data$nofveg > 5] <- "high"

plot_data$iswater[plot_data$water_dept...8 < 5] <- "no"
plot_data$iswater[plot_data$water_dept...8 > 5] <- "yes"

mixed.lmer_h <- lmer(veg_height_m  ~ H_max+V_ku+A_cover + (1|pdens_cat), data = plot_data)
summary(mixed.lmer_h)
mixed.lmer_h2 <- lmer(veg_height_m  ~ H_max+V_ku+A_cover + (1|lake), data = plot_data)
summary(mixed.lmer_h2)
mixed.lmer_h3 <- lmer(veg_height_m  ~ H_max+V_ku+A_cover + (1|veg_type_2), data = plot_data)
summary(mixed.lmer_h3)
mixed.lmer_h4 <- lmer(veg_height_m  ~ H_max+V_ku+A_cover + (1|iswater), data = plot_data)
summary(mixed.lmer_h4)

####################################### Pole

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
