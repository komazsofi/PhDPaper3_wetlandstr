library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

# Import
  
plot_data=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))

##### Visualization  

plot_data_c=plot_data[c(2:7,11,13,15,18,9)]
plot_data_c[plot_data_c$W_echw==0,6]<- NA
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
# we expect that we have minimum 4 pt/m2 otherwise most of the lidar metrics makes not much sense

plot_data_f=plot_data[(plot_data$nofallp>3 & plot_data$nofveg>1),]

# vegetation height

# FWF
model_fwf=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw, data = plot_data_f)
summary(model_fwf) 

#AIC model selection (step)
model_fwf_step<-step(model_fwf,direction = "backward")
summary(model_fwf_step)

# biomass

# all
model_all_b=lm(total.weight ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data)
summary(model_all_b) 

#AIC model selection (step)
model_all_step_b<-step(model_all_b,direction = "backward")
summary(model_all_step_b)

# simple model
lm_all_b=lm(total.weight ~ H_max, data = plot_data)

# FWF
model_fwf_b=lm(total.weight ~ H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw, data = plot_data[plot_data$lake!="Lake Balaton",])
summary(model_fwf_b) 

#AIC model selection (step)
model_fwf_step_b<-step(model_fwf_b,direction = "backward")
summary(model_fwf_step_b)

# simple model
lm_fwf_b=lm(total.weight ~ H_max, data = plot_data[plot_data$lake!="Lake Balaton",])

# nonFWF
model_nfwf_b=lm(total.weight ~ H_max + H_q25.25. + A_cover, data = plot_data[plot_data$lake=="Lake Balaton",])
summary(model_nfwf_b)

lm_nfwf_b=lm(total.weight ~ H_max, data = plot_data[plot_data$lake=="Lake Balaton",])

# export results

stargazer(model_fwf_step, model_nfwf, model_all_step, title="Vegetation height", align=TRUE,type="html",column.labels=c("fwf","discrete","all"),out="height_aic.doc")
stargazer(lm_fwf_h, lm_nfwf_h, lm_all_h, title="Vegetation height", align=TRUE,type="html",column.labels=c("fwf","discrete","all"),out="height_exp.doc")


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
