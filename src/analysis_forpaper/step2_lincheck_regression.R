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

plot_data05=plot_data05[plot_data05$OBJNAME!=120,]
plot_data2=plot_data2[plot_data2$OBJNAME!=120,]
plot_data5=plot_data5[plot_data5$OBJNAME!=120,]

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

# height

# all
model_all_h05=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data05)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# FWF
model_fwf_h05=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw, data = plot_data05[plot_data05$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

# nonFWF
model_nfwf_h05=lm(veg_height_m ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data05[plot_data05$lake=="Lake Balaton",])
summary(model_nfwf_h05)

#AIC model selection (step)
model_nfwf_step_h05<-step(model_nfwf_h05,direction = "backward")
summary(model_nfwf_step_h05)

stargazer(model_nfwf_h05, model_fwf_step_h05, model_all_h05,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="0.5 m")

# biomass

# all
model_all_b05=lm(total.weight ~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data05)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# FWF
model_fwf_b05=lm(total.weight ~ H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw, data = plot_data05[plot_data05$lake!="Lake Balaton",])
summary(model_fwf_b05) 

#AIC model selection (step)
model_fwf_step_b05<-step(model_fwf_b,direction = "backward")
summary(model_fwf_step_b05)

# nonFWF
model_nfwf_b05=lm(total.weight~ H_max+H_q25.25.+V_ku+A_std+A_cover, data = plot_data05[plot_data05$lake=="Lake Balaton",])
summary(model_nfwf_b05)

#AIC model selection (step)
model_nfwf_step_b05<-step(model_nfwf_b05,direction = "backward")
summary(model_nfwf_step_b05)

stargazer(model_nfwf_b05, model_fwf_step_b05, model_all_b05,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="0.5 m")
stargazer(model_nfwf_h05, model_fwf_step_h05, model_all_h05, model_nfwf_b05, model_fwf_step_b05, model_all_step_b05,align=TRUE,type="text",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="0.5 m")

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

# height

# all
model_all_h2=lm(veg_height_m ~ H_q25.25.+V_var+A_med+A_cover, data = plot_data2)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# FWF
model_fwf_h2=lm(veg_height_m ~ H_q25.25.+V_var+A_med+A_cover+W_echw, data = plot_data2[plot_data2$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

# nonFWF
model_nfwf_h2=lm(veg_height_m ~ H_q25.25.+V_var+A_med+A_cover, data = plot_data2[plot_data2$lake=="Lake Balaton",])
summary(model_nfwf_h2)

#AIC model selection (step)
model_nfwf_step_h2<-step(model_nfwf_h2,direction = "backward")
summary(model_nfwf_step_h2)

stargazer(model_nfwf_step_h2, model_fwf_step_h2, model_all_h2,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="2 m")

# biomass

# all
model_all_b2=lm(total.weight ~ H_q25.25.+V_var+A_med+A_cover, data = plot_data2)
summary(model_all_b) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# FWF
model_fwf_b2=lm(total.weight ~ H_q25.25.+V_var+A_med+A_cover+W_echw, data = plot_data2[plot_data2$lake!="Lake Balaton",])
summary(model_fwf_b2) 

#AIC model selection (step)
model_fwf_step_b2<-step(model_fwf_b2,direction = "backward")
summary(model_fwf_step_b2)

# nonFWF
model_nfwf_b2=lm(total.weight~ H_q25.25.+V_var+A_med+A_cover, data = plot_data2[plot_data2$lake=="Lake Balaton",])
summary(model_nfwf_b2)

#AIC model selection (step)
model_nfwf_step_b2<-step(model_nfwf_b2,direction = "backward")
summary(model_nfwf_step_b2)

stargazer(model_nfwf_step_b2, model_fwf_step_b2, model_all_b2,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="2 m")
stargazer(model_nfwf_step_h2, model_fwf_step_h2, model_all_h2, model_nfwf_step_b2, model_fwf_step_b2, model_all_step_b2,align=TRUE,type="text",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="2 m")

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

# height

# all
model_all_h5=lm(veg_height_m ~ H_max+V_ku+A_std+A_med+A_cover, data = plot_data5)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# FWF
model_fwf_h5=lm(veg_height_m ~ H_max+V_ku+A_std+A_med+A_cover+W_echw, data = plot_data5[plot_data5$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

# nonFWF
model_nfwf_h5=lm(veg_height_m ~ H_max+V_ku+A_std+A_med+A_cover, data = plot_data5[plot_data5$lake=="Lake Balaton",])
summary(model_nfwf_h5)

#AIC model selection (step)
model_nfwf_step_h5<-step(model_nfwf_h5,direction = "backward")
summary(model_nfwf_step_h5)

stargazer(model_nfwf_step_h5, model_fwf_step_h5, model_all_h5,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="5 m")

# biomass

# all
model_all_b5=lm(total.weight ~ H_max+V_ku+A_std+A_med+A_cover, data = plot_data5)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# FWF
model_fwf_b5=lm(total.weight ~ H_max+V_ku+A_std+A_med+A_cover+W_echw, data = plot_data5[plot_data5$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

# nonFWF
model_nfwf_b5=lm(total.weight~ H_max+V_ku+A_std+A_med+A_cover, data = plot_data5[plot_data5$lake=="Lake Balaton",])
summary(model_nfwf_b5)

#AIC model selection (step)
model_nfwf_step_b5<-step(model_nfwf_b5,direction = "backward")
summary(model_nfwf_step_b5)

stargazer(model_nfwf_step_b5, model_fwf_step_b5, model_all_b5,align=TRUE,type="text",column.labels=c("discrete","fwf","all"),title="5 m")
stargazer(model_nfwf_step_h5, model_fwf_step_h5, model_all_h5, model_nfwf_step_b5, model_fwf_step_b5, model_all_step_b5,align=TRUE,type="text",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="5 m")
