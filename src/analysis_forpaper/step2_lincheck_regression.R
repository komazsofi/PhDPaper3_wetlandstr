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

plot_data05$total.weight=plot_data05$total.weight/10000
plot_data2$total.weight=plot_data2$total.weight/10000
plot_data5$total.weight=plot_data5$total.weight/10000

##### Visualization

# 0.5 m

plot_data_c05=plot_data05[c(2:6,7,9,10,12,14,17)]
plot_data_c05[plot_data_c05$W_echw==0,5]<- NA
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

plot_data05=plot_data05[(plot_data05$OBJNAME!=209 & plot_data05$OBJNAME!=160 & plot_data05$OBJNAME!=120),]

# height

# all
model_all_h05=lm(veg_height_m ~ H_max+V_ku+A_std+A_cover, data = plot_data05)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# FWF
model_fwf_h05=lm(veg_height_m ~ H_max+V_ku+A_std+A_cover+W_echw, data = plot_data05[plot_data05$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

# nonFWF
model_nfwf_h05=lm(veg_height_m ~ H_max+V_ku+A_std+A_cover, data = plot_data05[plot_data05$lake=="Lake Balaton",])
summary(model_nfwf_h05)

#AIC model selection (step)
model_nfwf_step_h05<-step(model_nfwf_h05,direction = "backward")
summary(model_nfwf_step_h05)

# biomass

# all
model_all_b05=lm(total.weight ~ H_max+V_ku+A_std+A_cover, data = plot_data05)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# FWF
model_fwf_b05=lm(total.weight ~ H_max+V_ku+A_std+A_cover+W_echw, data = plot_data05[plot_data05$lake!="Lake Balaton",])
summary(model_fwf_b05) 

#AIC model selection (step)
model_fwf_step_b05<-step(model_fwf_b05,direction = "backward")
summary(model_fwf_step_b05)

# nonFWF
model_nfwf_b05=lm(total.weight~ H_max+V_ku+A_std+A_cover, data = plot_data05[plot_data05$lake=="Lake Balaton",])
summary(model_nfwf_b05)

#AIC model selection (step)
model_nfwf_step_b05<-step(model_nfwf_b05,direction = "backward")
summary(model_nfwf_step_b05)


stargazer(model_nfwf_h05, model_fwf_step_h05, model_all_h05, model_nfwf_b05, model_fwf_step_b05, model_all_step_b05,align=TRUE,type="html",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="0.5 m",out="height_biomass_05.doc")

# 2 m

plot_data_c2=plot_data2[c(2:5,6,8,9,11,13,16)]
plot_data_c2[plot_data_c2$W_echw==0,4]<- NA
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

plot_data2=plot_data2[(plot_data2$OBJNAME!=209 & plot_data2$OBJNAME!=160 & plot_data2$OBJNAME!=120),]
# height

# all
model_all_h2=lm(veg_height_m ~ H_median+A_med+A_cover, data = plot_data2)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# FWF
model_fwf_h2=lm(veg_height_m ~ H_median+A_med+A_cover+W_echw, data = plot_data2[plot_data2$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

# nonFWF
model_nfwf_h2=lm(veg_height_m ~ H_median+A_med+A_cover, data = plot_data2[plot_data2$lake=="Lake Balaton",])
summary(model_nfwf_h2)

#AIC model selection (step)
model_nfwf_step_h2<-step(model_nfwf_h2,direction = "backward")
summary(model_nfwf_step_h2)

# biomass

# all
model_all_b2=lm(total.weight ~ H_median+A_med+A_cover, data = plot_data2)
summary(model_all_b) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# FWF
model_fwf_b2=lm(total.weight ~ H_median+A_med+A_cover+W_echw, data = plot_data2[plot_data2$lake!="Lake Balaton",])
summary(model_fwf_b2) 

#AIC model selection (step)
model_fwf_step_b2<-step(model_fwf_b2,direction = "backward")
summary(model_fwf_step_b2)

# nonFWF
model_nfwf_b2=lm(total.weight~ H_median+A_med+A_cover, data = plot_data2[plot_data2$lake=="Lake Balaton",])
summary(model_nfwf_b2)

#AIC model selection (step)
model_nfwf_step_b2<-step(model_nfwf_b2,direction = "backward")
summary(model_nfwf_step_b2)

stargazer(model_nfwf_step_h2, model_fwf_step_h2, model_all_h2, model_nfwf_step_b2, model_fwf_step_b2, model_all_step_b2,align=TRUE,type="html",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="2 m",out="height_biomass_2.doc")

# 5 m

plot_data_c5=plot_data5[c(2:6,7,9,10,12,14,17)]
plot_data_c5[plot_data_c5$W_echw==0,5]<- NA
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

plot_data5=plot_data5[(plot_data5$OBJNAME!=209 & plot_data5$OBJNAME!=160 & plot_data5$OBJNAME!=120),]

# height

# all
model_all_h5=lm(veg_height_m ~ V_var+A_std+A_med+A_cover, data = plot_data5)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# FWF
model_fwf_h5=lm(veg_height_m ~ V_var+A_std+A_med+A_cover+W_echw, data = plot_data5[plot_data5$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

# nonFWF
model_nfwf_h5=lm(veg_height_m ~ V_var+A_std+A_med+A_cover, data = plot_data5[plot_data5$lake=="Lake Balaton",])
summary(model_nfwf_h5)

#AIC model selection (step)
model_nfwf_step_h5<-step(model_nfwf_h5,direction = "backward")
summary(model_nfwf_step_h5)

# biomass

# all
model_all_b5=lm(total.weight ~ V_var+A_std+A_med+A_cover, data = plot_data5)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# FWF
model_fwf_b5=lm(total.weight ~ V_var+A_std+A_med+A_cover+W_echw, data = plot_data5[plot_data5$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

# nonFWF
model_nfwf_b5=lm(total.weight~ V_var+A_std+A_med+A_cover, data = plot_data5[plot_data5$lake=="Lake Balaton",])
summary(model_nfwf_b5)

#AIC model selection (step)
model_nfwf_step_b5<-step(model_nfwf_b5,direction = "backward")
summary(model_nfwf_step_b5)

stargazer(model_nfwf_step_h5, model_fwf_step_h5, model_all_h5, model_nfwf_step_b5, model_fwf_step_b5, model_all_step_b5,align=TRUE,type="html",column.labels=c("discrete","fwf","all","discrete","fwf","all"),title="5 m",out="height_biomass_5.doc")

####################################### Pole

pole_data05=read.csv(paste("Pole_noncorr",0.5,".csv",sep=""))
pole_data2=read.csv(paste("Pole_noncorr",2,".csv",sep=""))
pole_data5=read.csv(paste("Pole_noncorr",5,".csv",sep=""))

##### Visualization

# 0.5 m

pole_data05_c=pole_data05[c(2:6,7,9,10,13,14)]
pole_data05_c[pole_data05_c$W_echw==0,5]<- NA
pole_data05_c_vis=pole_data05_c %>% gather(-c(gct_lai,lake,class,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=pole_data05_c_vis, aes(x=value , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=class),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Leaf area (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating leaf area (0.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

# all
model_all_lai05=lm(gct_lai ~ A_med+C_ppr+V_ku+V_var, data = pole_data05)
summary(model_all_lai05) 

#AIC model selection (step)
model_all_lai05_step<-step(model_all_lai05,direction = "backward")
summary(model_all_lai05_step)

# fwf high
model_fwfh_lai05=lm(gct_lai ~ A_med+C_ppr+V_ku+V_var+W_echw, data = pole_data05[pole_data05$lake=="Lake Tisza",])
summary(model_fwfh_lai05) 

#AIC model selection (step)
model_fwfh_lai05_step<-step(model_fwfh_lai05,direction = "backward")
summary(model_fwfh_lai05_step)

# fwf low
model_fwfl_lai05=lm(gct_lai ~ A_med+C_ppr+V_ku+V_var+W_echw, data = pole_data05[pole_data05$lake=="Lake Ferto",])
summary(model_fwfl_lai05) 

#AIC model selection (step)
model_fwfl_lai05_step<-step(model_fwfl_lai05,direction = "backward")
summary(model_fwfl_lai05_step)

# fwf all
model_fwf_lai05=lm(gct_lai ~ A_med+C_ppr+V_ku+V_var+W_echw, data = pole_data05[pole_data05$lake!= "Lake Balaton",])
summary(model_fwf_lai05) 

#AIC model selection (step)
model_fwf_lai05_step<-step(model_fwf_lai05,direction = "backward")
summary(model_fwf_lai05_step)

# non-fwf
model_fwfn_lai05=lm(gct_lai ~ A_med+C_ppr+V_ku+V_var, data = pole_data05[pole_data05$lake=="Lake Balaton",])
summary(model_fwfn_lai05) 

#AIC model selection (step)
model_fwfn_lai05_step<-step(model_fwfn_lai05,direction = "backward")
summary(model_fwfn_lai05_step)

stargazer(model_fwfn_lai05,model_fwfh_lai05_step,model_fwfl_lai05_step,model_fwf_lai05_step,model_all_lai05_step,align=TRUE,type="html",column.labels=c("discrete","fwf (20 pt/m2)","fwf (4 pt/m2)", "fwf","all"),title="0.5 m",out="lai_05.doc")

# 2 m

pole_data2_c=pole_data2[c(2:6,7,9,10,13,14)]
pole_data2_c[pole_data2_c$W_echw==0,5]<- NA
pole_data2_c_vis=pole_data2_c %>% gather(-c(gct_lai,lake,class,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=pole_data2_c_vis, aes(x=value , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=class),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Leaf area (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating leaf area (2 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

# all
model_all_lai2=lm(gct_lai ~ A_med+A_cover+V_ku+V_var, data = pole_data2)
summary(model_all_lai2) 

#AIC model selection (step)
model_all_lai2_step<-step(model_all_lai2,direction = "backward")
summary(model_all_lai2_step)

# fwf high
model_fwfh_lai2=lm(gct_lai ~ A_med+A_cover+V_ku+V_var+W_echw, data = pole_data2[pole_data2$lake=="Lake Tisza",])
summary(model_fwfh_lai2) 

#AIC model selection (step)
model_fwfh_lai2_step<-step(model_fwfh_lai2,direction = "backward")
summary(model_fwfh_lai2_step)

# fwf low
model_fwfl_lai2=lm(gct_lai ~ A_med+A_cover+V_ku+V_var+W_echw, data = pole_data2[pole_data2$lake=="Lake Ferto",])
summary(model_fwfl_lai2) 

#AIC model selection (step)
model_fwfl_lai2_step<-step(model_fwfl_lai2,direction = "backward")
summary(model_fwfl_lai2_step)

# fwf all
model_fwf_lai2=lm(gct_lai ~ A_med+A_cover+V_ku+V_var+W_echw, data = pole_data2[pole_data2$lake!= "Lake Balaton",])
summary(model_fwf_lai2) 

#AIC model selection (step)
model_fwf_lai2_step<-step(model_fwf_lai2,direction = "backward")
summary(model_fwf_lai2_step)

# non-fwf
model_fwfn_lai2=lm(gct_lai ~ A_med+A_cover+V_ku+V_var, data = pole_data2[pole_data2$lake=="Lake Balaton",])
summary(model_fwfn_lai2) 

#AIC model selection (step)
model_fwfn_lai2_step<-step(model_fwfn_lai2,direction = "backward")
summary(model_fwfn_lai2_step)

stargazer(model_fwfn_lai2,model_fwfh_lai2_step,model_fwfl_lai2_step,model_fwf_lai2_step,model_all_lai2_step,align=TRUE,type="html",column.labels=c("discrete","fwf (20 pt/m2)","fwf (4 pt/m2)", "fwf","all"),title="2 m",out="lai_2.doc")

# 5 m

pole_data5_c=pole_data5[c(2:6,7,9,10,13,14)]
pole_data5_c[pole_data5_c$W_echw==0,5]<- NA
pole_data5_c_vis=pole_data5_c %>% gather(-c(gct_lai,lake,class,nofallp,OBJNAME),key = "var", value = "value")

ggplot(data=pole_data5_c_vis, aes(x=value , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=class),size=2.5) +
  facet_wrap(~var,scales = "free") +
  theme_bw() +
  ylab("Leaf area (field)") +
  xlab("LiDAR metrics")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("Non-correlated metrics for estimating leaf area (5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

# all
model_all_lai5=lm(gct_lai ~ A_med+A_cover+H_max+V_ku, data = pole_data5)
summary(model_all_lai5) 

#AIC model selection (step)
model_all_lai5_step<-step(model_all_lai5,direction = "backward")
summary(model_all_lai5_step)

# fwf high
model_fwfh_lai5=lm(gct_lai ~ A_med+A_cover+H_max+V_ku+W_echw, data = pole_data5[pole_data5$lake=="Lake Tisza",])
summary(model_fwfh_lai5) 

#AIC model selection (step)
model_fwfh_lai5_step<-step(model_fwfh_lai5,direction = "backward")
summary(model_fwfh_lai5_step)

# fwf low
model_fwfl_lai5=lm(gct_lai ~ A_med+A_cover+H_max+V_ku+W_echw, data = pole_data5[pole_data5$lake=="Lake Ferto",])
summary(model_fwfl_lai5) 

#AIC model selection (step)
model_fwfl_lai5_step<-step(model_fwfl_lai5,direction = "backward")
summary(model_fwfl_lai5_step)

# fwf all
model_fwf_lai5=lm(gct_lai ~ A_med+A_cover+H_max+V_ku+W_echw, data = pole_data5[pole_data5$lake!= "Lake Balaton",])
summary(model_fwf_lai5) 

#AIC model selection (step)
model_fwf_lai5_step<-step(model_fwf_lai5,direction = "backward")
summary(model_fwf_lai5_step)

# non-fwf
model_fwfn_lai5=lm(gct_lai ~ A_med+A_cover+H_max+V_ku, data = pole_data5[pole_data5$lake=="Lake Balaton",])
summary(model_fwfn_lai5) 

#AIC model selection (step)
model_fwfn_lai5_step<-step(model_fwfn_lai5,direction = "backward")
summary(model_fwfn_lai5_step)

stargazer(model_fwfn_lai5,model_fwfh_lai5_step,model_fwfl_lai5_step,model_fwf_lai5_step,model_all_lai5_step,align=TRUE,type="html",column.labels=c("discrete","fwf (20 pt/m2)","fwf (4 pt/m2)", "fwf","all"),title="5 m",out="lai_5.doc")
