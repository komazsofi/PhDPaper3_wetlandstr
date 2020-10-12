library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(corrplot)
library(multcompView)

library(pdp)

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis8/"
#workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis8/"
setwd(workdir)

####################################### Import

plot_data05=read.csv(paste("Plot_db_",0.5,".csv",sep=""))
plot_data05$total.weight=plot_data05$total.weight/10000

plot_data05_scaled=scale(plot_data05[,c(3:10)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[(plot_data05_f$OBJNAME!=120 & plot_data05_f$OBJNAME!=209 & plot_data05_f$OBJNAME!=163),]

plot_data5=read.csv(paste("Plot_db_",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

plot_data5_scaled=scale(plot_data5[,c(3:10)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
plot_data5_f=plot_data5_f[(plot_data5_f$OBJNAME!=120 & plot_data5_f$OBJNAME!=209 & plot_data5_f$OBJNAME!=163),]

pole_data5=read.csv(paste("Pole_db_",5,"_filt.csv",sep=""))

pole_data5_scaled=scale(pole_data5[,c(4:11,18)])
colnames(pole_data5_scaled)=paste("Scaled_",colnames(pole_data5_scaled),sep="")
pole_data5_f=cbind(pole_data5,pole_data5_scaled)
pole_data5_f=pole_data5_f[pole_data5_f$OBJNAME!=187,]

####################################### Visualization best fit for fwf and all plot (predicted vs. actual)

# height

model_all_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data05_f)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

plot_data05_f$predicted_h=predict(model_all_step_h05)
plot_data05_f$predicted_h_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h05)

j=ggplot(data=plot_data05_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k=ggplot(data=plot_data05_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

##### biomass

model_all_b5=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# fwf
model_fwf_b5=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

plot_data5_f$predicted_h=predict(model_all_step_b5)
plot_data5_f$predicted_h_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",30]=predict(model_fwf_step_b5)

jb=ggplot(data=plot_data5_f,aes(x=predicted_h,y=total.weight))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

kb=ggplot(data=plot_data5_f,aes(x=predicted_h_fwf,y=total.weight))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linetype = "dashed")+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

##### LAI

# all
model_all_h5=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = pole_data5_f)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std+Scaled_W_echw, data =pole_data5_f[pole_data5_f$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

# fwfl
model_fwf_h5l=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std+Scaled_W_echw, data =pole_data5_f[pole_data5_f$lake=="Lake Ferto",])
summary(model_fwf_h5l) 

#AIC model selection (step)
model_fwf_step_h5l<-step(model_fwf_h5l,direction = "backward")
summary(model_fwf_step_h5l)

# fwfh
model_fwf_h5h=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std+Scaled_W_echw, data =pole_data5_f[pole_data5_f$lake=="Lake Tisza",])
summary(model_fwf_h5h) 

#AIC model selection (step)
model_fwf_step_h5h<-step(model_fwf_h5h,direction = "backward")
summary(model_fwf_step_h5h)

pole_data5_f$predicted_fwf<-NA
pole_data5_f$predicted_fwfl<-NA
pole_data5_f$predicted_fwfh<-NA
pole_data5_f[pole_data5_f$lake!="Lake Balaton",29]=predict(model_fwf_step_h5)
pole_data5_f[pole_data5_f$lake=="Lake Ferto",30]=predict(model_fwf_step_h5l)
pole_data5_f[pole_data5_f$lake=="Lake Tisza",31]=predict(model_fwf_step_h5h)
pole_data5_f$predicted_all=predict(model_all_step_h5)

g5=ggplot(data=pole_data5_f,aes(x=predicted_all,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

j5=ggplot(data=pole_data5_f,aes(x=predicted_fwf,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linetype = "dashed")+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5=ggplot(data=pole_data5_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

l5=ggplot(data=pole_data5_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

####################################### Visualization boxplot per important metrics

a=ggplot(data=plot_data05_f,aes(x=lake,y=V_var,fill=lake))+geom_boxplot()+theme_bw(base_size = 20)
b=ggplot(data=plot_data05_f,aes(x=lake,y=A_std,fill=lake))+geom_boxplot()+theme_bw(base_size = 20)

grid.arrange(a,b,
             nrow = 1,
             ncol = 2
)

c=ggplot(data=plot_data5_f,aes(x=lake,y=C_ppr,fill=lake))+geom_boxplot()+theme_bw(base_size = 20)



