library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(olsrr)
library(corrplot)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis8/"
setwd(workdir)

####################################### Import

plot_data05=read.csv(paste("Pole_db_",0.5,"_filt.csv",sep=""))

plot_data05_scaled=scale(plot_data05[,c(4:11,18)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[plot_data05_f$OBJNAME!=187,]

plot_data2=read.csv(paste("Pole_db_",2.5,"_filt.csv",sep=""))

plot_data2_scaled=scale(plot_data2[,c(4:11,18)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
plot_data2_f=plot_data2_f[plot_data2_f$OBJNAME!=187,]

plot_data5=read.csv(paste("Pole_db_",5,"_filt.csv",sep=""))

plot_data5_scaled=scale(plot_data5[,c(4:11,18)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
plot_data5_f=plot_data5_f[(plot_data5_f$OBJNAME!=187 & plot_data5_f$OBJNAME!=250),]

####################################### Correlation check
col <- colorRampPalette(c("#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444"))

vifcorr_plot05=vifcor(plot_data05_scaled, th=0.6, method='spearman')
print(vifcorr_plot05)

corr05=round(cor(plot_data05_scaled, method="spearman"),2)

corrplot(corr05, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

vifcorr_plot2=vifcor(plot_data2_scaled, th=0.6, method='spearman')
print(vifcorr_plot2)

corr2=round(cor(plot_data2_scaled, method="spearman"),2)

corrplot(corr2, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

vifcorr_plot5=vifcor(plot_data5_scaled, th=0.6, method='spearman')
print(vifcorr_plot5)

corr5=round(cor(plot_data5_scaled, method="spearman"),2)

corrplot(corr5, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE)


####################################### Scaled fit and report

##### 0.5 m

##### lai

# all
model_all_h05=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data05_f)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

# fwfl
model_fwf_h05l=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data05_f[plot_data05_f$lake=="Lake Ferto",])
summary(model_fwf_h05l) 

#AIC model selection (step)
model_fwf_step_h05l<-step(model_fwf_h05l,direction = "backward")
summary(model_fwf_step_h05l)

# fwfh
model_fwf_h05h=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data05_f[plot_data05_f$lake=="Lake Tisza",])
summary(model_fwf_h05h) 

#AIC model selection (step)
model_fwf_step_h05h<-step(model_fwf_h05h,direction = "backward")
summary(model_fwf_step_h05h)

##### 2.5 m

##### lai

# all
model_all_h2=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data2_f)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# fwf
model_fwf_h2=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

# fwfl
model_fwf_h2l=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data2_f[plot_data2_f$lake=="Lake Ferto",])
summary(model_fwf_h2l) 

#AIC model selection (step)
model_fwf_step_h2l<-step(model_fwf_h2l,direction = "backward")
summary(model_fwf_step_h2l)

# fwfh
model_fwf_h2h=lm(gct_lai~ Scaled_V_var+Scaled_A_cover+Scaled_A_std+Scaled_W_echw, data =plot_data2_f[plot_data2_f$lake=="Lake Tisza",])
summary(model_fwf_h2h) 

#AIC model selection (step)
model_fwf_step_h2h<-step(model_fwf_h2h,direction = "backward")
summary(model_fwf_step_h2h)

##### 5 m

##### lai

# all
model_all_h5=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

# fwfl
model_fwf_h5l=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake=="Lake Ferto",])
summary(model_fwf_h5l) 

#AIC model selection (step)
model_fwf_step_h5l<-step(model_fwf_h5l,direction = "backward")
summary(model_fwf_step_h5l)

# fwfh
model_fwf_h5h=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake=="Lake Tisza",])
summary(model_fwf_h5h) 

#AIC model selection (step)
model_fwf_step_h5h<-step(model_fwf_h5h,direction = "backward")
summary(model_fwf_step_h5h)

# discrete
model_dr_h5=lm(gct_lai~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake=="Lake Balaton",])
summary(model_dr_h5) 

#AIC model selection (step)
model_dr_step_h5<-step(model_dr_h5,direction = "backward")
summary(model_dr_step_h5)

## report for study

stargazer(model_fwf_step_h05,model_fwf_step_h05l,model_fwf_step_h05h,model_fwf_step_h2,model_fwf_step_h2l,model_fwf_step_h2h,model_fwf_step_h5,model_fwf_step_h5l,model_fwf_step_h5h,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","fwf low pdens [0.5 m]","fwf high pdens [0.5 m]","fwf [2.5 m]","fwf low pdens [2.5 m]","fwf high pdens [2.5 m]","fwf [5 m]","fwf low pdens [5 m]","fwf high pdens [5 m]"),title="Estimation of leaf area at different resolutions",out="lai_report.doc")

###### predicted vs actual

plot_data05_f$predicted_fwf<-NA
plot_data05_f$predicted_fwfl<-NA
plot_data05_f$predicted_fwfh<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",29]=predict(model_fwf_step_h05)
plot_data05_f[plot_data05_f$lake=="Lake Ferto",30]=predict(model_fwf_step_h05l)
plot_data05_f[plot_data05_f$lake=="Lake Tisza",31]=predict(model_fwf_step_h05h)

j=ggplot(data=plot_data05_f,aes(x=predicted_fwf,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k=ggplot(data=plot_data05_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

l=ggplot(data=plot_data05_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data2_f$predicted_fwf<-NA
plot_data2_f$predicted_fwfl<-NA
plot_data2_f$predicted_fwfh<-NA
plot_data2_f[plot_data2_f$lake!="Lake Balaton",29]=predict(model_fwf_step_h2)
plot_data2_f[plot_data2_f$lake=="Lake Ferto",30]=predict(model_fwf_step_h2l)
plot_data2_f[plot_data2_f$lake=="Lake Tisza",31]=predict(model_fwf_step_h2h)

j2=ggplot(data=plot_data2_f,aes(x=predicted_fwf,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k2=ggplot(data=plot_data2_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

l2=ggplot(data=plot_data2_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data5_f$predicted_fwf<-NA
plot_data5_f$predicted_fwfl<-NA
plot_data5_f$predicted_fwfh<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",29]=predict(model_fwf_step_h5)
plot_data5_f[plot_data5_f$lake=="Lake Ferto",30]=predict(model_fwf_step_h5l)
plot_data5_f[plot_data5_f$lake=="Lake Tisza",31]=predict(model_fwf_step_h5h)

j5=ggplot(data=plot_data5_f,aes(x=predicted_fwf,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5=ggplot(data=plot_data5_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

l5=ggplot(data=plot_data5_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j,k,l,j2,k2,l2,j5,k5,l5,
             nrow = 3,
             ncol = 3
)

###### partial dependence

plot_data5_f$part_res_C_ppr_fwfl=NA
plot_data5_f$part_res_C_ppr_fwfl_y=NA
plot_data5_f$part_res_C_ppr_fwfl_x=NA

plot_data5_f[plot_data5_f$lake=="Lake Ferto",29] <- resid(model_fwf_step_h5l) + plot_data5_f[plot_data5_f$lake=="Lake Ferto",]$Scaled_C_ppr*coef(model_fwf_step_h5l)["Scaled_C_ppr"]
plot_data5_f[plot_data5_f$lake=="Lake Ferto",30] <- termplot(model_fwf_step_h5l, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[plot_data5_f$lake=="Lake Ferto",31] <- termplot(model_fwf_step_h5l, partial=T, term=1, plot=F)$Scaled_C_ppr$x

plot_data5_f$part_res_C_ppr_fwfh=NA
plot_data5_f$part_res_C_ppr_fwfh_y=NA
plot_data5_f$part_res_C_ppr_fwfh_x=NA

plot_data5_f[plot_data5_f$lake=="Lake Tisza",32] <- resid(model_fwf_step_h5h) + plot_data5_f[plot_data5_f$lake=="Lake Tisza",]$Scaled_C_ppr*coef(model_fwf_step_h5h)["Scaled_C_ppr"]
plot_data5_f[plot_data5_f$lake=="Lake Tisza",33] <- termplot(model_fwf_step_h5h, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[plot_data5_f$lake=="Lake Tisza",34] <- termplot(model_fwf_step_h5h, partial=T, term=1, plot=F)$Scaled_C_ppr$x

plot_data5_f$part_res_V_Var_dr=NA
plot_data5_f$part_res_V_Var_dr_y=NA
plot_data5_f$part_res_V_Var_dr_x=NA

plot_data5_f[plot_data5_f$lake=="Lake Balaton",35] <- resid(model_dr_step_h5) + plot_data5_f[plot_data5_f$lake=="Lake Balaton",]$Scaled_A_std*coef(model_dr_step_h5)["Scaled_A_std"]
plot_data5_f[plot_data5_f$lake=="Lake Balaton",36] <- termplot(model_dr_step_h5, partial=T, term=1, plot=F)$Scaled_A_std$y
plot_data5_f[plot_data5_f$lake=="Lake Balaton",37] <- termplot(model_dr_step_h5, partial=T, term=1, plot=F)$Scaled_A_std$x

c5l=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_fwfl),show.legend = TRUE) +  
  geom_point(aes(color=lake),size=5,show.legend = TRUE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwfl_x,y=part_res_C_ppr_fwfl_y),color="black",size=2,linetype = "dashed")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.2,3.2)

abc5l=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_fwfh),show.legend = TRUE) +  
  geom_point(aes(color=lake),size=5,show.legend = TRUE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwfh_x,y=part_res_C_ppr_fwfh_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.2,3.2)

ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),], aes(x=Scaled_A_std , y=part_res_V_Var_dr),show.legend = TRUE) +  
  geom_point(aes(color=lake),size=5,show.legend = TRUE) +
  geom_line(data=plot_data5_f,aes(x=part_res_V_Var_dr_x,y=part_res_V_Var_dr_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.2,3.2)