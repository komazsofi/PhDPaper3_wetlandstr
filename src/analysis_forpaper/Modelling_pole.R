library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(olsrr)
library(corrplot)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
setwd(workdir)

####################################### Import

plot_data05=read.csv(paste("Pole_db_",0.5,"_filt.csv",sep=""))

plot_data05_scaled=scale(plot_data05[,c(4:11)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[plot_data05_f$OBJNAME!=187,]

plot_data2=read.csv(paste("Pole_db_",2.5,"_filt.csv",sep=""))

plot_data2_scaled=scale(plot_data2[,c(4:11)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
plot_data2_f=plot_data2_f[plot_data2_f$OBJNAME!=187,]

plot_data5=read.csv(paste("Pole_db_",5,"_filt.csv",sep=""))

plot_data5_scaled=scale(plot_data5[,c(4:11)])
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
model_all_h05=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data05_f[plot_data05_f$season=="leaf-off",])
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[(plot_data05_f$lake!="Lake Balaton" & plot_data05_f$season=="leaf-off"),])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

# fwfl
model_fwf_h05l=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake=="Lake Ferto",])
summary(model_fwf_h05l) 

#AIC model selection (step)
model_fwf_step_h05l<-step(model_fwf_h05l,direction = "backward")
summary(model_fwf_step_h05l)

# fwfh
model_fwf_h05h=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[(plot_data05_f$lake=="Lake Tisza" & plot_data05_f$season=="leaf-off"),])
summary(model_fwf_h05h) 

#AIC model selection (step)
model_fwf_step_h05h<-step(model_fwf_h05h,direction = "backward")
summary(model_fwf_step_h05h)

# discrete
model_dr_h05=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake=="Lake Balaton",])
summary(model_dr_h05) 

#AIC model selection (step)
model_dr_step_h05<-step(model_dr_h05,direction = "backward")
summary(model_dr_step_h05)

##### 2.5 m

##### lai

# all
model_all_h2=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data2_f[plot_data2_f$season=="leaf-off",])
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# fwf
model_fwf_h2=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[(plot_data2_f$lake!="Lake Balaton" & plot_data2_f$season=="leaf-off"),])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

# fwfl
model_fwf_h2l=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake=="Lake Ferto",])
summary(model_fwf_h2l) 

#AIC model selection (step)
model_fwf_step_h2l<-step(model_fwf_h2l,direction = "backward")
summary(model_fwf_step_h2l)

# fwfh
model_fwf_h2h=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[(plot_data2_f$lake=="Lake Tisza" & plot_data2_f$season=="leaf-off"),])
summary(model_fwf_h2h) 

#AIC model selection (step)
model_fwf_step_h2h<-step(model_fwf_h2h,direction = "backward")
summary(model_fwf_step_h2h)

# fwfh2
model_fwf_h2h2=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[(plot_data2_f$lake=="Lake Tisza" & plot_data2_f$season=="leaf-on"),])
summary(model_fwf_h2h2) 

#AIC model selection (step)
model_fwf_step_h2h2<-step(model_fwf_h2h2,direction = "backward")
summary(model_fwf_step_h2h2)

# discrete
model_dr_h2=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake=="Lake Balaton",])
summary(model_dr_h2) 

#AIC model selection (step)
model_dr_step_h2<-step(model_dr_h2,direction = "backward")
summary(model_dr_step_h2)

##### 5 m

##### lai

# all
model_all_h5=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f[plot_data5_f$season=="leaf-off",])
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[(plot_data5_f$lake!="Lake Balaton" & plot_data5_f$season=="leaf-off"),])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

# fwfl
model_fwf_h5l=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake=="Lake Ferto",])
summary(model_fwf_h5l) 

#AIC model selection (step)
model_fwf_step_h5l<-step(model_fwf_h5l,direction = "backward")
summary(model_fwf_step_h5l)

# fwfh
model_fwf_h5h=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),])
summary(model_fwf_h5h) 

#AIC model selection (step)
model_fwf_step_h5h<-step(model_fwf_h5h,direction = "backward")
summary(model_fwf_step_h5h)

# fwfh2
model_fwf_h5h2=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),])
summary(model_fwf_h5h2) 

#AIC model selection (step)
model_fwf_step_h5h2<-step(model_fwf_h5h2,direction = "backward")
summary(model_fwf_step_h5h2)

# discrete
model_dr_h5=lm(gct_lai~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake=="Lake Balaton",])
summary(model_dr_h5) 

#AIC model selection (step)
model_dr_step_h5<-step(model_dr_h5,direction = "backward")
summary(model_dr_step_h5)

## report for study

stargazer(model_fwf_step_h05,model_fwf_step_h05l,model_fwf_step_h05h,model_fwf_step_h2,model_fwf_step_h2l,model_fwf_step_h2h,model_fwf_step_h5,model_fwf_step_h5l,model_fwf_step_h5h,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","fwf low pdens [0.5 m]","fwf high pdens [0.5 m]","fwf [2.5 m]","fwf low pdens [2.5 m]","fwf high pdens [2.5 m]","fwf [5 m]","fwf low pdens [5 m]","fwf high pdens [5 m]"),title="Estimation of leaf area at different resolutions",out="lai_report.doc")

###### predicted vs actual

plot_data5_f$predicted_nfwf<-NA
plot_data5_f$predicted_fwfl<-NA
plot_data5_f$predicted_fwfh<-NA
plot_data5_f$predicted_fwfh2<-NA

plot_data5_f[plot_data5_f$lake=="Lake Balaton",28]=predict(model_dr_step_h5)
plot_data5_f[plot_data5_f$lake=="Lake Ferto",29]=predict(model_fwf_step_h5l)
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),30]=predict(model_fwf_step_h5h)
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),31]=predict(model_fwf_step_h5h2)

ggplot(data=plot_data5_f,aes(x=predicted_nfwf,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = TRUE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)

ggplot(data=plot_data5_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = TRUE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=2,linetype="dashed")+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)

ggplot(data=plot_data5_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = TRUE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)

ggplot(data=plot_data5_f,aes(x=predicted_fwfh2,y=gct_lai))+
  geom_point(aes(color=lake),size=5,show.legend = TRUE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted leaf area")+
  ylab("Actual leaf area")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)
