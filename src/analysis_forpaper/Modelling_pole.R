library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)
library(corrplot)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
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

p1=ggplot(data=plot_data5_f,aes(x=predicted_nfwf,y=gct_lai))+
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted LAI")+
  ylab("Observed LAI")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)+
  ggtitle("a) DR [Apr. 2014]")+
  annotate("text",x = 2, y = 6,label = expression(paste("R"^2, " = 0.29")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p2=ggplot(data=plot_data5_f,aes(x=predicted_fwfl,y=gct_lai))+
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2,linetype="dashed")+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted LAI")+
  ylab("Observed LAI")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)+
  theme(axis.title.y=element_blank())+
  ggtitle("b) FWF [Dec. 2011]")+
  annotate("text",x = 2, y = 6,label = expression(paste("R"^2, " = 0.08")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p3=ggplot(data=plot_data5_f,aes(x=predicted_fwfh,y=gct_lai))+
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted LAI")+
  ylab("Observed LAI")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)+
  theme(axis.title.y=element_blank())+
  ggtitle("c) FWF [Mar. 2012]")+
  annotate("text",x = 2, y = 6,label = expression(paste("R"^2, " = 0.26")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p4=ggplot(data=plot_data5_f,aes(x=predicted_fwfh2,y=gct_lai))+
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted LAI")+
  ylab("Observed LAI")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.9,5)+ylim(0,6.5)+
  theme(axis.title.y=element_blank())+
  ggtitle("d) FWF [Jun. 2013]")+
  annotate("text",x = 2, y = 6,label = expression(paste("R"^2, " = 0.30")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

##### Partial residual

plot_data5_f$part_res_C_ppr_fwfl=NA
plot_data5_f$part_res_C_ppr_fwfl_y=NA
plot_data5_f$part_res_C_ppr_fwfl_x=NA

plot_data5_f[plot_data5_f$lake=="Lake Ferto",32] <- resid(model_fwf_step_h5l) + plot_data5_f[plot_data5_f$lake=="Lake Ferto",]$Scaled_C_ppr*coef(model_fwf_step_h5l)["Scaled_C_ppr"]
plot_data5_f[plot_data5_f$lake=="Lake Ferto",33] <- termplot(model_fwf_step_h5l, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[plot_data5_f$lake=="Lake Ferto",34] <- termplot(model_fwf_step_h5l, partial=T, term=1, plot=F)$Scaled_C_ppr$x

p5=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_fwfl),show.legend = FALSE) +  
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwfl_x,y=part_res_C_ppr_fwfl_y),color="black",size=2,linetype = "dashed")+
  theme_bw(base_size = 25) +
  xlab("Scaled C_ppr")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.5,2.2)+
  theme(axis.title.y=element_blank())+
  ggtitle("f) FWF [Dec. 2011]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

plot_data5_f$part_res_C_ppr_fwfh=NA
plot_data5_f$part_res_C_ppr_fwfh_y=NA
plot_data5_f$part_res_C_ppr_fwfh_x=NA

plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),35] <- resid(model_fwf_step_h5h) + plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),]$Scaled_C_ppr*coef(model_fwf_step_h5h)["Scaled_C_ppr"]
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),36] <- termplot(model_fwf_step_h5h, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),37] <- termplot(model_fwf_step_h5h, partial=T, term=1, plot=F)$Scaled_C_ppr$x

p6=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-off"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_fwfh),show.legend = FALSE) +  
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwfh_x,y=part_res_C_ppr_fwfh_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  xlab("Scaled C_ppr")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.5,3.2)+
  theme(axis.title.y=element_blank())+
  ggtitle("g) FWF [Mar. 2012]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

plot_data5_f$part_res_C_ppr_fwfh2=NA
plot_data5_f$part_res_C_ppr_fwfh2_y=NA
plot_data5_f$part_res_C_ppr_fwfh2_x=NA

plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),38] <- resid(model_fwf_step_h5h2) + plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),]$Scaled_C_ppr*coef(model_fwf_step_h5h2)["Scaled_C_ppr"]
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),39] <- termplot(model_fwf_step_h5h2, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),40] <- termplot(model_fwf_step_h5h2, partial=T, term=1, plot=F)$Scaled_C_ppr$x

p7=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza" & plot_data5_f$season=="leaf-on"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_fwfh2),show.legend = FALSE) +  
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE,shape=17) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwfh2_x,y=part_res_C_ppr_fwfh2_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  xlab("Scaled C_ppr")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-3.5,3.2)+
  theme(axis.title.y=element_blank())+
  ggtitle("h) FWF [Jun. 2013]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

plot_data5_f$part_res_C_ppr_nfwf=NA
plot_data5_f$part_res_C_ppr_nfwf_y=NA
plot_data5_f$part_res_C_ppr_nfwf_x=NA

plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),41] <- resid(model_dr_step_h5) + plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),]$Scaled_H_p99.99.*coef(model_dr_step_h5)["Scaled_H_p99.99."]
plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),42] <- termplot(model_dr_step_h5, partial=T, term=1, plot=F)$Scaled_H_p99.99.$y
plot_data5_f[(plot_data5_f$lake=="Lake Balaton" ),43] <- termplot(model_dr_step_h5, partial=T, term=1, plot=F)$Scaled_H_p99.99.$x

p8=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),], aes(x=Scaled_H_p99.99. , y=part_res_C_ppr_nfwf),show.legend = FALSE) +  
  geom_point(aes(color=lake,shape=season),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_nfwf_x,y=part_res_C_ppr_nfwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  ylab("Partial dependence") +
  xlab("Scaled H_p99")+
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(-0.5,0)+ylim(-6.2,1)+
  ggtitle("e) DR [Apr. 2014]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

#### Figures

p0=ggplot(data=plot_data5_f,aes(x=predicted_fwfh2,y=gct_lai))+
  geom_point(aes(color=lake,shape=season),size=5,show.legend = TRUE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted LAI")+
  ylab("Observed LAI")+
  theme_bw(base_size = 30) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  scale_shape_manual(values=c(16,17),name="Season")+
  xlim(0.9,5)+ylim(0,6.5)

legend=get_legend(p0)

t_1 <- textGrob("Prediction plots",gp=gpar(fontsize=25, col="black", fontface="bold"))
t_2 <- textGrob("Partial dependence plots",gp=gpar(fontsize=25, col="black", fontface="bold"))

fig5=grid.arrange(p1,p2,p3,p4,p8,p5,p6,p7,legend,t_1,t_2,
             ncol=5,
             nrow=4,
             layout_matrix=rbind(c(11,11,11,11,10),c(1,2,3,4,10),c(12,12,12,12,10),c(6,7,8,9,10)),
             widths = c(1,1,1,1,0.5),
             heights = c(0.5,4,0.5,4))

ggsave("Fig5_subm1.png",plot = fig5,width = 28, height = 12)
