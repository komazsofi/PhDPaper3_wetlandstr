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

plot_data05=read.csv(paste("Plot_db_",0.5,".csv",sep=""))
plot_data05$total.weight=plot_data05$total.weight/10000

plot_data05_scaled=scale(plot_data05[,c(3:10)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[(plot_data05_f$OBJNAME!=120 & plot_data05_f$OBJNAME!=209 & plot_data05_f$OBJNAME!=163),]

plot_data2=read.csv(paste("Plot_db_",2.5,".csv",sep=""))
plot_data2$total.weight=plot_data2$total.weight/10000

plot_data2_scaled=scale(plot_data2[,c(3:10)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
plot_data2_f=plot_data2_f[(plot_data2_f$OBJNAME!=120 & plot_data2_f$OBJNAME!=209 & plot_data2_f$OBJNAME!=163),]

plot_data5=read.csv(paste("Plot_db_",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

plot_data5_scaled=scale(plot_data5[,c(3:10)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
plot_data5_f=plot_data5_f[(plot_data5_f$OBJNAME!=120 & plot_data5_f$OBJNAME!=209 & plot_data5_f$OBJNAME!=163),]

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

##### height

# all
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

##### biomass

model_all_b05=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data05_f)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# fwf
model_fwf_b05=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_b05) 

#AIC model selection (step)
model_fwf_step_b05<-step(model_fwf_b05,direction = "backward")
summary(model_fwf_step_b05)

stargazer(model_fwf_step_h05,model_all_step_h05,model_fwf_step_b05,model_all_step_b05,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="0.5 m")

##### 2.5 m

##### height

# all
model_all_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data2_f)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# fwf
model_fwf_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

##### biomass

model_all_b2=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data2_f)
summary(model_all_b2) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# fwf
model_fwf_b2=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_b2) 

#AIC model selection (step)
model_fwf_step_b2<-step(model_fwf_b2,direction = "backward")
summary(model_fwf_step_b2)

stargazer(model_fwf_step_h2,model_all_step_h2,model_fwf_step_b2,model_all_step_b2,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="2.5 m")

##### 5 m

##### height

# all
model_all_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data5_f)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

##### biomass

model_all_b5=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data5_f)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# fwf
model_fwf_b5=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

stargazer(model_fwf_step_h5,model_all_step_h5,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="5 m")

## report for study

stargazer(model_fwf_step_h05,model_all_step_h05,model_fwf_step_h2,model_all_step_h2,model_fwf_step_h5,model_all_step_h5,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","all [0.5 m]","fwf [2.5 m]","all [2.5 m]","fwf [5 m]","all [5 m]"),title="Estimation of vegetation height at different resolutions",out="height_report.doc")
stargazer(model_fwf_step_b05,model_all_step_b05,model_fwf_step_b2,model_all_step_b2,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","all [0.5 m]","fwf [2.5 m]","all [2.5 m]","fwf [5 m]","all [5 m]"),title="Estimation of biomass at different resolutions",out="biomass_report.doc")

# ols
full_fwf_h05=ols_step_all_possible(model_fwf_h05)
full_all_h05=ols_step_all_possible(model_all_h05)

full_fwf_b5=ols_step_all_possible(model_fwf_b5)
full_all_b5=ols_step_all_possible(model_all_b5)

####################################### Standard coefficients

std_coef_sum_h_05 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_05)<-c("coeff","metric")

std_coef_sum_h_05$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_05$coeff<-c(model_all_step_h05$coefficients[2],model_all_step_h05$coefficients[3],0)
std_coef_sum_h_05$type<-c("all 0.5m")

std_coef_sum_h_05_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_05_fwf)<-c("coeff","metric")

std_coef_sum_h_05_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_05_fwf$coeff<-c(model_fwf_step_h05$coefficients[2],model_fwf_step_h05$coefficients[3],0)
std_coef_sum_h_05_fwf$type<-c("fwf 0.5m")

std_coef_sum_h_2 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_2)<-c("coeff","metric")

std_coef_sum_h_2$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_2$coeff<-c(model_all_step_h2$coefficients[2],0,0)
std_coef_sum_h_2$type<-c("all 2.5m")

std_coef_sum_h_2_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_2_fwf)<-c("coeff","metric")

std_coef_sum_h_2_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_2_fwf$coeff<-c(model_fwf_step_h2$coefficients[2],model_fwf_step_h2$coefficients[3],0)
std_coef_sum_h_2_fwf$type<-c("fwf 2.5m")

std_coef_sum_h_5 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_5)<-c("coeff","metric")

std_coef_sum_h_5$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_5$coeff<-c(model_all_step_h5$coefficients[2],model_all_step_h5$coefficients[3],0)
std_coef_sum_h_5$type<-c("all 5m")

std_coef_sum_h_5_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_h_5_fwf)<-c("coeff","metric")

std_coef_sum_h_5_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_h_5_fwf$coeff<-c(model_fwf_step_h5$coefficients[2],0,0)
std_coef_sum_h_5_fwf$type<-c("fwf 5m")

std_coef_sum_h=rbind(std_coef_sum_h_05_fwf,std_coef_sum_h_05,std_coef_sum_h_2_fwf,std_coef_sum_h_2,std_coef_sum_h_5_fwf,std_coef_sum_h_5)

ggplot(data=std_coef_sum_h, aes(x=metric, y=coeff,fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
  xlab("LiDAR metrics")+ylab("Standardized coefficient")+
  ggtitle("Feature importance for vegetation height")+
  theme_classic(base_size=20)+
  scale_fill_manual(values=c("all 0.5m"="plum4","all 2.5m"="plum3","all 5m"="plum2","fwf 0.5m"="darkolivegreen4","fwf 2.5m"="darkolivegreen3","fwf 5m"="darkolivegreen2"),name="Type + resolution")

#########

std_coef_sum_b_05 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_05)<-c("coeff","metric")

std_coef_sum_b_05$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_05$coeff<-c(0,0,0)
std_coef_sum_b_05$type<-c("all 0.5m")

std_coef_sum_b_05_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_05_fwf)<-c("coeff","metric")

std_coef_sum_b_05_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_05_fwf$coeff<-c(0,0,0)
std_coef_sum_b_05_fwf$type<-c("fwf 0.5m")

std_coef_sum_b_2 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_2)<-c("coeff","metric")

std_coef_sum_b_2$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_2$coeff<-c(model_all_step_b2$coefficients[2],0,model_all_step_b2$coefficients[3])
std_coef_sum_b_2$type<-c("all 2.5m")

std_coef_sum_b_2_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_2_fwf)<-c("coeff","metric")

std_coef_sum_b_2_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_2_fwf$coeff<-c(0,0,0)
std_coef_sum_b_2_fwf$type<-c("fwf 2.5m")

std_coef_sum_b_5 <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_5)<-c("coeff","metric")

std_coef_sum_b_5$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_5$coeff<-c(0,0,model_all_step_b5$coefficients[2])
std_coef_sum_b_5$type<-c("all 5m")

std_coef_sum_b_5_fwf <- data.frame(matrix(ncol = 2, nrow = 3))
names(std_coef_sum_b_5_fwf)<-c("coeff","metric")

std_coef_sum_b_5_fwf$metric<-c("V_var","A_std","A_cover")
std_coef_sum_b_5_fwf$coeff<-c(0,0,model_fwf_step_b5$coefficients[2])
std_coef_sum_b_5_fwf$type<-c("fwf 5m")

std_coef_sum_b=rbind(std_coef_sum_b_05_fwf,std_coef_sum_b_05,std_coef_sum_b_2_fwf,std_coef_sum_b_2,std_coef_sum_b_5_fwf,std_coef_sum_b_5)

ggplot(data=std_coef_sum_b, aes(x=metric, y=coeff,fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
  xlab("LiDAR metrics")+ylab("Standardized coefficient")+
  ggtitle("Feature importance for biomass")+
  theme_classic(base_size=20)+
  scale_fill_manual(values=c("all 0.5m"="plum4","all 2.5m"="plum3","all 5m"="plum2","fwf 0.5m"="darkolivegreen4","fwf 2.5m"="darkolivegreen3","fwf 5m"="darkolivegreen2"),name="Type + resolution")

####################################### Predicted vs actual

# height
plot_data05_f$predicted_h=predict(model_all_step_h05)
plot_data05_f$predicted_h_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h05)

j=ggplot(data=plot_data05_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k=ggplot(data=plot_data05_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data2_f$predicted_h=predict(model_all_step_h2)
plot_data2_f$predicted_h_fwf<-NA
plot_data2_f[plot_data2_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h2)

j2=ggplot(data=plot_data2_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k2=ggplot(data=plot_data2_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data5_f$predicted_h=predict(model_all_step_h5)
plot_data5_f$predicted_h_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h5)

j5=ggplot(data=plot_data5_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5=ggplot(data=plot_data5_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j,k,j2,k2,j5,k5,
             nrow = 3,
             ncol = 2
)

# biomass
plot_data05_f$predicted_b=predict(model_all_step_b05)
plot_data05_f$predicted_b_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b05)

j_b=ggplot(data=plot_data05_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k_b=ggplot(data=plot_data05_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data2_f$predicted_b=predict(model_all_step_b2)
plot_data2_f$predicted_b_fwf<-NA
plot_data2_f[plot_data2_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b2)

j2_b=ggplot(data=plot_data2_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k2_b=ggplot(data=plot_data2_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data5_f$predicted_b=predict(model_all_step_b5)
plot_data5_f$predicted_b_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b5)

j5_b=ggplot(data=plot_data5_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5_b=ggplot(data=plot_data5_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j_b,k_b,j2_b,k2_b,j5_b,k5_b,
             nrow = 3,
             ncol = 2
)
