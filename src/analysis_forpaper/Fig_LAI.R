library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)

library(lidR)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Pole

plot_data5=read.csv(paste("Pole_noncorr",5,".csv",sep=""))

# LAI

lm_scaled<-lm(gct_lai ~ Scaled_H_max+Scaled_V_ku+Scaled_A_med+Scaled_A_cover,data = plot_data5)
summary(lm_scaled)

lm_scaled_nfwf<-lm(gct_lai ~ Scaled_H_max+Scaled_V_ku+Scaled_A_med+Scaled_A_cover,data = plot_data5[plot_data5$lake=="Lake Balaton",])
summary(lm_scaled_nfwf)

lm_scaled_fwfh<-lm(gct_lai ~ Scaled_H_max+Scaled_V_ku+Scaled_A_med+Scaled_A_cover+Scaled_W_echw,data = plot_data5[plot_data5$lake=="Lake Tisza",])
summary(lm_scaled_fwfh)

lm_scaled_fwfl<-lm(gct_lai ~ Scaled_H_max+Scaled_V_ku+Scaled_A_med+Scaled_A_cover+Scaled_W_echw,data = plot_data5[plot_data5$lake=="Lake Ferto",])
summary(lm_scaled_fwfl)

lm_scaled_fwf<-lm(gct_lai ~ Scaled_H_max+Scaled_V_ku+Scaled_A_med+Scaled_A_cover+Scaled_W_echw,data = plot_data5[plot_data5$lake!="Lake Balaton",])
summary(lm_scaled_fwf)

std_coef_sum <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum)<-c("coeff","metric")

std_coef_sum$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum$coeff<-c(lm_scaled$coefficients[4],0,lm_scaled$coefficients[5],0,lm_scaled$coefficients[2],0,0,0,0,0,lm_scaled$coefficients[3],0,0)
std_coef_sum$type<-c("all")

std_coef_sum_nfwf <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum_nfwf)<-c("coeff","metric")

std_coef_sum_nfwf$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum_nfwf$coeff<-c(lm_scaled_nfwf$coefficients[4],0,lm_scaled_nfwf$coefficients[5],0,lm_scaled_nfwf$coefficients[2],0,0,0,0,0,lm_scaled_nfwf$coefficients[3],0,0)
std_coef_sum_nfwf$type<-c("discrete")

std_coef_sum_fwfh <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum_fwfh)<-c("coeff","metric")

std_coef_sum_fwfh$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum_fwfh$coeff<-c(lm_scaled_fwfh$coefficients[4],0,lm_scaled_fwfh$coefficients[5],0,lm_scaled_fwfh$coefficients[2],0,0,0,0,0,lm_scaled_fwfh$coefficients[3],0,lm_scaled_fwfh$coefficients[6])
std_coef_sum_fwfh$type<-c("fwf high pdens")

std_coef_sum_fwfl <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum_fwfl)<-c("coeff","metric")

std_coef_sum_fwfl$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum_fwfl$coeff<-c(lm_scaled_fwfl$coefficients[4],0,lm_scaled_fwfl$coefficients[5],0,lm_scaled_fwfl$coefficients[2],0,0,0,0,0,lm_scaled_fwfl$coefficients[3],0,lm_scaled_fwfl$coefficients[6])
std_coef_sum_fwfl$type<-c("fwf low pdens")

std_coef_sum_fwf <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum_fwf)<-c("coeff","metric")

std_coef_sum_fwf$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum_fwf$coeff<-c(lm_scaled_fwf$coefficients[4],0,lm_scaled_fwf$coefficients[5],0,lm_scaled_fwf$coefficients[2],0,0,0,0,0,lm_scaled_fwf$coefficients[3],0,lm_scaled_fwf$coefficients[6])
std_coef_sum_fwf$type<-c("fwf")

std_coef_sum_h=rbind(std_coef_sum,std_coef_sum_nfwf,std_coef_sum_fwfh,std_coef_sum_fwfl,std_coef_sum_fwf)

a=ggplot(data=std_coef_sum_h, aes(x=coeff, y=metric,fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for LAI (res=5 m)")+
  theme_classic(base_size=20)
