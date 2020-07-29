library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggpubr)

library(olsrr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis4/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_plot.csv")
ferto_2m_r=read.csv("ferto_plot.csv")
balaton_2m_r=read.csv("balaton_plot.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(46,8,44,13,14,45,
                                          30,19,20,25,27,28,16,17,18))
names(tisza_2m_r_sub) <- c("lake","class","Biomass","x","y","type",
                           "H_max","lbiomass","C_er","V_nech","V_psw","C_ppr","S_amean","S_arms","V_sigmaz")

tisza_2m_r_sub=tisza_2m_r_sub[is.na(tisza_2m_r_sub$class)==FALSE,]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(45,8,43,13,14,44,
                                          30,19,20,25,27,28,16,17,18))
names(ferto_2m_r_sub) <- c("lake","class","Biomass","x","y","type",
                           "H_max","lbiomass","C_er","V_nech","V_psw","C_ppr","S_amean","S_arms","V_sigmaz")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(45,8,43,13,14,44,
                                              29,19,20,24,26,27,16,17,18))
names(balaton_2m_r_sub) <- c("lake","class","Biomass","x","y","type",
                             "H_max","lbiomass","C_er","V_nech","V_psw","C_ppr","S_amean","S_arms","V_sigmaz")

balaton_2m_r_sub_2=subset(balaton_2m_r,select=c(45,8,43,13,14,44,
                                              29,19,20,24,26,27,16,17,18,30))
names(balaton_2m_r_sub_2) <- c("lake","class","Biomass","x","y","type",
                             "H_max","lbiomass","C_er","V_nech","V_psw","C_ppr","S_amean","S_arms","V_sigmaz","S_reflmean")
#Filters
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="shrub",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="tree",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="grassland",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="scirpus",]

ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="shrub",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="tree",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="grassland",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="scirpus",]

balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="shrub",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="tree",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="grassland",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="scirpus",]

tisza_2m_r_sub$class=str_replace(tisza_2m_r_sub$class,"reed","phragmites")
ferto_2m_r_sub$class=str_replace(ferto_2m_r_sub$class,"reed","phragmites")
balaton_2m_r_sub$class=str_replace(balaton_2m_r_sub$class,"reed","phragmites")

#merging

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)
data_merged=merged %>% gather(-c(class,Biomass,lake,type,x,y),key = "var", value = "value")

# check correlation among metrics
merged=merged[complete.cases(merged), ]
summary(merged)

round(cor(merged[,c(7:15)], method="spearman"),2) # 
# Select: H_max + spearman r <0.7

# linear regression across all lakes

lm_biomass<-lm(Biomass~H_max+V_psw+C_ppr+S_amean,data=merged)
summary(lm_biomass)

#AIC model selection (step)
lm_biomass_step<-step(lm_biomass)
summary(lm_biomass_step)

#Residual histogram
hist((residuals(lm_biomass_step)))

#Figure relative importance of predictor variables
lm_biomass_scaled<-lm(Biomass~scale(H_max)+scale(V_psw)+scale(C_ppr)+scale(S_arms),data=merged)

std_coef_sum<-c(lm_biomass_scaled$coefficients[2],lm_biomass_scaled$coefficients[3],
                lm_biomass_scaled$coefficients[4],lm_biomass_scaled$coefficients[5]) 
names(std_coef_sum)<-c("H_max","V_psw","C_ppr","S_arms")
std_coef_sum

barplot(std_coef_sum, 
        beside =T,
        ylab="Standardized coefficient")

# linear regression across fwf

lm_biomass_fwf<-lm(Biomass~H_max+V_psw+C_ppr+S_amean,data=merged[merged$lake!="Lake Balaton",])
summary(lm_biomass_fwf)

#AIC model selection (step)
lm_biomass_fwf_step<-step(lm_biomass_fwf)
summary(lm_biomass_fwf_step)

#Residual histogram
hist((residuals(lm_biomass_fwf_step)))

#Figure relative importance of predictor variables
lm_biomass_fwf_scaled<-lm(Biomass~scale(H_max)+scale(V_psw)+scale(C_ppr)+scale(S_amean),data=merged[merged$lake!="Lake Balaton",])

std_coef_sum_fwf<-c(lm_biomass_fwf_scaled$coefficients[2],lm_biomass_fwf_scaled$coefficients[3],
                    lm_biomass_fwf_scaled$coefficients[4],lm_biomass_fwf_scaled$coefficients[5]) 
names(std_coef_sum_fwf)<-c("H_max","V_psw","C_ppr","S_amean")
std_coef_sum_fwf

barplot(std_coef_sum_fwf, 
        beside =T,
        ylab="Standardized coefficient")

# Partial residual

termplot(lm_biomass_nfwf_step, partial=T, term=1, pch=20, cex=1.5, col.term=0,
         lwd.term=3, col.res="dodgerblue",xlab="Annual precipitation (mm yr-1)", ylab="Partial residual")
lines(lwd=7, lty=2, col='red', termplot(lm_biomass_nfwf_step, partial=T, term=2, plot=F)$V_psw$x, 
      termplot(lm_biomass_nfwf_step, partial=T, term=2, plot=F)$V_psw$y)

# linear regression across nonfwf

lm_biomass_nfwf<-lm(Biomass~H_max+V_psw+C_ppr+S_reflmean,data=balaton_2m_r_sub_2)
summary(lm_biomass_nfwf)

#AIC model selection (step)
lm_biomass_nfwf_step<-step(lm_biomass_nfwf)
summary(lm_biomass_nfwf_step)

# Vis

fit <- lm(Biomass ~ H_max + V_psw, data = balaton_2m_r_sub_2)

plot(fit$fitted.values,fit$model$Biomass)

plot(fit$model$H_max,fit$model$Biomass)
plot(fit$model$V_psw,fit$model$Biomass)
plot(balaton_2m_r_sub_2$S_amean,balaton_2m_r_sub_2$Biomass)
