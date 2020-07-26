library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggpubr)

library(olsrr)
library(jtools)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis4/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_2m_r_v3.csv")
ferto_2m_r=read.csv("ferto_2m_r_v3.csv")
balaton_2m_r=read.csv("balaton_2m_r_v3.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(34,7,10,12,13,33,
                                          17,23,19,27,20))
names(tisza_2m_r_sub) <- c("lake","class","LAI","x","y","type",
                           "V_sigmaz","C_negop","C_er","C_ppr","V_ewidth")

tisza_2m_r_sub=tisza_2m_r_sub[is.na(tisza_2m_r_sub$class)==FALSE,]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(34,7,10,12,13,33,
                                          17,23,19,27,20))
names(ferto_2m_r_sub) <- c("lake","class","LAI","x","y","type",
                           "V_sigmaz","C_negop","C_er","C_ppr","V_ewidth")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(34,7,10,12,13,33,
                                              17,22,19,26,26))
names(balaton_2m_r_sub) <- c("lake","class","LAI","x","y","type",
                             "V_sigmaz","C_negop","C_er","C_ppr","V_ewidth")

balaton_2m_r_sub$V_ewidth<-NA

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
data_merged=merged %>% gather(-c(class,LAI,lake,type,x,y),key = "var", value = "value")

# check correlation among metrics without echowidth
merged=merged[complete.cases(merged[,c(1:10)]), ]
summary(merged)

round(cor(merged[,c(7:10)], method="spearman"),2) #without echowidth
round(cor(merged[merged$lake!="Lake Balaton",c(7:11)], method="spearman"),2) #with ecowidth -> drop ppr

# linear regression across all lakes

lm_lai<-lm(LAI~log(V_sigmaz)+log(C_negop)+log(C_er)+log(C_ppr),data=merged)
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

#Residual histogram
hist((residuals(lm_lai_step)))

#Figure relative importance of predictor variables
lm_LAI_scaled<-lm(LAI ~ scale(V_sigmaz) + scale(C_er) + scale(C_ppr),data=merged)

std_coef_sum<-c(lm_LAI_scaled$coefficients[2],lm_LAI_scaled$coefficients[3],
                lm_LAI_scaled$coefficients[4]) 
names(std_coef_sum)<-c("V_sigmaz","C_er","C_ppr")
std_coef_sum

barplot(std_coef_sum, 
        beside =T,
        ylab="Standardized coefficient")

# linear regression across fwf
lm_LAI_fwf<-lm(LAI~log(V_sigmaz)+log(C_er)+log(V_ewidth),data=merged[merged$lake!="Lake Balaton",])
summary(lm_LAI_fwf)

#AIC model selection (step)
lm_LAI_fwf_step<-step(lm_LAI_fwf)
summary(lm_LAI_fwf_step)

#Residual histogram
hist((residuals(lm_LAI_fwf_step)))

#Figure relative importance of predictor variables
lm_LAI_fwf_scaled<-lm(LAI ~ scale(C_negop) + scale(C_er) + scale(V_ewidth),data=merged[merged$lake!="Lake Balaton",])

std_coef_sum_fwf<-c(lm_LAI_fwf_scaled$coefficients[2],lm_LAI_fwf_scaled$coefficients[3],
                    lm_LAI_fwf_scaled$coefficients[4]) 
names(std_coef_sum_fwf)<-c("C_negop","C_er","V_ewidth")
std_coef_sum_fwf

barplot(std_coef_sum_fwf, 
        beside =T,
        ylab="Standardized coefficient")

# linear regression across fwf with high pdens

lm_LAI_fwf<-lm(LAI~log(V_sigmaz)+log(C_negop)+log(C_er)+log(V_ewidth),data=merged[merged$lake=="Lake Tisza",])
summary(lm_LAI_fwf)

#AIC model selection (step)
lm_LAI_fwf_step<-step(lm_LAI_fwf)
summary(lm_LAI_fwf_step)

#Residual histogram
hist((residuals(lm_LAI_fwf_step)))

#Figure relative importance of predictor variables
lm_LAI_fwf_scaled<-lm(LAI ~ scale(log(C_negop)) + scale(log(C_er)) + scale(log(V_ewidth)),data=merged[merged$lake=="Lake Tisza",])

std_coef_sum_fwf<-c(lm_LAI_fwf_scaled$coefficients[2],lm_LAI_fwf_scaled$coefficients[3],
                    lm_LAI_fwf_scaled$coefficients[4]) 
names(std_coef_sum_fwf)<-c("C_negop","C_er","V_ewidth")
std_coef_sum_fwf

barplot(std_coef_sum_fwf, 
        beside =T,
        ylab="Standardized coefficient")

# predicted vs. field

fit <- lm(LAI~V_sigmaz+C_negop+C_er+V_ewidth, data = merged[merged$lake=="Lake Tisza",])
effect_plot(fit, pred = V_ewidth, interval = TRUE, plot.points = TRUE)

plot(fit$fitted.values,merged[merged$lake=="Lake Tisza",3])

# linear regression across non-fwf

lm_lai<-lm(LAI~V_sigmaz+C_negop+C_er+C_ppr,data=merged[merged$lake=="Lake Balaton",])
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)
