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
tisza_2m_r_leafon=read.csv("tisza_2m_r_leafon_v3.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"
tisza_2m_r_leafon$type<-"FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"
tisza_2m_r_leafon$lake<-"Lake Tisza"

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

tisza_2m_r_sub_leafon=subset(tisza_2m_r_leafon,select=c(34,7,10,12,13,33,
                                          17,23,19,27,20))
names(tisza_2m_r_sub_leafon) <- c("lake","class","LAI","x","y","type",
                           "V_sigmaz","C_negop","C_er","C_ppr","V_ewidth")

tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[is.na(tisza_2m_r_sub_leafon$class)==FALSE,]

#Filters
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="shrub",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="tree",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="grassland",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="scirpus",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="artificial",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="flat",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="land",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="mudflat",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="not_known",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="water",]

ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="shrub",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="tree",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="grassland",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="scirpus",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="artificial",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="flat",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="land",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="mudflat",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="not_known",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="water",]

balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="shrub",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="tree",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="grassland",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="scirpus",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="artificial",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="flat",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="land",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="mudflat",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="not_known",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="water",]

tisza_2m_r_sub$class=str_replace(tisza_2m_r_sub$class,"reed","phragmites")
ferto_2m_r_sub$class=str_replace(ferto_2m_r_sub$class,"reed","phragmites")
balaton_2m_r_sub$class=str_replace(balaton_2m_r_sub$class,"reed","phragmites")

tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="shrubs",]
tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="trees",]
tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="water",]
tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="artificial",]
tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="grassland",]
tisza_2m_r_sub_leafon=tisza_2m_r_sub_leafon[tisza_2m_r_sub_leafon$class!="scirpus",]

tisza_2m_r_sub_leafon$class=str_replace(tisza_2m_r_sub_leafon$class,"reed","phragmites")

#merging

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)
merged=merged[is.na(merged$class)==FALSE,]
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

# linear regression across fwf with high pdens + leafon

lm_LAI_fwf<-lm(LAI~log(V_sigmaz)+log(C_negop)+log(C_er)+log(V_ewidth),data=tisza_2m_r_sub_leafon)
summary(lm_LAI_fwf)

#AIC model selection (step)
lm_LAI_fwf_step<-step(lm_LAI_fwf)
summary(lm_LAI_fwf_step)

# predicted vs. field

fit <- lm(LAI ~ log(V_sigmaz) + log(C_negop) + log(C_er), data = tisza_2m_r_sub_leafon)
effect_plot(fit, pred = V_sigmaz, interval = TRUE, plot.points = TRUE)

# linear regression across non-fwf

lm_lai<-lm(LAI~V_sigmaz+C_negop+C_er+C_ppr,data=merged[merged$lake=="Lake Balaton",])
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

# Visualization

tisza_2m_r_sub_leafon$id=seq(1,63,1)
merged$id=seq(1,201,1)

ggplot(data=merged, aes(x=V_sigmaz, y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=tisza_2m_r_sub_leafon, aes(x=C_negop, y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged[merged$lake=="Lake Tisza",], aes(x=C_er, y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged[merged$lake=="Lake Tisza",], aes(x=log(V_ewidth), y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")
