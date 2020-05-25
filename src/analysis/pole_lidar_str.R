library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggpubr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis3/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_2m_r_v2.csv")
ferto_2m_r=read.csv("ferto_2m_r_v2.csv")
balaton_2m_r=read.csv("balaton_2m_r_v2.csv")

tisza_2m_r$type<-"tisza"
ferto_2m_r$type<-"ferto"
balaton_2m_r$type<-"balaton"

tisza_2m_r_sub=subset(tisza_2m_r,select=c(14:29,8,9,10,11))
names(tisza_2m_r_sub)<-c("MeanZ","Amp_rms","SigmaZ","Lbiomass","echowidth","MedianZ","P95thZ","NegOpp","NrofEchoMax","pdens",
                         "PSW","PulsePen","rao_FHD","FHD","StdZ","VarZ","pole_height","Sumpole_cont","LAI","pole_FHD")

ferto_2m_r_sub=subset(ferto_2m_r,select=c(14:29,8,9,10,11))
names(ferto_2m_r_sub)<-c("MeanZ","Amp_rms","SigmaZ","Lbiomass","echowidth","MedianZ","P95thZ","NegOpp","NrofEchoMax","pdens",
                         "PSW","PulsePen","rao_FHD","FHD","StdZ","VarZ","pole_height","Sumpole_cont","LAI","pole_FHD")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(14:28,8,9,10,11))
names(balaton_2m_r_sub)<-c("MeanZ","Amp_rms","SigmaZ","Lbiomass","FHD","MedianZ","P95thZ","NegOpp","NrofEchoMax","pdens",
                           "PSW","PulsePen","rao_FHD","StdZ","VarZ","pole_height","Sumpole_cont","LAI","pole_FHD")

# per lake

data_tisza=tisza_2m_r_sub %>% gather(-c(pole_height,Sumpole_cont,LAI,pole_FHD),key = "var", value = "value")

ggplot(data=data_tisza, aes(x=LAI , y=value),show.legend = TRUE) +  
  geom_point(color='darkblue') +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

data_ferto=ferto_2m_r_sub %>% gather(-c(pole_height,Sumpole_cont,LAI,pole_FHD),key = "var", value = "value")

ggplot(data=data_ferto, aes(x=LAI , y=value),show.legend = TRUE) +  
  geom_point(color='firebrick') +
  geom_smooth(method="lm",colour="firebrick",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

data_balaton=balaton_2m_r_sub %>% gather(-c(pole_height,Sumpole_cont,LAI,pole_FHD),key = "var", value = "value")

ggplot(data=data_balaton, aes(x=LAI , y=value),show.legend = TRUE) +  
  geom_point(color='darkorange') +
  geom_smooth(method="lm",colour="darkorange",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# merged
merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)

data=merged %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

p1=ggplot(data=data, aes(x=pole_height , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
