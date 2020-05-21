library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis3/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_2m_r_v2.csv")
ferto_2m_r=read.csv("ferto_2m_r_v2.csv")
balaton_2m_r=read.csv("balaton_2m_r_v2.csv")

tisza_2m_r$type<-"tisza_2m_r_leafoff"
ferto_2m_r$type<-"ferto_2m_r_leafoff"
balaton_2m_r$type<-"balaton_2m_r_leafoff"

tisza_2m_r_sub=subset(tisza_2m_r,select=c(14:29,8,9,10,11))
names(tisza_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                                     "sum_pole_contact","pole_lai","pole_fhd")

ferto_2m_r_sub=subset(ferto_2m_r,select=c(14:29,8,9,10,11))
names(ferto_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                         "sum_pole_contact","pole_lai","pole_fhd")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(14:28,8,9,10,11))
names(balaton_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                         "sum_pole_contact","pole_lai","pole_fhd")

# per lake

data_tisza=tisza_2m_r_sub %>% gather(-c(pl_hght,sm_pl_c,gct_lai,fhd_pol),key = "var", value = "value")

ggplot(data=data_tisza, aes(x=gct_lai , y=value),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour="black",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

data_ferto=ferto_2m_r_sub %>% gather(-c(pl_hght,sm_pl_c,gct_lai,fhd_pol),key = "var", value = "value")

ggplot(data=data_ferto, aes(x=gct_lai , y=value),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour="black",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

data_balaton=balaton_2m_r_sub %>% gather(-c(pl_hght,sm_pl_c,gct_lai,fhd_pol),key = "var", value = "value")

ggplot(data=data_balaton, aes(x=gct_lai , y=value),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour="black",se=FALSE)+
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
