library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis2/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_2m_r.csv")
ferto_2m_r=read.csv("ferto_2m_r.csv")
balaton_2m_r=read.csv("balaton_2m_r.csv")

tisza_2m_r$type<-"tisza_2m_r_leafoff"
ferto_2m_r$type<-"ferto_2m_r_leafoff"
balaton_2m_r$type<-"balaton_2m_r_leafoff"

tisza_2m_r_sub=subset(tisza_2m_r,select=c(15,18,20,19,17,14,22,8,9,10,11))
names(tisza_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                                     "sum_pole_contact","pole_lai","pole_fhd")

ferto_2m_r_sub=subset(ferto_2m_r,select=c(15,19,21,20,18,14,22,8,9,10,11))
names(ferto_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                         "sum_pole_contact","pole_lai","pole_fhd")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(15,18,20,19,17,14,21,8,9,10,11))
names(balaton_2m_r_sub)<-c("biomass","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type","pole_height",
                         "sum_pole_contact","pole_lai","pole_fhd")

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)

data=merged %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

p1=ggplot(data=data, aes(x=pole_height , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggsave("lakes_pole_height.png",plot = p1)
