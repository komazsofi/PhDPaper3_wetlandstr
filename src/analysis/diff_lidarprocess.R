library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis2/"
setwd(workdir)

tisza_05m_r_leafon=read.csv("tisza_05m_r_leafon.csv")
tisza_1m_r_leafon=read.csv("tisza_1m_r_leafon.csv")
tisza_2m_r_leafon=read.csv("tisza_2m_r_leafon.csv")
tisza_2m_pc_leafon=read.csv("tisza_2m_pc_leafon.csv")

tisza_05m_r_leafon$type<-"tisza_05m_r_leafon"
tisza_1m_r_leafon$type<-"tisza_1m_r_leafon"
tisza_2m_r_leafon$type<-"tisza_2m_r_leafon"
tisza_2m_pc_leafon$type<-"tisza_2m_pc_leafon"

tisza_2m_pc_leafon_sub=subset(tisza_2m_pc_leafon,select=c(14,15,16,17,18,19,20,21))
names(tisza_2m_pc_leafon_sub)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type")

tisza_2m_r_leafon_sub=subset(tisza_2m_r_leafon,select=c(15,16,18,20,19,17,14,22))
names(tisza_2m_r_leafon_sub)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type")

tisza_1m_r_leafon_sub=subset(tisza_1m_r_leafon,select=c(15,16,18,20,19,17,14,22))
names(tisza_1m_r_leafon_sub)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type")

tisza_05m_r_leafon_sub=subset(tisza_05m_r_leafon,select=c(15,16,18,20,19,17,14,22))
names(tisza_05m_r_leafon_sub)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","type")

# boxplot about metrics
tisza_leafon_merged=rbind(tisza_05m_r_leafon_sub,tisza_1m_r_leafon_sub,tisza_2m_r_leafon_sub,tisza_2m_pc_leafon_sub)

data_sel2=tisza_leafon_merged %>% gather(-type,key = "var", value = "value")

b1=ggplot(data=data_sel2, aes(x=type , y=value, fill=type),show.legend = TRUE) +  
  geom_boxplot() +
  facet_wrap(~var,scales = "free") +
  scale_fill_manual(values=c("goldenrod4","green3","deeppink","black")) +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("tisza_leafon_all.png",plot = b1,width = 10, height = 10)

tisza_leafon_merged_r=rbind(tisza_05m_r_leafon_sub,tisza_1m_r_leafon_sub,tisza_2m_r_leafon_sub)

data_sel=tisza_leafon_merged_r %>% gather(-type,key = "var", value = "value")

b2=ggplot(data=data_sel, aes(x=type , y=value, fill=type),show.legend = TRUE) +  
  geom_boxplot() +
  facet_wrap(~var,scales = "free") +
  scale_fill_manual(values=c("goldenrod4","green3","deeppink")) +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggsave("tisza_leafon_onlyraster.png",plot = b2,width = 10, height = 10)

# scatter plots lidar metrics against veg str parameters

tisza_2m_r_leafon_sub_fstr=subset(tisza_2m_r_leafon,select=c(15,16,18,20,19,17,14,21,22,8,9,10,11))
names(tisza_2m_r_leafon_sub_fstr)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","fhd","type","pole_height",
                                "sum_pole_contact","pole_lai","pole_fhd")

tisza_1m_r_leafon_sub_fstr=subset(tisza_1m_r_leafon,select=c(15,16,18,20,19,17,14,21,22,8,9,10,11))
names(tisza_1m_r_leafon_sub_fstr)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","fhd","type","pole_height",
                                     "sum_pole_contact","pole_lai","pole_fhd")

tisza_05m_r_leafon_sub_fstr=subset(tisza_05m_r_leafon,select=c(15,16,18,20,19,17,14,21,22,8,9,10,11))
names(tisza_05m_r_leafon_sub_fstr)<-c("biomass","echowidth","openness","pulsepen","pseudowaveform","h90thperc","sigmaz","fhd","type","pole_height",
                                     "sum_pole_contact","pole_lai","pole_fhd")

tisza_leafon_merged_r_fstr=rbind(tisza_2m_r_leafon_sub_fstr,tisza_1m_r_leafon_sub_fstr,tisza_05m_r_leafon_sub_fstr)

data_sel3=tisza_leafon_merged_r_fstr %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

ggplot(data=data_sel3, aes(x=pole_lai , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# scatter plots lidar metrics against veg str parameters only one spatial resolution

tisza_2m_r_leafon=tisza_2m_r_leafon_sub_fstr %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

p1=ggplot(data=tisza_2m_r_leafon, aes(x=pole_fhd , y=value),show.legend = TRUE) +  
  geom_point(colour="deeppink") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggsave("tisza_2m_r_leafon_pole_fhd.png",plot = p1)

tisza_1m_r_leafon=tisza_1m_r_leafon_sub_fstr %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

p2=ggplot(data=tisza_1m_r_leafon, aes(x=pole_fhd , y=value),show.legend = TRUE) +  
  geom_point(colour="green3") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggsave("tisza_1m_r_leafon_pole_fhd.png",plot = p2)

tisza_05m_r_leafon=tisza_05m_r_leafon_sub_fstr %>% gather(-c(type,pole_height,sum_pole_contact,pole_lai,pole_fhd),key = "var", value = "value")

p3=ggplot(data=tisza_05m_r_leafon, aes(x=pole_fhd , y=value),show.legend = TRUE) +  
  geom_point(colour="goldenrod4") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggsave("tisza_05m_r_leafon_pole_fhd.png",plot = p3)