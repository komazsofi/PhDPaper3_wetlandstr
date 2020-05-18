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

plot_tisza_2m_r=read.csv("tisza_plot.csv")
plot_ferto_2m_r=read.csv("ferto_plot.csv")
plot_balaton_2m_r=read.csv("balaton_plot.csv")

plot_tisza_2m_r_sub=subset(plot_tisza_2m_r,select=c(15,17:18,21:30,38,39,40,41))
names(plot_tisza_2m_r_sub)<-c("MeanZ","SigmaZ","LBiomass","95P_Z","NegOpp","NofEmax","pdens","psw","pulsepen","rao_fhd",
                              "FHD","Std_Z","VarZ","Veg_h","fhd_bio","sum_leaf_w","biomass")

plot_ferto_2m_r_sub=subset(plot_ferto_2m_r,select=c(15,17:18,21:30,37,38,39,40))
names(plot_ferto_2m_r_sub)<-c("MeanZ","SigmaZ","LBiomass","95P_Z","NegOpp","NofEmax","pdens","psw","pulsepen","rao_fhd",
                              "FHD","Std_Z","VarZ","Veg_h","fhd_bio","sum_leaf_w","biomass")

plot_balaton_2m_r_sub=subset(plot_balaton_2m_r,select=c(15,17:18,20:29,36,37,38,39))
names(plot_balaton_2m_r_sub)<-c("MeanZ","SigmaZ","LBiomass","95P_Z","NegOpp","NofEmax","pdens","psw","pulsepen","rao_fhd",
                              "FHD","Std_Z","VarZ","Veg_h","fhd_bio","sum_leaf_w","biomass")

plot_tisza_2m_r_sub$type <- "tisza"
plot_ferto_2m_r_sub$type <- "ferto"
plot_balaton_2m_r_sub$type <- "balaton"

merged=rbind(plot_tisza_2m_r_sub,plot_ferto_2m_r_sub,plot_balaton_2m_r_sub)

data=merged %>% gather(-c(type,Veg_h,fhd_bio,sum_leaf_w,biomass),key = "var", value = "value")

p1=ggplot(data=data, aes(x=Veg_h , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",aes(color=type)) +
  geom_smooth(method="lm",colour="black",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

p2=ggplot(data=data, aes(x=fhd_bio , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",aes(color=type)) +
  geom_smooth(method="lm",colour="black",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

p4=ggplot(data=data, aes(x=biomass , y=value, color=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",aes(color=type)) +
  geom_smooth(method="lm",colour="black",se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 


