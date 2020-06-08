library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggpubr)

library(olsrr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis3/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_plot.csv")
ferto_2m_r=read.csv("ferto_plot.csv")
balaton_2m_r=read.csv("balaton_plot.csv")

tisza_2m_r$type<-"tisza"
ferto_2m_r$type<-"ferto"
balaton_2m_r$type<-"balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(42,8,38,39,41,
                                          15,17,18,20,21,22,23,25,26,28,29,30,19))
names(tisza_2m_r_sub) <- c("type","class","Height","FHD_bio","Biomass",
                           "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","NegOp","NofEmax","PSW","Pulsepen","FHD","Std_Z","Var_Z","Echowidth")

#tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="shrub",]
#tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="tree",]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(41,8,37,38,40,
                                          15,17,18,20,21,22,23,25,26,28,29,30,19))
names(ferto_2m_r_sub) <- c("type","class","Height","FHD_bio","Biomass",
                           "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","NegOp","NofEmax","PSW","Pulsepen","FHD","Std_Z","Var_Z","Echowidth")

#ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="shrub",]
#ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="tree",]

balaton_2m_r_sub=subset(balaton_2m_r,select=c(40,8,36,37,39,
                                                     15,17,18,19,20,21,22,24,25,27,28,29,1))
names(balaton_2m_r_sub) <- c("type","class","Height","FHD_bio","Biomass",
                             "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","NegOp","NofEmax","PSW","Pulsepen","FHD","Std_Z","Var_Z","Echowidth")
balaton_2m_r_sub$Echowidth<-NA

#balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="shrub",]
#balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="tree",]

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)
data_merged=merged %>% gather(-c(type,class,Height,FHD_bio,Biomass),key = "var", value = "value")

# which one?

ggplot(data=data_merged, aes(x=value , y=Height),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=Height,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=Height),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=Height,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# visualize pole metrics
lidarstr="P_95"
field="Height"

a=ggplot(data=tisza_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

aa=ggplot(data=ferto_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

aaa=ggplot(data=balaton_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

grid.arrange(
  aaa+ggtitle("Balaton"),
  aa+ggtitle("Ferto"),
  a+ggtitle("Tisza"),
  nrow = 1
)

ggplot(data=merged, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 
