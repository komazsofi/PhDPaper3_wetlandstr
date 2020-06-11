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

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(30,7,10,11,
                                          14,16,17,19,20,21,22,24,25,27,28,29,18))
names(tisza_2m_r_sub) <-  c("type","class","LAI","FHD_pole",
                            "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","C_nop","NofEmax","PSW",
                            "C_ppr","FHD","Std_Z","Var_Z","VV_echw")

tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="shrub",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="tree",]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(30,7,10,11,
                                          14,16,17,19,20,21,22,24,25,27,28,29,18))
names(ferto_2m_r_sub) <- c("type","class","LAI","FHD_pole",
                               "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","C_nop","NofEmax","PSW",
                               "C_ppr","FHD","Std_Z","Var_Z","VV_echw")

ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="shrub",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="tree",]

balaton_2m_r_sub=subset(balaton_2m_r,select=c(29,7,10,11,
                                              14,16,17,18,19,20,21,23,24,26,27,28,1))
names(balaton_2m_r_sub) <- c("type","class","LAI","FHD_pole",
                             "Mean_Z","SigmaZ","Lbiomass","Median_Z","P_95","C_nop","NofEmax","PSW",
                             "C_ppr","FHD","Std_Z","Var_Z","VV_echw")
balaton_2m_r_sub$VV_echw<-NA

balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="shrub",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="tree",]

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)
data_merged=merged %>% gather(-c(type,class,LAI,FHD_pole),key = "var", value = "value")

# which one?

ggplot(data=data_merged, aes(x=value , y=LAI),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method = "gam")+
  stat_cor(method = "spearman") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=LAI),show.legend = TRUE) +  
  geom_point(aes(colour=class)) +
  geom_smooth(method = "gam")+
  stat_cor(method = "spearman") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=LAI,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method = "gam")+
  stat_cor(method = "spearman") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=LAI),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=data_merged, aes(x=value , y=LAI,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# visualize pole metrics
lidarstr="C_nop"
field="LAI"

a=ggplot(data=tisza_2m_r_sub, aes_string(x=field , y=lidarstr),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="gam",size=4)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,6.5)+ylim(-4,6)

aa=ggplot(data=ferto_2m_r_sub, aes_string(x=field , y=lidarstr),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="gam",size=4)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,6.5)+ylim(-4,6)

aaa=ggplot(data=balaton_2m_r_sub, aes_string(x=field , y=lidarstr),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="gam",size=4)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,6.5)+ylim(-4,6)

grid.arrange(
  aaa+ggtitle("Balaton"),
  aa+ggtitle("Ferto"),
  a+ggtitle("Tisza"),
  nrow = 1
)

ggplot(data=merged, aes_string(x=field , y=lidarstr),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="gam",size=4)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ggplot(data=merged, aes_string(x=field , y=lidarstr),show.legend = TRUE) +  
  geom_point(aes(color=type),size=4) +
  geom_smooth(method="gam",size=4)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 