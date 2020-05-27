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

# For LAI

tisza_2m_r_sub_LAI=subset(tisza_2m_r,select=c(7,10,21,25))
names(tisza_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

tisza_2m_r_sub_LAI=tisza_2m_r_sub_LAI[tisza_2m_r_sub_LAI$class!="shrub",]

a=ggplot(data=tisza_2m_r_sub_LAI, aes(x=NegOpeness , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

b=ggplot(data=tisza_2m_r_sub_LAI, aes(x=Pulsepen , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

ferto_2m_r_sub_LAI=subset(ferto_2m_r,select=c(7,10,21,25))
names(ferto_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

ferto_2m_r_sub_LAI=ferto_2m_r_sub_LAI[ferto_2m_r_sub_LAI$class!="shrub",]
ferto_2m_r_sub_LAI=ferto_2m_r_sub_LAI[ferto_2m_r_sub_LAI$class!="tree",]

aa=ggplot(data=ferto_2m_r_sub_LAI, aes(x=NegOpeness , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

bb=ggplot(data=ferto_2m_r_sub_LAI, aes(x=Pulsepen , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

balaton_2m_r_sub_LAI=subset(balaton_2m_r,select=c(7,10,20,24))
names(balaton_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

balaton_2m_r_sub_LAI=balaton_2m_r_sub_LAI[balaton_2m_r_sub_LAI$class!="shrub",]
balaton_2m_r_sub_LAI=balaton_2m_r_sub_LAI[balaton_2m_r_sub_LAI$class!="tree",]

aaa=ggplot(data=balaton_2m_r_sub_LAI, aes(x=NegOpeness , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

bbb=ggplot(data=balaton_2m_r_sub_LAI, aes(x=Pulsepen , y=LAI),show.legend = TRUE) +  
  geom_point(aes(color=class)) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE)+
  stat_cor() +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

grid.arrange(
  b,
  bb,
  bbb,
  nrow = 1
)



