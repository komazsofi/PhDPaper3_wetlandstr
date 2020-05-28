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

tisza_2m_r_sub_LAI=subset(tisza_2m_r,select=c(7,10,21,25))
names(tisza_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

tisza_2m_r_sub_LAI=tisza_2m_r_sub_LAI[tisza_2m_r_sub_LAI$class!="shrub",]
tisza_2m_r_sub_LAI=tisza_2m_r_sub_LAI[tisza_2m_r_sub_LAI$class!="tree",]

ferto_2m_r_sub_LAI=subset(ferto_2m_r,select=c(7,10,21,25))
names(ferto_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

ferto_2m_r_sub_LAI=ferto_2m_r_sub_LAI[ferto_2m_r_sub_LAI$class!="shrub",]
ferto_2m_r_sub_LAI=ferto_2m_r_sub_LAI[ferto_2m_r_sub_LAI$class!="tree",]

balaton_2m_r_sub_LAI=subset(balaton_2m_r,select=c(7,10,20,24))
names(balaton_2m_r_sub_LAI) <- c("class","LAI","NegOpeness","Pulsepen")

balaton_2m_r_sub_LAI=balaton_2m_r_sub_LAI[balaton_2m_r_sub_LAI$class!="shrub",]
balaton_2m_r_sub_LAI=balaton_2m_r_sub_LAI[balaton_2m_r_sub_LAI$class!="tree",]

# visualize pole metrics
lidarstr="Pulsepen"
field="LAI"

a=ggplot(data=tisza_2m_r_sub_LAI, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

aa=ggplot(data=ferto_2m_r_sub_LAI, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",colour='darkblue',se=FALSE,size=4)+
  stat_cor(size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

aaa=ggplot(data=balaton_2m_r_sub_LAI, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
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

# corrplot
cor(balaton_2m_r_sub_LAI[,c(3,4)], balaton_2m_r_sub_LAI[,2],  method = "pearson", use = "complete.obs")
cor.test(tisza_2m_r_sub_LAI[,3], tisza_2m_r_sub_LAI[,2], method = "pearson")
