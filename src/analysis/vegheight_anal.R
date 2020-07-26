library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(ggpubr)

library(olsrr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis4/"
setwd(workdir)

tisza_2m_r=read.csv("tisza_plot.csv")
ferto_2m_r=read.csv("ferto_plot.csv")
balaton_2m_r=read.csv("balaton_plot.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(46,8,41,13,14,45,
                                          15,22,23,30,32,33))
names(tisza_2m_r_sub) <- c("lake","class","Height","x","y","type",
                           "H_mean","H_median","H_95p","H_max","V_std","V_var")

tisza_2m_r_sub=tisza_2m_r_sub[is.na(tisza_2m_r_sub$class)==FALSE,]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(45,8,40,13,14,44,
                                          15,22,23,30,32,33))
names(ferto_2m_r_sub) <- c("lake","class","Height","x","y","type",
                           "H_mean","H_median","H_95p","H_max","V_std","V_var")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(45,8,40,13,14,44,
                                              15,21,22,29,32,33))
names(balaton_2m_r_sub) <- c("lake","class","Height","x","y","type",
                             "H_mean","H_median","H_95p","H_max","V_std","V_var")

#Filters
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="shrub",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="tree",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="grassland",]
tisza_2m_r_sub=tisza_2m_r_sub[tisza_2m_r_sub$class!="scirpus",]

ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="shrub",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="tree",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="grassland",]
ferto_2m_r_sub=ferto_2m_r_sub[ferto_2m_r_sub$class!="scirpus",]

balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="shrub",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="tree",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="grassland",]
balaton_2m_r_sub=balaton_2m_r_sub[balaton_2m_r_sub$class!="scirpus",]

tisza_2m_r_sub$class=str_replace(tisza_2m_r_sub$class,"reed","phragmites")
ferto_2m_r_sub$class=str_replace(ferto_2m_r_sub$class,"reed","phragmites")
balaton_2m_r_sub$class=str_replace(balaton_2m_r_sub$class,"reed","phragmites")

#merging

merged=rbind(tisza_2m_r_sub,ferto_2m_r_sub,balaton_2m_r_sub)
data_merged=merged %>% gather(-c(class,Height,lake,type,x,y),key = "var", value = "value")

# check correlation among metrics
merged=merged[complete.cases(merged), ]
summary(merged)

round(cor(merged[,c(7,9,10,11,12)], method="spearman"),2) # median excluded: only reflects on the ground points (most of the points are ground - shall we calculate only for vegetation?)

# Select: H_max - that would be the least sensitive for ground points and otherwise all metrics are highly correlated >0.8 spearman r

# linear regression across all lakes

lm_height<-lm(Height~H_max,data=merged)
summary(lm_height)

#Residual histogram
hist((residuals(lm_height)))

lm_height_log<-lm(Height~log(H_max),data=merged)
summary(lm_height_log)

# linear regression across only FWF

lm_height_fwf<-lm(Height~H_max,data=merged[merged$lake!="Lake Balaton",])
summary(lm_height_fwf)

#Residual histogram
hist((residuals(lm_height_fwf)))

# linear regression across only non-FWF

lm_height_nonfwf<-lm(Height~H_max,data=merged[merged$lake=="Lake Balaton",])
summary(lm_height_nonfwf)

#Residual histogram
hist((residuals(lm_height_nonfwf)))

# Visualization

merged$id=seq(1,33,1)

ggplot(data=merged, aes_string(x="H_max", y="Height"),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged[merged$lake!="Lake Balaton",], aes_string(x="H_max", y="Height"),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")