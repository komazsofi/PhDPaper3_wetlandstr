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

tisza_2m_r=read.csv("tisza_plot_2.csv")
ferto_2m_r=read.csv("ferto_plot_2.csv")
balaton_2m_r=read.csv("balaton_plot_2.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(48,8,42,13,14,47,
                                          31,20,24,18,28,29))
names(tisza_2m_r_sub) <- c("lake","class","FHD","x","y","type",
                           "V_fhd","C_er","C_negopp","V_sigmaz","C_ppr","V_fhdrao")

tisza_2m_r_sub=tisza_2m_r_sub[is.na(tisza_2m_r_sub$class)==FALSE,]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(47,8,41,13,14,46,
                                          31,20,24,18,28,29))
names(ferto_2m_r_sub) <- c("lake","class","FHD","x","y","type",
                           "V_fhd","C_er","C_negopp","V_sigmaz","C_ppr","V_fhdrao")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(47,8,41,13,14,46,
                                              31,20,23,18,27,28))
names(balaton_2m_r_sub) <- c("lake","class","FHD","x","y","type",
                             "V_fhd","C_er","C_negopp","V_sigmaz","C_ppr","V_fhdrao")

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

# check correlation among metrics
merged=merged[complete.cases(merged), ]
summary(merged)

round(cor(merged[,c(7:12)], method="spearman"),2)

# linear regression across all lakes

lm_fhd<-lm(FHD~V_fhd,data=merged)
summary(lm_fhd)

lm_fhd<-lm(FHD~V_fhd+C_negopp+V_sigmaz+C_ppr,data=merged)
summary(lm_fhd)

#Residual histogram
hist((residuals(lm_fhd)))

lm_fhd_step<-step(lm_fhd)
summary(lm_fhd_step)

# linear regression across fwf

lm_fhd_fwf<-lm(FHD~V_fhd,data=merged[merged$lake!="Lake Balaton",])
summary(lm_fhd_fwf)

lm_fhd_fwf<-lm(FHD~V_fhd+C_negopp+V_sigmaz+C_ppr,data=merged[merged$lake!="Lake Balaton",])
summary(lm_fhd_fwf)

#Residual histogram
hist((residuals(lm_fhd)))

lm_fhd_step<-step(lm_fhd_fwf)
summary(lm_fhd_step)

# linear regression across nonfwf

lm_fhd_nfwf<-lm(FHD~V_fhd,data=merged[merged$lake=="Lake Balaton",])
summary(lm_fhd_nfwf)

lm_fhd_nfwf<-lm(FHD~V_fhd+C_negopp+V_sigmaz+C_ppr,data=merged[merged$lake=="Lake Balaton",])
summary(lm_fhd_nfwf)

#Residual histogram
hist((residuals(lm_fhd)))

lm_fhd_step<-step(lm_fhd_nfwf)
summary(lm_fhd_step)

# Visualization

merged$id=seq(1,33,1)

ggplot(data=merged, aes(x=C_ppr, y=FHD),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")


