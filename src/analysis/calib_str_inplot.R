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

tisza_2m_r=read.csv("tisza_plot_edited.csv")
ferto_2m_r=read.csv("ferto_plot_edited.csv")
balaton_2m_r=read.csv("balaton_plot_edited.csv")

tisza_2m_r$type<-"FWF"
ferto_2m_r$type<-"FWF"
balaton_2m_r$type<-"non-FWF"

tisza_2m_r$lake<-"Lake Tisza"
ferto_2m_r$lake<-"Lake Ferto"
balaton_2m_r$lake<-"Lake Balaton"

# Prep data 

tisza_2m_r_sub=subset(tisza_2m_r,select=c(43,8,38,39,41,7,13,14,42,
                                          15,17,18,20,21,22,23,25,26,28,29,30,19))
names(tisza_2m_r_sub) <- c("lake","class","Height","FHD_bio","Biomass","veg_type","x","y","type",
                           "H_m","VV_sigmaz","VV_lbio","H_50p","H_95p","C_negop","VV_nofech","VV_psw","C_ppr","VV_fhd","VV_std","VV_var","VV_echw")

tisza_2m_r_sub=tisza_2m_r_sub[is.na(tisza_2m_r_sub$class)==FALSE,]

ferto_2m_r_sub=subset(ferto_2m_r,select=c(42,8,37,38,40,7,13,14,41,
                                          15,17,18,20,21,22,23,25,26,28,29,30,19))
names(ferto_2m_r_sub) <- c("lake","class","Height","FHD_bio","Biomass","veg_type","x","y","type",
                           "H_m","VV_sigmaz","VV_lbio","H_50p","H_95p","C_negop","VV_nofech","VV_psw","C_ppr","VV_fhd","VV_std","VV_var","VV_echw")

balaton_2m_r_sub=subset(balaton_2m_r,select=c(41,8,36,37,39,7,13,14,40,
                                                     15,17,18,19,20,21,22,24,25,27,28,29,1))
names(balaton_2m_r_sub) <- c("lake","class","Height","FHD_bio","Biomass","veg_type","x","y","type",
                             "H_m","VV_sigmaz","VV_lbio","H_50p","H_95p","C_negop","VV_nofech","VV_psw","C_ppr","VV_fhd","VV_std","VV_var","VV_echw")
balaton_2m_r_sub$VV_echw<-NA

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
data_merged=merged %>% gather(-c(veg_type,class,Height,FHD_bio,Biomass,lake,type),key = "var", value = "value")

# which one?

# all together

ggplot(data=data_merged, aes(x=value , y=FHD_bio),show.legend = TRUE) +  
  geom_point(aes(colour=class)) +
  geom_smooth(method = "gam")+
  stat_cor(method = "spearman") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Height") +
  xlab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+ggtitle("Spearman correlation coefficient")

ggplot(data=data_merged, aes(x=value , y=FHD_bio),show.legend = TRUE) +  
  geom_point(aes(colour=class)) +
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Height") +
  xlab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+ggtitle("Pearson correlation coefficient")

# FWF, non-FWF

ggplot(data=data_merged, aes(x=value , y=FHD_bio,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method = "gam")+
  stat_cor(method = "spearman") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Height") +
  xlab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+ggtitle("Spearman correlation coefficient")

ggplot(data=data_merged, aes(x=value , y=FHD_bio,colour=type),show.legend = TRUE) +  
  geom_point() +
  geom_smooth(method = "lm")+
  stat_cor(method = "pearson") +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("Height") +
  xlab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1))+ggtitle("Pearson correlation coefficient")

# per lakes
lidarstr="VV_fhd"
field="FHD_bio"

a=ggplot(data=tisza_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5) 

aa=ggplot(data=ferto_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5)

aaa=ggplot(data=balaton_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5)

aaaa=ggplot(data=merged, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

grid.arrange(
  aaa+ggtitle("Balaton"),
  aa+ggtitle("Ferto"),
  a+ggtitle("Tisza"),
  nrow = 1
)

a=ggplot(data=tisza_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=type),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5) 

aa=ggplot(data=ferto_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=type),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5)

aaa=ggplot(data=balaton_2m_r_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=type),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+xlim(0,2)+ylim(0,5)

aaaa=ggplot(data=merged, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=class),size=4) +
  geom_smooth(method="lm",formula=y~poly(x,1),colour='darkblue',size=2,se=FALSE)+
  geom_smooth(se=FALSE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

grid.arrange(
  aaa+ggtitle("Balaton"),
  aa+ggtitle("Ferto"),
  a+ggtitle("Tisza"),
  nrow = 1
)

# FWF, non-FWF
lidarstr="VV_lbio"
field="Biomass"

merged$id=seq(1,34,1)

ggplot(data=merged, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

merged_sub=merged[merged$lake!="Lake Balaton",]

ggplot(data=merged_sub, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0)+
  ggtitle("Spearman correlation coefficient")

merged_sub2=merged[merged$lake=="Lake Balaton",]

ggplot(data=merged_sub2, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0)+
  ggtitle("Spearman correlation coefficient")

# export
write.csv(merged,"Plot_merged_cleaned.csv")

# Extra attributes

merged_withextra=read.csv("Plot_merged_cleaned2.csv")

ggplot(data=merged_withextra, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=density,shape=composition),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged_withextra, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=age,shape=composition),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged_withextra, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=knick,shape=composition),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged_withextra, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=ruderal,shape=composition),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")

ggplot(data=merged_withextra, aes_string(x=lidarstr , y=field),show.legend = TRUE) +  
  geom_point(aes(color=dieback,shape=composition),size=4) +
  geom_smooth(se=TRUE)+
  stat_cor(method = "spearman",size=5) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_text(aes(label=id),hjust=0, vjust=0) +
  ggtitle("Spearman correlation coefficient")
