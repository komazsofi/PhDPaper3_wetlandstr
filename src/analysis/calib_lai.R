library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis5/"
setwd(workdir)

# Import

balaton_m=read.csv("Balaton_lidarmetrics_5.csv")
tisza_m=read.csv("Tisza_lidarmetrics_5.csv")
ferto_m=read.csv("Ferto_lidarmetrics_5.csv")
tisza_m_leafon=read.csv("Tisza_lidarmetrics_5_leafon.csv")

tisza_2m_r=read.csv("tisza_2m_r_v3.csv")
ferto_2m_r=read.csv("ferto_2m_r_v3.csv")
balaton_2m_r=read.csv("balaton_2m_r_v3.csv")
tisza_2m_r_leafon=read.csv("tisza_2m_r_leafon_v3.csv")

fieldsp_tisza = readOGR(dsn="tisza_full.shp")
fieldsp_tisza_df=fieldsp_tisza@data
fieldsp_tisza_df_sel=fieldsp_tisza_df[,c(1,2,17,19)]

fieldsp_ferto = readOGR(dsn="w_point.shp")
fieldsp_ferto_df=fieldsp_ferto@data
fieldsp_ferto_df_sel=fieldsp_ferto_df[,c(1,2,20,22)]

fieldsp_balaton = readOGR(dsn="w_point_balaton.shp")
fieldsp_balaton_df=fieldsp_balaton@data
fieldsp_balaton_df_sel=fieldsp_balaton_df[,c(1,2,20,22)]

# cleaining

balaton_m_c=balaton_m[complete.cases(balaton_m), ]
tisza_m_c=tisza_m[complete.cases(tisza_m), ]
ferto_m_c=ferto_m[complete.cases(ferto_m), ]
tisza_m_c_leafon=tisza_m[complete.cases(tisza_m_leafon), ]

# merge

balaton_plot=merge(balaton_m_c,fieldsp_balaton_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
balaton_plot2=merge(balaton_plot,balaton_2m_r[c(3,19,25,17,22)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))
balaton_plot2$EchoWidth2EchoWidthmedian.grd<-0

tisza_plot=merge(tisza_m_c,fieldsp_tisza_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot2=merge(tisza_plot,tisza_2m_r[c(3,19,26,17,23,20)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))

ferto_plot=merge(ferto_m_c,fieldsp_ferto_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
ferto_plot2=merge(ferto_plot,ferto_2m_r[c(3,19,26,17,23,20)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))

tisza_plot_leafon=merge(tisza_m_c_leafon,fieldsp_tisza_df_sel, by.x=c('OBJNAME'), by.y=c('OBJNAME'))
tisza_plot2_leafon=merge(tisza_plot_leafon,tisza_2m_r_leafon[c(3,19,26,17,23,20)], by.x=c('OBJNAME'), by.y=c('OBJNAME'))

balaton_plot3=balaton_plot2[c(5:23,25:31,1)]
ferto_plot3=ferto_plot2[c(5:23,25:31,1)]
tisza_plot3=tisza_plot2[c(3:21,23:29,1)]
tisza_plot3_leafon=tisza_plot2_leafon[c(3:21,23:29,1)]

balaton_plot3$lake="Lake Balaton"
ferto_plot3$lake="Lake Ferto"
tisza_plot3$lake="Lake Tisza"
tisza_plot3_leafon$lake="Lake Tisza leaf-on"

merged=rbind(balaton_plot3,ferto_plot3,tisza_plot3,tisza_plot3_leafon)

merged=merged[merged$class!="water",]
merged=merged[merged$class!="tree",]
merged=merged[merged$class!="land",]
merged=merged[merged$class!="grassland",]
merged=merged[merged$class!="scirpus",]
merged=merged[merged$class!="mudflat",]
merged=merged[merged$class!="artificial",]
merged=merged[merged$class!="shrub",]
merged=merged[merged$class!="not_known",]
merged=merged[merged$class!="flat",]
merged=merged[merged$class!="shrubs",]
merged=merged[merged$class!="trees",]

merged=merged[complete.cases(merged), ]
merged$class=str_replace(merged$class,"reed","phragmites")

merged_balaton=merged[merged$lake=="Lake Balaton",]
merged_ferto=merged[merged$lake=="Lake Ferto",]
merged_tisza=merged[merged$lake=="Lake Tisza",]
merged_tisza_leafon=merged[merged$lake=="Lake Tisza leaf-on",]

merged_tisza_filt05<-subset(merged_tisza, OBJNAME %in% c(118,119,120,121,122,123,132,133,134,149,150,151,152,153,154,155,156,157,157,158,158,159,176,176,177,177,179,179,180,180,181,181,182,182,183, 183,184,184,185,186,187))
merged_ferto_filt05<-subset(merged_ferto, OBJNAME %in% c(119,121,123,125,131,132,133,135,136,137,162,163,164,165,166,168,169,170,174,175,198,199,200,203,225,227,228,229,230,231,232,233,251,252,255, 256,313,315,316,317,319,321,321,322,323,323,325,325,327,327))
merged_balaton_filt05<-subset(merged_balaton, OBJNAME %in% c(102,103,104,117,119,120,121,132,134,159,160,162,200,203,204,205,206,207,208,243,244,247,248,250,251,252))
merged_tisza_filt05_leafon<-subset(merged_tisza_leafon, OBJNAME %in% c(118,119,120,121,122,123,132,133,134,151,152,153,154,155,156,156,157,157,158,158,159,176,176,177,177,179,179,180,180,181,181,182,182,183,183, 184,184,185,186,187))

merged2=rbind(merged_tisza_filt05,merged_ferto_filt05)
merged3=rbind(merged2,merged_balaton_filt05)
merged4=rbind(merged3,merged_tisza_filt05_leafon)

# visualize metrics against field data

merged_forvis=merged4 %>% gather(-c(gct_lai,class,lake,OBJNAME),key = "var", value = "value")

ggplot(data=merged_forvis, aes(x=value , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(shape=class,colour=lake)) +
  facet_wrap(~var,scales = "free") +
  theme_minimal() +
  ylab("LAI") +
  xlab("LiDAR metrics") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) 

# biomass

round(cor(merged[,c(1,7:19)], method="spearman"),2) # 

# linear regression across all lakes
lm_lai<-lm(gct_lai~C_ppr2+A_std+EchoWidth2EchoWidthmedian.grd,data=merged_tisza_filt05)
summary(lm_lai)

lm_lai<-lm(gct_lai~C_ppr2+A_std+EchoWidth2EchoWidthmedian.grd,data=merged_tisza_filt05_leafon)
summary(lm_lai)

lm_lai<-lm(gct_lai~C_ppr2+A_std+EchoWidth2EchoWidthmedian.grd,data=merged_ferto_filt05)
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

# non fwf
lm_lai<-lm(gct_lai~C_ppr2+A_std,data=merged_balaton_filt05)
summary(lm_lai)

#AIC model selection (step)
lm_lai_step<-step(lm_lai)
summary(lm_lai_step)

# visualization (used ones)

p1=ggplot(data=merged_ferto_filt05,aes(x=C_ppr2,y=gct_lai))+geom_point(aes(shape=class),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)+xlab("C_ppr2")+ylab("LAI")
p2=ggplot(data=merged_ferto_filt05,aes(x=A_std,y=gct_lai))+geom_point(aes(shape=class),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)+xlab("A_std")+ylab("LAI")
p3=ggplot(data=merged_ferto_filt05,aes(x=EchoWidth2EchoWidthmedian.grd,y=gct_lai))+geom_point(aes(shape=class),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)+xlab("V_echw")+ylab("LAI")

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1
)
