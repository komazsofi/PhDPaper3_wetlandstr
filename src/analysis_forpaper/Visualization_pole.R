library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(olsrr)
library(corrplot)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis8/"
setwd(workdir)

####################################### Import

plot_data05=read.csv(paste("Pole_db_",0.5,"_filt.csv",sep=""))

plot_data05_scaled=scale(plot_data05[,c(4:11,18)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[plot_data05_f$OBJNAME!=187,]

plot_data2=read.csv(paste("Pole_db_",2.5,"_filt.csv",sep=""))

plot_data2_scaled=scale(plot_data2[,c(4:11,18)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
plot_data2_f=plot_data2_f[plot_data2_f$OBJNAME!=187,]

plot_data5=read.csv(paste("Pole_db_",5,"_filt.csv",sep=""))

plot_data5_scaled=scale(plot_data5[,c(4:11,18)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
plot_data5_f=plot_data5_f[plot_data5_f$OBJNAME!=187,]

############################################ Height

a5h=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

b5h=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

c5h=ggplot(data=plot_data5_f[plot_data5_f$lake!="Lake Balaton",], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(-1.5,3.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

b05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

c05h=ggplot(data=plot_data05_f[plot_data05_f$lake!="Lake Balaton",], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(-1.5,3.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2h=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

b2h=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(-1.5,3.2)

c2h=ggplot(data=plot_data2_f[plot_data2_f$lake!="Lake Balaton",], aes(x=Scaled_H_median , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(-1.5,3.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(a05h,b05h,c05h,
             a2h,b2h,c2h,
             a5h,b5h,c5h,
             nrow = 3,
             ncol = 3
)

###############

a5b=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

b5b=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

c5b=ggplot(data=plot_data5_f[plot_data5_f$lake!="Lake Balaton",], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(0.2,1.7)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05b=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

b05b=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

c05b=ggplot(data=plot_data05_f[plot_data05_f$lake!="Lake Balaton",], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(0.2,1.7)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2b=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

b2b=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,8.5)+
  xlim(0.2,1.7)

c2b=ggplot(data=plot_data2_f[plot_data2_f$lake!="Lake Balaton",], aes(x=Scaled_W_echw , y=gct_lai),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=class),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Leaf area (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,8.5)+
  xlim(0.2,1.7)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(a05b,b05b,c05b,
             a2b,b2b,c2b,
             a5b,b5b,c5b,
             nrow = 3,
             ncol = 3
)


grid.arrange(a2b,b2b,c2b,
             nrow = 1,
             ncol = 3
)