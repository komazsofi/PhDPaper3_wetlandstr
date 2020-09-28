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

plot_data05=read.csv(paste("Plot_db_",0.5,".csv",sep=""))
plot_data05$total.weight=plot_data05$total.weight/10000

plot_data05_scaled=scale(plot_data05[,c(3:10)])
colnames(plot_data05_scaled)=paste("Scaled_",colnames(plot_data05_scaled),sep="")
plot_data05_f=cbind(plot_data05,plot_data05_scaled)
plot_data05_f=plot_data05_f[(plot_data05_f$OBJNAME!=120 & plot_data05_f$OBJNAME!=209 & plot_data05_f$OBJNAME!=163),]

plot_data2=read.csv(paste("Plot_db_",2.5,".csv",sep=""))
plot_data2$total.weight=plot_data2$total.weight/10000

plot_data2_scaled=scale(plot_data2[,c(3:10)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
plot_data2_f=plot_data2_f[(plot_data2_f$OBJNAME!=120 & plot_data2_f$OBJNAME!=209 & plot_data2_f$OBJNAME!=163),]

plot_data5=read.csv(paste("Plot_db_",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

plot_data5_scaled=scale(plot_data5[,c(3:10)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
plot_data5_f=plot_data5_f[(plot_data5_f$OBJNAME!=120 & plot_data5_f$OBJNAME!=209 & plot_data5_f$OBJNAME!=163),]

############################################ Height

a5h=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

d5h=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

b5h=ggplot(data=plot_data5_f[plot_data5_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)

c5h=ggplot(data=plot_data5_f, aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2h=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

d2h=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

b2h=ggplot(data=plot_data2_f[plot_data2_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)

c2h=ggplot(data=plot_data2_f, aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

d05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-1.2,2.2)

b05h=ggplot(data=plot_data05_f[plot_data05_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)

c05h=ggplot(data=plot_data05_f, aes(x=Scaled_V_var , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(d05h,a05h,b05h,c05h,
             d2h,a2h,b2h,c2h,
             d5h,a5h,b5h,c5h,
             nrow = 3,
             ncol = 4
)

###############

a5h_2=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

d5h_2=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

b5h_2=ggplot(data=plot_data5_f[plot_data5_f$lake=="Lake Balaton",], aes(x=Scaled_A_std  , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)

c5h_2=ggplot(data=plot_data5_f, aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2h_2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

d2h_2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

b2h_2=ggplot(data=plot_data2_f[plot_data2_f$lake=="Lake Balaton",], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)

c2h_2=ggplot(data=plot_data2_f, aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05h_2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

d05h_2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,5)+
  xlim(-2.5,2.2)

b05h_2=ggplot(data=plot_data05_f[plot_data05_f$lake=="Lake Balaton",], aes(x=Scaled_A_std  , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)

c05h_2=ggplot(data=plot_data05_f, aes(x=Scaled_A_std , y=veg_height_m),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Height (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,5)+
  xlim(-2.5,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(d05h_2,a05h_2,b05h_2,c05h_2,
             d2h_2,a2h_2,b2h_2,c2h_2,
             d5h_2,a5h_2,b5h_2,c5h_2,
             nrow = 3,
             ncol = 4
)

############################################ Biomass

a5=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

d5=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

b5=ggplot(data=plot_data5_f[plot_data5_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)

c5=ggplot(data=plot_data5_f, aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

d2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

b2=ggplot(data=plot_data2_f[plot_data2_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)

c2=ggplot(data=plot_data2_f, aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

d05=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-1.2,2.2)

b05=ggplot(data=plot_data05_f[plot_data05_f$lake=="Lake Balaton",], aes(x=Scaled_V_var  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)

c05=ggplot(data=plot_data05_f, aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-1.2,2.2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(d05,a05,b05,c05,
             d2,a2,b2,c2,
             d5,a5,b5,c5,
             nrow = 3,
             ncol = 4
)

###############

a5_2=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

d5_2=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

b5_2=ggplot(data=plot_data5_f[plot_data5_f$lake=="Lake Balaton",], aes(x=Scaled_A_cover  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)

c5_2=ggplot(data=plot_data5_f, aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a2_2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Tisza"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

d2_2=ggplot(data=plot_data2_f[(plot_data2_f$lake=="Lake Ferto"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

b2_2=ggplot(data=plot_data2_f[plot_data2_f$lake=="Lake Balaton",], aes(x=Scaled_A_cover  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)

c2_2=ggplot(data=plot_data2_f, aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

a05_2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="blue")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

d05_2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="darkgreen")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  ylim(0,2)+
  xlim(-2,2)

b05_2=ggplot(data=plot_data05_f[plot_data05_f$lake=="Lake Balaton",], aes(x=Scaled_A_cover  , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(shape=veg_type_2),size=5,color="red",show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,size=2,color="red")+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)

c05_2=ggplot(data=plot_data05_f, aes(x=Scaled_A_cover , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_smooth(method="lm",se=TRUE,color="black",size=2)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  ylim(0,2)+
  xlim(-2,2)+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

grid.arrange(d05_2,a05_2,b05_2,c05_2,
             d2_2,a2_2,b2_2,c2_2,
             d5_2,a5_2,b5_2,c5_2,
             nrow = 3,
             ncol = 4
)