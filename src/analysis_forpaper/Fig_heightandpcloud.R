library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)

library(lidR)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

plot_data05=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))
plot_data5=read.csv(paste("Plot_noncorr",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

las_bal <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad_reclass/Balaton_OBJNAME204_25mrad_reclass.laz")
las_fert <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto_25mrad_reclass/Ferto_OBJNAME321_25mrad_reclass.laz")
las_tisza <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_leafon_reclass/Tisza_OBJNAME186_25mrad_reclass.laz")


##### Visualization 

g1=ggplot(data=plot_data05,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_classic(base_size=20)+
  xlab("H_max (LiDAR, res.=0.5 m)")+ylab("Vegetation height [m] (field)")+
  ggtitle("Most important metric for estimating vegetation height")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point",range=c(3,10))+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)

g2=ggplot(data=plot_data5,aes(x=A_cover,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_classic(base_size=20)+
  xlab("V_var (LiDAR, res.= 5 m)")+ylab("Biomass [kg/m2] (field)")+
  ggtitle("Most important metric for estimating biomass")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(250,500,1000),name="Number of vegetation point",range=c(3,10))+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)

grid.arrange(g1,g2,
  nrow = 1
)

##### Standardized coefficient
# height
plot_data05_filt=plot_data05[(plot_data05$OBJNAME!=209 & plot_data05$OBJNAME!=160 & plot_data05$OBJNAME!=120),]

lm_scaled<-lm(veg_height_m ~ Scaled_H_max+Scaled_V_ku+Scaled_A_std+Scaled_A_cover,data = plot_data05_filt)
summary(lm_scaled)

std_coef_sum=as.data.frame(lm_scaled$coefficients[2:5])
names(std_coef_sum)<-c("Coeff")

ggplot(data=std_coef_sum, aes(x=Coeff, y=rownames(std_coef_sum))) +
  geom_bar(stat="identity")+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for estimating vegetation height (res=0.5 m)")+
  theme_classic(base_size=20)

# biomass

plot_data5_filt=plot_data5

lm_scaled_b<-lm(total.weight ~ Scaled_V_var+Scaled_A_std+Scaled_A_med+Scaled_A_cover,data = plot_data5_filt)
summary(lm_scaled_b)

std_coef_sum_b=as.data.frame(lm_scaled_b$coefficients[2:5])
names(std_coef_sum_b)<-c("Coeff")

ggplot(data=std_coef_sum_b, aes(x=Coeff, y=rownames(std_coef_sum_b))) +
  geom_bar(stat="identity")+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for estimating biomass (res=5 m)")+
  theme_classic(base_size=20)

##### Pcloud visualization

bal=plot_data05[plot_data05$OBJNAME==204,]
fert=plot_data05[plot_data05$OBJNAME==321,]
tisza=plot_data05[plot_data05$OBJNAME==186,]


plot_crossection <- function(las,
                             p1,
                             p2,
                             bal,
                             width = 2, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 5) + coord_equal() + theme_minimal(base_size=20)+
    geom_vline(xintercept=bal$coords.x1, linetype="dashed", color = "red")+
    geom_vline(xintercept=bal$coords.x1-0.5, linetype="dashed", color = "blue")+
    geom_vline(xintercept=bal$coords.x1+0.5, linetype="dashed", color = "blue")+
    geom_vline(xintercept=bal$coords.x1-2.5, linetype="dashed", color = "black")+
    geom_vline(xintercept=bal$coords.x1+2.5, linetype="dashed", color = "black")+
    scale_colour_manual(values=c("1"="darkgreen", "2"="deeppink"),label=c("Non-ground points","Ground points"),name="Classification")
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

plot_crossection(las_bal,p1 = c(bal$coords.x1-5, bal$coords.x2),p2 = c(bal$coords.x1+5, bal$coords.x2),bal,colour_by = factor(Classification))
plot_crossection(las_fert,p1 = c(fert$coords.x1[1]+5, fert$coords.x2[1]),p2 = c(fert$coords.x1[1]-5, fert$coords.x2[1]),fert,colour_by = factor(Classification))
plot_crossection(las_tisza,p1 = c(tisza$coords.x1[1]+5, tisza$coords.x2[1]),p2 = c(tisza$coords.x1[1]-5, tisza$coords.x2[1]),tisza,colour_by = factor(Classification))

las_tisza@data=las_tisza@data[(las_tisza@data$Z<135 & las_tisza@data$Z>110),]

plot(las_bal,size=4,axis=TRUE)
plot(las_fert,size=4,axis=TRUE)
plot(las_tisza,size=4,axis=TRUE)
