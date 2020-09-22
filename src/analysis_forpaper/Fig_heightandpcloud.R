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
plot_data05_filt=plot_data05[(plot_data05$OBJNAME!=160),]

lm_scaled<-lm(veg_height_m ~ Scaled_H_max+Scaled_V_ku+Scaled_A_std+Scaled_A_cover,data = plot_data05_filt)
summary(lm_scaled)

lm_scaled_nfwf<-lm(veg_height_m ~ Scaled_H_max+Scaled_V_ku+Scaled_A_std+Scaled_A_cover,data = plot_data05_filt[plot_data05_filt$lake=="Lake Balaton",])
summary(lm_scaled_nfwf)

lm_scaled_fwf<-lm(veg_height_m ~ Scaled_H_max+Scaled_V_ku+Scaled_A_std+Scaled_A_cover,data = plot_data05_filt[plot_data05_filt$lake!="Lake Balaton",])
summary(lm_scaled_fwf)

std_coef_sum2 <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum2)<-c("coeff","metric")

std_coef_sum2$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum2$coeff<-c(0,lm_scaled$coefficients[4],lm_scaled$coefficients[5],0,lm_scaled$coefficients[2],0,0,0,0,0,lm_scaled$coefficients[3],0,0)
std_coef_sum2$type<-c("all")

std_coef_sum22 <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum22)<-c("coeff","metric")

std_coef_sum22$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum22$coeff<-c(0,0,0,0,0,0,0,0,0,0,0,0,0)
std_coef_sum22$type<-c("discrete")

std_coef_sum222 <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum222)<-c("coeff","metric")

std_coef_sum222$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum222$coeff<-c(0,lm_scaled_fwf$coefficients[4],lm_scaled_fwf$coefficients[5],0,lm_scaled_fwf$coefficients[2],0,0,0,0,0,lm_scaled_fwf$coefficients[3],0,0)
std_coef_sum222$type<-c("fwf")

std_coef_sum_h=rbind(std_coef_sum2,std_coef_sum22,std_coef_sum222)

a=ggplot(data=std_coef_sum_h, aes(x=coeff, y=metric,fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for vegetation height (res=0.5 m)")+
  theme_classic(base_size=20)

# biomass

plot_data5_filt=plot_data5[(plot_data5$OBJNAME!=120),]

lm_scaled_b<-lm(total.weight ~ Scaled_V_var+Scaled_A_std+Scaled_A_med+Scaled_A_cover,data = plot_data5_filt)
summary(lm_scaled_b)

lm_scaled_b_nfwf<-lm(total.weight ~ Scaled_V_var+Scaled_A_std+Scaled_A_med+Scaled_A_cover,data = plot_data5_filt[plot_data5_filt$lake=="Lake Balaton",])
summary(lm_scaled_b_nfwf)

lm_scaled_b_fwf<-lm(total.weight ~ Scaled_V_var+Scaled_A_std+Scaled_A_med+Scaled_A_cover,data = plot_data5_filt[plot_data5_filt$lake!="Lake Balaton",])
summary(lm_scaled_b_fwf)

std_coef_sum2_b <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum2_b)<-c("coeff","metric")

std_coef_sum2_b$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum2_b$coeff<-c(lm_scaled_b_nfwf$coefficients[4],lm_scaled_b_nfwf$coefficients[3],lm_scaled_b_nfwf$coefficients[5],0,0,0,0,0,0,lm_scaled_b_nfwf$coefficients[2],0,0,0)
std_coef_sum2_b$type<-c("discrete")

std_coef_sum2_b1 <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum2_b1)<-c("coeff","metric")

std_coef_sum2_b1$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum2_b1$coeff<-c(lm_scaled_b_fwf$coefficients[4],lm_scaled_b_fwf$coefficients[3],lm_scaled_b_fwf$coefficients[5],0,0,0,0,0,0,lm_scaled_b_fwf$coefficients[2],0,0,0)
std_coef_sum2_b1$type<-c("fwf")

std_coef_sum2_b11 <- data.frame(matrix(ncol = 2, nrow = 13))
names(std_coef_sum2_b11)<-c("coeff","metric")

std_coef_sum2_b11$metric<-c("A_med","A_std","A_cover","C_ppr","H_max","H_mean","H_med","H_p95","V_std","V_var","V_ku","V_sk","W_echw")
std_coef_sum2_b11$coeff<-c(lm_scaled_b$coefficients[4],lm_scaled_b$coefficients[3],lm_scaled_b$coefficients[5],0,0,0,0,0,0,lm_scaled_b$coefficients[2],0,0,0)
std_coef_sum2_b11$type<-c("all")

std_coef_sum_b=rbind(std_coef_sum2_b11,std_coef_sum2_b,std_coef_sum2_b1)

b=ggplot(data=std_coef_sum_b, aes(x=coeff, y=metric, fill=type)) +
  geom_bar(stat="identity",position=position_dodge())+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for biomass (res=5 m)")+
  theme_classic(base_size=20)

grid.arrange(a,b,
             nrow = 1
)

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
