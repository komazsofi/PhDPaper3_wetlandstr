library(dplyr)
library(stringr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(GGally)

library(rgdal)
library(raster)
library(sp)

library(corrplot)
library(usdm)

library(olsrr)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import

plot_0.5=read.csv("Plot_db_0.5_filt.csv")
plot_0.5_filt=plot_0.5[plot_0.5$nofveg>2,]

plot_0.5_filt_c=distinct(plot_0.5_filt,OBJNAME,.keep_all=TRUE)

plot_0.5_filt_c$drybiomass=plot_0.5_filt_c$total.weight-plot_0.5_filt_c$sum_leaf_weight

# Non correlated feature group for modelling step
corr=round(cor(plot_0.5_filt[,c(3:16,24)], method="spearman"),2)

col <- colorRampPalette(c("#77AADD", "#4477AA", "#FFFFFF","#BB4444", "#EE9988"))
corrplot(corr, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

#vifstep(plot_0.5_filt[,c(3:16,24)],th=5)
vifcor(plot_0.5_filt[,c(3:16,24)], th=0.6, method='spearman')

# Model

###### Height

#all
lm_all_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_cover,data=plot_0.5_filt_c)
summary(lm_all_h)

ols_step_forward_aic(lm_all_h,details = TRUE)


#fwf
lm_fwf_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt_c[plot_0.5_filt_c$lake!="Lake Balaton",])
summary(lm_fwf_h)

ols_step_forward_aic(lm_fwf_h,details = TRUE)

# Visualize

lm_eqn <- function(df){
  m <- lm(veg_height_m ~ H_max, df);
  eq <- substitute(italic("y") == a + b %.% italic("H_max")*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot(data=plot_0.5,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal(base_size=12)+
  geom_smooth(data=plot_0.5_filt_c,aes(x=H_max,y=veg_height_m),method="lm",se=TRUE,color="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=3)+
  xlab("H_max (LiDAR)")+ylab("Vegetation height (field)")+
  ggtitle("Estimation of vegetation height")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))+
  geom_text(x = 1, y = 5, label = lm_eqn(plot_0.5[plot_0.5$nofveg>2,]), parse = TRUE)
  

###### Biomass

plot_0.5_filt_c=plot_0.5_filt_c[plot_0.5_filt_c$veg_type_2!="carex",]

#all
lm_all_b=lm(drybiomass~H_max+H_q25.25.+V_ku+A_cover,data=plot_0.5_filt_c)
summary(lm_all_b)

ols_step_forward_aic(lm_all_b,details = TRUE)

#fwf
lm_fwf_b=lm(drybiomass~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt_c[plot_0.5_filt_c$lake!="Lake Balaton",])
summary(lm_fwf_b)

ols_step_forward_aic(lm_fwf_b,details = TRUE)

# Visualize
ggplot(data=plot_0.5_filt_c,aes(x=H_max,y=drybiomass))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal(base_size=12)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=3)+
  xlab("H_max (LiDAR)")+ylab("Biomass (field)")+
  ggtitle("Estimation of biomass")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))
