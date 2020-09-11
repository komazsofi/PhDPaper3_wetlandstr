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

workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import

plot_0.5=read.csv("Plot_db_5_filt.csv")
plot_0.5_filt=plot_0.5[plot_0.5$nofveg>2,]

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
lm_all_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_std+A_cover,data=plot_0.5_filt)
summary(lm_all_h)

ols_step_forward_aic(lm_all_h,details = TRUE)

lm_all_hfit=lm(veg_height_m~H_max,data=plot_0.5_filt)
summary(lm_all_hfit)


#fwf
lm_fwf_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",])
summary(lm_fwf_h)

ols_step_forward_aic(lm_fwf_h,details = TRUE)

lm_fwf_hfit=lm(veg_height_m~H_max,data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",])
summary(lm_fwf_hfit)

# Visualize
ggplot(data=plot_0.5_filt,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

ggplot(data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",],aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

###### Biomass

plot_0.5_filt2=plot_0.5[plot_0.5$nofveg>5,]

#all
lm_all_b=lm(total.weight~H_max+H_q25.25.+V_ku+A_std+A_cover,data=plot_0.5_filt)
summary(lm_all_b)

ols_step_forward_aic(lm_all_b,details = TRUE)

#fwf
lm_fwf_b=lm(total.weight~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",])
summary(lm_fwf_b)

ols_step_forward_aic(lm_fwf_b,details = TRUE)

# Visualize
ggplot(data=plot_0.5_filt,aes(x=W_echw,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

ggplot(data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",],aes(x=W_echw,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)
