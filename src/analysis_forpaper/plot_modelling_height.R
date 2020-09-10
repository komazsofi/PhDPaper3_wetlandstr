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

plot_0.5=read.csv("Plot_db_2_filt.csv")
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
lm_all_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt)
summary(lm_all_h)

lm_all_h_step<-step(lm_all_h)
summary(lm_all_h_step)

ols_step_both_p(lm_all_h)
ols_step_both_aic(lm_all_h)


#fwf
lm_fwf_h=lm(veg_height_m~H_max+H_q25.25.+V_ku+A_std+A_cover+W_echw,data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",])
summary(lm_fwf_h)

lm_fwf_h_step<-step(lm_fwf_h)
summary(lm_fwf_h_step)

ols_step_both_aic(lm_fwf_h)

###### Biomass

#all
lm_all_b=lm(total.weight~V_std+A_std,data=plot_0.5_filt)
summary(lm_all_b)

lm_all_b_step<-step(lm_all_b)
summary(lm_all_b_step)

ols_step_all_possible(lm_all_b)
ols_step_both_p(lm_all_b)
ols_step_both_aic(lm_all_b)


#fwf
lm_fwf_b=lm(total.weight~V_std+A_std,data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",])
summary(lm_fwf_b)

lm_fwf_h_step<-step(lm_fwf_b)
summary(lm_fwf_h_step)

ols_step_both_aic(lm_fwf_b)

# Visualize
ggplot(data=plot_0.5_filt,aes(x=V_std,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)

ggplot(data=plot_0.5_filt[plot_0.5_filt$lake!="Lake Balaton",],aes(x=H_max,y=total.weight))+geom_point(aes(color=lake,shape=veg_type_2),size=4)+theme_minimal()+geom_smooth(method="lm",se=TRUE)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0)
