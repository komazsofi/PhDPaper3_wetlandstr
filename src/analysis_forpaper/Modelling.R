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
#plot_data05_f=plot_data05_f[(plot_data05_f$OBJNAME!=120 & plot_data05_f$OBJNAME!=209),]

plot_data2=read.csv(paste("Plot_db_",2.5,".csv",sep=""))
plot_data2$total.weight=plot_data2$total.weight/10000

plot_data2_scaled=scale(plot_data2[,c(3:10)])
colnames(plot_data2_scaled)=paste("Scaled_",colnames(plot_data2_scaled),sep="")
plot_data2_f=cbind(plot_data2,plot_data2_scaled)
#plot_data2_f=plot_data2_f[(plot_data2_f$OBJNAME!=120 & plot_data2_f$OBJNAME!=209),]

plot_data5=read.csv(paste("Plot_db_",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

plot_data5_scaled=scale(plot_data5[,c(3:10)])
colnames(plot_data5_scaled)=paste("Scaled_",colnames(plot_data5_scaled),sep="")
plot_data5_f=cbind(plot_data5,plot_data5_scaled)
#plot_data5_f=plot_data5_f[(plot_data5_f$OBJNAME!=120 & plot_data5_f$OBJNAME!=209),]

####################################### Correlation check
col <- colorRampPalette(c("#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444"))

vifcorr_plot05=vifcor(plot_data05_scaled, th=0.6, method='spearman')
print(vifcorr_plot05)

corr05=round(cor(plot_data05_scaled, method="spearman"),2)

corrplot(corr05, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

vifcorr_plot2=vifcor(plot_data2_scaled, th=0.6, method='spearman')
print(vifcorr_plot2)

corr2=round(cor(plot_data2_scaled, method="spearman"),2)

corrplot(corr2, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)

vifcorr_plot5=vifcor(plot_data5_scaled, th=0.6, method='spearman')
print(vifcorr_plot5)

corr5=round(cor(plot_data5_scaled, method="spearman"),2)

corrplot(corr5, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE)


####################################### Scaled fit and report

##### 0.5 m

##### height

# all
model_all_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data05_f)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

##### biomass

model_all_b05=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data05_f)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# fwf
model_fwf_b05=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_b05) 

#AIC model selection (step)
model_fwf_step_b05<-step(model_fwf_b05,direction = "backward")
summary(model_fwf_step_b05)

stargazer(model_fwf_step_h05,model_all_step_h05,model_fwf_step_b05,model_all_step_b05,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="0.5 m")

##### 2.5 m

##### height

# all
model_all_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data2_f)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# fwf
model_fwf_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

##### biomass

model_all_b2=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data2_f)
summary(model_all_b2) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# fwf
model_fwf_b2=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_b2) 

#AIC model selection (step)
model_fwf_step_b2<-step(model_fwf_b2,direction = "backward")
summary(model_fwf_step_b2)

stargazer(model_fwf_step_h2,model_all_step_h2,model_fwf_step_b2,model_all_step_b2,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="2.5 m")

##### 5 m

##### height

# all
model_all_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

##### biomass

model_all_b5=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# fwf
model_fwf_b5=lm(total.weight ~ Scaled_V_var+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

stargazer(model_fwf_step_h5,model_all_step_h5,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="5 m")

####################################### Visualization

##### 0.5 m
# height

#biomass


##### 2.5 m

plot_data2_f=plot_data2_f[(plot_data2_f$OBJNAME!=120 & plot_data2_f$OBJNAME!=163 & plot_data2_f$OBJNAME!=209),]
plot_data2_f$type="not known"
plot_data2_f[plot_data2_f$lake=="Lake Balaton",29]<- "dr"
plot_data2_f[plot_data2_f$lake=="Lake Ferto",29]<- "fwf"
plot_data2_f[plot_data2_f$lake=="Lake Tisza",29]<- "fwf"

# height

#biomass
ggplot(data=plot_data2_f, aes(x=Scaled_V_var , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2,size=nofallp)) +
  geom_smooth(method="lm",aes(color=lake),se=FALSE)+
  geom_smooth(method="lm",color="black",se=FALSE)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  xlab("V_Var (scaled)")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("LiDAR metrics for estimating biomass (2.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes and types")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))+
  scale_size_continuous(breaks=c(250,500,1000),name="Number of points",range=c(1,6))

ggplot(data=plot_data2_f, aes(x=Scaled_C_ppr , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=5) +
  geom_smooth(method="lm",aes(color=type),se=FALSE)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  xlab("C_ppr (scaled)")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("LiDAR metrics for estimating biomass (2.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))

ggplot(data=plot_data2_f, aes(x=Scaled_A_std , y=total.weight),show.legend = TRUE) +  
  geom_point(aes(colour=lake,shape=veg_type_2),size=5) +
  geom_smooth(method="lm",aes(color=type),se=FALSE)+
  theme_bw(base_size = 20) +
  ylab("Biomass (field)") +
  xlab("A_std (scaled)")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)+
  ggtitle("LiDAR metrics for estimating biomass (2.5 m)")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))


##### 5 m
# height

#biomass