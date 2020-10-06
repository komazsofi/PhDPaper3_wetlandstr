library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(corrplot)
library(multcompView)

library(pdp)

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

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

####################################### Scaled fit and report selection 1: V_Var, C_ppr, A_std

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

####################################### Partial residual plot

termplot(model_all_step_h05, partial=T, term=1, pch=20, cex=1.5, col.term=0,
         lwd.term=3)
lines(lwd=3, lty=2, col='red', termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$x, 
      termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$y)

part_res <- resid(model_all_step_h05) + plot_data05_f$Scaled_V_var*coef(model_all_step_h05)["Scaled_V_var"]
plot(part_res~ plot_data05_f$Scaled_V_var,ylab = "Partial residuals", xlab = "Scaled_V_var", las = 1)

plot_data05_f$part_res_V_var_all=resid(model_all_step_h05) + plot_data05_f$Scaled_V_var*coef(model_all_step_h05)["Scaled_V_var"]
plot_data05_f$part_res_V_var_all_y=termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$y
plot_data05_f$part_res_V_var_all_x=termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$x

plot_data05_f$part_res_V_var_fwf=NA
plot_data05_f$part_res_V_var_fwf_y=NA
plot_data05_f$part_res_V_var_fwf_x=NA

plot_data05_f[plot_data05_f$lake!="Lake Balaton",32] <- resid(model_fwf_step_h05) + plot_data05_f[plot_data05_f$lake!="Lake Balaton",]$Scaled_V_var*coef(model_fwf_step_h05)["Scaled_V_var"]
plot_data05_f[plot_data05_f$lake!="Lake Balaton",33] <- termplot(model_fwf_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$y
plot_data05_f[plot_data05_f$lake!="Lake Balaton",34] <- termplot(model_fwf_step_h05, partial=T, term=1, plot=F)$Scaled_V_var$x

a05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_V_var , y=part_res_V_var_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_fwf_x,y=part_res_V_var_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.2,1.5)+ylim(-2.2,2.2)

b05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_V_var , y=part_res_V_var_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_fwf_x,y=part_res_V_var_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen"),name="Lakes")+
  xlim(-1.2,1.5)+ylim(-2.2,2.2)

c05h=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Balaton"),], aes(x=Scaled_V_var , y=part_res_V_var_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_fwf_x,y=part_res_V_var_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red"),name="Lakes")+
  xlim(-1.2,1.5)+ylim(-2.2,2.2)

abc05h=ggplot(data=plot_data05_f, aes(x=Scaled_V_var , y=part_res_V_var_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_fwf_x,y=part_res_V_var_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.2,1.5)+ylim(-2.2,2.2)

#####

plot_data05_f$part_res_A_std_all=resid(model_all_step_h05) + plot_data05_f$Scaled_A_std*coef(model_all_step_h05)["Scaled_A_std"]
plot_data05_f$part_res_A_std_all_y=termplot(model_all_step_h05, partial=T, term=3, plot=F)$Scaled_A_std$y
plot_data05_f$part_res_A_std_all_x=termplot(model_all_step_h05, partial=T, term=3, plot=F)$Scaled_A_std$x

plot_data05_f$part_res_A_std_fwf=NA
plot_data05_f$part_res_A_std_fwf_y=NA
plot_data05_f$part_res_A_std_fwf_x=NA

plot_data05_f[plot_data05_f$lake!="Lake Balaton",38] <- resid(model_fwf_step_h05) + plot_data05_f[plot_data05_f$lake!="Lake Balaton",]$Scaled_A_std*coef(model_fwf_step_h05)["Scaled_A_std"]
plot_data05_f[plot_data05_f$lake!="Lake Balaton",39] <- termplot(model_fwf_step_h05, partial=T, term=2, plot=F)$Scaled_A_std$y
plot_data05_f[plot_data05_f$lake!="Lake Balaton",40] <- termplot(model_fwf_step_h05, partial=T, term=2, plot=F)$Scaled_A_std$x

a05h2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Tisza"),], aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_all_x,y=part_res_A_std_all_y),color="deeppink",size=2,linetype = "dashed")+
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_fwf_x,y=part_res_A_std_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)

b05h2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Ferto"),], aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_all_x,y=part_res_A_std_all_y),color="deeppink",size=2,linetype = "dashed")+
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_fwf_x,y=part_res_A_std_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)

c05h2=ggplot(data=plot_data05_f[(plot_data05_f$lake=="Lake Balaton"),], aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_all_x,y=part_res_A_std_all_y),color="deeppink",size=2,linetype = "dashed")+
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_fwf_x,y=part_res_A_std_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)

abc05h2=ggplot(data=plot_data05_f, aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_all_x,y=part_res_A_std_all_y),color="deeppink",size=2,linetype = "dashed")+
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_fwf_x,y=part_res_A_std_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)

#####

plot_data5_f$part_res_C_ppr_all=resid(model_all_step_b5) + plot_data5_f$Scaled_C_ppr*coef(model_all_step_b5)["Scaled_C_ppr"]
plot_data5_f$part_res_C_ppr_all_y=termplot(model_all_step_b5, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f$part_res_C_ppr_all_x=termplot(model_all_step_b5, partial=T, term=1, plot=F)$Scaled_C_ppr$x

plot_data5_f$part_res_C_ppr_fwf=NA
plot_data5_f$part_res_C_ppr_fwf_y=NA
plot_data5_f$part_res_C_ppr_fwf_x=NA

plot_data5_f[plot_data5_f$lake!="Lake Balaton",32] <- resid(model_fwf_step_b5) + plot_data5_f[plot_data5_f$lake!="Lake Balaton",]$Scaled_C_ppr*coef(model_fwf_step_b5)["Scaled_C_ppr"]
plot_data5_f[plot_data5_f$lake!="Lake Balaton",33] <- termplot(model_fwf_step_b5, partial=T, term=1, plot=F)$Scaled_C_ppr$y
plot_data5_f[plot_data5_f$lake!="Lake Balaton",34] <- termplot(model_fwf_step_b5, partial=T, term=1, plot=F)$Scaled_C_ppr$x

a5b=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Tisza"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_all_x,y=part_res_C_ppr_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwf_x,y=part_res_C_ppr_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)

b5b=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Ferto"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_all_x,y=part_res_C_ppr_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwf_x,y=part_res_C_ppr_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)

c5b=ggplot(data=plot_data5_f[(plot_data5_f$lake=="Lake Balaton"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_all_x,y=part_res_C_ppr_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwf_x,y=part_res_C_ppr_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)

abc5b=ggplot(data=plot_data5_f, aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = TRUE) +  
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_all_x,y=part_res_C_ppr_all_y),color="deeppink",size=2,linetype = "solid")+
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwf_x,y=part_res_C_ppr_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 20) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)

fig4=grid.arrange(a05h,b05h,c05h,abc05h,
             a05h2,b05h2,c05h2,abc05h2,
             a5b,b5b,c5b,abc5b,
             nrow = 3,
             ncol = 4
)

ggsave("fig4_v1.png",fig4,width=35,height=30,units="cm")


####################################### Boxplot + Tukey test

plot_data05_f$type="fwf"
plot_data05_f[plot_data05_f$lake=="Lake Balaton",41] <- "discrete"

plot_data05_f$pdens_act=NA
plot_data05_f[plot_data05_f$nofallp<5,42] <- "low"
plot_data05_f[(plot_data05_f$nofallp>5 & plot_data05_f$nofallp<15),42] <- "medium"
plot_data05_f[plot_data05_f$nofallp>15,42] <- "high"

###

lakes1 = lm(veg_height_m ~ lake, data = plot_data05_f)
summary(lakes1)

ANOVA=aov(lakes1)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(veg_height_m ~lake, data=plot_data05_f, mean)
vegh_dep<-merge(labels,yvalue) 

lakes1_b = lm(total.weight ~ lake, data = plot_data05_f)
summary(lakes1_b)

ANOVA=aov(lakes1_b)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(total.weight ~lake, data=plot_data05_f, mean)
total.weight_dep<-merge(labels,yvalue)

#### metrics per lake

m1 = lm(Scaled_V_var ~ lake, data = plot_data05_f)
summary(m1)

ANOVA=aov(m1)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_V_var ~lake, data=plot_data05_f, mean)
V_var_dep<-merge(labels,yvalue) 

m2 = lm(Scaled_A_std ~ lake, data = plot_data05_f)
summary(m2)

ANOVA=aov(m2)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_A_std ~lake, data=plot_data05_f, mean)
A_std_dep<-merge(labels,yvalue)

m3 = lm(Scaled_C_ppr ~ lake, data = plot_data05_f)
summary(m2)

ANOVA=aov(m3)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_C_ppr ~lake, data=plot_data05_f, mean)
C_ppr_dep<-merge(labels,yvalue)

#### metrics per pdens

m1 = lm(Scaled_V_var ~ pdens_act, data = plot_data05_f)
summary(m1)

ANOVA=aov(m1)
TUKEY <- TukeyHSD(x=ANOVA, 'pdens_act', conf.level=0.95)
labels<-generate_label_df(TUKEY , "pdens_act")
names(labels)<-c('Letters','pdens_act')
yvalue<-aggregate(Scaled_V_var ~pdens_act, data=plot_data05_f, mean)
V_var_dep_t<-merge(labels,yvalue) 

m2 = lm(Scaled_A_std ~ pdens_act, data = plot_data05_f)
summary(m2)

ANOVA=aov(m2)
TUKEY <- TukeyHSD(x=ANOVA, 'pdens_act', conf.level=0.95)
labels<-generate_label_df(TUKEY , "pdens_act")
names(labels)<-c('Letters','pdens_act')
yvalue<-aggregate(Scaled_A_std ~pdens_act, data=plot_data05_f, mean)
A_std_dep_t<-merge(labels,yvalue)

m3 = lm(Scaled_C_ppr ~ pdens_act, data = plot_data05_f)
summary(m2)

ANOVA=aov(m3)
TUKEY <- TukeyHSD(x=ANOVA, 'pdens_act', conf.level=0.95)
labels<-generate_label_df(TUKEY , "pdens_act")
names(labels)<-c('Letters','pdens_act')
yvalue<-aggregate(Scaled_C_ppr ~pdens_act, data=plot_data05_f, mean)
C_ppr_dep_t<-merge(labels,yvalue)

#### metrics per lake 5m

m1 = lm(Scaled_V_var ~ lake, data = plot_data5_f)
summary(m1)

ANOVA=aov(m1)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_V_var ~lake, data=plot_data5_f, mean)
V_var_dep5<-merge(labels,yvalue) 

m2 = lm(Scaled_A_std ~ lake, data = plot_data5_f)
summary(m2)

ANOVA=aov(m2)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_A_std ~lake, data=plot_data5_f, mean)
A_std_dep5<-merge(labels,yvalue)

m3 = lm(Scaled_C_ppr ~ lake, data = plot_data5_f)
summary(m2)

ANOVA=aov(m3)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(Scaled_C_ppr ~lake, data=plot_data5_f, mean)
C_ppr_dep5<-merge(labels,yvalue)

###boxplot visualization

m11=ggplot(data=plot_data05_f,aes(x=lake,y=Scaled_V_var,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = V_var_dep, aes(x = lake, y = c(1.1,1.1,1.1), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled V_var")+theme_bw(base_size = 18)+ylim(-1,1.6)

m21=ggplot(data=plot_data05_f,aes(x=lake,y=Scaled_C_ppr,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = C_ppr_dep, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled C_ppr")+theme_bw(base_size = 18)+ylim(-1.5,2.2)

m31=ggplot(data=plot_data05_f,aes(x=lake,y=Scaled_A_std,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = A_std_dep, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled A_std")+theme_bw(base_size = 18)+ylim(-2.2,2.2)

###

m115=ggplot(data=plot_data5_f,aes(x=lake,y=Scaled_V_var,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = V_var_dep5, aes(x = lake, y = c(1.5,1.5,1.5), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled V_var")+theme_bw(base_size = 18)+ylim(-1,2)

m215=ggplot(data=plot_data5_f,aes(x=lake,y=Scaled_C_ppr,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = C_ppr_dep5, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled C_ppr")+theme_bw(base_size = 18)+ylim(-1.5,2.2)

m315=ggplot(data=plot_data5_f,aes(x=lake,y=Scaled_A_std,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = A_std_dep5, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlab("Lakes")+ylab("Scaled A_std")+theme_bw(base_size = 18)+ylim(-2.2,2.2)

###

m12=ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_V_var,fill=pdens_act))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = V_var_dep_t, aes(x = pdens_act, y = c(1,1,1), label = Letters),vjust=-3.5,hjust=-.5)+
  scale_fill_manual(values=c("high"="darkgoldenrod","medium"="deeppink","low"="cornsilk4"),name="Point density")+
  xlab("Point densities")+ylab("Scaled V_var")+theme_bw(base_size = 18)+ylim(-1,1.2)

m22=ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_C_ppr,fill=pdens_act))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = C_ppr_dep_t, aes(x = pdens_act, y = c(1.9,1.9,1.9), label = Letters),vjust=-3.5,hjust=-.5)+
  scale_fill_manual(values=c("high"="darkgoldenrod","medium"="deeppink","low"="cornsilk4"),name="Point density")+
  xlab("Lakes")+ylab("Scaled C_ppr")+theme_bw(base_size = 18)+ylim(-1.5,2.2)

m32=ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_A_std,fill=pdens_act))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = A_std_dep_t, aes(x = pdens_act, y = c(1.8,1.8,1.8), label = Letters),vjust=-3.5,hjust=-.5)+
  scale_fill_manual(values=c("high"="darkgoldenrod","medium"="deeppink","low"="cornsilk4"),name="Point density")+
  xlab("Lakes")+ylab("Scaled A_std")+theme_bw(base_size = 18)+ylim(-2.2,2.2)

fig5=grid.arrange(m11,m21,m31,m115,m215,m315,
             nrow = 2,
             ncol = 3
)

ggsave("fig5_v1.png",fig5,width=35,height=25,units="cm")

grid.arrange(m11,m21,m31,
             nrow = 1,
             ncol = 3
)

####################################### Predicted vs actual

# height
plot_data05_f$predicted_h=predict(model_all_step_h05)
plot_data05_f$predicted_h_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",44]=predict(model_fwf_step_h05)

j=ggplot(data=plot_data05_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="deeppink",size=3)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Vegetation height (field)")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k=ggplot(data=plot_data05_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=3)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Vegetation height (field)")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data2_f$predicted_h=predict(model_all_step_h2)
plot_data2_f$predicted_h_fwf<-NA
plot_data2_f[plot_data2_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h2)

j2=ggplot(data=plot_data2_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k2=ggplot(data=plot_data2_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data5_f$predicted_h=predict(model_all_step_h5)
plot_data5_f$predicted_h_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",35]=predict(model_fwf_step_h5)

j5=ggplot(data=plot_data5_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5=ggplot(data=plot_data5_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j,k,j2,k2,j5,k5,
             nrow = 3,
             ncol = 2
)

# biomass
plot_data05_f$predicted_b=predict(model_all_step_b05)
plot_data05_f$predicted_b_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",46]=predict(model_fwf_step_b05)

j_b=ggplot(data=plot_data05_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="deeppink",size=3)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k_b=ggplot(data=plot_data05_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=3)+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data2_f$predicted_b=predict(model_all_step_b2)
plot_data2_f$predicted_b_fwf<-NA
plot_data2_f[plot_data2_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b2)

j2_b=ggplot(data=plot_data2_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k2_b=ggplot(data=plot_data2_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

plot_data5_f$predicted_b=predict(model_all_step_b5)
plot_data5_f$predicted_b_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",38]=predict(model_fwf_step_b5)

j5_b=ggplot(data=plot_data5_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="deeppink",size=3)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Biomass (field)")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5_b=ggplot(data=plot_data5_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black",size=3)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Biomass (field)")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j_b,k_b,j2_b,k2_b,j5_b,k5_b,
             nrow = 3,
             ncol = 2
)

#################Fig4

a=ggplot(data=plot_data05_f,aes(x=lake,y=veg_height_m,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = vegh_dep, aes(x = lake, y = c(4.25,4.25,4.25), label = Letters),vjust=-3.5,hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  ylim(2,5)+xlab("Lakes")+ylab("Vegetation height (field)")+theme_bw(base_size = 18)
  
b=ggplot(data=plot_data05_f,aes(x=lake,y=total.weight,fill=lake))+geom_boxplot(show.legend = FALSE)+
  geom_text(data = total.weight_dep, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),hjust=-.5,size=5)+
  scale_fill_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  ylim(0,1.8)+xlab("Lakes")+ylab("Biomass (field)")+theme_bw(base_size = 18)
  

fig6=grid.arrange(j+xlim(1.7,5)+ylim(1.7,5),k+xlim(1.7,5)+ylim(1.7,5),a,j5_b+xlim(0,1.25)+ylim(0,1.8),k5_b+xlim(0,1.25)+ylim(0,1.8),b,
             nrow = 2,
             ncol = 3
)

ggsave("fig6_v1.png",fig6,width=35,height=25,units="cm")

