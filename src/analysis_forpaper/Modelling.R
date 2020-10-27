library(ggplot2)
library(gridExtra)
library(ggrepel)
library(grid)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(corrplot)

library(pdp)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
#workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
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
model_all_h05=lm(veg_height_m ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data05_f)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(veg_height_m ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

##### biomass

model_all_b05=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data05_f)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# fwf
model_fwf_b05=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
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

model_all_b2=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data2_f)
summary(model_all_b2) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# fwf
model_fwf_b2=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
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

model_all_b5=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data = plot_data5_f)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# fwf
model_fwf_b5=lm(total.weight ~ Scaled_H_p99.99.+Scaled_C_ppr+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

stargazer(model_fwf_step_h5,model_all_step_h5,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="5 m")

####################################### Partial residual plot

plot_data05_f$part_res_V_var_all=resid(model_all_step_h05) + plot_data05_f$Scaled_H_p99.99.*coef(model_all_step_h05)["Scaled_H_p99.99."]
plot_data05_f$part_res_V_var_all_y=termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_H_p99.99.$y
plot_data05_f$part_res_V_var_all_x=termplot(model_all_step_h05, partial=T, term=1, plot=F)$Scaled_H_p99.99.$x

plot_data05_f$part_res_V_var_fwf=NA
plot_data05_f$part_res_V_var_fwf_y=NA
plot_data05_f$part_res_V_var_fwf_x=NA

plot_data05_f[plot_data05_f$lake!="Lake Balaton",32] <- resid(model_fwf_step_h05) + plot_data05_f[plot_data05_f$lake!="Lake Balaton",]$Scaled_H_p99.99.*coef(model_fwf_step_h05)["Scaled_H_p99.99."]
plot_data05_f[plot_data05_f$lake!="Lake Balaton",33] <- termplot(model_fwf_step_h05, partial=T, term=1, plot=F)$Scaled_H_p99.99.$y
plot_data05_f[plot_data05_f$lake!="Lake Balaton",34] <- termplot(model_fwf_step_h05, partial=T, term=1, plot=F)$Scaled_H_p99.99.$x

p1=ggplot(data=plot_data05_f[(plot_data05_f$lake!="Lake Balaton"),], aes(x=Scaled_H_p99.99. , y=part_res_V_var_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_fwf_x,y=part_res_V_var_fwf_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  xlab("Scaled H_p99")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.7,1.5)+ylim(-2,2.2)+
  ggtitle("c) FWF [0.5 m]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p2=ggplot(data=plot_data05_f, aes(x=Scaled_H_p99.99. , y=part_res_V_var_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  xlab("Scaled H_p99")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.7,1.5)+ylim(-2,2.2)+
  ggtitle("d) All [0.5 m]")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

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

p3=ggplot(data=plot_data05_f[(plot_data05_f$lake!="Lake Balaton"),], aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_fwf_x,y=part_res_A_std_fwf_y),color="black",size=2,linetype = "dotted")+
  theme_bw(base_size = 25) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p4=ggplot(data=plot_data05_f, aes(x=Scaled_A_std , y=part_res_A_std_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data05_f,aes(x=part_res_A_std_all_x,y=part_res_A_std_all_y),color="black",size=2,linetype = "dashed")+
  theme_bw(base_size = 25) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-2.2,2.2)+ylim(-1.5,2.2)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

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

p5=ggplot(data=plot_data5_f[(plot_data5_f$lake!="Lake Balaton"),], aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_fwf_x,y=part_res_C_ppr_fwf_y),color="black",size=2,linetype = "dashed")+
  theme_bw(base_size = 25) +
  xlab("Scaled C_ppr")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)+
  ggtitle("c) FWF [5 m]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p6=ggplot(data=plot_data5_f, aes(x=Scaled_C_ppr , y=part_res_C_ppr_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = FALSE) +
  geom_line(data=plot_data5_f,aes(x=part_res_C_ppr_all_x,y=part_res_C_ppr_all_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 25) +
  xlab("Scaled C_ppr")+
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.5,2.2)+ylim(-1,1.2)+
  theme(axis.title.y=element_blank())+
  ggtitle("d) All [5 m]")+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

####################################### Predicted vs actual

# height
plot_data05_f$predicted_h=predict(model_all_step_h05)
plot_data05_f$predicted_h_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",42]=predict(model_fwf_step_h05)

p7=ggplot(data=plot_data05_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted height [m]")+
  ylab("Observed height [m]")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  ylim(1.5,5)+xlim(1.5,5)+
  ggtitle("b) All [0.5 m]")+
  annotate("text",x = 2, y = 4.8,label = expression(paste("R"^2, " = 0.69")),size=10)+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p8=ggplot(data=plot_data05_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab("Predicted height [m]")+
  ylab("Observed height [m]")+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  ylim(1.5,5)+xlim(1.5,5)+
  ggtitle("a) FWF [0.5 m]")+
  annotate("text",x = 2, y = 4.8,label = expression(paste("R"^2, " = 0.84")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

# biomass

plot_data5_f$predicted_b=predict(model_all_step_b5)
plot_data5_f$predicted_b_fwf<-NA
plot_data5_f[plot_data5_f$lake!="Lake Balaton",36]=predict(model_fwf_step_b5)

p9=ggplot(data=plot_data5_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2)+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab(expression(paste("Predicted biomass [kg/m"^"2","]")))+
  ylab(expression(paste("Observed biomass [kg/m"^"2","]")))+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.2,1)+ylim(0,1.7)+
  theme(axis.title.y=element_blank())+
  ggtitle("b) All [5 m]")+
  annotate("text",x = 0.35, y = 1.6,label = expression(paste("R"^2, " = 0.22")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))

p10=ggplot(data=plot_data5_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake),size=5,show.legend = FALSE)+
  #geom_smooth(method = "lm", se = FALSE, colour="black",size=2,linetype = "dashed")+
  #geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  geom_abline()+
  xlab(expression(paste("Predicted biomass [kg/m"^"2","]")))+
  ylab(expression(paste("Observed biomass [kg/m"^"2","]")))+
  theme_bw(base_size = 25) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")+
  xlim(0.2,1)+ylim(0,1.7)+
  ggtitle("a) FWF [5 m]")+
  annotate("text",x = 0.35, y = 1.6,label = expression(paste("R"^2, " = 0.20")),size=10)+
  theme(axis.text.x = element_text(size=30),axis.text.y = element_text(size=30))


#### Figures

p0=ggplot(data=plot_data05_f, aes(x=Scaled_V_var , y=part_res_V_var_all),show.legend = FALSE) +  
  geom_point(aes(color=lake),size=5,show.legend = TRUE) +
  geom_line(data=plot_data05_f,aes(x=part_res_V_var_all_x,y=part_res_V_var_all_y),color="black",size=2,linetype = "solid")+
  theme_bw(base_size = 30) +
  ylab("Partial dependence") +
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  xlim(-1.2,1.5)+ylim(-2.2,2.2)

legend=get_legend(p0)

t_1 <- textGrob("Prediction plots",gp=gpar(fontsize=25, col="black", fontface="bold"))
t_2 <- textGrob("Partial dependence plots",gp=gpar(fontsize=25, col="black", fontface="bold"))

fig3=grid.arrange(p8,p7,p1,p2,legend,t_1,t_2,
             ncol=3,
             nrow=4,
             layout_matrix=rbind(c(6,6,5),c(1,2,5),c(7,7,5),c(3,4,5)),
             widths = c(1,1,0.5),
             heights = c(0.5,4,0.5,4))

ggsave("Fig3_subm1.png",plot = fig3,width = 18, height = 15)

fig4=grid.arrange(p10,p9,p5,p6,legend,t_1,t_2,
             ncol=3,
             nrow=4,
             layout_matrix=rbind(c(6,6,5),c(1,2,5),c(7,7,5),c(3,4,5)),
             widths = c(1,1,0.5),
             heights = c(0.5,4,0.5,4))

ggsave("Fig4_subm1.png",plot = fig4,width = 18, height = 15)
