library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)
library(usdm)

library(corrplot)
library(multcompView)

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


####################################### Scaled fit and report

##### 0.5 m

##### height

# all
model_all_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data05_f)
summary(model_all_h05) 

#AIC model selection (step)
model_all_step_h05<-step(model_all_h05,direction = "backward")
summary(model_all_step_h05)

# fwf
model_fwf_h05=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_h05) 

#AIC model selection (step)
model_fwf_step_h05<-step(model_fwf_h05,direction = "backward")
summary(model_fwf_step_h05)

##### biomass

model_all_b05=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data05_f)
summary(model_all_b05) 

#AIC model selection (step)
model_all_step_b05<-step(model_all_b05,direction = "backward")
summary(model_all_step_b05)

# fwf
model_fwf_b05=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data05_f[plot_data05_f$lake!="Lake Balaton",])
summary(model_fwf_b05) 

#AIC model selection (step)
model_fwf_step_b05<-step(model_fwf_b05,direction = "backward")
summary(model_fwf_step_b05)

stargazer(model_fwf_step_h05,model_all_step_h05,model_fwf_step_b05,model_all_step_b05,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="0.5 m")

##### 2.5 m

##### height

# all
model_all_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data2_f)
summary(model_all_h2) 

#AIC model selection (step)
model_all_step_h2<-step(model_all_h2,direction = "backward")
summary(model_all_step_h2)

# fwf
model_fwf_h2=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_h2) 

#AIC model selection (step)
model_fwf_step_h2<-step(model_fwf_h2,direction = "backward")
summary(model_fwf_step_h2)

##### biomass

model_all_b2=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data2_f)
summary(model_all_b2) 

#AIC model selection (step)
model_all_step_b2<-step(model_all_b2,direction = "backward")
summary(model_all_step_b2)

# fwf
model_fwf_b2=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data2_f[plot_data2_f$lake!="Lake Balaton",])
summary(model_fwf_b2) 

#AIC model selection (step)
model_fwf_step_b2<-step(model_fwf_b2,direction = "backward")
summary(model_fwf_step_b2)

stargazer(model_fwf_step_h2,model_all_step_h2,model_fwf_step_b2,model_all_step_b2,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="2.5 m")

##### 5 m

##### height

# all
model_all_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data5_f)
summary(model_all_h5) 

#AIC model selection (step)
model_all_step_h5<-step(model_all_h5,direction = "backward")
summary(model_all_step_h5)

# fwf
model_fwf_h5=lm(veg_height_m ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_h5) 

#AIC model selection (step)
model_fwf_step_h5<-step(model_fwf_h5,direction = "backward")
summary(model_fwf_step_h5)

##### biomass

model_all_b5=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data = plot_data5_f)
summary(model_all_b5) 

#AIC model selection (step)
model_all_step_b5<-step(model_all_b5,direction = "backward")
summary(model_all_step_b5)

# fwf
model_fwf_b5=lm(total.weight ~ Scaled_V_var+Scaled_A_cover+Scaled_A_std, data =plot_data5_f[plot_data5_f$lake!="Lake Balaton",])
summary(model_fwf_b5) 

#AIC model selection (step)
model_fwf_step_b5<-step(model_fwf_b5,direction = "backward")
summary(model_fwf_step_b5)

stargazer(model_fwf_step_h5,model_all_step_h5,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="text",column.labels=c("fwf","all","fwf","all"),title="5 m")

## report for study

stargazer(model_fwf_step_h05,model_all_step_h05,model_fwf_step_h2,model_all_step_h2,model_fwf_step_h5,model_all_step_h5,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","all [0.5 m]","fwf [2.5 m]","all [2.5 m]","fwf [5 m]","all [5 m]"),title="Estimation of vegetation height at different resolutions",out="height_report.doc")
stargazer(model_fwf_step_b05,model_all_step_b05,model_fwf_step_b2,model_all_step_b2,model_fwf_step_b5,model_all_step_b5,align=TRUE,type="html",column.labels=c("fwf [0.5 m]","all [0.5 m]","fwf [2.5 m]","all [2.5 m]","fwf [5 m]","all [5 m]"),title="Estimation of biomass at different resolutions",out="biomass_report.doc")

####################################### Dependence + ANOVA

plot_data05_f$type="fwf"
plot_data05_f[plot_data05_f$lake=="Lake Balaton",29] <- "discrete"

plot_data05_f$pdens_act=NA
plot_data05_f[plot_data05_f$nofallp<5,30] <- "low"
plot_data05_f[(plot_data05_f$nofallp>5 & plot_data05_f$nofallp<15),30] <- "medium"
plot_data05_f[plot_data05_f$nofallp>15,30] <- "high"

###

lakes1 = lm(veg_height_m ~ lake, data = plot_data05_f)
summary(lakes1)

ANOVA=aov(lakes1)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(veg_height_m ~lake, data=plot_data05_f, mean)
vegh_dep<-merge(labels,yvalue) 

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Ferto") 
lakes2 = lm(veg_height_m ~ lake, data = plot_data05_f)
summary(lakes2)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Tisza") 
lakes3 = lm(veg_height_m ~ lake, data = plot_data05_f)
summary(lakes3)

m1 = lm(V_var ~ lake, data = plot_data05_f)
summary(m1)

ANOVA=aov(m1)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(V_var ~lake, data=plot_data05_f, mean)
V_var_dep<-merge(labels,yvalue) 

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Ferto") 
m1_2 = lm(V_var ~ lake, data = plot_data05_f)
summary(m1_2)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Tisza") 
m1_3 = lm(V_var ~ lake, data = plot_data05_f)
summary(m1_3)

m2 = lm(A_std ~ lake, data = plot_data05_f)
summary(m2)

ANOVA=aov(m2)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(A_std ~lake, data=plot_data05_f, mean)
A_std_dep<-merge(labels,yvalue)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Ferto") 
m2_2 = lm(A_std ~ lake, data = plot_data05_f)
summary(m2_2)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Tisza") 
m2_3 = lm(A_std ~ lake, data = plot_data05_f)
summary(m2_3)

m3 = lm(A_cover ~ lake, data = plot_data05_f)
summary(m2)

ANOVA=aov(m3)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(A_cover ~lake, data=plot_data05_f, mean)
A_cover_dep<-merge(labels,yvalue)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Ferto") 
m3_2 = lm(A_cover ~ lake, data = plot_data05_f)
summary(m2_2)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Tisza") 
m3_3 = lm(A_cover ~ lake, data = plot_data05_f)
summary(m2_3)

lakes1_b = lm(total.weight ~ lake, data = plot_data05_f)
summary(lakes1_b)

ANOVA=aov(lakes1_b)
TUKEY <- TukeyHSD(x=ANOVA, 'lake', conf.level=0.95)
labels<-generate_label_df(TUKEY , "lake")
names(labels)<-c('Letters','lake')
yvalue<-aggregate(total.weight ~lake, data=plot_data05_f, mean)
total.weight_dep<-merge(labels,yvalue)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Ferto") 
lakes2_b = lm(total.weight ~ lake, data = plot_data05_f)
summary(lakes2_b)

plot_data05_f$lake = relevel(plot_data05_f$lake, ref="Lake Tisza") 
lakes3_b = lm(total.weight ~ lake, data = plot_data05_f)
summary(lakes3_b)

###boxplot visualization

ggplot(data=plot_data05_f,aes(x=lake,y=veg_height_m,fill=lake))+geom_boxplot()+geom_text(data = vegh_dep, aes(x = lake, y = c(4,4,4), label = Letters),vjust=-3.5,hjust=-.5)
ggplot(data=plot_data05_f,aes(x=lake,y=total.weight,fill=lake))+geom_boxplot()+geom_text(data = total.weight_dep, aes(x = lake, y = c(1.6,1.6,1.6), label = Letters),hjust=-.5)

ggplot(data=plot_data05_f,aes(x=lake,y=V_var,fill=lake))+geom_boxplot()+geom_text(data = V_var_dep, aes(x = lake, y = V_var, label = Letters),vjust=-3.5,hjust=-.5)
ggplot(data=plot_data05_f,aes(x=lake,y=A_std,fill=lake))+geom_boxplot()+geom_text(data = A_std_dep, aes(x = lake, y = A_std, label = Letters),vjust=-3.5,hjust=-.5)
ggplot(data=plot_data05_f,aes(x=lake,y=A_cover,fill=lake))+geom_boxplot()+geom_text(data = A_cover_dep, aes(x = lake, y = A_cover, label = Letters),vjust=-3.5,hjust=-.5)

ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_V_var,fill=pdens_act))+geom_boxplot()
ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_A_std,fill=pdens_act))+geom_boxplot()
ggplot(data=plot_data05_f,aes(x=pdens_act,y=Scaled_A_cover,fill=pdens_act))+geom_boxplot()

ggplot(data=plot_data05_f,aes(x=type,y=Scaled_V_var,fill=type))+geom_boxplot()
ggplot(data=plot_data05_f,aes(x=type,y=Scaled_A_std,fill=type))+geom_boxplot()
ggplot(data=plot_data05_f,aes(x=type,y=Scaled_A_cover,fill=type))+geom_boxplot()


####################################### Predicted vs actual

# height
plot_data05_f$predicted_h=predict(model_all_step_h05)
plot_data05_f$predicted_h_fwf<-NA
plot_data05_f[plot_data05_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h05)

j=ggplot(data=plot_data05_f,aes(x=predicted_h,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k=ggplot(data=plot_data05_f,aes(x=predicted_h_fwf,y=veg_height_m))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted height")+
  ylab("Actual height")+
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
plot_data5_f[plot_data5_f$lake!="Lake Balaton",30]=predict(model_fwf_step_h5)

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
plot_data05_f[plot_data05_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b05)

j_b=ggplot(data=plot_data05_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k_b=ggplot(data=plot_data05_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
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
plot_data5_f[plot_data5_f$lake!="Lake Balaton",32]=predict(model_fwf_step_b5)

j5_b=ggplot(data=plot_data5_f,aes(x=predicted_b,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

k5_b=ggplot(data=plot_data5_f,aes(x=predicted_b_fwf,y=total.weight))+
  geom_point(aes(color=lake,shape=veg_type_2),size=5,show.legend = FALSE)+
  geom_smooth(method = "lm", se = FALSE, colour="black")+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=4)+
  xlab("Predicted biomass")+
  ylab("Actual biomass")+
  theme_bw(base_size = 20) +
  scale_color_manual(values=c("Lake Ferto"="darkgreen","Lake Tisza"="blue","Lake Balaton"="red"),name="Lakes")

grid.arrange(j_b,k_b,j2_b,k2_b,j5_b,k5_b,
             nrow = 3,
             ncol = 2
)
