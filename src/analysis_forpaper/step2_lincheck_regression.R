

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import
  
plot_data=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))
  
# vegetation height

model_all=lm(veg_height_m ~ Scaled_H_max+Scaled_H_q25.25.+Scaled_V_ku+Scaled_A_std+Scaled_A_cover, data = plot_data)
summary(model_all) 

#AIC model selection (step)
model_all_step<-step(model_all)
summary(model_all_step)
