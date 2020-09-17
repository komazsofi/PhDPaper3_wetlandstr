library(usdm)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

# Import

rad=c(0.5,1,2,5)

if (file.exists("step1_noncorr_rep_vif.txt")) file.remove("step1_noncorr_rep_vif.txt")
sink("step1_noncorr_rep_vif.txt", append=TRUE)

for(i in rad) {
  
  print(paste("Pole: selected radius is:",i,"m",sep=""))
  
  pole_data=read.csv(paste("Pole_db_",i,"_filt.csv",sep=""))
  
  vifcorr_pole=vifcor(pole_data[,c(4:17,24)], th=0.6, method='spearman')
  print(vifcorr_pole)
  
  pole_noncorr=exclude(pole_data,vifcorr_pole)
  scale_pole_noncorr=scale(pole_noncorr)
  colnames(scale_pole_noncorr)=paste("Scaled_",colnames(scale_pole_noncorr),sep="")
  
  pole_noncor_full=cbind(pole_noncorr,pole_data[,c(2,18,19,20,21,22,23,25)],scale_pole_noncorr)
  write.csv(pole_noncor_full,paste("Pole_noncorr",i,".csv",sep=""))
  
  ###################
  
  print(paste("Plot: selected radius is:",i,"m",sep=""))
  
  plot_data=read.csv(paste("Plot_db_",i,"_filt.csv",sep=""))
  
  vifcorr_plot=vifcor(plot_data[,c(3:16,24)], th=0.6, method='spearman')
  print(vifcorr_plot)
  
  plot_noncorr=exclude(plot_data,vifcorr_plot)
  scale_plot_noncorr=scale(plot_noncorr)
  colnames(scale_plot_noncorr)=paste("Scaled_",colnames(scale_plot_noncorr),sep="")
  
  plot_noncor_full=cbind(plot_noncorr,plot_data[,c(2,17,18,19,20,21,22,23,25,26,27)],scale_plot_noncorr)
  write.csv(plot_noncor_full,paste("Plot_noncorr",i,".csv",sep=""))
  
}

sink()