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
  
  print(paste("Plot: selected radius is:",i,"m",sep=""))
  
  plot_data=read.csv(paste("Plot_db_",i,"_filt.csv",sep=""))
  
  vifcorr_plot=vifcor(pole_data[,c(3:16,24)], th=0.6, method='spearman')
  print(vifcorr_plot)
  
}

sink()