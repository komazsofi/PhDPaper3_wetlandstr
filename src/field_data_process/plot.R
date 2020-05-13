"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : analyse field data
"

library(readxl)
library(data.table)

workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis3/"
setwd(workingdir)

data <- read_excel("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/quadrat_forR.xlsx",sheet = 1)

# veg height
data$veg_height_m=data$veg_height/100

#FHD
data$fhd_bio<-NA
fhd_bio=subset(data,select=c(47,51,55,59,63,67,71,75,79))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_bio[i] <- entropy
  
}

# LAI
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

# export quadrat info to lidar
data_quadtrat_tolidar=subset(data,select=c(1:7,88,89,90,38))
write.csv(data_quadtrat_tolidar,"data_quadtrat_tolidar.csv")
