library(readxl)
library(data.table)

workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis4/"
setwd(workingdir)

data <- read_excel("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/quadrat_forR.xlsx",sheet = 1)

# veg height
data$veg_height_m=data$veg_height/100

# leaf biomass
data$sum_leaf_weight=rowSums(data[,c(44,50,53,57,62,66,70,74,78,82)])

# export quadrat info to lidar
data_quadtrat_tolidar=subset(data,select=c(1:8,88,89,38))
write.csv(data_quadtrat_tolidar,"C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/data_quadtrat_tolidar_forarticle.csv")
