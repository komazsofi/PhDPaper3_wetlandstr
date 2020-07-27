"
@author: Zsofia Koma, UvA
Aim: process wetland OTKA project data step : analyse field data
"

library(readxl)
library(data.table)

workingdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis4/"
setwd(workingdir)

data <- read_excel("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/quadrat_forR.xlsx",sheet = 1)

# veg height
data$veg_height_m=data$veg_height/100

#FHD
data$fhd_bio<-NA
fhd_bio=subset(data,select=c(43,47,51,55,59,63,67,71,75,79))

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_bio[i] <- entropy
  
}

# FHD 1m height layers
fhd_bio_1m_1=fhd_bio$`0-0.5_1_total_weight`+fhd_bio$`0-0.5_1_total_weight`
fhd_bio_1m_2=fhd_bio$`1_1.5_total_weight`+fhd_bio$`1.5_2_total_weight`
fhd_bio_1m_3=fhd_bio$`2_2.5_total_weight`+fhd_bio$`2.5_3_total_weight`
fhd_bio_1m_4=fhd_bio$`3_3.5_total_weight`+fhd_bio$`3.5_4_total_weight`
fhd_bio_1m_5=fhd_bio$`4_4.5_total_weight`+fhd_bio$`4.5_5_total_weight`

fhd_bio_1m=cbind(fhd_bio_1m_1,fhd_bio_1m_2,fhd_bio_1m_3,fhd_bio_1m_4,fhd_bio_1m_5)
fhd_bio_1m=as.data.frame(fhd_bio_1m)

data$fhd_bio_1m<-NA

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio_1m[i,])
  
  p<- table(v)
  p <- p/sum(p)
  entropy=sum(-p*log(p))
  
  data$fhd_bio_1m[i] <- entropy
  
}

#FHD rao
data$fhd_bio_rao<-NA

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio[i,])
  p <- v/sum(v)
  
  h <- as.matrix(seq(0.5,5,0.5))
  D=dist(h)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_bio_rao[i] <- quadratic_ent
  
}

data$fhd_bio_rao_1m<-NA

for (i in seq(1,35)) {
  print(i)
  
  v=as.numeric(fhd_bio_1m[i,])
  p <- v/sum(v)
  
  h <- as.matrix(seq(0.1,5,1))
  D=dist(h)
  D <- as.matrix(D)
  
  quadratic_ent=c(crossprod(p, D %*% p)) / 2
  
  data$fhd_bio_rao_1m[i] <- quadratic_ent
  
}

# export quadrat info to lidar
data_quadtrat_tolidar=subset(data,select=c(1:7,88,89,90,91,92,38))
write.csv(data_quadtrat_tolidar,"data_quadtrat_tolidar_2.csv")
