library(ggplot2)
library(gridExtra)

library(dplyr)
library(tidyr)

library(stargazer)

library(lidR)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis9/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

plot_data05=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))
plot_data5=read.csv(paste("Plot_noncorr",5,".csv",sep=""))
plot_data5$total.weight=plot_data5$total.weight/10000

las_bal <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad_reclass/Balaton_OBJNAME204_25mrad_reclass.laz")
las_fert <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto_25mrad_reclass/Ferto_OBJNAME321_25mrad_reclass.laz")
#las_tisza <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_leafon_reclass/Tisza_OBJNAME186_25mrad_reclass.laz")
las_tisza <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/tisza_25mrad_reclass/Tisza_OBJNAME186_25mrad_reclass.laz")

##### Pcloud visualization

bal=plot_data05[plot_data05$OBJNAME==204,]
fert=plot_data05[plot_data05$OBJNAME==321,]
tisza=plot_data05[plot_data05$OBJNAME==186,]


plot_crossection <- function(las,
                             p1,
                             p2,
                             bal,
                             width = 2, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 5) + coord_equal() + theme_minimal(base_size=30)+
    geom_vline(xintercept=bal$coords.x1, linetype="dashed", color = "red")+
    geom_vline(xintercept=bal$coords.x1-0.5, linetype="dashed", color = "blue")+
    geom_vline(xintercept=bal$coords.x1+0.5, linetype="dashed", color = "blue")+
    geom_vline(xintercept=bal$coords.x1-2.5, linetype="dashed", color = "black")+
    geom_vline(xintercept=bal$coords.x1+2.5, linetype="dashed", color = "black")+
    scale_colour_manual(values=c("1"="darkgreen", "2"="deeppink"),label=c("Non-ground points","Ground points"),name="Classification")
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

p1=plot_crossection(las_bal,p1 = c(bal$coords.x1-5, bal$coords.x2),p2 = c(bal$coords.x1+5, bal$coords.x2),bal,colour_by = factor(Classification))
p2=plot_crossection(las_fert,p1 = c(fert$coords.x1[1]+5, fert$coords.x2[1]),p2 = c(fert$coords.x1[1]-5, fert$coords.x2[1]),fert,colour_by = factor(Classification))
p3=plot_crossection(las_tisza,p1 = c(tisza$coords.x1[1]+5, tisza$coords.x2[1]),p2 = c(tisza$coords.x1[1]-5, tisza$coords.x2[1]),tisza,colour_by = factor(Classification))

ggsave("Figcross_bal.png",plot = p1,width = 22, height = 12)
ggsave("Figcross_fer.png",plot = p2,width = 22, height = 12)
#ggsave("Figcross_tiszaon.png",plot = p3,width = 22, height = 12)
ggsave("Figcross_tiszaoff.png",plot = p3,width = 22, height = 12)


las_tisza@data=las_tisza@data[(las_tisza@data$Z<135 & las_tisza@data$Z>110),]

plot(las_bal,size=4,axis=TRUE)
plot(las_fert,size=4,axis=TRUE)
plot(las_tisza,size=4,axis=TRUE)
