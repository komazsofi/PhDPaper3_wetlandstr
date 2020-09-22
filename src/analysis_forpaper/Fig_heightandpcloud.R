library(ggplot2)
library(dplyr)
library(tidyr)

library(stargazer)

library(lidR)

workdir="C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
#workdir="D:/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/Analysis7/"
setwd(workdir)

####################################### Plot

plot_data05=read.csv(paste("Plot_noncorr",0.5,".csv",sep=""))

las_bal <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/balaton_25mrad_reclass/Balaton_OBJNAME204_25mrad_reclass.laz")
las_fert <- readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/3_Dataprocessing/pcloud/ferto_25mrad_reclass/Ferto_OBJNAME321_25mrad_reclass.laz")


##### Visualization Height

ggplot(data=plot_data05,aes(x=H_max,y=veg_height_m))+geom_point(aes(color=lake,shape=veg_type_2,size=nofveg))+theme_classic(base_size=20)+
  xlab("H_max (LiDAR, res.=0.5 m)")+ylab("Vegetation height [m] (field)")+
  ggtitle("Most important metric for estimating vegetation height")+
  scale_colour_manual(values=c("Lake Balaton"="red", "Lake Ferto"="darkgreen","Lake Tisza"="blue"),name="Lakes")+
  scale_size_continuous(breaks=c(5,10,25),name="Number of vegetation point")+
  scale_shape_manual(values=c("carex"=16,"phragmites"=17,"typha"=15),name="Species",labels=c("Carex spec.","Phragmites australis","Typha spec."))+
  geom_text(aes(label=OBJNAME),hjust=0, vjust=0,size=2)

# Standardized coefficient
plot_data05_filt=plot_data05[(plot_data05$OBJNAME!=209 & plot_data05$OBJNAME!=160 & plot_data05$OBJNAME!=120),]

lm_scaled<-lm(veg_height_m ~ Scaled_H_max+Scaled_V_ku+Scaled_A_std+Scaled_A_cover,data = plot_data05_filt)
summary(lm_scaled)

std_coef_sum=as.data.frame(lm_scaled$coefficients[2:5])
names(std_coef_sum)<-c("Coeff")

ggplot(data=std_coef_sum, aes(x=Coeff, y=rownames(std_coef_sum))) +
  geom_bar(stat="identity")+
  xlab("Standardized coefficient")+ylab("LiDAR metrics")+
  ggtitle("Feature importance for estimating vegetation height (res=0.5 m)")+
  theme_classic(base_size=20)

# Pcloud visualization

bal=plot_data05[plot_data05$OBJNAME==204,]


plot_crossection <- function(las,
                             p1 = c(bal$coords.x1-2.5, bal$coords.x2),
                             p2 = c(bal$coords.x1+2.5, bal$coords.x2),
                             width = 2, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 5) + coord_equal() + theme_minimal(base_size=20)+
    geom_vline(xintercept=bal$coords.x1, linetype="dashed", color = "red")+
    geom_vline(xintercept=bal$coords.x1-0.5, linetype="dashed", color = "blue")+
    geom_vline(xintercept=bal$coords.x1+0.5, linetype="dashed", color = "blue")+
    scale_colour_manual(values=c("1"="darkgreen", "2"="deeppink"),label=c("Non-ground points","Ground points"),name="Classification")
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

plot_crossection(las_bal, colour_by = factor(Classification))

clip_bal=lasclipCircle(las_bal,bal$coords.x1,bal$coords.x2,10)

#rasterplot
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

defcolpal=palette(c("gray48","gold","lawngreen","chartreuse4"))
defcolpal=addalpha(defcolpal, 0.45)

rasterplot<-function(clipped_4,x=184877,y=511157,bird="Great reed warbler"){
  
  dtm = grid_metrics(clipped_4, min(Z), res=1)
  clipped_4_norm=lasnormalize(clipped_4, dtm)
  
  hperc09all = grid_metrics(clipped_4_norm, max(Z), res=1)
  crs(hperc09all) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
  
  slope <- terrain(hperc09all, opt='slope')
  aspect <- terrain(hperc09all, opt='aspect')
  dsm_shd <- hillShade(slope, aspect, 40, 270)
  
  height_class=reclassify(hperc09all, c(-Inf,0.5,1,0.5,3,2,3,5,3,5,Inf,4))
  
  coords = matrix(c(x, y), 
                  ncol = 2, byrow = TRUE)
  
  
  birdpoint = SpatialPoints(coords, proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  coords2 = matrix(c(x, y-0.5,
                     x, y+0.5), 
                   ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords2)
  line_cr = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  coords3 = matrix(c(x-0.5, y,
                     x+0.5, y), 
                   ncol = 2, byrow = TRUE)
  
  
  P12 = Polygon(coords3)
  line_cr2 = SpatialPolygons(list(Polygons(list(P12), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  defcolpal=palette(c("gray48","gold","lawngreen","green4"))
  defcolpal=addalpha(defcolpal, 0.45)
  
  par(mfrow=c(1,1)) 
  par(cex.lab=1, cex.axis=1, cex.main=2)
  plot(dsm_shd, col=grey(0:100/100), legend=FALSE, main=bird)
  plot(birdpoint,pch=1,lwd = 3,col="black",add=TRUE)
  plot(line_cr, lwd=3,col="black",add=TRUE)
  plot(line_cr2, lwd=3,col="black",add=TRUE)
  plot(height_class, col=defcolpal,breaks=c(0,1,2,3,4), add=TRUE,
       lab.breaks = c("0","0.5","3","5","20"),
       legend.args=list(text='Height [m]', side=4, font=2, line=2.5,cex=2))
  
}

rasterplot(las_bal,x=bal$coords.x1,y=bal$coords.x2,bird="Lake Balaton ID=204")
