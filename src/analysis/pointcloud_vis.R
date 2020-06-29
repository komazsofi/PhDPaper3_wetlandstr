library(lidR)
library(rgdal)
library(sp)

workdirectory=setwd("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/point_cloud/balaton/")
workdirectory=setwd("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/point_cloud/tiszaToLeafOff/")
workdirectory=setwd("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/point_cloud/fertoTo_HU_AT/")

# Cut example files

catalog=catalog(workdirectory)

clipped_1=lasclipCircle(catalog,681650.3622,5175399.689,100)
clipped_1b=lasclipCircle(catalog,681650.3622,5175399.689,10)

writeLAS(clipped_1,"mariafurdo_29_plot.laz")
writeLAS(clipped_1b,"mariafurdo_29_plot_10.laz")

clipped_1=lasclipCircle(catalog,716729.5711,5199560.366,100)
clipped_1b=lasclipCircle(catalog,716729.5711,5199560.366,10)

writeLAS(clipped_1,"sukoro_24_plot.laz")
writeLAS(clipped_1b,"sukoro_24_plot_10.laz")

clipped_1=lasclipCircle(catalog,716707.3335,5199535.155,100)
clipped_1b=lasclipCircle(catalog,716707.3335,5199535.155,10)

writeLAS(clipped_1,"sukoro_34_plot.laz")
writeLAS(clipped_1b,"sukoro_34_plot_10.laz")

clipped_1=lasclipCircle(catalog,479346.9173,5274509.591,100)
clipped_1b=lasclipCircle(catalog,479346.9173,5274509.591,10)

writeLAS(clipped_1,"tisza_1_plot.laz")
writeLAS(clipped_1b,"tisza_1_plot_10.laz")

clipped_1=lasclipCircle(catalog,632535.1232,5282175.53,100)
clipped_1b=lasclipCircle(catalog,632535.1232,5282175.53,10)

writeLAS(clipped_1,"fertohun_6_plot.laz")
writeLAS(clipped_1b,"fertohun_6_plot_10.laz")

clipped_1=lasclipCircle(catalog,629745.0183,5283627.69,100)
clipped_1b=lasclipCircle(catalog,629745.0183,5283627.69,10)

writeLAS(clipped_1,"fertohun_21_plot.laz")
writeLAS(clipped_1b,"fertohun_21_plot_10.laz")

# Import for vis
clipped_4=readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/pointcloud_forfielddata/mariafurdo_29_plot.laz")
clipped_4=readLAS("sukoro_34_plot.laz")
clipped_4=readLAS("tisza_1_plot.laz")
clipped_4=readLAS("fertohun_6_plot.laz")
clipped_4=readLAS("C:/Koma/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/pointcloud_forfielddata/fertohun_21_plot.laz")

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
  
  dtm = grid_metrics(clipped_4, min(Z), res=5)
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
  
  coords2 = matrix(c(x, y-10,
                     x, y+10), 
                   ncol = 2, byrow = TRUE)
  
  
  P1 = Polygon(coords2)
  line_cr = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"))
  
  coords3 = matrix(c(x-10, y,
                     x+10, y), 
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

rasterplot(clipped_4,x=629745.0183,y=5283627.69,bird="Plot 21 [1 m]")
