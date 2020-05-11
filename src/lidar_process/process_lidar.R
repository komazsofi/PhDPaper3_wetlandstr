library(gdalUtils)
library(rgdal)
library(raster)
library(dplyr)
library(sdm)
library(stringr)

workingdir="D:/Koma/_PhD/Sync/_Amsterdam/_PhD/Chapter2_habitat_str_lidar/2_Dataset/selected_lidarlayers_v2/balaton_2m_r/"

setwd(workingdir)
dir.create("crop")

crs_proj="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
#crs_proj="+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"

#shp=readOGR("tisza_pole_df_org_wstr_org.shp")
#shp=readOGR("ferto_pole_df_wstr_org.shp")
shp=readOGR("balaton_pole_df_wstr_org.shp")

proj4string(shp)<- CRS(crs_proj)

filelist=list.files(pattern = "*.tif")

id=sub("_.*", "", filelist)
id=unique(id)

feaname=substring(filelist, 10)

feaname=str_remove(feaname, "[1_]")

feaname=unique(feaname)

for (i in id) {
  print(i)
  
  rastlist=list.files(pattern=paste("^",i,".*\\.tif$",sep=""))
  raster_example=raster(rastlist[1],crs=crs_proj)
  
  for (k in 1:length(rastlist)){
    r <- raster(rastlist[k], crs=crs_proj)
    rp <- projectRaster(from = r, to = raster_example,
                        filename = file.path("./crop", rastlist[k]),
                        method = "bilinear",
                        format = "raster",
                        overwrite = TRUE)
  }
  
  rastlist2=list.files(path="./crop",pattern=paste("^",i,".*\\.grd$",sep=""),full.names = TRUE)
  rastlist3=list.files(path="./crop",pattern=paste("^",i,".*\\.grd$",sep=""),full.names = FALSE)
  
  feaname=substring(rastlist3, 10)
  feaname=str_remove(feaname, "[1_]")
  feaname=str_remove(feaname, "[_]")
  feaname=str_remove(feaname, "[_]")
  
  rasters=stack(rastlist2)
  
  crs(rasters) <- crs_proj
  names(rasters) <- feaname
  
  writeRaster(rasters,paste(i,"merged.grd",sep=""),overwrite=TRUE)
  
}

grdlist=list.files(pattern = "*.grd")

for (j in grdlist) {
  print(j)
  name=sub('\\..*', "", j)
  
  raster=stack(j)
  print(dim(raster))
  
  intersected=raster::extract(raster,shp)
  intersected.df=as.data.frame(intersected)
  
  shp_wlidar=cbind(shp@data,intersected.df)
  shp_wlidar_filt=shp_wlidar[!is.na(shp_wlidar[12]),]
  
  write.csv(shp_wlidar_filt,paste(name,"intersected.csv",sep=""))
}

files <- list.files(pattern = "*intersected.csv")

allcsv <- lapply(files,function(g){
  read.csv(g, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

write.csv(allcsv_df,"balaton_2m_r_v2.csv")
