library(raster)
library(tidyverse)
library(ggspatial)

maps <- stack(list.files(path = file.path(getwd(), "/out/SEBA"), full.names = T))

LASEcombined <- overlay(maps, fun=mean)

state <- shapefile(file.path("./shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))

state <- spTransform(state, CRS("+proj=longlat +datum=WGS84"))

LASEcombined <- projectRaster(LASEcombined, crs = proj4string(state), res = 0.1)

state <- raster::crop(state, extent(LASEcombined))

LASEcombined <- mask(LASEcombined, state)

LASEcombined <- crop(LASEcombined, extent(-108,-65,22,50))

state <- crop(state, LASEcombined)

ggplot() +
  layer_spatial(LASEcombined) +
  scale_fill_viridis_c(na.value=NA) +
  layer_spatial(state, color = "black", fill = NA)

normal_01 <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

values(LASEcombined) <- values(LASEcombined)/sum(values(LASEcombined), na.rm = T)
# normalize so adds to 1

library(isocat)

LASEcombined_odds <- makeOddsSurfaces(LASEcombined)
LASEcombined_cumsum <- makecumsumSurface(LASEcombined)
LASEcontour <- rasterToContour(LASEcombined_cumsum, levels = c(0.05, 0.25, 0.5, 0.75))


hex <- c("#FF0000","#FFFF00", "#000066")

ggplot() +
  layer_spatial(LASEcombined_cumsum) +
  scale_fill_gradientn(colours = rev(hex)) + 
  layer_spatial(state, color = "black", fill = NA) +
  layer_spatial(LASEcontour, aes(color = level))


sum(values(LASEcombined), na.rm = T)


# bin them ?

LASEcombined_binned <- LASEcombined
values(LASEcombined_binned) <- case_when(values(LASEcombined_binned) < 0.5 ~ 0,
                                         values(LASEcombined_binned) >=0.5 & 
                                           values(LASEcombined_binned) < 0.75 ~ 1,
                                         values(LASEcombined_binned) >=0.75 ~ 2)
