library(raster)
library(tidyverse)
library(ggspatial)
library(isocat)
library(ggnewscale)

maps <- stack("./out/quantsimProbabilityMaps.gri")
#plot(maps)
maps <- subset(maps,
               names(maps)[grepl("SEBA", names(maps))])
#names(maps)

state <- shapefile(file.path("./shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))

state <- spTransform(state, CRS("+proj=longlat +datum=WGS84"))

maps <- projectRaster(maps, crs = proj4string(state))
maps <- crop(maps, extent(-107,-66,22,48))
#plot(maps)

values(maps)[values(maps) >= (0.2)] <- 1
values(maps)[values(maps) < (0.2)] <- 0

mapsCombined <- overlay(maps, fun=function(x){sum(x)/12})

state <- raster::crop(state, extent(maps))

hex <- c("#FF0000","#FFFF00", "blue")

library(RColorBrewer)
heat <- colorRampPalette(rev(brewer.pal(n = 7, name =
                                          "RdYlBu")))(100)

ggplot() +
  layer_spatial(mapsCombined) +
  scale_fill_gradientn(colours = heat, na.value = NA) + 
  layer_spatial(state, color = "black", fill = NA) +
  scale_color_manual(values = c("black", "black")) +
  scale_linetype("dashed")

distTravelled <- read.csv("./out/mydata_distDirStats.csv")

LASEpoints <- distTravelled %>%
  filter(species == "SEBA") %>%
  mutate(OccurrenceLat = lat,
         OccurrenceLon = lon,
         OriginLat = closestLikely_lat_dd,          
         OriginLon = closestLikely_lon_dd) %>%
  select(OccurrenceLat, OccurrenceLon, OriginLat, OriginLon)
LASEpoints <- data.frame(Type = c(rep("Fatality Location", 12),
                                  rep("Closest Likely Origin", 12)
                                  ),
                         Lat = c(LASEpoints$OccurrenceLat, LASEpoints$OriginLat),
                         Lon = c(LASEpoints$OccurrenceLon, LASEpoints$OriginLon))

LASEpoints$Bat <- c(1:12, 1:12)

jpeg("./figs/SEBA_Combine_Origins_Map.jpeg",
     width = 7, height = 7, units = "in",
     res = 2000)
ggplot() +
  layer_spatial(mapsCombined) +
  scale_fill_viridis_c(
    "Proportion of Bat Sample Likely Originating  ",
    option = "turbo", direction = 1, limits = c(0,1),
    na.value = NA
  ) + 
  new_scale("fill")+
  layer_spatial(state, color = "grey70", fill = NA,
                size = 0.3) +
  geom_segment( aes( x = LASEpoints$Lon[1:12], 
                     y = LASEpoints$Lat[1:12],
                     xend = LASEpoints$Lon[13:24],
                     yend = LASEpoints$Lat[13:24]),
                color = "grey97",
                linetype = "dotted",
                size = 0.5) +
  geom_point(data = LASEpoints,
             aes(x = Lon, y = Lat,
                 fill = Type),
             color = "black",
             pch=21, size = 2, stroke = 1.1)+
  scale_y_continuous(expand = c(0,0,0,0),
                     limits = c(22.5, 48)) +
  scale_x_continuous(expand = c(0,0,0,0))+
  scale_fill_manual("",values = c("white", "hotpink")) +
  labs(x = "Longitude",
       y = "Latitude",
       color = "") +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "grey95"),
        legend.box="vertical", legend.margin=margin(),
        legend.key.width = unit(1.5, "cm"))
dev.off()


######## adding a map for the first manuscript #########

perryRange <- shapefile("./shapefiles/perry2018approximate.shp")
county <- shapefile(file.path("./shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"))

perryRange <- spTransform(perryRange, CRS("+proj=longlat +datum=WGS84"))
county <- spTransform(county, CRS("+proj=longlat +datum=WGS84"))

county <- crop(county, extent(-107,-66,22,48))
plot(perryRange)
perryRange <- rgeos::gIntersection(perryRange, state)
plot(perryRange)

perryPositives <- read.csv("./data/countyRecords/Perry2018PositiveCountyCentroids.csv")
perryPositives <- SpatialPoints(coords = data.frame(x =perryPositives$long,y = perryPositives$lat),
                                proj4string = CRS("+proj=longlat +datum=WGS84"))


### need to get our positives in there
# LASE points

LASEpoints <- rbind(readxl::read_excel("C://Users/mtrue/Documents/WEST/projects/OneOffs/LASE_personal/All_SEBA_data.xlsx"),
                    readxl::read_excel("C://Users/mtrue/Documents/WEST/projects/OneOffs/LASE_personal/All_SEBA_data.xlsx", sheet = 2),
                    readxl::read_excel("C://Users/mtrue/Documents/WEST/projects/OneOffs/LASE_personal/All_SEBA_data.xlsx", sheet = 3))



LASEpoints <- LASEpoints %>%
  filter(Permission == "Yes")

LASEpoints <- SpatialPointsDataFrame(coords = data.frame(x = as.numeric(LASEpoints$Lon),
                                                y = as.numeric(LASEpoints$Lat)),
                            data = LASEpoints,
                            proj4string = CRS("+proj=longlat +datum=WGS84"))


jpeg("./figs/Manuscript1_2019_2020_LASE_records.jpg", 
     width  = 8, 
     height = 6.5, res = 1000,
     units = "in")
ggplot() +
  layer_spatial(county, fill = "grey82", alpha = 0.2, color = "grey82") +
  layer_spatial(state, fill = NA) +
  layer_spatial(perryRange, fill = "grey20", 
                aes(color = "Summer (adapted from Perry [2018])"), 
                size = 1,
                alpha = 0.2) +
  scale_color_manual(values = c("black"), name = "LASE range",
                     guide = guide_legend(order = 2)) +
  layer_spatial(LASEpoints, aes(fill = "This study"),
                shape = 21, size = 2.2) +
  layer_spatial(perryPositives, aes(fill = "Perry (2018)"),
                shape = 21, size = 2.2) +
  scale_fill_manual(values = c( "red","#0072B2"),
                    guide = guide_legend(order = 1),
                    limits = c("This study", "Perry (2018)")) +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.border = element_rect(size = 1, fill = NA),
        legend.box="vertical") +
  labs(x = NULL, y = NULL, color = NULL, fill = "County occurrence records") +
  scale_x_continuous(expand = c(0,0,0,1)) +
  scale_y_continuous(expand = c(0,1,0,0))
dev.off()

################# turbine density maps ##################

turbines <- read.csv("./data/turbineDatabase/USGSturbineDatabase.csv")

turbines <- filter(turbines, t_cap >= 1000) # just 1 MW or above

turbines <- turbines %>% 
  dplyr::group_by(p_name) %>%
  dplyr::summarize(t_cap = sum(t_cap),
                   xlong = mean(xlong),
                   ylat = mean(ylat))
  
turbines <- SpatialPoints(coords = data.frame(x = as.numeric(turbines$xlong),
                                              y = as.numeric(turbines$ylat)),
                          proj4string = CRS("+proj=longlat +datum=WGS84"))

turbines <- crop(turbines, state)

turbines_xy <- as.data.frame(turbines)


library(ggnewscale)

jpeg("./figs/Manuscript1_TurbDist_LASE_records.jpg", 
     width  = 7, 
     height = 7, res = 800,
     units = "in")
ggplot() +
  layer_spatial(county, fill = "grey82", alpha = 0.2, color = "grey82") +
  layer_spatial(state, fill = NA) +
  stat_density2d(data = turbines_xy,
                 contour_var = "ndensity",
                 aes(x = x, y = y,
                     color = after_stat(level)),
                 binwidth = 0.1) +
  scale_color_viridis_c(name = "Wind facility density  ",
                        limits = c(0,1),
                        guide = guide_legend(order = 4)) +
  labs(color = NULL) +
  new_scale_color() +
  geom_point(data = turbines_xy,
             aes(x = x, y = y,
                 color = "United States wind facilities"),
             alpha = 1,
             size = 0.4) +
  scale_color_manual(values = "grey25", guide = guide_legend(order = 3)) +
  labs(color = NULL) +
  new_scale_color() +
  layer_spatial(perryRange, fill = "grey20", 
                aes(color = "Summer (adapted from Perry [2018])"), 
                size = 1,
                alpha = 0.2) +
  scale_color_manual(values = c("black"), name = "LASE range",
                     guide = guide_legend(order = 2)
  ) +
  layer_spatial(LASEpoints, aes(fill = "This study"),
                shape = 21, size = 2.2) +
  layer_spatial(perryPositives, aes(fill = "Perry (2018)"),
                shape = 21, size = 2.2) +
  scale_fill_manual(values = c("red","#0072B2"),
                    guide = guide_legend(order = 1),
                    limits = c("This study", "Perry (2018)")) +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.border = element_rect(size = 1, fill = NA),
        legend.box="vertical") +
  labs(x = NULL, y = NULL, color = NULL, fill = "LASE county occurrence records") +
  scale_x_continuous(expand = c(0,0,0,1)) +
  scale_y_continuous(expand = c(0,1,0,0))
dev.off()

library(alphahull)
st_perryPositives <- as(perryPositives, "sf")
st_LASEpoints <- as(LASEpoints, "sf")

st_LASEpoints <- c(st_geometry(st_perryPositives), st_geometry(st_LASEpoints))

LASE_points_xy <- do.call(rbind, st_geometry(st_LASEpoints)) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))
library(rangeBuilder)
LASE_range <- rangeBuilder::getDynamicAlphaHull(x = as.matrix(LASE_points_xy[,c(1,2)]), buff = 100000, fraction = 1, partCount = 3, initialAlpha = 1000, proj = "+proj=longlat +datum=WGS84")

LASE_range <- LASE_range[[1]]
plot(LASE_range)
plot(st_LASEpoints, add = T)
plot(state, add = T)

LASE_range <- rgeos::gIntersection(LASE_range, state)
perryRange <- rgeos::gIntersection(perryRange, LASE_range)

jpeg("./figs/Manuscript1_UpdatedRange.jpg", 
     width  = 7, 
     height = 7, res = 800,
     units = "in")
ggplot() +
  layer_spatial(county, fill = "grey82", alpha = 0.2, color = "grey82") +
  layer_spatial(state, fill = NA) +
  # stat_density2d(data = turbines_xy,
  #                contour_var = "ndensity",
  #                aes(x = x, y = y,
  #                    color = after_stat(level)),
  #                binwidth = 0.05) +
  # scale_color_viridis_c(name = "Wind turbine density  ",
  #                       limits = c(0,1),
  #                       guide = guide_legend(order = 4)) +
  # labs(color = NULL) +
  # new_scale_color() +
  geom_point(data = turbines_xy,
             aes(x = x, y = y,
                 color = "United States wind facilities"),
             size = 0.4) +
  scale_color_manual(values = "grey25", guide = guide_legend(order = 3),
                     ) +
  labs(color = NULL) +
  new_scale_color() +
  layer_spatial(LASE_range, 
                aes(fill = "Fall Vagrant Area"), 
                size = 1,
                color = "grey10",
                alpha = 0.4,
                linetype = "dashed") +
  layer_spatial(perryRange,
                aes(fill = "Summer (adapted from Perry [2018])"), 
                size = 1,
                color = "grey10",
                alpha = 0.4) +
  scale_fill_manual(values = c("grey70","grey35"), 
                    name = "LASE range",
                    guide = guide_legend(order = 2)) +
  new_scale_fill() +
  layer_spatial(LASEpoints, aes(fill = "This study"),
                shape = 21, size = 2.2) +
  layer_spatial(perryPositives, aes(fill = "Perry (2018)"),
                shape = 21, size = 2.2) +
  scale_fill_manual(values = c("red","#0072B2"),
                    limits = c("This study", "Perry (2018)"),
                    guide = guide_legend(order = 1)) +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.border = element_rect(size = 1, fill = NA),
        legend.box="vertical") +
  labs(x = NULL, y = NULL, color = NULL, fill = "LASE county occurrence records") +
  scale_x_continuous(expand = c(0,0,0,1)) +
  scale_y_continuous(expand = c(0,1,0,0))
dev.off()
