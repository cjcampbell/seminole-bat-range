
library(terra)
library(tidyterra)
library(ggpubr)
library(data.table)

# Load maps.
probMaps <- rast( file.path(wd$out, "maps", "quantsimProbabilityMaps.grd"))

# Reclassify to probable origins, sum.
rcl <- matrix(data = c(1e-5,1-0.66,0,1-0.66,1,1), byrow = T, ncol = 3)
probMaps_binary <- classify(probMaps, rcl)
probMaps_prop <- sum(probMaps_binary)/17

# Load movement data.
mydata_distDirStats <- fread(file.path(wd$out, "mydata_distDirStats.csv"))

# Plot with points and minimum distance traveled.
# gradientCols <- c("#8ecae6","#219ebc", "#023047" )
# gradientCols <- c("black","#5466FF","#FFBD00", "yellow","#FFFEC3")
gradientCols <-  c("#8ecae6","#219ebc", "#30123BFF" , "black")
p_map <- ggplot() +
  geom_spatraster(probMaps_prop, mapping = aes()) +
  scale_fill_gradientn("Proportion of\nlikely origins", colors = gradientCols,  limits = c(1e-5, 1), na.value = NA ) +
  
  # Add map details.
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2, color = "grey50") +
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2, color = "grey50")+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3, color = "grey50") +
  
  # Add points and segments 
  geom_segment(
    data = mydata_distDirStats, 
    aes(x = closestLikely_lon_m, y = closestLikely_lat_m, xend = metersLongitude, yend = metersLatitude), 
    color = "grey30",   arrow = arrow(length = unit(0.15, "cm")), linewidth = 0.2 ) +
  
  geom_point(data = mydata_distDirStats, aes(x=metersLongitude,y=metersLatitude), color = "black", size = 2) +
  geom_point(data = mydata_distDirStats, aes(x=metersLongitude,y=metersLatitude), color = "white", size = 1) +
  
  # More plot details.
  theme_minimal()+
  theme(
    legend.background = element_rect(fill = NA,  color = NA),
    legend.position = c(0,1.15),
    legend.key.height = unit(0.55, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.justification = c(0,1),
    legend.direction = "vertical",
    axis.title = element_blank(),
    axis.text = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.title.align=0.5
  ) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5) ) +
  coord_sf(xlim=c(-7e5,19e5),ylim=c(-16.5e5,8e5), crs = sf::st_crs(probMaps_prop))


# Plot direction traveled.

# mydata_distDirStats %>%
#   arrange(probOfOrigin_south_OfSampleSite) %>% 
#   dplyr::mutate(ID = factor(ID, levels = .$ID)) %>% 
#   ggplot() +
#   geom_point(aes(x=probOfOrigin_south_OfSampleSite, y = ID)) +
#   scale_x_continuous(limits = c(0.5,1))

p_distDir <- mydata_distDirStats %>%
  ggplot() +
  geom_point(aes(x=probOfOrigin_south_OfSampleSite, y = minDist_km), size = 2, color = "black") +
  geom_point(aes(x=probOfOrigin_south_OfSampleSite, y = minDist_km), size = 1, color = "white") +
  scale_x_continuous("Probability of southern summer origin", limits = c(0.5,1), breaks = seq(0,1,by=0.1)) +
  scale_y_continuous("Minimum distance traveled (km)", limits = c(0,500), breaks = seq(0,500,by=100)) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

p_distDir2 <- ggExtra::ggMarginal(
  p_distDir, type = "histogram",
  yparams = list(binwidth = 100),
  xparams = list(binwidth = 0.05)
  )


# arrange -----------------------------------------------------------------

library(patchwork)
p_combo <- p_map + p_distDir2 + plot_annotation(tag_levels = 'A') + plot_layout (widths = c(1,0.8))

ggsave(p_combo , file = file.path("figs", "dDResultsPlot.png"), width = 6, height = 3.2, dpi = 1000)
