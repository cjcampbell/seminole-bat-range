
library(dplyr)
library(sf)
library(ggpubr)
library(ggstar)

maps_df <- readRDS(file.path(wd$bin, "maps_df.rds"))
mydata_distDirStats <- read.csv(file.path(wd$out, "mydata_distDirStats.csv"))

# This is specific to my local computer.
source("~/LASE_dD/R_setup/locationSpatialData.R")
NoAm <- readRDS( file.path(wd$bin, "NoAm.rds") )

IDs <- unique(maps_df$ID)

# Individually ------------------------------------------------------------

states <- readRDS(file.path(wd$bin, "states.rds"))
lapply(c("OR", "quantsim"), function(m) {
  
  lapply(IDs, function(myID) {
    
    myStats <- mydata_distDirStats %>% dplyr::filter(ID == myID)
    
    p <- maps_df %>% 
      dplyr::filter(ID == myID, method == m) %>% 
      ggplot()  +
      ggtitle(myID) +
      geom_tile(mapping=aes(x=x,y=y,fill=value, color = value))
    
    if(m == "OR") {
      p <- p + scale_fill_viridis_c(
        "Odds of origin",
        option = "turbo", direction = 1, limits = c(0,1)
      ) +
        scale_color_viridis_c(
          "Odds of origin",
          option = "turbo", direction = 1, limits = c(0,1)
        ) +
        geom_sf(
          NoAm, mapping=aes(),
          fill = NA, color = "grey20", size = 0.25
        ) 
    }
    if(m == "quantsim") {
      p <- p + scale_fill_viridis_c(
        "Prob of origin (quant-sim)",
        option = "mako", direction = 1, limits = c(0,1)
      ) +
        scale_color_viridis_c(
          "Prob of origin (quant-sim)",
          option = "mako", direction = 1, limits = c(0,1)
        ) +
        geom_sf(
          NoAm, mapping=aes(),
          fill = NA, color = "grey20", size = 0.25
        ) 
    }
    
    
    if(myStats$minDist_km >= 100) {
      p <- p + geom_segment(
        data = myStats, aes(
          x=closestLikely_lon_m, xend = metersLongitude,
          y=closestLikely_lat_m, yend = metersLatitude
        ),
        arrow = arrow(length = unit(0.25, "cm"))
      )
    }
    
    p <- p + 
      geom_sf(states , mapping = aes(), fill = NA, size = 0.25) +
      geom_star(
      data = myStats, aes(x=metersLongitude, y=metersLatitude),
      size = 2.5, fill = "white"
    ) +
      coord_sf(
        xlim = c(-14e5, 28e5),
        ylim = c(-20e5, 15e5)
      ) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.9,0.05),
        legend.justification = c(0.5, 0.5),
        legend.direction = "horizontal"
      ) +
      guides(
        fill = guide_colorbar(title.position="top", title.hjust = 0.5),
        color = guide_colorbar(title.position="top", title.hjust = 0.5)
      )
    
    ggsave(p, filename = file.path(wd$figs, paste0(myID, "_", m, "_origin.png")))
  })
})

