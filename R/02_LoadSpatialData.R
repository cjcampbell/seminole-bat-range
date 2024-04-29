
# Setup -------------------------------------------------------------------
download_GADM <- FALSE

# This script assumes that there are candidate isoscapes downloaded from isoMAP
# somewhere in the wd$data directory.
reload_isoscapes <- FALSE

# This script assumes that there are IUCN rangemaps somewhere in the wd$iucn
# directory.
reload_IUCN_rangemaps <- FALSE

# Load GADM data ----------------------------------------------------------
if(download_GADM == TRUE){
  source("~/LASE_dD/R_setup/locationSpatialData.R")
  library(rmapshaper)
  message("Loading GADM data...")

  # First, make NoAm_sf (an "sf" object) fit extent object (my_extent).
  # my_extent_aea_st <- st_bbox(st_transform(st_as_sfc(st_bbox(my_extent, crs = 4326)), myCRS))
  # saveRDS(my_extent_aea_st, file = file.path(wd$bin, "my_extent_aea_st.rds"))

  # Get GADM data to state level.
  USA <- raster::getData('GADM', path = locationGADMData, country='USA', level=0)
  MEX <- raster::getData('GADM', path = locationGADMData, country='MEX', level=0)
  CAN <- raster::getData('GADM', path = locationGADMData, country='CAN', level=0)
  GTM <- raster::getData('GADM', path = locationGADMData, country='GTM', level=0)

  # Prepare to remove areas outside of desired extent.
  # ## Remove Hawaii
  USA_1 <- raster::getData('GADM', path = wd$bin, country='USA', level=1)
  hawaii <- USA_1[USA_1@data$NAME_1 == "Hawaii",] # Select Hawaii
  hawaii_simpl <- hawaii %>%
    sf::st_as_sf() %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5000) %>%
    st_buffer(dist = 1e6)
  # ## Remove water bodies.
  USA_2 <- raster::getData('GADM', path = locationGADMData, country='USA', level=2)
  MEX_2 <- raster::getData('GADM', path = locationGADMData, country='MEX', level=2)
  CAN_2 <- raster::getData('GADM', path = locationGADMData, country='CAN', level=2)
  GTM_2 <- raster::getData('GADM', path = locationGADMData, country='GTM', level=2)
  waterbodies <- lapply(list(USA_2,MEX_2,CAN_2,GTM_2), function(x){
    x[(x$ENGTYPE_2) == "Water body",]
  }) %>%
    do.call(rbind, .) %>%
    sf::st_as_sf() %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5000)
  saveRDS(waterbodies, file = file.path(wd$bin, "waterbodies.rds"))
  

  # Combine into one polygon, convert to sf object.
  NoAm <- raster::bind(
    MEX, USA, CAN, GTM#, BLZ, SLV, HND, NIC
  ) %>% 
    sf::st_as_sf(.) %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5e3) %>%
    st_difference(., hawaii_simpl) # Remove Hawaii
  
  saveRDS(NoAm, file = file.path(wd$bin, "NoAm.rds"))
  
  NoAm_boundary_aea <- NoAm %>% 
    st_buffer(dist = 5e4) %>%
    rmapshaper::ms_erase(., waterbodies)  # Remove water bodies

  saveRDS(NoAm_boundary_aea, file = file.path(wd$bin, "NoAm_boundary_aea.rds"))

  ## States ---- 
  USA_1 <- raster::getData('GADM', path = locationGADMData, country='USA', level=1)
  MEX_1 <- raster::getData('GADM', path = locationGADMData, country='MEX', level=1)
  CAN_1 <- raster::getData('GADM', path = locationGADMData, country='CAN', level=1)
  
  states <- raster::bind(
    MEX_1, USA_1, CAN_1
  ) %>% 
    sf::st_as_sf(.) %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5e3)
  saveRDS(states, file = file.path(wd$bin, "states.rds"))
  
} else message("Not redownloading GADM Data...")


# Load isoscapes ----------------------------------------------------------

if(reload_isoscapes == TRUE){
  message("reloading isoscapes...")
  
  NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )
  
  myisoscape <- c()
  myisoscape$isoscape  <- raster::raster(
    file.path(wd$data, "isoscape", "GlobalPrecip", "d2h_MA.tif")
    ) %>%
    raster::projectRaster(., crs = myCRS) %>%
    raster::extend( ., my_extent_aea ) %>%
    raster::crop(   ., my_extent_aea ) %>%
    raster::mask(   ., NoAm_boundary_aea  )
  myisoscape$se <- raster::raster(
    file.path(wd$data, "isoscape", "GlobalPrecip", "d2h_se_MA.tif")
    ) %>%
    raster::projectRaster(., crs = myCRS) %>%
    raster::extend( ., my_extent_aea ) %>%
    raster::crop(   ., my_extent_aea ) %>%
    raster::mask(   ., NoAm_boundary_aea  )
  
  # Save.
  save(myisoscape, file = file.path(wd$bin, "my_isoscapes.RData"))
  
} else message("Not reloading isoscapes, loading saved version...")


# Load and buffer IUCN Rangemaps -----------------------------------------------------
if(reload_IUCN_rangemaps == TRUE){
  library(rangeBuilder)
  
  preBuffAmount <- 100e3
  postBuffAmount <- 250e3
  
  load(file.path(wd$bin, "my_isoscapes.RData"), verbose = TRUE)
  if(!exists("NoAm_boundary_aea")) { 
    NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )
  }
  states <- readRDS( file.path(wd$bin, "states.rds") )

  ## Make seminolus alpha hull -----
  occurrencesLASE <- read.csv( file.path(wd$data, "countyRecords", "Perry2018PositiveCountyCentroids.csv"))
  shape <- rangeBuilder::getDynamicAlphaHull(x = occurrencesLASE, buff = preBuffAmount, fraction = 1, partCount = 1, initialAlpha = 20, 
                               clipToCoast = "terrestrial", proj = "+proj=longlat +datum=WGS84")
  saveRDS(shape, file = file.path(wd$bin, "LASE_range_unbuffered.rds"))
  LASE_range <- st_as_sf(shape[[1]]) %>% 
    st_transform(myCRS) %>% 
    st_buffer(dist=postBuffAmount)
  saveRDS(LASE_range, file = file.path(wd$bin, "LASE_range.rds"))
  p <- ggplot() +
    geom_sf(LASE_range, mapping = aes() ) + theme_void() +
    geom_point(as.data.frame(sf::sf_project(occurrencesLASE, from =  "+proj=longlat +datum=WGS84", to=myCRS)), mapping = aes(x=V1, y=V2)) +
    geom_sf(states, mapping = aes(), fill = NA, size = 0.25) +
    coord_sf(xlim = my_extent_aea[1:2], ylim = my_extent_aea[3:4])
  ggsave(p, filename = file.path(wd$figs, "LASE_alphahull.png"))
  
  ## Convert rangemaps to rasters ----
  makeRangeRaster <- function(range_sf, filename, iso = myisoscape$isoscape) {
    ex_rast <- iso
    ex_rast[] <- 1
    rangeRaster <- raster::mask(
      ex_rast, 
      mask = as_Spatial(range_sf), 
      updatevalue = NA
    ) %>%
      raster::mask(., NoAm_boundary_aea) %>%
      raster::crop(., my_extent_aea)
    writeRaster(rangeRaster, filename = filename, overwrite = T)
  }
  makeRangeRaster(LASE_range, filename = file.path(wd$out, "LASErange.tif"))
  #makeRangeRaster(LABO_range, filename = file.path(wd$out, "LABOrange.tif"))
  
} else message("Not recreating rangemaps...")
