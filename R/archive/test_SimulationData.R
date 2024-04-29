source("~/LASE_dD/R/00_Setup.R")

# Load lit data -----------------------------------------------------------

litDat <- read.csv("/Users/cjcampbell/BatMigratoryDistDir/data/alldat.csv") %>% 
  dplyr::filter(Species == "LABO", yDay >= 165, yDay <= 215, wind_killed == "no") %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84")

iso_ma <- raster::raster("/Users/cjcampbell/BigZaddyData/isoscapes/GlobalPrecip/d2h_MA.tif")
litDat$ma_val <- raster::extract(iso_ma, litDat)

(m1 <- smatr::sma(d2H~ma_val, data = litDat))

ggplot(litDat) +
  geom_point(aes(x=gs_val, y=d2H, color = Sourcefile)) +
  geom_abline(slope = transferFunctionData_Campbelletal$mean_slope, intercept = transferFunctionData_Campbelletal$mean_intercept, linetype = 1) +
  geom_abline(slope = coefficients(m1)[2], intercept = coefficients(m1)[1], linetype = 2)


# Load isoscapes ---------------------------------------------------------------


NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )
myisoscape <- c()
myisoscape$isoscape <- iso_ma %>%
  raster::projectRaster(., crs = myCRS) %>%
  raster::extend( ., my_extent_aea ) %>%
  raster::crop(   ., my_extent_aea ) %>%
  raster::mask(   ., NoAm_boundary_aea  )

myisoscape$sd <- raster::raster("/Users/cjcampbell/BigZaddyData/isoscapes/GlobalPrecip/d2h_se_MA.tif") %>%
  raster::projectRaster(., crs = myCRS) %>%
  raster::extend( ., my_extent_aea ) %>%
  raster::crop(   ., my_extent_aea ) %>%
  raster::mask(   ., NoAm_boundary_aea  )


# Create maps. -----------------------------------------------------------
mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )

mapPath <- file.path("/Users/cjcampbell/LASE_dD/test")
if(!dir.exists(mapPath)) dir.create(mapPath)

lapply(c("ERBA", "SEBA"), function(myspp) {
  
  df <- mydata_transformed %>% filter(species == myspp)
  
  mypath <- file.path( mapPath, myspp)
  if(!dir.exists(mypath)) dir.create(mypath)
  
  if(myspp == "ERBA") {
    range_raster <- raster::raster( file.path(wd$out, "LABOrange.tif"))
  } else if(myspp == "SEBA") {
    range_raster <- raster::raster( file.path(wd$out, "LASErange.tif"))
  }
  
  # Switch to running for each individual.
  lapply(1:nrow(df), function(i) {
    
    croppedSurface <- isocat::isotopeAssignmentModel(
      ID               = df$ID[i],
      isotopeValue     = df$dDprecip[i],
      SD_indv          = df$sdResid[i],
      precip_raster    = myisoscape$isoscape,
      precip_SD_raster = myisoscape$sd, 
      additionalModels = range_raster,
      additionalModel_name = "cropped"
    )
    raster::writeRaster(
      croppedSurface, 
      file = file.path(mypath, paste0(df$ID[i], "_cropped.tif")), 
      overwrite = T)
    
  })
})

