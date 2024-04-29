
# Setup -------------------------------------------------------------------
source("~/LASE_dD/R/00_Setup.R")
mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )

load( file.path(wd$bin, "my_isoscapes.RData") )

mapPath <- file.path( wd$out, "maps")
if(!dir.exists(mapPath)) dir.create(mapPath)

# Create maps. -----------------------------------------------------------

range_raster <- raster::raster( file.path(wd$out, "LASErange.tif"))

# Create maps for each individual 
lapply(1:nrow(mydata_transformed), function(i) {
  
  croppedSurface <- isocat::isotopeAssignmentModel(
    ID               = mydata_transformed$ID[i],
    isotopeValue     = mydata_transformed$dDprecip[i],
    SD_indv          = mydata_transformed$sdResid[i],
    precip_raster    = myisoscape$isoscape,
    precip_SD_raster = myisoscape$se, 
    additionalModels = range_raster,
    additionalModel_name = "cropped"
  )
  raster::writeRaster(
    croppedSurface, 
    file = file.path(mapPath, paste0(mydata_transformed$ID[i], "_cropped.grd")), 
    format = "raster",
    overwrite = T)
  
})

# Convert maps to data.frame format for later use -------------------------

# Load maps.
maps_cropped <- list.files(
  mapPath, pattern = "*_cropped.grd$", full.names = TRUE, recursive = T) %>%
  terra::rast() %>% 
  raster::stack()

# Calculate probability quantiles.
maps_quantile_stack <- 
  pbmcapply::pbmclapply(1:nlayers(maps_cropped), mc.cores = 3, function(i) {
    isocat::makeQuantileSurfaces(maps_cropped[[i]])
    }) %>% 
  stack()
names(maps_quantile_stack) <- paste0(names(maps_cropped), "_quantile")
writeRaster(maps_quantile_stack, filename = file.path(mapPath, "quantileProbabilityMaps.grd"), overwrite = TRUE)

# Calculate quantile-simulation surfaces.
if(!exists("knownOriginQuants")) knownOriginQuants <- readRDS(file.path(wd$bin, "knownOriginQuants.rds"))
if(!exists("maps_quantile_stack")) maps_quantile_stack <- raster::stack(file.path(mapPath, "quantileProbabilityMaps.tif") )

maps_quantsim_stack <- pbmcapply::pbmclapply(
  1:nlayers(maps_quantile_stack), mc.cores = 3, function(i) {
    set.seed(42)
    makeQuantileSimulationSurface(
      probabilitySurface = maps_quantile_stack[[i]] ,
      ValidationQuantiles = knownOriginQuants
    )
 }) %>%
  raster::stack()
names(maps_quantsim_stack) <- paste0(names(maps_cropped), "_quantsim")
writeRaster(maps_quantsim_stack, filename = file.path(mapPath, "quantsimProbabilityMaps.grd"), overwrite = TRUE)

# And odds ratios.
maps_odds_stack <- 
  pbmcapply::pbmclapply(1:nlayers(maps_cropped), mc.cores = 3, function(i){
    isocat::makeOddsSurfaces(maps_cropped[[i]])
    }) %>% 
  stack()
names(maps_odds_stack) <- paste0(names(maps_cropped), "_OR")
writeRaster(maps_odds_stack, filename = file.path(mapPath, "ORProbabilityMaps.grd"), overwrite = TRUE)

# Combine, make dataframe. -----------------------------------------------------
# Do this in batches b/c it's pretty resource-intensive.
names(maps_cropped) <- paste0(names(maps_cropped), "_raw")

wd$tmp_df <- file.path(wd$bin,"tmp_df")
if(dir.exists(wd$tmp_df)) unlink(wd$tmp_df, recursive=TRUE)
if(!dir.exists(wd$tmp_df) ) dir.create(wd$tmp_df)

pbmcapply::pbmclapply(
  1:nlayers(maps_cropped), mc.cores = 3, function(i){
    basename <- gsub("_cropped_raw", "", names(maps_cropped[[i]]))
    mystack <- stack(maps_cropped[[i]], maps_quantile_stack[[i]], maps_quantsim_stack[[i]], maps_odds_stack[[i]])
    names(mystack) <- gsub("_cropped", "", names(mystack) )
    mdf <- mystack %>% 
      raster::as.data.frame(xy = TRUE, long = FALSE, na.rm = FALSE) %>%
      # Hackey fixes:
      tidyr::pivot_longer(-c("x", "y"), names_to = "layer", values_to = "value") %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::separate(col = layer, into = c("ID", "method"), sep = "_")
    saveRDS(mdf, file = file.path(wd$tmp_df, paste0("df_list_", basename, ".rds")))
})

maps_df <- list.files(wd$tmp_df, pattern = "df_list.*rds$", full.names = T) %>%
  lapply(readRDS) %>%
  plyr::ldply()

# Save.
saveRDS(maps_df, file = file.path(wd$bin, "maps_df.rds"))
