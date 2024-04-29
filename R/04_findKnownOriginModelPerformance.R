


# Load known-origin data from Campbell et al -----------------------------
knownOrig0 <- read.csv(file = file.path(wd$knownOrigin, "knownOrigindata.csv")) %>% 
  #st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84") %>% 
  {data.frame(.,
              sf::sf_project(
                from =  "+proj=longlat +datum=WGS84", 
                to = myCRS, 
                pts = .[ , c("decimalLongitude", "decimalLatitude") ] 
                )
  )
    } %>% 
  dplyr::rename(metersLongitude = X1, metersLatitude = X2)

# Apply transfer function.
if(!exists("transferFunctionData_Campbelletal")) { 
  transferFunctionData_Campbelletal <- readRDS(file.path(wd$data, "transferFunctions", "transferFunctionData_Campbelletal.rds"))
}

knownOrig <- dplyr::mutate(
  knownOrig0,
  dDprecip =  (d2H - transferFunctionData_Campbelletal$mean_intercept) / transferFunctionData_Campbelletal$mean_slope,
  sdResid = transferFunctionData_Campbelletal$mean_sdRes
)

# load data for assignments -----------------------------------------------

load( file.path(wd$bin, "my_isoscapes.RData") )
range_raster <- raster::raster( file.path(wd$out, "LASErange.tif"))

# Make known-origin assignments -------------------------------------------

knownOriginSurfaces <- isocat::isotopeAssignmentModel(
  ID               = knownOrig$ID,
  isotopeValue     = knownOrig$dDprecip,
  SD_indv          = knownOrig$sdResid,
  precip_raster    = myisoscape$isoscape,
  precip_SD_raster = myisoscape$se, 
  additionalModels = range_raster
)

# Make quantile surfaces --------------------------------------------------

myQuants_list <- pbmcapply::pbmclapply(1:nrow(knownOrig), mc.cores = 3, function(i) {
  ID <- knownOrig[i, "ID"]
  rast <- knownOriginSurfaces[[ID]]
  myQuant <- quantileAtSamplingLocation(
    indivraster = rast, 
    Lat = knownOrig[i, "metersLatitude"],
    Lon = knownOrig[i, "metersLongitude"]
    )
  return(myQuant)
})
knownOriginQuants <- unlist(myQuants_list)

saveRDS(knownOriginQuants, file = file.path(wd$bin, "knownOriginQuants.rds"))
