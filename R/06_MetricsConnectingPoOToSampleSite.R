# Load objects -------------------------------------------------------------------
library(data.table)
mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )
maps_df    <- readRDS( file.path(wd$bin, "maps_df.rds") )

# Assemble lat/lon coordinates --------------------------------------------

# Surfaces must be converted to WGS84 (see ?geosphere::distGeo)

# Get unique coordinates to convert from each surface.
coords_aea <- maps_df %>%
  dplyr::select(x,y) %>%
  distinct() 
coords_dd <- coords_aea %>%
  SpatialPointsDataFrame(coords = .[, 1:2], proj4string = CRS(myCRS)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame %>%
  dplyr::rename(x_dd = X, y_dd = Y)

mydata_xyz <- cbind(coords_aea, coords_dd) %>%
  right_join(maps_df, by = c("x", "y")) %>%
  full_join(mydata_transformed, by = "ID")
saveRDS(mydata_xyz, file = file.path(wd$bin, "mydata_xyz.rds"))


# Functions ----------------------------------------------------------------
# Function that extracts distance and bearing relating each potential cell of
# geographic origin with individual's sample site.

getDistanceDirection <- function( rowNumber, dataframe, ID, fromLat, toLat, 
                                  fromLon, toLon, getDistance = TRUE, 
                                  getDirection = TRUE, roundTo = 2 ) { 
  
  p1 <- c( dataframe[ rowNumber, fromLon ], dataframe[ rowNumber, fromLat ] )
  p2 <- c( dataframe[ rowNumber, toLon ],   dataframe[ rowNumber, toLat ] )
  myResults <- data.table::data.table()
  if( getDistance == TRUE ){
    dist_km   <- round( geosphere::distGeo(p1, p2) / 1000 , roundTo) #Convert to km, round.
    myResults <- data.table::data.table(myResults, dist_km)
  }
  if( getDirection == TRUE ){
    theta_from_site   <- round( geosphere::bearing(p2, p1), roundTo)
    theta_from_origin <- round( geosphere::bearing(p1, p2), roundTo)
    myResults         <- data.table::data.table(myResults, theta_from_site, theta_from_origin)
  }
  return(myResults)
  
}

# Apply -------------------------------------------------------------------

# Measure distance and direction from every cell to each sampling location.

# Set to 'FALSE' is you don't want to run in parallel.
nclusters <- parallel::detectCores() - 1

# First make df with unique combinations of from/to variables.
mydata_FromTo <- mydata_xyz %>%
  dplyr::select(lat, lon, x_dd, y_dd) %>%
  dplyr::distinct()

distDirection_list <- pbmcapply::pbmclapply(
  FUN = getDistanceDirection, mc.cores = nclusters,
  1:nrow(mydata_FromTo),
  dataframe =  mydata_FromTo,
  fromLat = "lat", toLat = "y_dd", fromLon = "lon", toLon = "x_dd",
  getDistance = TRUE, getDirection = TRUE
)

if(!exists("mydata_xyz")) mydata_xyz <- readRDS( file.path(wd$bin, "mydata_xyz.rds") )
mydata_distDir <- distDirection_list %>% 
  do.call(rbind,.) %>% 
  data.frame(mydata_FromTo, . ) %>% 
  data.table::merge.data.table(., mydata_xyz, by = c("lat", "lon", "x_dd", "y_dd"))

saveRDS(mydata_distDir, file = file.path(wd$bin, "mydata_distDir.rds"))


# Find likely directions of origin ---------------------------------------------
# No distance weighting-- all origins in study region considered independent and
# equally likely as origin given dD value.
set.seed(42)
n <- 10000
likelyDirectionOfOrigin <- mydata_distDir %>% 
  filter(method == "raw") %>% 
  group_by(ID) %>% 
  sample_n(size = n, weight = value, replace = TRUE) %>% 
  dplyr::summarise(
    probOfOrigin_north_OfSampleSite = sum(abs(theta_from_site) > 90)/n,
    probOfOrigin_south_OfSampleSite = sum(abs(theta_from_site) < 90)/n
  )
saveRDS(likelyDirectionOfOrigin, file = file.path(wd$bin, "likelyDirectionOfOrigin.rds"))


# Find minimum distance traveled ------------------------------------------
# Use the quantile-simulation threshold. If you want X % of known-origin 
# individuals assigned correctly over a threshold, the threshold should be set
# to 1-X/100.

threshold <- 1-0.66  # 66% correct assignments
md <- mydata_distDir %>% 
  dplyr::filter(method == "quantsim")

# First, check if any individuals don't have a point over the threshold.
a <- md %>% 
  group_by(ID) %>% 
  dplyr::summarise(
    n_over = sum(value >= threshold)
  ) %>% 
  arrange(n_over)
if(min(a$n_over) ==0) {
  warning("Not enough cells in at least one surface")
  } else {
    print(min(a$n_over))
  }
# All individuals have at least 16782 cells over the 66% quant-sim threshold. Great!


# Identify the site and distance over 66% threshold and find
# the potential origin closest to the sample site with the smallest distance traveled.
distInfo <- md %>% 
  dplyr::filter(value >= threshold ) %>% 
  group_by(ID) %>% 
  arrange(dist_km) %>% 
  slice(1) 

saveRDS(distInfo, file = file.path(wd$bin, "distInfo.rds"))

# Combine results ---------------------------------------------------------
if(!exists("distInfo")) distInfo <- readRDS( file.path(wd$bin, "distInfo.rds") )
if(!exists("likelyDirectionOfOrigin")) likelyDirectionOfOrigin <- readRDS(file.path(wd$bin, "likelyDirectionOfOrigin.rds") )

mydata_distDirStats <- distInfo %>% 
  inner_join(likelyDirectionOfOrigin) %>% 
  dplyr::rename(
    minDist_km = dist_km,
    closestLikely_lon_dd = x_dd,
    closestLikely_lat_dd = y_dd,
    closestLikely_lon_m = x,
    closestLikely_lat_m = y
  ) %>% 
  dplyr::select(names(mydata_transformed), everything(), -c(method, value))

data.table::fwrite(mydata_distDirStats, file = file.path(wd$out, "mydata_distDirStats.csv"), row.names = F)
