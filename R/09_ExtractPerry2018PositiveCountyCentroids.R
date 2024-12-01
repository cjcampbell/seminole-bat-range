records <- readxl::read_xls("./data/countyRecords/SEMINOLE DATA FROM COLLECTIONS.xls")


library(tidyverse)
library(sf)
library(raster)
library(geosphere)

county <- read_sf("./shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
county$COUNTYFP

FIPS <- readxl::read_xlsx("./data/countyRecords/County FIDs.xlsx")

FIPS$FIPS <- as.character(FIPS$FIPS)
FIPS$FIPS[str_length(FIPS$FIPS) == 4] <- paste0("0", FIPS$FIPS[str_length(FIPS$FIPS) == 4])

records$COUNTY[!records$FIPS %in% county$GEOID]

records$FIPS <- as.character(records$FIPS)

records_simple <- data.frame(GEOID = as.character(records$FIPS),
                             county = records$COUNTY,
                             state = records$STATE,
                             record = 1)

county_wRecords <- left_join(county, 
                             records_simple, 
                             by = "GEOID")

county_wRecords <- county_wRecords[!duplicated(county_wRecords$GEOID),]

sum(county_wRecords$record, na.rm = T)

county_wRecords$record[is.na(county_wRecords$record)] <- 0

sum(county_wRecords$record)

library(ggspatial)

county_wRecords <- filter(county_wRecords, 
                           STATEFP != "02",
                           STATEFP != "72",
                           STATEFP != "15",
                           STATEFP != "66",
                           STATEFP != "69",
                           STATEFP != "78",
                           STATEFP != "60",
                           STATEFP != "69")

ggplot() +
  layer_spatial(county_wRecords, 
                aes(fill = record))

county_wRecords <- as_Spatial(county_wRecords)

library(raster)

county_wRecords <- spTransform(county_wRecords, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

county_wRecords <- as(county_wRecords, "sf")

county_wRecords <- filter(county_wRecords, record == 1)

county_wRecords <- as_Spatial(county_wRecords)

centroids <- centroid(county_wRecords)

centroids <- as.data.frame(centroids)

names(centroids) <- c("long", "lat")

ggplot() +
  layer_spatial(county_wRecords, 
                aes(fill = record)) +
  geom_point(data = centroids,
             aes(x = long,
                 y = lat))

write.csv(centroids, 
          "./data/countyRecords/Perry2018PositiveCountyCentroids.csv",
          row.names = F)

