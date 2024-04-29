

mydata0 <- read.csv( file.path( wd$data, "SEBA_data_dD_tidy.csv"))

# Load the file.
mydata1 <- mydata0 %>% 
  dplyr::mutate(
    ID = make.names(`ID`),
    species = "SEBA"
  ) %>% 
  dplyr::select(ID, species, d2H, lat, lon, State)

mydata <- mydata1 %>%
# Convert to projection.
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = myCRS) %>%
  st_coordinates() %>%
  as.data.frame %>%
  dplyr::rename(
    metersLatitude  = Y,
    metersLongitude = X
  ) %>%
  cbind(mydata1) %>%
  dplyr::select(ID, lon, lat, everything() )

# Optionally, check it out and make sure it looks okay.
# View(mydata)

