source("R/00_Setup.R")

# Load data and tidy.
df_0 <- lapply(1:4, function(s){
    readxl::read_excel("data/All_SEBA_data_FINAL_For_Submission.xlsx", sheet = s)
  }) %>% 
  bind_rows()

df_1 <- df_0 %>% 
  dplyr::mutate(
    ID = make.names(row_number()),
    d2H = as.numeric(d2hResults)
    ) %>% 
  dplyr::rename(
    lat = County_Centroid_Lat,
    lon = County_Centroid_Lon
  ) %>% 
  dplyr::select(ID, d2H, lat, lon)

mydata <- df_1 %>%
# Project to AEA
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = myCRS) %>%
  st_coordinates() %>%
  as.data.frame %>%
  dplyr::rename(
    metersLatitude  = Y,
    metersLongitude = X
  ) %>%
  cbind(df_1) %>%
  dplyr::select( ID, lon, lat, everything() ) %>% 
  dplyr::filter(!is.na(d2H))
