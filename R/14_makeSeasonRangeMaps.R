library(tidyverse)
library(sf)

# Load key with county IDs.
FIPS <- readxl::read_xlsx("./data/countyRecords/County FIDs.xlsx") %>% 
  dplyr::mutate(
    FIPS=as.numeric(FIPS)
    ) %>% 
  dplyr::select(FIPS, STATE, COUNTY) %>% 
  distinct()

# Load county shapefiles.
county <- read_sf("./shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp") %>% 
  dplyr::mutate(
    STATEFP = as.numeric(STATEFP),
    COUNTYFP = as.numeric(COUNTYFP),
    GEOID = as.numeric(GEOID)
  ) %>% 
  st_as_sf() %>% 
  st_transform("+proj=longlat +datum=WGS84")

# Load LASE records.
## From Perry 2018
records <- readxl::read_xls("./data/countyRecords/SEMINOLE DATA FROM COLLECTIONS.xls") %>% 
  dplyr::mutate(
    FIPS=as.numeric(FIPS),
    source = "Perry 2018"
  ) 

## From this study.
source("~/LASE_dD/R/00_Setup.R")
mydata <- lapply(1:4, function(x){
  readxl::read_excel(file.path("data", "All_SEBA_data_FINAL_Permissions_Given.xlsx"), sheet = x) %>% 
    dplyr::mutate(
      DateFound = as.character(DateFound),
      source = "WEST")
}) %>% 
  bind_rows() 

mydat_FIPS <-  mydata %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  st_intersects(., county)
mydata$FIPS <- county$GEOID[unlist(mydat_FIPS)]
## Combine.
allrecords <- mydata %>% 
  dplyr::mutate(Season = "F", thisStudy="Y") %>% 
  full_join(., records) %>% 
  dplyr::select(Season,FIPS, everything())

# Combine.
records_county <- left_join(allrecords, FIPS, by = join_by("FIPS")) %>% 
  left_join(., county, by=join_by("FIPS"=="GEOID")) %>% 
  st_as_sf() %>% 
  dplyr::filter(!is.na(Season)) %>% 
  dplyr::mutate(
    seasongroup = case_when(Season == "F" ~ "Fall", Season == "W" ~ "Winter", TRUE ~ "Summer") )
    # Perry_cat = case_when(
    #   source == "Perry 2018" & STATEFP %in% c( "02", "72", "15", "66", "69", "78", "60", "69") ~ "Perry outlier", source == "Perry" ~ "Perry core", TRUE ~ as.character(NA))
    # )

records_centroids <- st_centroid(records_county)

# Load gadm data.
usaDat <- geodata::gadm(country="USA", path= wd$bin, level=1) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance=1000)

usa_centroids <- usaDat %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame()

usaDat_east <- usaDat[usa_centroids$X>-110,]

MEXDat <- geodata::gadm(country="MEX", path= wd$bin, level=1) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance=1000)
CANDat <- geodata::gadm(country="CAN", path= wd$bin, level=1) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance=1000)
gadm <- bind_rows(usaDat_east, MEXDat) %>% 
  bind_rows(CANDat)

NoAm <- readRDS(file.path(wd$bin, "NoAm.rds"))
waterbodies <- readRDS( file.path(wd$bin, "waterbodies.rds"))

# Map occs by month/season ------------------------------------------------

library(rangeBuilder)
myranges <- records_centroids %>%
  split(.$seasongroup) %>% 
  map(~st_coordinates(.)) %>% 
  map(~rangeBuilder::getDynamicAlphaHull(
    x = ., buff = 50e3, fraction = 0.95, partCount = 1, initialAlpha = 20, alphaIncrement = 3,
    clipToCoast = "terrestrial", proj = "+proj=longlat +datum=WGS84",verbose = T)
    )

ranges_sf <- myranges %>% 
  map(~st_as_sf(.[[1]])) %>% 
  bind_rows()
ranges_sf$seasongroup <- names(myranges)

ranges_sf %>% 
  ggplot() +
  geom_sf( mapping = aes(fill = seasongroup,color=seasongroup), alpha = 0.5) +
  theme_minimal() +
  geom_sf(records_centroids,mapping=aes(color=seasongroup)) +
  facet_wrap(~seasongroup)


# testing of whether bats are observed at higher latitude --------

records_centroids2 <- records_centroids %>% 
  dplyr::mutate(
    seasonName = case_when(seasongroup == "Fall" ~ "Autumn", TRUE ~ seasongroup),
    YEAR = case_when(is.na(YEAR) ~ Year, TRUE ~ YEAR),
    thisStudy = case_when(is.na(thisStudy) ~ "no", TRUE ~ thisStudy),
    ) %>% 
  st_transform(myCRS)

df <- records_centroids2 %>% 
  st_coordinates() %>% 
  cbind(records_centroids2,.) %>% 
  dplyr::filter(!is.na(YEAR), !is.na(Y))

df %>% 
  ggplot()+
  aes(y=Y,x=YEAR,color=seasongroup, shape = thisStudy)+
  geom_point() +
  facet_wrap(~seasongroup) +
  stat_smooth(method="lm")

df  %>% 
  dplyr::filter(!is.na(YEAR), !is.na(Y)) %>% 
  dplyr::filter(seasongroup == "Winter") %>% 
  dplyr::mutate(decade20 = cut(YEAR, seq(1800,2020,by=20))) %>% 
  ggplot()+
  aes(Y)+
  geom_histogram()  +
  geom_boxplot(aes(y = 25 ), outlier.shape = NA, color = "green") +
  facet_grid(rows = vars(decade20)) +
  theme_minimal()

df %>% 
  dplyr::filter(Y > 0) %>% 
  dplyr::summarise(min_year = min(YEAR, na.rm = T))
df %>% 
  dplyr::filter(YEAR == 1900)


# library(glmmTMB)
# m1<-glmmTMB::glmmTMB( Y ~ YEAR*seasongroup, data = df)
# summary(m1)
# 
# sjPlot::plot_model(m1)
# sjPlot::plot_model(m1,type="pred")
# 
# df_pred <- expand.grid(YEAR = 1980:2020, seasongroup = unique(df$seasongroup))
# df_pred$fit <- predict(m1, df_pred, allow.new.levels=TRUE)
# df_pred %>%
#   ggplot() +
#   geom_path(aes(x=YEAR,y=fit,color=seasongroup))



## Make an ebird-style map ------

yrround <- st_intersection(ranges_sf[ranges_sf$seasongroup == "Summer", ],ranges_sf[ranges_sf$seasongroup == "Winter", ])
summerOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Summer", ],ranges_sf[ranges_sf$seasongroup == "Winter", ])
winterOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Winter", ],ranges_sf[ranges_sf$seasongroup == "Summer", ])
autumnOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Fall", ],ranges_sf[ranges_sf$seasongroup == "Summer", ])

rangemap <- bind_rows(yrround, summerOnly, winterOnly, autumnOnly) %>% 
  st_transform(myCRS)
rangemap$seasonName <- c("Year-round", "Summer", "Winter", "Autumn")

# Remove lakes.
w2 <-st_transform(waterbodies, myCRS) %>% 
  st_union()
rangemap <- st_difference(rangemap,w2)


## How different are range maps in size -----

plot(autumnOnly)
st_area(autumnOnly)  %>% measurements::conv_unit(from = "m2", to = "km2")

## Plot range maps -----

library(scales)
fillvals <- c("Year-round" = "#9e9cd0","Summer" = "#f19d79", "Winter" = "#8dc0e3", "Autumn" = "#f5e671")
p_seasonmap <- ggplot() +
  geom_sf(rangemap,  mapping = aes(fill = seasonName), color = NA , alpha = 0.8) +
  #geom_sf(records_centroids2,mapping=aes(fill=seasonName), shape=21, size = 0.7,color="grey40", linewidth=0.1) +
  scale_fill_manual( "Season", values=fillvals,breaks=names(fillvals)) +
  #scale_color_manual("Season", values=scales::muted(fillvals),breaks=names(fillvals)) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3)+
  theme_minimal()+
  # coord_sf(xlim=c(-105,-70),ylim=c(23,45)) +
  coord_sf(xlim=c(-7e5,19e5),ylim=c(-16.5e5,8e5))
ggsave(p_seasonmap, filename = file.path("figs", "season_ebirdMap.png"), dpi = 600)



# Plot turbines -----------------------------------------------------------

turbs <- list.files(file.path("data", "uswtdbSHP"), pattern = "shp", full.names = T) %>% 
  st_read()

seasonWithTurbs <- ggplot() +
  geom_sf(rangemap,  mapping = aes(fill = seasonName), color = NA , alpha = 0.8) +
  scale_fill_manual( "Season", values=fillvals,breaks=names(fillvals)) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3)+
  geom_sf(turbs, mapping = aes(), size = 0.01, color = "grey40") +
  theme_minimal()+
  coord_sf(xlim=c(-5e5,20e5),ylim=c(-16.5e5,7e5),  crs = sf::st_crs(rangemap)) +
  theme(legend.position = c(0.78, 0.45), legend.justification = c(0,1))
ggsave(seasonWithTurbs, filename = file.path("figs", "season_turbines.png"), dpi = 600)




# Plot range maps together ------------------------------------------------
## Reproduce ranges used by Perry.
perryranges <- dplyr::filter(records_centroids, source == "Perry 2018")
perryranges <- cbind(perryranges, st_coordinates(perryranges))

# Manually find counties based on relative location in state.
perryranges %>%  dplyr::filter(STATE.x == "Texas") %>% arrange(X)
perryranges %>%  dplyr::filter(STATE.x == "Oklahoma") %>% arrange(X)
perryranges %>%  dplyr::filter(STATE.x == "Kentucky") %>% arrange(desc(Y))

centroids_perry_core <- perryranges %>% 
  dplyr::mutate(
    core = case_when(
      STATE.x %in% c( "Florida","Tennessee","Arkansas","Missouri","Louisiana", "Mississippi","South Carolina", "North Carolina", "Georgia", "Alabama") ~ "yes",
      STATE.x == "Texas" & NAME != "Val Verde" ~ "yes",
      STATE.x == "Oklahoma" & NAME %in% c("McCurtain", "Cherokee") ~ "yes",
      STATE.x == "Kentucky" & NAME != "Fayette" ~ "yes",
      TRUE ~ "no" )
  )

perry_range <- centroids_perry_core %>% 
  dplyr::filter(core == "yes") %>% 
  st_coordinates() %>% 
  rangeBuilder::getDynamicAlphaHull(
    x = ., buff = 60e3, fraction = 0.95, partCount = 1, initialAlpha = 10,
    clipToCoast = "terrestrial", proj = "+proj=longlat +datum=WGS84",verbose = T) %>% 
  .[[1]] %>% 
  st_as_sf()  %>% 
  st_transform(sf::st_crs(rangemap))


ggplot() +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.3) +
  geom_sf(data = perry_range, mapping = aes(), fill = "grey50", alpha = 0.8) +
  geom_sf(data = centroids_perry_core, mapping = aes(color = core)) +
  theme_minimal()+
  coord_sf(xlim=c(-7e5,19e5),ylim=c(-16.5e5,8e5), crs = sf::st_crs(rangemap))


ranges_lasiurus <- list.files(file.path("data", "redlist_species_data_3a5d81e3-bc99-43ba-9513-6ad2220d170d"), pattern = "shp", full.names = T) %>% 
  st_read() %>% 
  st_transform(sf::st_crs(rangemap))

LASE_range <- ranges_lasiurus %>% 
  dplyr::filter(SCI_NAME == "Lasiurus seminolus")

LABO_range <- ranges_lasiurus %>% 
  dplyr::filter(SCI_NAME == "Lasiurus borealis", LEGEND != "Extinct")

perry1 <- st_read(file.path("shapefiles", "perry2018.shp")) %>% 
  st_transform(sf::st_crs(rangemap)) %>% 
  dplyr::mutate(range_source = "perry") %>% 
  st_make_valid %>% 
  st_intersection(., LABO_range) %>% 
  dplyr::select(range_source)

combo_ranges <- dplyr::mutate(LASE_range, range_source = "IUCN") %>% 
  dplyr::select(range_source) %>% 
  rbind(.,perry1)

library(ggnewscale)

# perryCol <- "#8572C9"
# ournewCol <- "#ee9713"
# IUCNLASECol <- "grey10"

ournewCol <- "#0DABD2"
perryCol <- "#8D6A25"
perryCol2 <- "#C29A4B"
IUCNLASECol <- "grey10"


ranges_together <- 
  ggplot() +
  geom_sf(LABO_range,mapping=aes(fill = SCI_NAME), linewidth=0.5, color = NA) +
  scale_fill_manual("", values = "grey92",breaks = "Lasiurus borealis",labels = "IUCN L. borealis") +
  ggnewscale::new_scale_fill() + 
  
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3) +
  
  geom_sf(records_county,  mapping = aes(fill = source, shape = source), color = NA) + 
  
  geom_sf(combo_ranges, mapping = aes(color = range_source, linewidth = range_source), fill = NA, linewidth = 0.5) +
  scale_color_manual("Range source", values = c("IUCN" = IUCNLASECol, "perry" = perryCol), labels = c("IUCN L. seminolus", "Perry 2018")) +
  scale_linewidth(breaks = c(0.3, 0.5)) +
  
  # geom_sf(LASE_range,mapping=aes(), fill=NA, color = "#E952DE", linewidth = 0.6) +
  # geom_sf(perry_range,mapping=aes(), color = "#101399", fill = NA,linewidth=0.5) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2) +
  # geom_sf(records_centroids, mapping = aes(fill = source, shape = source), color = "grey30") +
  scale_fill_manual( "Record source", values = c("Perry 2018" = perryCol2, "WEST" = ournewCol), labels = c("Perry 2018", "This study")) +
  scale_shape_manual("Record source", values = c("Perry 2018" = 21, "WEST" = 22), labels = c("Perry 2018", "This study")) +
  theme_minimal()+
  coord_sf(xlim=c(-5e5,20e5),ylim=c(-16.5e5,7e5),  crs = sf::st_crs(rangemap)) +
  theme(legend.position = c(0.78, 0.45), legend.justification = c(0,1))
ggsave(ranges_together, filename = file.path("figs", "ranges_together.png"), dpi = 600, width = 7, height = 6)



### New version
rbind(LABO_range, )


combo_ranges2 <- dplyr::mutate(LASE_range, range_source = "IUCN") %>% 
  dplyr::select(range_source) %>% 
  rbind(.,perry1) %>% 
  rbind(., LABO_range)







# # Combine -----------------------------------------------------------------
# 
# library(patchwork)
# p_together <- ranges_together + seasonWithTurbs+ plot_annotation(tag_levels = 'A')
# ggsave(p_together, filename = file.path("figs", "ranges_turbs_combo.png"), dpi = 800, width = 10, height = 5)
# 



