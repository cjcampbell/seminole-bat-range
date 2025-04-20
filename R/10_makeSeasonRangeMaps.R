library(tidyverse)
library(sf)

# Load key with county IDs.
FIPS <- readxl::read_xlsx("data/LASE_records/countyRecords/County FIDs.xlsx") %>% 
  dplyr::mutate(
    FIPS=as.numeric(FIPS)
  ) %>% 
  dplyr::select(FIPS, STATE, COUNTY) %>% 
  distinct()

# Load county shapefiles.
county <- read_sf("data/LASE_records/shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp") %>% 
  dplyr::mutate(
    STATEFP = as.numeric(STATEFP),
    COUNTYFP = as.numeric(COUNTYFP),
    GEOID = as.numeric(GEOID)
  ) %>% 
  st_as_sf() %>% 
  st_transform("+proj=longlat +datum=WGS84")

# Load LASE records.
## From Perry 2018
records <- readxl::read_xls("data/LASE_records/countyRecords/SEMINOLE DATA FROM COLLECTIONS.xls") %>% 
  dplyr::mutate(
    FIPS=as.numeric(FIPS),
    source = "Perry 2018"
  ) 

## From this study.
source("~/LASE_dD/R/00_Setup.R")
mydata <- lapply(1:4, function(x){
  readxl::read_excel(file.path("data", "All_SEBA_data_FINAL_For_Submission.xlsx"), sheet = x) %>% 
    dplyr::mutate(
      DateFound = as.character(DateFound),
      source = "WEST") %>% 
    dplyr::rename(
      Lon = County_Centroid_Lon,
      Lat = County_Centroid_Lat)
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

records_centroids <- st_centroid(records_county)

# Load gadm data.
# source("R/R_setup/locationSpatialData.R")
if(!exists("locationGADMData")) stop("object `locationGADMData` must be specified")

usaDat <- geodata::gadm(country="USA", path= locationGADMData, level=1) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance=1000)

usa_centroids <- usaDat %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame()

usaDat_east <- usaDat[usa_centroids$X>-110,]

MEXDat <- geodata::gadm(country="MEX", path= locationGADMData, level=1) %>% 
  st_as_sf() %>% 
  st_simplify(dTolerance=1000)
CANDat <- geodata::gadm(country="CAN", path= locationGADMData, level=1) %>% 
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
    clipToCoast = "terrestrial", verbose = T)
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


# testing of whether bats are observed at higher latitude given year --------

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




## Make an ebird-style map ------
ranges_sf <- ranges_sf %>% st_make_valid()
yrround <- st_intersection(ranges_sf[ranges_sf$seasongroup == "Summer", ],ranges_sf[ranges_sf$seasongroup == "Winter", ])
summerOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Summer", ],ranges_sf[ranges_sf$seasongroup == "Winter", ])
winterOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Winter", ],ranges_sf[ranges_sf$seasongroup == "Summer", ])
autumnOnly <- st_difference(ranges_sf[ranges_sf$seasongroup == "Fall", ],ranges_sf[ranges_sf$seasongroup == "Summer", ])

rangemap <- bind_rows(yrround, summerOnly, winterOnly, autumnOnly) %>% 
  st_transform(myCRS) %>% 
  st_make_valid()
rangemap$seasonName <- c("Year-round", "Summer", "Winter", "Autumn")

# Remove lakes.
w2 <-st_transform(waterbodies, myCRS) %>% 
  st_union()
rangemap <- st_difference(rangemap,w2)


## How different are range maps in size -----

plot(autumnOnly)
set_units(st_area(yrround), km^2)
set_units(st_area(autumnOnly), km^2)
set_units(st_area(winterOnly), km^2)

# Vanishingly small. Revise to remove from plot (for clarity).

rangemap_noWinter <- dplyr::filter(rangemap, SeasonName != "Winter")



## Plot range maps -----

library(scales)
fillvals <- c("Year-round" = "#9e9cd0","Summer" = "#f19d79", "Winter" = "#8dc0e3", "Autumn" = "#f5e671")
p_seasonmap <- ggplot() +
  geom_sf(rangemap_noWinter,  mapping = aes(fill = seasonName), color = NA , alpha = 0.8) +
  #geom_sf(records_centroids2,mapping=aes(fill=seasonName), shape=21, size = 0.7,color="grey40", linewidth=0.1) +
  scale_fill_manual( "Season", values=fillvals, breaks=names(fillvals)) +
  #scale_color_manual("Season", values=scales::muted(fillvals),breaks=names(fillvals)) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3)+
  theme_minimal()+
  # coord_sf(xlim=c(-105,-70),ylim=c(23,45)) +
  coord_sf(xlim=c(-7e5,19e5),ylim=c(-16.5e5,8e5))
ggsave(p_seasonmap, filename = file.path("figs", "season_ebirdMap.png"), dpi = 600)



# Plot map with turbines -----------------------------------------------------------

turbs2 <- list.files(file.path("data","turbineLocations", "uswtdbSHP"), pattern = "shp", full.names = T) %>% 
  st_read()

seasonWithTurbs <- ggplot() +
  geom_sf(rangemap,  mapping = aes(fill = seasonName), color = NA , alpha = 0.8) +
  scale_fill_manual( "Season", values=fillvals,breaks=names(fillvals)) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2)+
  geom_sf(NoAm,mapping=aes(),fill=NA,linewidth=0.3)+
  geom_sf(turbs2, mapping = aes(), size = 0.01, color = "grey30") +
  theme_minimal()+
  coord_sf(xlim=c(-5e5,20e5),ylim=c(-16.5e5,7e5),  crs = sf::st_crs(rangemap)) +
  theme(legend.position = c(0.78, 0.45), legend.justification = c(0,1))
ggsave(seasonWithTurbs, filename = file.path("figs", "season_turbines_updated.png"), dpi = 1000)

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
    clipToCoast = "terrestrial", verbose = T) %>% 
  .[[1]] %>% 
  st_as_sf()  %>% 
  st_transform(sf::st_crs(rangemap))

# Plot perry points and ranges.
ggplot() +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.3) +
  geom_sf(data = perry_range, mapping = aes(), fill = "grey50", alpha = 0.8) +
  geom_sf(data = centroids_perry_core, mapping = aes(color = core)) +
  theme_minimal()+
  coord_sf(xlim=c(-7e5,19e5),ylim=c(-16.5e5,8e5), crs = sf::st_crs(rangemap))

## Generate our own map.----

# source("R/R_setup/locationSpatialData.R")
if(!exists("locationIUCNData")) stop("object `locationIUCNData` must be specified")

ranges_lasiurus <- list.files(file.path(locationIUCNData, "redlist_species_data_d9b37aa6-89b6-4fe4-9ebc-8567d9a63a8c"), pattern = "shp", full.names = T) %>% 
  st_read() %>% 
  st_transform(sf::st_crs(rangemap))

LASE_range <- ranges_lasiurus %>% 
  dplyr::filter(BINOMIAL == "Lasiurus seminolus")

LABO_range <- ranges_lasiurus %>% 
  dplyr::filter(BINOMIAL == "Lasiurus borealis", LEGEND != "Extinct")

perry1 <- st_read(file.path("data/LASE_records","shapefiles", "perry2018.shp")) %>% 
  st_transform(sf::st_crs(rangemap)) %>% 
  dplyr::mutate(range_source = "perry") %>% 
  st_make_valid %>% 
  st_intersection(., LABO_range) %>% 
  dplyr::select(range_source)

combo_ranges <- dplyr::mutate(LASE_range, range_source = "IUCN") %>% 
  dplyr::select(range_source) %>% 
  rbind(.,perry1)

### Plot ----
library(ggnewscale)
library(ggtext)

ournewCol <- "#AB3777"
perryCol <- "#77579A"
perryCol2 <- "#546BAE"
IUCNLASECol <- "#52A675"
LABOcol1 <- "grey80" # Outline color
LABOcol2 <- "grey30" # Fill color

combo_ranges2 <- dplyr::mutate(LABO_range, range_source = "IUCN_LABO") %>% 
  dplyr::select(range_source) %>% 
  rbind(combo_ranges) %>% 
  mutate(range_source = factor(range_source, levels = c("perry", "IUCN", "IUCN_LABO")))

records_county <- dplyr::mutate(
  records_county,
  source = factor(source, levels = c("WEST", "Perry 2018"))
)

ranges_together2 <- ggplot() +
  geom_sf(gadm,mapping=aes(),fill="white",linewidth=0.2) +
  geom_sf(
    combo_ranges2, 
    mapping = aes(color = range_source, fill = range_source, linewidth = range_source),
    alpha = 0.08, linewidth = 0.8
  ) +
  scale_color_manual("Range source", values = c("perry" = perryCol, "IUCN" = IUCNLASECol, "IUCN_LABO" = LABOcol1), labels = c("Perry 2018", "IUCN <i>L. seminolus</i>", "IUCN <i>L. borealis</i>")) +
  scale_fill_manual( "Range source", values = c("perry" = perryCol, "IUCN" = IUCNLASECol, "IUCN_LABO" = LABOcol2), labels = c("Perry 2018", "IUCN <i>L. seminolus</i>", "IUCN <i>L. borealis</i>")) +
  ggnewscale::new_scale_fill() + 
  
  geom_sf(records_county,  mapping = aes(fill = source, shape = source), color = NA) + 
  scale_fill_manual( "Record source", values = c("WEST" = ournewCol, "Perry 2018" = perryCol2), labels = c("This study", "Perry 2018")) +
  scale_shape_manual("Record source", values = c("WEST" = 22, "Perry 2018" = 21),               labels = c("This study", "Perry 2018")) +
  
  geom_sf(waterbodies,mapping=aes(),fill=NA,linewidth=0.2) +
  geom_sf(gadm,mapping=aes(),fill=NA,linewidth=0.2) +
  
  geom_sf(
    dplyr::filter(combo_ranges2, range_source == "perry"),
    mapping = aes(), linewidth = 0.8, fill = NA, color = perryCol
  )  +
  geom_sf(
    dplyr::filter(combo_ranges2, range_source == "IUCN"),
    mapping = aes(), linewidth = 0.8, fill = NA, color = IUCNLASECol
  )  +
  
  theme_minimal()+
  coord_sf(xlim=c(-5e5,20e5),ylim=c(-16.5e5,7e5),  crs = sf::st_crs(rangemap)) +
  theme(
    legend.position = c(0.81, 0.45),
    legend.justification = c(0,1),
    legend.text = element_markdown()
  )

ggsave(ranges_together2, filename = file.path("figs", "fig_ranges_combined.png"), dpi = 1000, width = 7, height = 6)
