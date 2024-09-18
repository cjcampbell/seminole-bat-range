
# Load packages.
if(packageVersion("isocat") < "0.2.4.9000") warning("Install development version of isocat.")
# devtools::install_github("cjcampbell/isocat")

# Load libraries required for analyses.
library(isocat)
library(dplyr)
library(sf)

# Load libraries relied on for a few analyses.
library(geosphere)
library(ggplot2)
library(raster)
library(readxl)
library(stringr)
library(tidyr)

# Make an object to help navigate the subdirectories.
# Be sure to fill this in if you're reproducing these analyses!
my_dir_path <- "/Users/cjcampbell/seminole-bat-range"
if(!exists("wd")) { wd <- list() }
wd$R       <- file.path( my_dir_path, "R" )
wd$bin     <- file.path( my_dir_path, "bin" )
wd$data    <- file.path( my_dir_path, "data" )
wd$knownOrigin <- file.path(wd$data, "knownOriginDat")
wd$figs    <- file.path( my_dir_path, "figs" )
wd$out    <- file.path( my_dir_path, "out" )

# Check for presence of subdirectories. Create if needed.
invisible({
  lapply(wd, function(i) if( dir.exists(i) != 1 ) dir.create(i) )
})


# Define extent of spatial analysis.
# Units == meters
my_extent_aea <- raster::extent(
  -16e5, 34e5,
  -30e5, 16e5
)

# CRS for aea projection:
myCRS <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
