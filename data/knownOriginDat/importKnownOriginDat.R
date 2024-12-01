# Note: this file shows the filters used to subset data from Campbell et al. 2024
# The output, but not the input file will be included on github.
source("R/00_Setup.R")
read.csv("/Users/cjcampbell/BatMigratoryDistDir/data/alldat.csv") %>% 
  dplyr::filter(Species == "LABO", yDay >= 165, yDay <= 215, wind_killed == "no") %>% 
  dplyr::select(ID, Species, decimalLatitude, decimalLongitude,uncertainty,Day,Month,Year,yDay,d2H) %>% 
  write.csv(file=file.path(wd$knownOrigin, "knownOrigindata.csv"), row.names = F)
