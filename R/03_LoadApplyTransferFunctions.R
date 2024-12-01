# Load  ----------------------------------------------------------

if(!exists("mydata")) { source(file.path(wd$R, "01_loadIsotopedata.R")) }
transferFunctionData_Campbelletal <- 
    readRDS(
      file.path(wd$data, "transferFunctions",
                "transferFunctionData_Campbelletal.rds") )

# Apply transfer functions, define molt status  ---------------------------------

mydata_transformed <- mydata %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    dDprecip =  (d2H - transferFunctionData_Campbelletal$mean_intercept) / transferFunctionData_Campbelletal$mean_slope,
    sdResid = transferFunctionData_Campbelletal$mean_sdRes
  )

saveRDS(mydata_transformed, file = file.path(wd$bin, "mydata_transformed.rds"))

