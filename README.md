# LASE_dD
Range delineation and stable hydrogen isotope analyses of <i>Lasiurus seminolus</i>.

### /R/
Scripts used to read, tidy, and standardize data and run analyses.
| Script                                 | Description |
| -----------                            | ----------- |
| 00_Setup.R                             | Set up workspace, define extents, CRS |
| 01_loadIsotopeData.R                   | Import data, combine with metadata, tidy |
| 02_LoadSpatialData.R                   | Load administrative area, create  isoscapes, and create range map datasets |
| 03_LoadApplyTransferFunctions.R        | Load and apply transfer functions to be used in theses analyses |
| 04_findKnownOriginModelPerformance.R   | Load testing dataset to apply quantile-simulation transformation to define "likely" origins |
| 05_MakeProbabilityOfOriginMaps.R       | Create probability-of-origin maps and quantile-simulation transformed maps for each sample |
| 06_MetricsConnectingPoOToSampleSite.R  | Estimate direction and minimum distance traveled for each individual |
| 07_PlotIndividualSurfaces.R            | Plot maps for each stable isotope sample. |
| 08_plot_aggregated_origins.R           | Plot aggregate origins and direction of travel for all samples. |
| 09_perry2018CountyRecords.R            | Extract records from Perry 2018. |
| 10_makeSeasonalRangeMaps.R             | Generate seasonal range maps based on currently available records.| 

## Session Info

R version 4.1.2 (2021-11-01)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tidyr_1.2.0      stringr_1.4.0    readxl_1.3.1     ggplot2_3.3.5   
 [5] geosphere_1.5-14 sf_1.0-6         dplyr_1.0.8      isocat_0.2.6    
 [9] raster_3.5-15    sp_1.4-6        

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8         cellranger_1.1.0   pillar_1.7.0      
 [4] compiler_4.1.2     iterators_1.0.14   class_7.3-20      
 [7] tools_4.1.2        gtable_0.3.0       lifecycle_1.0.1   
[10] tibble_3.1.6       lattice_0.20-45    pkgconfig_2.0.3   
[13] rlang_1.0.1        foreach_1.5.2      DBI_1.1.2         
[16] cli_3.2.0          parallel_4.1.2     terra_1.5-21      
[19] e1071_1.7-9        withr_2.4.3        generics_0.1.2    
[22] vctrs_0.3.8        classInt_0.4-3     grid_4.1.2        
[25] tidyselect_1.1.2   glue_1.6.1         R6_2.5.1          
[28] fansi_1.0.2        purrr_0.3.4        magrittr_2.0.2    
[31] scales_1.1.1       codetools_0.2-18   ellipsis_0.3.2    
[34] units_0.8-0        assertthat_0.2.1   colorspace_2.0-3  
[37] utf8_1.2.2         KernSmooth_2.23-20 stringi_1.7.6     
[40] proxy_0.4-26       munsell_0.5.0      crayon_1.5.0   
