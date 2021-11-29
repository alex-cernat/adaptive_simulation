# Simulating the consequences of adaptive survey design in two household panel studies

This is the code associated with the paper: Watson, N. & Cernat, A. (2021). Simulating the consequences of adaptive survey design in two household panel studies. *Journal of Survey Statistics and Methodology*.

The data cleaning and analysis for the Understanding Society data was done using `R` (see session info bellow). The data cleaning and analysis of the HILDA data was done using Stata. All the graphs were done in `R`.

For `R` the following syntax produce the cleaning, analysis and results:
 - "01.Maste.R" - does the data cleaning
 - "02.adaptive_regression.R" - does regressions for estimating R indicators
 - "03.r_indicators.R" - explores R indicator results
 - "04.r_indicators_random.R" - creates R indicators for random targeting
 - "05.graphs.R" - does the graphs for the paper


```
R version 3.6.1 (2019-07-05)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19041)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ModelGood_1.0.9 pROC_1.16.2     devtools_2.3.1  usethis_1.6.1   lubridate_1.7.9
 [6] haven_2.3.1     forcats_0.5.0   stringr_1.4.0   dplyr_0.8.3     purrr_0.3.3    
[11] readr_1.3.1     tidyr_1.1.2     tibble_3.0.3    ggplot2_3.3.2   tidyverse_1.3.0

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3        prettyunits_1.1.1 ps_1.3.4          assertthat_0.2.1 
 [5] rprojroot_1.3-2   digest_0.6.23     packrat_0.5.0     R6_2.4.1         
 [9] cellranger_1.1.0  plyr_1.8.6        backports_1.1.5   reprex_0.3.0     
[13] httr_1.4.2        pillar_1.4.6      rlang_0.4.7       readxl_1.3.1     
[17] rstudioapi_0.11   callr_3.4.4       blob_1.2.1        desc_1.2.0       
[21] munsell_0.5.0     broom_0.7.0       compiler_3.6.1    modelr_0.1.8     
[25] pkgconfig_2.0.3   pkgbuild_1.1.0    tidyselect_1.1.0  fansi_0.4.0      
[29] crayon_1.3.4      dbplyr_1.4.4      withr_2.2.0       grid_3.6.1       
[33] jsonlite_1.6      gtable_0.3.0      lifecycle_0.2.0   DBI_1.1.0        
[37] magrittr_1.5      scales_1.1.1      cli_2.0.2         stringi_1.4.3    
[41] fs_1.5.0          remotes_2.2.0     testthat_2.3.2    xml2_1.3.2       
[45] ellipsis_0.3.1    generics_0.0.2    vctrs_0.3.4       tools_3.6.1      
[49] glue_1.3.1        hms_0.5.3         processx_3.4.4    pkgload_1.1.0    
[53] colorspace_1.4-1  sessioninfo_1.1.1 rvest_0.3.6       memoise_1.1.0 
```

