# get data from UKHLS 
# 
# assumes you want hh and ind data from resp files



get_us_main_data <- function(folder, varsi, varh){
  
  file <- list.files(folder, 
                     pattern = ".dta")
  
  # import data
  ind_data <- read_dta(str_subset(
    str_c(folder, file),
    "indresp"))
  
  hh_data <- read_dta(str_subset(
    str_c(folder, file),
    "hhresp"))
  
  
  # remove prefixes
  
  ind_data <- ind_data %>% 
    rename_all(funs(str_remove_all(., "^[a-z]\\_") %>% 
                      str_replace(., "^scsf([2-9a-z]{1,2})$",
                                  "sf\\1")))
  
  hh_data <- hh_data %>% 
    rename_all(funs(str_remove_all(., "^[a-z]\\_")))
  
  
  
  # check if all variables are present
  
  stopifnot(mean(varsi %in% names(ind_data)) == 1)
  stopifnot(mean(varsh %in% names(hh_data)) == 1)
  
  
  # select variables and merge data
  
  ind_data <- ind_data %>% 
    dplyr::select(varsi)
  
  data <- hh_data %>% 
    dplyr::select(varsh) %>% 
    right_join(ind_data) %>% 
    rename_all(funs(str_remove(. , "\\_dv$")))
  
  data
  
}
