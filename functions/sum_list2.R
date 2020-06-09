# function to make descriptives based on the list of data
# uses sum_tab initially

sum_list2 <- function(data, prefix = T) {
  # get summuries for all variables
  desc_list <- map(data, sum_tab)
  
  # select only continious and valid summuries
  cont_list <- map(desc_list, function(x)
    x$`Continuous variables`)
  cont_list <- cont_list[!map_lgl(cont_list, is.null)]
  
  # select only categorical and valid summuries
  cat_list <- map(desc_list, function(x)
    x$`Categorical variables`)
  cat_list <- cat_list[!map_lgl(cat_list, is.null)]
  
  # reshape from long to wide if they have prefixes
  if (prefix) {
    cat_desc_data <- reduce(cat_list, rbind) %>%
      mutate(Wave = str_extract(Variable, "[0-9]$"),
             Variable = str_remove(Variable, "_[0-9]$")) %>% 
      pivot_wider(names_from = Wave, 
                  values_from = c("Freq.", "Perc.")) 
    
    cont_desc_data <- reduce(cont_list, rbind) %>%
      mutate(Wave = str_extract(Variable, "[0-9]$"),
             Variable = str_remove(Variable, "_[0-9]$")) %>%
      filter(!is.na(Wave)) %>% 
      pivot_wider(names_from = Wave, 
                  values_from = c("Mean", "SD", "Miss"))
  } else {
  
  
  # descriptives continious
  cont_desc_data <- cont_list %>%
    reduce(full_join, by = "Variable") %>%
    set_names(c("Variable",
                str_c(
                  c("Mean", "SD", "Miss"),
                  "_",
                  rep(names(cont_list), each = 3)
                )))
  
  # descriptives categorical
  cat_desc_data <- cat_list %>%
    reduce(full_join, by = c("Variable", "Code")) %>%
    set_names(c("Variable", "Code",
                str_c(
                  c("Freq", "Perc"),
                  "_",
                  rep(names(cat_list), each = 2)
                )))
  }
  
  output <- list(Continious = cont_desc_data,
                 Categorical = cat_desc_data)
  
  output
  
  
}









