
# function to make factor using labels drom dplyr

relabel <- function(x) {
  labs <- attr(x, "labels")
  
  # code missing if they have no labels
  x[!x %in% labs[!is.na(labs)]] <- NA
  
  # delete empty categories
  codes_available <- table(x) %>% names() %>% as.numeric() 
  codes_all <- labs %>% as.numeric()
  labs <- labs[codes_all %in% codes_available]
  
  # make factor
  y <- factor(x, labels = names(labs[!is.na(labs)]))
  
  y
}

