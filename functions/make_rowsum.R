# function to make rowsum
make_rowsum <- function(data, name_radical, value = FALSE) {
  if (isFALSE(value)) {
    data %>%
      select(matches(name_radical)) %>%
      mutate(x = rowSums(.)) %>%
      .[["x"]]
  } else {
    data %>%
      select(matches(name_radical)) %>%
      mutate_all( ~ ifelse(. == {{ value }}, 1, 0)) %>%
      mutate(x = rowSums(.)) %>%
      .[["x"]]
    
  }
  
}