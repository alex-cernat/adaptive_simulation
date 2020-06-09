
# function to move prefix to suffix
move_lab <- function(x) {
  index <-  str_extract(x, "^[a-z]")
  prefix <- which(letters %in% {{ index}} )
  
  str_remove(x, str_c(index, "_")) %>% str_c("_", prefix) %>% unlist()
}