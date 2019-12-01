

relabel2 <- function(x) {

  attributes(x)
    labs <- attr(x, "labels")

y <- cbind(unname(labs), names(labs)) %>%
    tbl_df() 
  
  names(y) <- c("V1", "V2")
  
  y <- data.frame(x = x) %>%
    tbl_df() %>%
    mutate(V1 = as.character(x)) %>%
    left_join(y, by = "V1") %>%
    dplyr::select(V2)
  
   as.factor(unname(unlist(y)))
  
}

