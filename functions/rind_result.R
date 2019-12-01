rind_result <- function(robject){
  
  out <- list(NULL)
  
  out[[1]] <- tbl_df(cbind(Indicator = c("R", "CV"),
                      Value = c(robject$R,
                               robject$CV),
                      ValueSE = as.numeric(c(robject$RSE,
                               robject$CVSE))))
  
  out[[1]]$Value <- round(as.numeric(out[[1]]$Value), 4)
  
  names(out)[1] <- "R and CV"
  
  if("partialR" %in% names(robject) == T){
    select <- c("variable", "Pu", "PuSE", "Pc", "PcSEApprox")
    out[[2]] <- robject$partialR$byVariables[select]
    out[[2]][, 2:5] <- round(out[[2]][, 2:5], 4)
    
    
    out[[3]] <- robject$partialR$byCategories
    
    for(i in 1:length(out[[3]])) {
      out[[3]][[i]][, 2:5] <- round(out[[3]][[i]][, 2:5], 4)
    }
  
    names(out)[2:3] <- c("R variable lvl.", "R category lvl.")  
      
  }

  out
}
