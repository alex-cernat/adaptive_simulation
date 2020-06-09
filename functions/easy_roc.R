# function to calculate AUC
easy_roc <- function(object) {
  x <- ModelGood::Roc(object,
                      formula = object$formula,
                      data = object$data)
  
  unlist(x$Auc)
}
