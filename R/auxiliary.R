# Some internal auxiliary functions

cleanzero <- function(x){
  # Internal function: remove leadig zeros
  # x, vector of values
  
  idx <- which(x == 0)
  n <- length(x)
  l <- length(idx)
  
  if (l>0 & idx[1]==1){
    d.idx <- diff(idx)
    loc <- which(d.idx > 1)[1]
    if (is.na(loc)){
      loc <- l
    }
    x <- x[(loc+1):n]
  } else {
    loc <- 1
  }
  
  return(list("x" = x, "loc" = loc))
}