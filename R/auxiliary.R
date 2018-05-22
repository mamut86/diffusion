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


getse <- function(x, fit, l, cumulative) {
  # calculate squared error
  
  if (cumulative == FALSE) {
    if (l == -1) {
      # se <- x - fit[, 2]
      se <- log(x) - log(fit[, 2])
      se <- sum(se[se>0]) + sum(-se[se<0])
    } else if (l == 1){
      se <- sum(abs(log(x)-log(fit[, 2])))
      # se <- sum(abs(x - fit[, 2]))
    } else if (l == 2){
      # se <- sum((x - fit[, 2])^2)
      se <- sum((log(x) - log(fit[, 2]))^2)
    } else {
      # se <- sum(abs(x - fit[, 2])^l)
      se <- sum(abs(log(x) - log(fit[, 2]))^l)
    }
  } else {
    if (l == -1) {
      se <- cumsum(x) - fit[, 1]
      # se <- log(cumsum(x)) - log(fit[, 1])
      se <- sum(se[se>0]) + sum(-se[se<0])
    } else if (l == 1) {
      # se <- sum(abs(log(cumsum(x)) - log(fit[, 1])))
      se <- sum(abs(cumsum(x) - fit[, 1]))
    } else if (l == 2) {
      # se <- sum((log(cumsum(x)) - log(fit[, 1]))^2)
      se <- sum((cumsum(x) - fit[, 1])^2)
    } else {
      # se <- sum(abs(log(cumsum(x)) - log(fit[, 1]))^l)
      se <- sum(abs(cumsum(x) - fit[, 1])^l)
    }
  }
  
  return(se)
}