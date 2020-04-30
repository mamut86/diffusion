# Some internal auxiliary functions

cleanzero <- function(x) {
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


cleanna <- function(x, silent = c(FALSE, TRUE)) {
  
  # internal function to remove NA values. Stops if NA is within time series.
  # # x, vector of values
  # verbose, when TRUE warnings when removing leading and trailing NAs
  
  silent <- silent[1]
  
  # state for error message
  tl <- ld <- FALSE
  locld <- 1  # location of lead trim
  loctl <- length(x) # length trail trim
  
  # check if any NA
  if (any(is.na(x))) {
    loc <- which(is.na(x))
  } else {
    loc <- NULL
  }
  
  # check i
  if (any(loc > 0)) {
    
    # remove leading NA
    if (loc[1] == 1) {
      
      idx <- diff(loc)
      if (any(idx > 1)) {
        rem <- which(idx > 1)[1]
        x <- x[-(1:rem)]
      } else {
        rem <- length(idx) + 1
        x <- x[-(1:rem)]
      }
      ld <- TRUE
      locld <- length(rem)
    }
    
    # remove trailing NA
    if (loc[length(loc)] == length(x)) {
      
      idx <- diff(rev(loc))
      if (any(idx < -1)) {
        rem <- which(idx < -1)[1]
        x <- head(x, -rem)
      } else {
        rem <- length(idx) + 1
        x <- head(x, -rem)
      }
      tl <- TRUE
      loctl <- length(rem)
    }
    
    # abort  remove remaining NA
    if (any(is.na(x))) {
      stop("Cannot estimate model as NA values present within time series.")
    }
  }
  
  # Warning message if needed
  if (silent == T) {
    if (tl == T && ld == T) {
      message("Removed leading and trailing NA values to fit model")
    }
    
    if (tl == T && ld == F) {
      message("Removed trailing NA values to fit model")
    }
    
    if (tl == F && ld == T) {
      message("Removed leading NA values to fit model")
    }
  }
  
  return(list("x" = x, "loc" = loc, "locLead" = locld, "locTrail" = loctl))
  
}


getse <- function(x, fit, l, cumulative) {
  # calculate squared error
  
  if (cumulative == FALSE) {
    if (l == -1) {
      se <- x - fit[, 2]
      # se <- log(x) - log(fit[, 2])
      se <- sum(se[se>0]) + sum(-se[se<0])
    } else if (l == 1){
      # se <- sum(abs(log(x)-log(fit[, 2])))
      se <- sum(abs(x - fit[, 2]))
    } else if (l == 2){
      se <- sum((x - fit[, 2])^2)
      # se <- sum((log(x) - log(fit[, 2]))^2)
    } else {
      se <- sum(abs(x - fit[, 2])^l)
      # se <- sum(abs(log(x) - log(fit[, 2]))^l)
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