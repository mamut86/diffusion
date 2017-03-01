# Gompertz model ----------------------------------------------------------

Gompertz <- function(x, startval = NULL){
  
  # Inputs
  # x                       vector of cumulative demand data
  # startval                Vector with start values needed for manual estimation.
  #                         Vector values need to be named in the form of
  #                         (a, b, c). Else automated approximation of NLS is
  #                         is taken by using Jukic et al. 2004 approach.
  
  # some error handling
  startvalNames <- c("a","b", "c")
  parnum <- 3
  
  # Error handling
  if(!is.null(startval)){
    if(length(startval) != parnum){
      stop("Lenght of startvalues not correct")
    }
    
    if(all(names(startval), startvalNames)){
      stop("Startvalues vector need to be named in (a, b, c) form")
      }
  }
  
  if(any(diff(x) < 0)){
    stop("x needs to be in strictly positive cumulative form (monotonic)")
  }
  
  # get starting values if needed
  if(is.null(startval)){
    startval <- StartvalGen(x)
  }
  
  # estimate curve
  param <- EstimGomp(x, startval)
  
  return(list("param" = param))
  
}


EstimGomp <- function(x, startval){
  
  t0 <- 1:length(x)
  fitGomp <- nls(x ~ exp(a - b*exp(-c*t0)), start = startval)

  return(coef(fitGomp))
}

StartvalGen <- function(x){
  # get approximation of initial values using Jukic et al. 2004 approach
  
  # get largest distance between t1 and t3 possible, t2 = (t1 + t3)/2
  t0 <- c(1, floor((1+length(x))/2), length(x))
  x0 <- x[t0]
  
  a <- log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1])))
  
  b <- ((-(log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))) *
    ((log(x0[2]) - log(x0[1])) / (log(x0[3]) - log(x0[2])))^(2*t0[1] / (t0[3]-t0[1]))
  
  c <- (-2 / (t0[3] - t0[1])) * log((log(x0[3]) - log(x0[2])) / (log(x0[2]) - log(x0[1])))
  
  startval <- c(a=a, b=b, c=c)
  
  return(startval)
}



