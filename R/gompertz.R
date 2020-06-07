# Gompertz function -------------------------------------------------------
#
# References
# Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares fitting Gompertz
# curve. Journal of Computational and Applied Mathematics, 169, 359-375.
#
# author Oliver Schaer, info@oliverschaer.ch
# author Nikolaos Kourentzes, nikolaos@kourentzes.com

gompertzCurve <- function(n, w){
  # Generate Gompertz curve
  # n, sample size
  # w, vector of parameters
  
  t <- 1:n
  # Cumulative
  # At <- w[3] * exp(-w[1] * (exp(-w[2] * t)))
  At <- w[1] * exp(-w[2] * (exp(-w[3] * t)))
  
  # Adoption
  at <- diff(c(0, At))
  x <- cbind(At, at)
  colnames(x) <- c("Cumulative Adoption", "Adoption")
  
  return(x)
}

gompertzInit <- function(y, loss, method, multisol, initpar, mscal){
  # Internal function: get initial values
  # get approximation of initial values using Jukic et al. 2004 approach adopted
  # m to allow for y to be adoption per period
  
  # make sure leading 0s are removed
  y <- cleanzero(y)$x
  
  n <- length(y)
  Y <- cumsum(y)
  
  # get largest distance between t1 and t3 possible, t2 = (t1 + t3)/2
  t0 <- c(1, floor((1 + n)/2), n)
  x0 <- Y[t0]
  
  # make sure that all values are slightly different
  if(anyDuplicated(x0) > 0){
    x0[anyDuplicated(x0)] <- x0[anyDuplicated(x0)]+(x0[anyDuplicated(x0)]*0.00001)
  }
  # m <- (x0[1]) - ((((x0[2]) - (x0[1]))^2)/((x0[3]) - (2 * (x0[2])) + (x0[1])))
  # m <- exp(log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))))
  
  # calling bass estimates
  what <- diffusionEstim(y, loss, pvalreps = 0, type = "bass", method = method,
                         multisol = multisol, initpar = initpar, mscal = mscal)$w
  
  m <- what[1]
  
  a <- ((-(log(x0[2]) - log(x0[1]))^2)/(log(x0[3]) - (2 * log(x0[2])) + 
                                          log(x0[1]))) * ((log(x0[2]) - log(x0[1]))/(log(x0[3]) -
                                                                                       log(x0[2])))^(2 * t0[1]/(t0[3] - t0[1]))
  
  b <- (-2/(t0[3] - t0[1])) * log((log(x0[3]) -
                                     log(x0[2]))/(log(x0[2]) - log(x0[1])))
  w <- c(m, a, b)
  names(w) <- c("m", "a", "b")
  
  #   a <- log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1])))
  #   
  #   b <- ((-(log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))) *
  #     ((log(x0[2]) - log(x0[1])) / (log(x0[3]) - log(x0[2])))^(2*t0[1] / (t0[3]-t0[1]))
  
  return(w)
}
