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
  At <- w[3] * exp(-w[1] * exp(-w[2] * t))
  # At <- w[3] * exp(-exp(-(w[1] + (w[2] *t))))
  
  # Adoption
  at <- diff(c(0, At))
  Y <- cbind(At, at)
  colnames(Y) <- c("Cumulative Adoption", "Adoption")
  
  return(Y)
}

gompertzInit <- function(x, l, optim){
  # Internal function: get initial values
  # get approximation of initial values using Jukic et al. 2004 approach adopted
  # m to allow for x to be adoption per period
  
  n <- length(x)
  X <- cumsum(x)
  
  # get largest distance between t1 and t3 possible, t2 = (t1 + t3)/2
  t0 <- c(1, floor((1 + n)/2), n)
  x0 <- X[t0]
  
  # make sure that all values are slightly different
  if(anyDuplicated(x0) > 0){
    x0[anyDuplicated(x0)] <- x0[anyDuplicated(x0)]+(x0[anyDuplicated(x0)]*0.00001)
  }
  # m <- (x0[1]) - ((((x0[2]) - (x0[1]))^2)/((x0[3]) - (2 * (x0[2])) + (x0[1])))
  # m <- exp(log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))))
  
  # calling bass estimates
  what <- diffusionEstim(x, l, pvalreps = 0, type = "bass", optim = optim)$w
  m <- what[3]

  a <- ((-(log(x0[2]) - log(x0[1]))^2)/(log(x0[3]) - (2 * log(x0[2])) + 
        log(x0[1]))) * ((log(x0[2]) - log(x0[1]))/(log(x0[3]) -
        log(x0[2])))^(2 * t0[1]/(t0[3] - t0[1]))
  
  b <- (-2/(t0[3] - t0[1])) * log((log(x0[3]) -
                                     log(x0[2]))/(log(x0[2]) - log(x0[1])))
  w <- c(a, b, m)
  names(w) <- c("a", "b", "m")
  
#   a <- log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1])))
#   
#   b <- ((-(log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))) *
#     ((log(x0[2]) - log(x0[1])) / (log(x0[3]) - log(x0[2])))^(2*t0[1] / (t0[3]-t0[1]))

  return(w)
}

gompertzCost <- function(w, x, l, w.idx = rep(TRUE, 3), prew = NULL, cumulative=c(TRUE,FALSE)){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # x, adoption per period
  # l, the l-norm (1 is absolute errors, 2 is squared errors)
  # w.idx, logical vector with three elements. Use FALSE to not estimate
  # respective parameter
  # prew, the w of the previous generation - this is used for sequential fitting
  # cumulative, use cumulative adoption or not
  
  cumulative <- cumulative[1]
  n <- length(x)
  
  # If some elements of w are not optimised, sort out vectors
  w.all <- rep(0, 3)
  w.all[w.idx] <- w
  
  # If sequential construct total parameters
  if (is.null(prew)){
    gompw <- w.all    
  } else {
    gompw <- w.all + prew
  }
  
  fit <- gompertzCurve(n, gompw)
  
  se <- getse(x, fit, l, cumulative) # auxiliary.R
  
  # Ensure positive coefficients
  if (any(gompw <= 0)){
    se <- 10e200
  }
  
  return(se)
}
