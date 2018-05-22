# Weibull model -----------------------------------------------------
#
# References 
# Abernethy, R.B. 2006. The New Weibull Handbook. Reliablity & Statistical
# Analysis for Predicting Life, Safety, Risk, Support Costs, Failures, and
# Forecasting Warrantey Claims, Substaniation and Accelerated Testing, Using
# Weibull, Log Normal, Crow-AMSAA, Probit, and Kaplan-Meier Models. (5th eds.).
# Abernethy, North Palm Beach.
# 
# Sharif, N.M. and Islam, M.N. 1980. The Weibull Distribution as a
# General Model for Forecasting Technological Change. Technological Forecasting
# and Social Change, 18, 247-256.
#
# author Oliver Schaer, info@oliverschaer.ch
# author Nikolaos Kourentzes, nikolaos@kourentzes.com

weibullCurve <- function(n, w){
  # Generate weibull curve
  # n, sample size
  # w, vector of parameters w[a,b,m]
  
  # Cumulative adoption
  ts <- 1:n
  At <- w[3] * (1-exp(-(ts/w[1])^w[2]))
  
  # Adoption per period
  at <- diff(c(0, At))
  Y <- cbind(At, at)
  colnames(Y) <- c("Cumulative Adoption", "Adoption")
  
  return(Y)
}

weibullInit <- function(x){
  # Internal function
  # get initial values using rank-median with OLS (see Abernethy 2006)
  # x
  
  ts <- 1:length(x)
  
  # we are fitting on the cumulative adoption
  x <- cumsum(x)
  
  # calculate Median rank
  n <- length(y1)
  mdrk <- (1:n-0.3)/(n+0.4) #Benard's approximation faster
  # mdrk <- qbeta(p = 0.5,1:n,n:1) # alternative using more precise InvBetadist
  
  # Define Y and X for OLS
  # Abernethy (2006) suggest to estimate X on Y for improved accuracy
  L <- 1 # we fix
  X <- log(log(L/(L-mdrk)))
  Y <- log(y1)

  wbfit <- lm(Y ~ X)
  
  b <- fit1$coefficients[2]
  a <- exp(-(fit1$coefficients[1]/beta.hat))
  m <- x[length(x)]
  
  init <- c(a, b, m)
  names(init) <- c("a", "b", "m")
  
  return(init)
}



weibullCost <- function(w, x, l, w.idx = rep(TRUE, 3), prew = NULL, cumulative = c(TRUE, FALSE)){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # x, adoption per period
  # l, the l-norm (1 is absolute errors, 2 is squared errors)
  # w.idx, logical vector with three elements. Use FALSE to not estimate respective parameter
  # prew, the w of the previous generation - this is used for sequential fitting
  # cumulative, use cumulative adoption or not
  
  cumulative <- cumulative[1]
  n <- length(x)
  
  # If some elements of w are not optimised, sort out vectors
  w.all <- rep(0, 3)
  w.all[w.idx] <- w
  
  # If sequential construct total parameters
  if (is.null(prew)) {
    weibullw <- w.all    
  } else {
    weibullw <- w.all + prew
  }
  
  fit <- weibullCurve(n, weibullw)
  
  se <- getse(x, fit, l, cumulative) # auxiliary.R
  
  # Ensure positive coefficients
  if (any(weibullw <= 0)){
    se <- 10e200
  }
  
  return(se)
  
}
