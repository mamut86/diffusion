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
  At <- w[1] * (1-exp(-(ts/w[2])^w[3]))
  
  # Adoption per period
  at <- diff(c(0, At))
  x <- cbind(At, at)
  colnames(x) <- c("Cumulative Adoption", "Adoption")
  
  return(x)
}

weibullInit <- function(y){
  # Internal function
  # get initial values using rank-median with OLS (see Abernethy 2006)

  # we are fitting on the cumulative adoption
  Y <- cumsum(y)
  
  # calculate Median rank
  n <- length(Y)
  mdrk <- (1:n-0.3)/(n+0.4) #Benard's approximation faster
  # mdrk <- qbeta(p = 0.5,1:n,n:1) # alternative using more precise InvBetadist
  L <- 1 # we fix

  # Abernethy (2006) suggest to estimate X on Y for improved accuracy
  wbfit <- lm(log(Y) ~ log(log(L/(L-mdrk))))
  
  b <- wbfit$coefficients[2]
  a <- exp(-(wbfit$coefficients[1]/b))
  m <- Y[length(Y)]
  
  init <- c(m, a, b)
  names(init) <- c("m", "a", "b")
  
  return(init)
}