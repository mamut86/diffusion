# Bass model functions ----------------------------------------------------
#
# References
# Bass, F.M., 1969. A new product growth for model consumer durables. Management
# Science 15(5), 215-227.
# Srinivasan, V., and Mason, C.H., 1986. Nonlinear Least Squares Estimation of
# New Product Diffusion MOdels. Marketing Science, 5(2), 169-178.
#
# author Oliver Schaer, info@oliverschaer.ch
# author Nikolaos Kourentzes, nikolaos@kourentzes.com

bassCurve <- function(n, w){
  # Generate bass curve
  # n, sample size
  # w, vector of parameters
  
  # Cumulative adoption
  t <- 1:n
  At <- w[3] * (1-exp(-(w[1]+w[2])*t)) / (1+(w[2]/w[1])*exp(-(w[1]+w[2])*t))
  
  # Adoption
  at <- diff(c(0, At))
  
  # Separate into innovator and imitators
  innov <- w[1]*(w[3] - At)
  imit <- at - innov
  
  # Merge
  Y <- cbind(At, at, innov, imit)
  colnames(Y) <- c("Cumulative Adoption", "Adoption",
                   "Innovators", "Imitators")
  
  return(Y)
}

bassInit <- function(x){
  # Internal function: get initial values using linear regression
  # x in adoption per period
  
  # Estimate via linear regression as shown by Bass (1969)
  X <- cumsum(x)
  X2 <- X^2
  cf <- stats::lm(x ~ X + X2)$coefficients
  
  # Solve the quadratic and get all p, q, m
  m <- polyroot(cf) 
  m <- Re(m)        
  m <- max(m)       
  p <- cf[1]/m
  q <- cf[2]+p
  
  init <- c(p, q, m)
  names(init) <- c("p", "q", "m")
  # make sure no negative paramters appear
  # init[init < 0] <- 0
  
  return(init)
}

bassCost <- function(w, x, l, w.idx = rep(TRUE, 3), prew = NULL, cumulative = c(TRUE, FALSE)){
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
    bassw <- w.all    
  } else {
    bassw <- w.all + prew
  }
  
  fit <- bassCurve(n, bassw)

  se <- getse(x, fit, l, cumulative) # auxiliary.R

  # Ensure positive coefficients
  if (any(bassw <= 0)){
    se <- 10e200
  }
  
  return(se)
  
}
