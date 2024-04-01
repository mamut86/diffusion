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
  At <- w[1] * (1-exp(-(w[2]+w[3])*t)) / (1+(w[3]/w[2])*exp(-(w[2]+w[3])*t))
  
  # Adoption
  at <- diff(c(0, At))
  
  # Separate into innovator and imitators
  innov <- w[2]*(w[1] - At)
  imit <- at - innov
  
  # Merge
  x <- cbind(At, at, innov, imit)
  colnames(x) <- c("Cumulative Adoption", "Adoption",
                   "Innovators", "Imitators")
  
  return(x)
}

#' @importFrom stats lm
bassInit <- function(y){
  # Internal function: get initial values using linear regression
  # y in adoption per period
  
  # Estimate via linear regression as shown by Bass (1969)
  Y <- cumsum(y)
  Y2 <- Y^2
  cf <- lm(y ~ Y + Y2)$coefficients
  
  # Solve the quadratic and get all p, q, m
  m <- polyroot(cf) 
  m <- Re(m)        
  m <- max(m)       
  p <- cf[1]/m
  q <- cf[2]+p
  
  init <- c(m, p, q)
  names(init) <- c("m", "p", "q")
  # make sure no negative paramters appear
  # init[init < 0] <- 0
  
  return(init)
}
