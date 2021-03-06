# gamma/shifted Gompertz (G/SG) function ----------------------------------------------------
#
# References
# Bemmaor, A.C. 1994. Modeling the Diffusion of New Durable Goods: Word-of-Mouth
# Effect versus Consumer Heterogeneity. In G. Laurent, G.L. Lilien, and B. Pras
# (Eds.). Research Traditions in Marketing. Boston: Kluwer. pp. 201-223.
# 
# Bemmaor, A.C. and Lee, J. 2002. The Impact of Heterogeinity and
# Ill-Conditioning on Diffusion Model Paremeter Estimates. Marketing Science,
# 21(2), 209-220.
#
# author Oliver Schaer, info@oliverschaer.ch

gsgCurve <- function(n, w){
  # Generate Gompertz curve
  # n, sample size
  # w, vector of parameters
  
  t <- 1:n
  # Cumulative
  At <- w[4] * ((1 - exp(-w[2] * t)) * (1 + w[1] * exp(-w[2] * t))^-w[3])
  # Adoption
  at <- diff(c(0, At))
  Y <- cbind(At, at)
  colnames(Y) <- c("Cumulative Adoption", "Adoption")
  
  return(Y)
}

gsgInit <- function(x, l, optim){
  # Internal function: get initial values
  # We use Bass model paramters assuming c = 1 (see Bemmaor 1994)
  # x in adoption per period
  
  # calling bass estimates
  what <- diffusionEstim(x, l, pvalreps = 0, type = "bass", optim = optim)$w

  # Bemmaor shows that if a = 1, Beta = p/q and b = p + q
  a <- what[1] / what[2] # the shape parameter beta
  b <- what[1] + what[2] # the scale parameter b
  m <- what[3] # the market size
  c <- 1 # this is the shifting parameter alpha
  
  w <- c(a, b, c, m)
  names(w) <- c("a", "b", "c", "m")
  
  return(w)
}

gsgCost <- function(w, x, l, w.idx = rep(TRUE, 4), prew = NULL,
                          cumulative = c(TRUE, FALSE)) {
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
  w.all <- rep(0, 4)
  w.all[w.idx] <- w
  
  # If sequential construct total parameters
  if (is.null(prew)){
    gsgpw <- w.all    
  } else {
    gsgpw <- w.all + prew
  }
  
  fit <- gsgCurve(n, gsgpw)

  se <- getse(x, fit, l, cumulative) # auxiliary.R
  
  # Ensure positive coefficients
  if (any(gsgpw <= 0)){
    se <- 10e200
  }
  
  return(se)
}
