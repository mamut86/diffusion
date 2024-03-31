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
  # At <- w[4] * ((1 - exp(-w[2] * t)) * (1 + w[1] * exp(-w[2] * t))^-w[3])
  At <- w[1] * ((1 - exp(-w[3] * t)) * (1 + w[2] * exp(-w[3] * t))^-w[4])
  # Adoption
  at <- diff(c(0, At))
  x <- cbind(At, at)
  colnames(x) <- c("Cumulative Adoption", "Adoption")
  
  return(x)
}

gsgInit <- function(y, loss, method, multisol, initpar, mscal){
  # Internal function: get initial values
  # We use Bass model paramters assuming c = 1 (see Bemmaor 1994)
  # y in adoption per period
  
  # calling bass estimates
  what <- diffusionEstim(y, loss, pvalreps = 0, type = "bass", method = method,
                        multisol = multisol, initpar = initpar, mscal = mscal)$w

  # Bemmaor shows that if a = 1, Beta = p/q and b = p + q
  a <- what[2] / what[3] # the shape parameter beta
  b <- what[2] + what[3] # the scale parameter b
  m <- what[1] # the market size
  c <- 1 # this is the shifting parameter alpha
  
  w <- c(m, a, b, c)
  names(w) <- c("m", "a", "b", "c")
  
  return(w)
}