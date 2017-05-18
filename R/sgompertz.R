# shifted-Gompertz function ----------------------------------------------------
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

sgompertzCurve <- function(n, w){
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

sgompertzInit <- function(x, l){
  # Internal function: get initial values
  # We use Bass model paramters assuming c = 1 (see Bemmaor 1994)
  # x in adoption per period
  
  # calling bass estimates
  what <- diffusionEstim(x, l, pvalreps = 0, type = "bass")$w

  # Bemmaor shows that if a = 1, Beta = p/q and b = p + q
  a <- what[1] / what[2] # the shape parameter beta
  b <- what[1] + what[2] # the scale parameter b
  m <- what[3] # the market size
  c <- 1 # this is the shifting parameter alpha
  
  w <- c(a, b, c, m)
  names(w) <- c("a", "b", "c", "m")
  
  return(w)
}

sgompertzCost <- function(w, x, l, w.idx = rep(TRUE, 3), prew = NULL){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # x, adoption per period
  # l, the l-norm (1 is absolute errors, 2 is squared errors)
  # w.idx, logical vector with three elements. Use FALSE to not estimate
  # respective parameter
  # prew, the w of the previous generation - this is used for sequential fitting
  
  n <- length(x)
  
  # If some elements of w are not optimised, sort out vectors
  w.all <- rep(0, 4)
  w.all[w.idx] <- w
  
  # If sequential construct total parameters
  if (is.null(prew)){
    sgompw <- w.all    
  } else {
    sgompw <- w.all + prew
  }
  
  fit <- sgompertzCurve(n, sgompw)
  
  if (l == 1){
    se <- sum(abs(x-fit[, 2]))
  } else if (l == 2){
    se <- sum((x-fit[, 2])^2)
  } else {
    se <- sum(abs(x-fit[, 2])^l)
  }
  
  # Ensure positive coefficients
  if (any(sgompw <= 0)){
    se <- 10e200
  }
  
  return(se)
}

forecast.sgompertz <- function(object, h){
  # Produce forecasts for shifted-Gompertz
  # object, estimated gompertz model using diffuse
  # h, forecast horizon
  
  n <- length(object$x)
  xhat <- sgompertzCurve(n+h, object$w)[(n+1):(n+h), ]
  
  # Append forecasts to bass object
  return(structure(c(object, list("mean" = xhat[, 2],"xhat" = xhat)),
                   class = "sgompertz"))
}

#' @method print sgompertz
#' @export
print.sgompertz <- function(x, ...){
  # Print console output for shifted-gompertz
  # x, object estimated using diffusion
  
  print.diffusion(x, ...)
}

#' @method plot sgompertz
#' @export
plot.sgompertz <- function(x, cumulative = c(FALSE, TRUE), ...){
  # Plot shifted-Gompertz curves
  # x, object estimated using shifted-Gompertz
  # cumulative, if TRUE plot cumulative adoption
  
  plot.diffusion(x, cumulative, ...)
}