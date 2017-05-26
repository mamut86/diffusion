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

gompertzInit <- function(x, l){
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
  what <- diffusionEstim(x, l, pvalreps = 0, type = "bass")$w
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
  
  if (cumulative == FALSE){
    if (l == 1){
      se <- sum(abs(x-fit[, 2]))
    } else if (l == 2){
      se <- sum((x-fit[, 2])^2)
    } else {
      se <- sum(abs(x-fit[, 2])^l)
    }
  } else {
    if (l == 1){
      se <- sum(abs(cumsum(x)-fit[, 1]))
    } else if (l == 2){
      se <- sum((cumsum(x)-fit[, 1])^2)
    } else {
      se <- sum(abs(cumsum(x)-fit[, 1])^l)
    }
  }
  
  # Ensure positive coefficients
  if (any(gompw <= 0)){
    se <- 10e200
  }
  
  return(se)
}

#' @method predict gompertz
#' @export
predict.gompertz <- function(object, h, np = F){
  # Produce forecasts for Gompertz
  # object, estimated gompertz model using diffuse
  # h, forecast horizon
  # np, if TRUE forecast is considered from t+1 
  
  n <- length(object$x)
  if (np == F) {
    xhat <- gompertzCurve(n+h, object$w)[(n+1):(n+h), ]
  } else {
    xhat <- gompertzCurve(h, object$w)
  }
  
  
  # Append forecasts to bass object
  return(structure(c(object, list("mean" = xhat[, 2],"xhat" = xhat)),
                   class = "gompertz"))
}

#' @method print gompertz
#' @export
print.gompertz <- function(x, ...){
  # Print console output for gompertz
  # x, object estimated using diffusion
  
  diffusionPrint(x, ...)
}

#' @method plot gompertz
#' @export
plot.gompertz <- function(x, cumulative = c(FALSE, TRUE), ...){
  # Plot bass curves
  # x, object estimated using bass
  # cumulative, if TRUE plot cumulative adoption
  
  diffusionPlot(x, cumulative, ...)
}
