# Gompertz function -------------------------------------------------------
#
# References
# Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares fitting Gompertz
# curve. Journal of Computational and Applied Mathematics, 169, 359-375.
#
# author Oliver Schaer, info@oliverschaer.ch
# author Nikolaos Kourentzes, nikolaos@kourentzes.com

gompertzInit2 <- function(x, loss, optim){
  # Internal function: get initial values
  # get approximation of initial values using Jukic et al. 2004 approach adopted
  # m to allow for x to be adoption per period
  
  # make sure leading 0s are removed
  x <- cleanzero(x)$x
  
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
  what <- diffusionEstim(x, loss, pvalreps = 0, type = "bass", optim = optim)$w
  m <- what[3]
  
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

diffCost <- function(w, y, loss, type, w.idx, prew, cumulative, mscal, ibound){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # x, adoption per period
  # loss, the l-norm (1 is absolute errors, 2 is squared errors)
  # type, the model type
  # w.idx, logical vector with three elements. Use FALSE to not estimate
  # respective parameter
  # prew, the w of the previous generation - this is used for sequential fitting
  # cumulative, use cumulative adoption or not
  # mscal, should market parameter be scaled
  # bound, parameters to be checked for being >0 if no bounds are provided in the optimiser
  
  n <- length(y)
  
  # If some elements of w are not optimised, sort out vectors
  w.all <- rep(0, length(w))
  w.all[w.idx] <- w
  
  # If sequential construct total parameters
  if (!is.null(prew)){
    w.all <- w.all + prew
  }
  
  if (mscal == TRUE) {
    # Fix scale of first parameter, instead of being the maximum it is a multiplier on current value
    w.all[1] <- w.all[1]*(10*max(cumsum(y)))  # The 10x should be data driven. Something that would bring w_1 closer to 1-10?
  }
  
  switch(type,
         "bass" = fit <- bassCurve(n, w.all),
         "gompertz" = fit <- gompertzCurve(n, w.all),
         "gsgompertz" = fit <- gsgCurve(n, w.all),
         "weibull" = fit <- weibullCurve(n, w.all),
         "gompertz2" = fit <- gompertzCurve(n, w.all),
         "bass2" = fit <- bassCurve2(n, w.all)
         )
  
  se <- getse(y, fit, loss, cumulative) # auxiliary.R
  
  # Ensure positive coefficients for optimisers without bounds
  if (ibound == TRUE) {
    if (any(w.all <= 0)){
      se <- 10e200
    }
  }
  
  return(se)
}


callOptim <- function(x, loss, optim, type, init, w.idx = rep(TRUE, 3),
                      prew = NULL, cumulative = c(TRUE, FALSE),
                      optsol = c("multi", "single"), mscal = c(TRUE, FALSE)) {
  # Optimise with rescaling and multiple initial values for the market potential
  # The alpha parameter of Gompertz curve is notoriously hard to optimise. 
  # We will try different initial values and check the resulting costs.
  # We also have to worry about sample randomness, so instead of picking the 
  # absolute minimum, we will prefer a local minimum amonst the lowest locally.
  # We also need to worry about the scale of the parameters. We scale alpha by
  # 10*max(temp) to bring it to a region closer to gamma and beta.  
  # optsol, using mulitiple initial values to derive at more optimal solutions
  
  # make a check which optimiser is selected and whether internal bound is needed
  ibound <- FALSE
  lbound <- rep(0.00001, length(init)) # set lower bounds
  # add a option for maxiter
  # turn warning off when everything works nice
  
  if (optsol == "multi") {
    # corse search
    optSols <- list()
    for (s in 1:10) {
      # At each iteration we start the search from an 10*s-fold increase of cases
      w <- as.vector(c(s, init[2:length(init)]))
      # nmlk seems to work well but without bounds
      # L-BFGS with bounds
      optSols[[s]] <-  optimx::optimx(w, diffCost, method = "L-BFGS-B",
                                      lower = lbound, y = x,
                                      loss = loss, type = type, prew = prew, cumulative = cumulative,
                                      w.idx = w.idx, mscal = mscal, ibound = ibound,
                                      control = list(trace = 0, dowarn = TRUE)
      )
    }
    
    # Get all the loss function results and find the lowest, this will indicate our more local search
    cf <- unlist(lapply(optSols, function(x) {x$value}))
    # t(abind((lapply(optSols,function(x){c(x$p1,x$p2,x$p3,x$value)})),along=2))
    idx <- which.min(cf)
    
    # Now do more detailed search - ideally most solutions should converge to the same!
    optSols <- list()
    for (s in 1:19){
      # w <- as.vector(c(init[1:2], idx+seq(-0.9, 0.9, 0.1))[s]))
      # w <- as.vector(c(0.5, 0.5, (idx+seq(-0.9, 0.9, 0.1))[s]))
      w <- as.vector(c((idx + seq(-0.9, 0.9, 0.1))[s], init[2:length(init)]))
      optSols[[s]] <- optimx::optimx(w, diffCost, method = "L-BFGS-B",
                                     lower = lbound, y = x,
                                     loss = loss, type = type, prew = prew, cumulative = cumulative,
                                     w.idx = w.idx, mscal = mscal, ibound = ibound,
                                     control = list(trace = 0, dowarn = TRUE)
      )
    }
    cf <- unlist(lapply(optSols, function(x) {x$value}))
    opt <- optSols[[which.min(cf)]]
    
    w <- unlist(opt[1:length(init)])
    if (mscal == TRUE) {
      w[1] <- w[1]*10*max(cumsum(x))
    }
    
  } else {
    init <- as.vector(init)
    # single optimisation
    opt <- optimx::optimx(init, diffCost, method = "L-BFGS-B",
                          lower = lbound, y = x,
                          loss = loss, type = type, prew = prew, cumulative = cumulative,
                          w.idx = w.idx, mscal = mscal, ibound = ibound,
                          control = list(trace = 0, dowarn = TRUE))
    
    w <- unlist(opt[1:length(init)])
    if (mscal == TRUE) {
      w[1] <- w[1]*10*max(cumsum(x))
    }
    
  }
    

    
  
  return(w)
}
