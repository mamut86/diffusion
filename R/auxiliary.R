# Some internal auxiliary functions


cleanzero <- function(x, c.zero = c("lead", "trail")) {
  # Internal function: remove leadig or trailing zeros
  # x, vector of values
  # c.zero, removes either leading or trailing 0s
  
  c.zero <- c.zero[1]
  
  idx <- which(x == 0)
  n <- length(x)
  l <- length(idx)
  
  
  if (c.zero == "lead") {
    
    if (l > 0 & idx[1] == 1){
      d.idx <- diff(idx)
      
      
      loc <- which(d.idx > 1)[1]
      if (is.na(loc)){
        loc <- l
      }
      x <- x[(loc+1):n]
    } else {
      
      loc <- 1
    }
  }
  
  if (c.zero == "trail") {
    
    if (l > 0 & idx[l] == n){
      d.idx <- rev(diff(idx))
      
      loc <- n - which(d.idx > 1)[1]
      if (is.na(loc)){
        loc <- n-l
      }
      x <- x[1:loc]
    } else {
      loc <- n
    }
  }  
  return(list("x" = x, "loc" = loc))
}


cleanna <- function(x, silent = c(TRUE, FALSE)) {
  
  # internal function to remove NA values. Stops if NA is within time series.
  # # x, vector of values
  # silent, when TRUE warnings when removing leading and trailing NAs
  
  silent <- silent[1]
  
  # state for error message
  tl <- ld <- FALSE
  locld <- 1  # location of lead trim
  loctl <- length(x) # length trail trim
  
  # check if any NA
  if (any(is.na(x))) {
    loc <- which(is.na(x))
  } else {
    loc <- NULL
  }
  
  # check i
  if (any(loc > 0)) {
    
    # remove leading NA
    if (loc[1] == 1) {
      
      idx <- diff(loc)
      if (any(idx > 1)) {
        rem <- which(idx > 1)[1]
        x <- x[-(1:rem)]
      } else {
        rem <- length(idx) + 1
        x <- x[-(1:rem)]
      }
      ld <- TRUE
      locld <- length(rem)
    }
    
    # remove trailing NA
    if (loc[length(loc)] == length(x)) {
      
      idx <- diff(rev(loc))
      if (any(idx < -1)) {
        rem <- which(idx < -1)[1]
        x <- head(x, -rem)
      } else {
        rem <- length(idx) + 1
        x <- head(x, -rem)
      }
      tl <- TRUE
      loctl <- length(rem)
    }
    
    # abort  remove remaining NA
    if (any(is.na(x))) {
      stop("Cannot estimate model as NA values present within time series.")
    }
  }
  
  # Warning message if needed
  if (silent == F) {
    if (tl == T && ld == T) {
      message("Removed leading and trailing NA values to fit model")
    }
    
    if (tl == T && ld == F) {
      message("Removed trailing NA values to fit model")
    }
    
    if (tl == F && ld == T) {
      message("Removed leading NA values to fit model")
    }
  }
  
  return(list("x" = x, "loc" = loc, "locLead" = locld, "locTrail" = loctl))
  
}


getse <- function(y, fit, loss, cumulative) {
  # calculate squared error
  
  if (cumulative == FALSE) {
    if (loss == -1) {
      se <- y - fit[, 2]
      # se <- log(y) - log(fit[, 2])
      se <- sum(se[se>0]) + sum(-se[se<0])
    } else if (loss == 1){
      # se <- sum(abs(log(y)-log(fit[, 2])))
      se <- sum(abs(y - fit[, 2]))
    } else if (loss == 2){
      se <- sum((y - fit[, 2])^2)
      # se <- sum((log(y) - log(fit[, 2]))^2)
    } else {
      se <- sum(abs(y - fit[, 2])^loss)
      # se <- sum(abs(log(y) - log(fit[, 2]))^loss)
    }
  } else {
    if (loss == -1) {
      se <- cumsum(y) - fit[, 1]
      # se <- log(cumsum(y)) - log(fit[, 1])
      se <- sum(se[se>0]) + sum(-se[se<0])
    } else if (loss == 1) {
      # se <- sum(abs(log(cumsum(y)) - log(fit[, 1])))
      se <- sum(abs(cumsum(y) - fit[, 1]))
    } else if (loss == 2) {
      # se <- sum((log(cumsum(y)) - log(fit[, 1]))^2)
      se <- sum((cumsum(y) - fit[, 1])^2)
    } else {
      # se <- sum(abs(log(cumsum(y)) - log(fit[, 1]))^loss)
      se <- sum(abs(cumsum(y) - fit[, 1])^loss)
    }
  }
  
  return(se)
}


callOptim <- function(y, loss, method, maxiter, type, init, wIdx = rep(TRUE, length(init)),
                      prew = NULL, cumulative = c(TRUE, FALSE),
                      multisol = c(FALSE, TRUE), mscal = c(TRUE, FALSE), ibound, lbound) {
  # function to call optimisation process
  # The function will always output a vector equal to the number of model parameters
  # Fixed parameters will be locked to initials. If prew is provided, then fixed parameters will be locked to 0.
  
  # multisol, using mulitiple initial values to derive at more optimal solutions
  # mscal, decides whether m parameter is being rescaled
  # ibound, whether intrabounds of cost function should be used
  # lbound, what lower bound is needed
  
  # Take care of scaling of m
  if (mscal == TRUE & wIdx[1] == TRUE) {
    # Fix scale of first parameter
    init[1] <- scaleM(y, init[1], scaledir = "down")
    prew[1] <- scaleM(y, prew[1], scaledir = "down")
  }
  
  # Limit number of parameters to estimate according to wIdx
  lbound <- lbound[wIdx]
  initF <- as.vector(init[wIdx])
  wFix <- as.vector(init[!wIdx])
  
  if (multisol == TRUE & wIdx[1] == TRUE) { # This makes sense only for m, not the other parameters
    # The m parameter of growth curves is notoriously hard to optimise. 
    # We will try different initial values and check the resulting costs.
    # We also have to worry about sample randomness, so instead of picking the 
    # absolute minimum, we will prefer a local minimum amonst the lowest locally.
    # We also need to worry about the scale of the parameters. We scale m by
    # 10*max(y) to bring it to a region closer to the other paramters.  
    
    # Step 1: coarse search
    optSols <- list()
    for (s in 1:10) {
      
      if (length(initF) > 1) {
        w <- as.vector(c(s*initF[1], initF[2:length(initF)]))
      } else {
        w <- as.vector(s*initF[1])
      }
      
      optSols[[s]] <- runOptim(w, method, lbound, y, loss, type, cumulative,
                               wIdx, wFix, prew, mscal, ibound, maxiter)
    }
    # Get all the loss function results and find the lowest, this will indicate our more local search
    cf <- unlist(lapply(optSols, function(x) {x$value}))
    idx <- which.min(cf)
    
    # Go in the second step if the coarse search resulted in substantial differences
    if (sd(cf)/mean(cf) <= 0.1){
      # End optimisation
      
    } else {
      
      # Step 2: detailed search - ideally most solutions should converge to the same!
      optSols <- list()
      for (s in 1:19){
        
        if (length(initF)>1){
          w <- as.vector(c(((idx + seq(-0.9, 0.9, 0.1))[s])*initF[1], initF[2:length(initF)]))
        } else {
          w <- as.vector((idx + seq(-0.9, 0.9, 0.1)[s])*initF[1])
        }
        optSols[[s]] <- runOptim(w, method, lbound, y, loss, type, cumulative,
                                 wIdx, wFix, prew, mscal, ibound, maxiter)
      }
      cf <- unlist(lapply(optSols, function(x) {x$value}))
      
    }
    
    # Extract parameters from bets performing run
    opt <- optSols[[which.min(cf)]]
    # cbind(unlist(lapply(optSols, function(x) {x$value})), unlist(lapply(optSols, function(x) {x$p1})))
    
  } else { # multisol == FALSE
    
    # print(paste(lbound))
    # single optimisation 
    opt <- runOptim(w = initF, method, lbound, y, loss, type, cumulative,
                    wIdx, wFix, prew, mscal, ibound, maxiter)
  }
  
  # Distribute optimal and fixed values
  w <- rep(0,length(init))
  w[wIdx] <- unlist(opt[1:sum(wIdx)])
  
  # Revert scaling
  if (mscal == TRUE & wIdx[1] == TRUE){
    w[1] <- scaleM(y, w[1], scaledir = "up")
  }
  
  # name parameter output
  w <- nameParameters(as.matrix(w), type)[, 1]
  if(any(is.na(w))){browser()}
  return(w)
}

runOptim <- function(w, method, lbound, y, loss, type, cumulative, wIdx, wFix,
                     prew, mscal, ibound, maxiter) {
  # function that runs the optimiser function
  # reverts to Rcgmin in case only one parameter to estimate
  # Includes a few fallbacks for convcode 9999
  
  if (length(w) > 1 | method == "Rcgmin") {
    # These optimisation algorithms are multidimensional, so revert to BFGS if needed unless it is Rcgmin
    opt <- optimx(w, difCost, method = method, lower = lbound, y = y,
                   loss = loss, type = type, cumulative = cumulative,
                   wIdx = wIdx, wFix = wFix, prew = prew, mscal = mscal, ibound = ibound,
                   control = list(trace = 0, dowarn = TRUE,
                                  maxit = maxiter, starttests = FALSE))
    
  } else {
    # revert to single parameter optimisation algorithms
    # Max iterations included in the BFGS
    opt <- optimx(w, difCost, method = "L-BFGS-B", lower = -Inf, y = y,
                          loss = loss, type = type, cumulative = cumulative,
                          wIdx = wIdx, wFix = wFix, prew = prew, mscal = mscal, ibound = F,
                          control = list(trace = 0, dowarn = TRUE, maxit = maxiter, starttests = FALSE))
  }
  
  
  # revert to Rcgmin when error optimiser - this will fix an error we observed with L-BFGS-B
  # if (opt$convcode == 9999) {browser()} # to trackerror
  if (opt$convcode == 9999) {
    # first try Rcgmin
    opt <- optimx(w, difCost, method = "Rcgmin", lower = -Inf, y = y,
                          loss = loss, type = type, cumulative = cumulative,
                          wIdx = wIdx, wFix = wFix, prew = prew, mscal = mscal, ibound = T,
                          control = list(trace = 0, dowarn = TRUE, maxit = maxiter, starttests = FALSE))
  } else if (opt$convcode == 9999) {
    # revert to BFGS
    opt <- optimx(w, difCost, method = "BFGS", lower = lbound, y = y,
                          loss = loss, type = type, cumulative = cumulative,
                          wIdx = wIdx, wFix = wFix, prew = prew, mscal = mscal, ibound = ibound,
                          control = list(trace = 0, dowarn = TRUE, maxit = maxiter, starttests = FALSE))
  } else if (opt$convcode == 9999) {
    warning("Failed to optimize all parameters")
  }

  return(opt)
}


difCost <- function(w, y, loss, type, wIdx, wFix, prew, cumulative, mscal, ibound){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # , adoption per period
  # loss, the l-norm (1 is absolute errors, 2 is squared errors)
  # type, the model type
  # wIdx, logical vector with three elements. Use FALSE to not estimate respective parameter
  # wFix, contains parameters that will not be estimated
  # prew, the w of the previous generation - this is used for sequential fitting
  # cumulative, use cumulative adoption or not
  # mscal, should market parameter be scaled
  # bound, parameters to be checked for being >0 if no bounds are provided in the optimiser

  n <- length(y)
  
  # If some elements of w are not optimised, sort out vectors
  wAll <- rep(0, length(wIdx))
  wAll[wIdx] <- w
  wAll[!wIdx] <- wFix
  
  # If sequential construct total parameters
  if (!is.null(prew)){
    wAll[wIdx] <- wAll[wIdx] + prew[wIdx]
    wAll[!wIdx] <- prew[!wIdx]
  } 
  
  if (mscal == TRUE & wIdx[1] == TRUE) {
    # Fix scale of first parameter, instead of being the maximum it is a multiplier on current value
    wAll[1] <- scaleM(y, wAll[1], scaledir = "up")
  }
  
  fit <- getCurve(n, wAll, type)
  se <- getse(y, fit, loss, cumulative) # auxiliary.R
  
  # Ensure positive coefficients for optimisers without bounds
  if (ibound == TRUE) {
    if (any(wAll <= 0)){
      se <- 10e200
    }
  }
  
  return(se)
}


checkInit <- function(init, method, prew, y, mscal) {
  # function to check the initalisation for scale and sets bounds
  # init, the initalisation parameters
  # method, the optimisation algorithm selected
  # prew, any previous generations, if available - is inputed scaled if mscal == TRUE
  # y, actuals for the scaling
  # mscal, should scaling be done
  
  # "L-BFGS-B" needs lower bounds
  # ibounds uses internal bounds
  if (method == "L-BFGS-B") {
    lbound <- rep(1e-9, length(init))
    # lbound <- -Inf
    ibound <- FALSE
  } else {
    lbound <- -Inf
    ibound <- TRUE
  }
  
  # We adjust lower bounds by prew - which can be scaled
  # at this point prew is either a vector of 0's or values from a different diffusion generation
  if (!is.null(prew)){

    if (mscal == T) {
      prew[1] <- scaleM(y, prew[1], scaledir = "down")
      lbound[1] <- scaleM(y, lbound[1], scaledir = "down")
    }

    # browser()
    lbound <- lbound - prew
    ibound <- TRUE
  }
  
  hbound <- Inf
  
  # make sure init is scaled and matching lbounds
  if (mscal == T) {
    init[1] <- scaleM(y, init[1], scaledir = "down")
  }
  
  init[init < lbound] <- lbound[init < lbound]
  
  # run a scalecheck
  check <- scalechk(init, lbound, hbound, dowarn = FALSE)
  checkRes <- c(check$lpratio, check$lbratio)
  
  # revert to normal scale
  if (mscal == T) {
    init[1] <- scaleM(y, init[1], scaledir = "up")
  }
  
  # use same heuristic as suggested in OptimX
  if (max(checkRes, na.rm = TRUE) > 3) {
    warnScal <- TRUE
  } else {
    warnScal <- FALSE
  }
  
  return(list("init" = init, "lbound" = lbound, "ibound" = ibound, "warnScal" = warnScal))
  
}


nameParameters <- function(x, type) {
  # function that names paramters according to curve type
  # x, the paramter vector
  # type, the diffusion curve type
  
  # Alternative more descriptive naming
  # Gs/Gompertz: c(a - displacement", "b - growth", "c - shift")
  # Gompertz: c(a - displacement", "b - growth")
  # Weibull: c("m - Market potential", "a - scale", "b - shape")
  # Bass: c("m - Market potential", "p - innovation", "q - imitation")
  
  switch(type,
           "bass" = rownames(x) <- c("m", "p", "q"),
           "gompertz" = rownames(x) <- c("m", "a", "b"),
           "gsgompertz" = rownames(x) <- c("m", "a", "b", "c"),
           "weibull" = rownames(x) <- c("m", "a", "b")
    )
  
  return(x)
}


getCurve <- function(n, w, type) {
  # function that returns the curve values
  # n, number of observations
  # w, curve paramteres
  # type, curve type selected
  
  switch(type,
         "bass" = {x <- bassCurve(n, w)},
         "gompertz" = {x <- gompertzCurve(n, w)},
         "gsgompertz" = {x <- gsgCurve(n, w)},
         "weibull" = {x <- weibullCurve(n, w)}
  )
  
  return(x)
}


scaleM <- function(y, m, scaledir = c("up", "down")) {
  # function that up- or down-scales the market potential
  # y, the actuals needed for fixing it to the scales
  # m, the market potential paramter to be scaled
  # scaledir, the scaling direction
  
  # The 10x should be data driven. Something that would bring w_1 closer to 1-10?
  switch (scaledir,
    "up" = mNew <- m*(10*sum(y)),
    "down" = mNew <- m/(10*sum(y))
  )
  
  return(mNew)
}

numberParameters <- function(type) {
  # function that returns the number of parameters required for each diffusion model
  # type, the type of model to be estimated
  
  if (type == "bass" | type == "gompertz" | type == "weibull"){
    noW <- 3
  } else if (type == "gsgompertz"){
    noW <- 4
  }
  
  return(noW)
  
}
