# Some internal auxiliary functions

cleanzero <- function(x) {
  # Internal function: remove leadig zeros
  # x, vector of values
  
  idx <- which(x == 0)
  n <- length(x)
  l <- length(idx)
  
  if (l>0 & idx[1]==1){
    d.idx <- diff(idx)
    loc <- which(d.idx > 1)[1]
    if (is.na(loc)){
      loc <- l
    }
    x <- x[(loc+1):n]
  } else {
    loc <- 1
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

callOptim <- function(y, loss, optim, maxiter, type, init, wIdx = rep(TRUE, 3),
                      prew = NULL, cumulative = c(TRUE, FALSE),
                      optsol = c("multi", "single"), mscal = c(TRUE, FALSE)) {
  # function to call optimisation process
  # optsol, using mulitiple initial values to derive at more optimal solutions
  # mscal, decides whether m parameter is being rescaled

  # "L-BFGS-B" needs lower bounds
  if (optim == "L-BFGS-B") {
    lbound <- rep(1e-9, length(init))
    ibound <- FALSE
  } else {
    ibound <- TRUE
    lbound <- -Inf
  }
  
  # make sure init is matching lbounds
  init[init < lbound] <- lbound[init < lbound]
  
  if (mscal == TRUE & wIdx[1]==TRUE) {
    # Fix scale of first parameter
    init[1] <- init[1]/(10*sum(y))  # The 10x should be data driven. Something that would bring w_1 closer to 1-10?
    prew[1] <- prew[1]/(10*sum(y))
  }
  
  # initalisation warning catch
  warScal <- FALSE
  
  if (optsol == "multi" & wIdx[1] == TRUE) { # This makes sense only for m, not the other parameters
    # The m parameter of growth curves is notoriously hard to optimise. 
    # We will try different initial values and check the resulting costs.
    # We also have to worry about sample randomness, so instead of picking the 
    # absolute minimum, we will prefer a local minimum amonst the lowest locally.
    # We also need to worry about the scale of the parameters. We scale m by
    # 10*max(y) to bring it to a region closer to the other paramters.  
    
    # Step 1: coarse search
    optSols <- list()
    for (s in 1:10) {
      
      if (length(init)>1){
        w <- as.vector(c(s*init[1], init[2:length(init)]))
      } else {
        w <- as.vector(s*init[1])
      }
      
      optSols[[s]] <-  optimx::optimx(w, difCost, method = optim,
                                                lower = lbound, y = y,
                                                loss = loss, type = type, prew = prew, cumulative = cumulative,
                                                wIdx = wIdx, mscal = mscal, ibound = ibound,
                                                control = list(trace = 0, dowarn = TRUE, maxit = maxiter))
    }
    # Get all the loss function results and find the lowest, this will indicate our more local search
    cf <- unlist(lapply(optSols, function(x) {x$value}))
    idx <- which.min(cf)
    
    # Step 2: detailed search - ideally most solutions should converge to the same!
    optSols <- list()
    for (s in 1:19){
      
      w <- as.vector(c(((idx + seq(-0.9, 0.9, 0.1))[s])*init[1], init[2:length(init)]))
      optSols[[s]] <- optimx::optimx(w, difCost, method = optim,
                                     lower = lbound, y = y,
                                     loss = loss, type = type, prew = prew, cumulative = cumulative,
                                     wIdx = wIdx, mscal = mscal, ibound = ibound,
                                     control = list(trace = 0, dowarn = TRUE, maxit = maxiter)
      )
    }
    cf <- unlist(lapply(optSols, function(x) {x$value}))
    # cbind(unlist(lapply(optSols, function(x) {x$value})), unlist(lapply(optSols, function(x) {x$p1})))
    opt <- optSols[[which.min(cf)]]
    
  } else { # optsol == "single"

    init <- as.vector(init)
    # single optimisation 
    # withCallingHandlers({
      opt <- optimx::optimx(init, difCost, method = optim,
                                               lower = lbound, y = y,
                                               loss = loss, type = type, prew = prew, cumulative = cumulative,
                                               wIdx = wIdx, mscal = mscal, ibound = ibound,
                                               control = list(trace = 0, dowarn = TRUE, maxit = maxiter))
    # }, warning = function(war) {
    #   
    #   msg <- conditionMessage(war)
    #   
    #   if (!is.na(pmatch("Parameters or bounds appear to have different scalings", msg))) {
    #     warScal <<- TRUE
    #     invokeRestart("muffleWarning")
    #   } else {
    #     warning(paste("warning in optimx:", msg))
    #     invokeRestart("muffleWarning")
    #   }
    # })
  }
  
  w <- unlist(opt[1:length(init)])
  if (mscal == TRUE & wIdx[1]==TRUE){
    w[1] <- w[1]*10*sum(y)
  }
  
  return(w)
}

difCost <- function(w, y, loss, type, wIdx, prew, cumulative, mscal, ibound){
  # Internal function: cost function for numerical optimisation
  # w, current parameters
  # , adoption per period
  # loss, the l-norm (1 is absolute errors, 2 is squared errors)
  # type, the model type
  # wIdx, logical vector with three elements. Use FALSE to not estimate
  # respective parameter
  # prew, the w of the previous generation - this is used for sequential fitting
  # cumulative, use cumulative adoption or not
  # mscal, should market parameter be scaled
  # bound, parameters to be checked for being >0 if no bounds are provided in the optimiser

  n <- length(y)
  
  # If some elements of w are not optimised, sort out vectors
  wAll <- rep(0, length(wIdx))
  wAll[wIdx] <- w
  
  # If sequential construct total parameters
  if (!is.null(prew)){
    wAll <- wAll + prew
  }
  
  if (mscal == TRUE & wIdx[1] == TRUE) {
    # Fix scale of first parameter, instead of being the maximum it is a multiplier on current value
    wAll[1] <- wAll[1]*(10*sum(y))  # The 10x should be data driven. Something that would bring w_1 closer to 1-10?
  }
  
  switch(type,
         "bass" = fit <- bassCurve(n, wAll),
         "gompertz" = fit <- gompertzCurve(n, wAll),
         "gsgompertz" = fit <- gsgCurve(n, wAll),
         "weibull" = fit <- weibullCurve(n, wAll)
  )
  
  se <- getse(y, fit, loss, cumulative) # auxiliary.R
  
  # Ensure positive coefficients for optimisers without bounds
  if (ibound == TRUE) {
    if (any(wAll <= 0)){
      se <- 10e200
    }
  }
  
  return(se)
}


checkOptimError <- function(war) {
  # helper for warning catching
  # war, the warning input from tryCatch or withCallingHandlers
  
  msg <- conditionMessage(war)
  
  if (!is.na(pmatch("Parameters or bounds appear to have different scalings", msg))) {
    warScal <<- TRUE
    invokeRestart("muffleWarning")
  } else {
    warning(paste("warning in optimx:", msg))
    invokeRestart("muffleWarning")
  }
}
