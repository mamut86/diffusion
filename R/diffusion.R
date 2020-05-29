#' Fit various diffusion curves.
#' 
#' This function fits diffusion curves that can be of \code{"bass"}, 
#' \code{"gompertz"}, \code{"gsgompertz"} (Gamma/Shifted Gompertz curve) or \code{"Weibull"} type. 
#' 
#' @section Bass curve:
#' The optimization of the Bass curve is initialized by the linear
#' approximation suggested in Bass (1969).
#' 
#' @section Gompertz curve:
#' The initialization of the Gompertz curve uses the
#' approach suggested by Jukic et al. (2004), but is adapted to allow for the
#' non-exponential version of the Gompertz curve. This makes the market potential
#' parameter equivalent to the Bass curves and the market potential from
#' Bass curve is used for initialization.
#' 
#' @section Gamma/Shifted Gompertz:
#' The curve is initialized by assuming the shift operator to be 1 and 
#' becomes equivalent to the Bass curve, as shown in Bemmaor (1994). A Bass
#' curve is therefore used as an estimator for the remaining initial parameters.
#' 
#' @section Weibull:
#' The initialization is obtained through by a linear approximation
#' median-ranked OLS described in Sharif and Islam 1980.
#' 
#' @param y vector with adoption per period
#' @param w vector of curve parameters (see note). Parameters set to NA will be
#'   optimized. If \code{w = NULL} (default) all paramters are optimized.
#' @param cleanlead removes leading zeros for fitting purposes (default == TRUE)
#' @param loss the l-norm (1 is absolute errors, 2 is squared errors).
#' @param cumulative If TRUE optimisation is done on cumulative adoption.
#' @param verbose if TRUE console output is provided during estimation (default == FALSE)
#' @param type diffusion curve to use. This can be "bass", "gompertz" and "gsgompertz"
#' @param optim optimization method to use. These can be \code{"Nelder-Meade"},
#'   \code{"L-BFGS-B"}, \code{"BFGS"}, \code{"hjkb"}, \code{"Rcgmin"},
#'   \code{"bobyqa"}. Typically, good performance is achieved with
#'   \code{"Nelder-Meade"} and \code{"L-BFGS-B"}. \code{"hjkb"} and
#'   \code{"Rcgmin"} might be an alternative for complex shapes but have
#'   substantially higher computational costs. For further details on
#'   optimisation algorithms we refer to the optimx package documentation
#' @param maxiter number of iterations the optimser takes (default == \code{5000})
#' @param opttol Tolerance for convergence (default == 1.e-06)
#' @param optsol when \code{"multi"} multiple optmisation solutions from
#'   different initialisations of the market parameter are used (default ==
#'   \code{"single"})
#' @param initpar vector of initalisation parameters. If set to \code{preset} a
#'   predfined set of internal initalisation parameters is used while
#'   \code{"linearize"} uses linearized initalisation methods (default == \code{"linearize"}.
#' @param mscal scales market potential at initalisation with the maximum of the
#'   observed market potential for better optimisation results (default == \code{TRUE})
#' @param ... accepts \code{pvalreps}, bootstrap repetitions to estimate
#'   (marginal) p-values; \code{eliminate}, if TRUE eliminates insignificant
#'   parameters from the estimation (forces \code{pvalreps = 1000} if left to 0)
#'   \code{sig}, significance level used to eliminate parameters.
#' 
#' @return Returns an object of class \code{diffusion}, which contains:
#' \itemize{
#' \item \code{type} diffusion curve type used
#' \item \code{call} calls function fitted
#' \item \code{w} named vector of fitted parameters
#' \item \code{y} actuals
#' \item \code{fit} fitted values of model
#' \item \code{frc} forecasts for future periods. This is \code{NULL} until \code{\link{predict.diffusion}} is called.
#' \item \code{mse} insample Mean Squared Error
#' \item \code{prew} the \code{w} of the previous generation
#' \item \code{pval} p-values for \code{w}
#' \item \code{init} the initial values that have been used for the optimizer
#' }
#' 
#' @note vector \code{w} needs to be provided for the Bass curve in the order of
#'   \code{"m", "p", "q"}, where "p" is the coefficient of innovation, "q" is the
#'   coefficient of imitation and "m" is the market size coefficient.
#'   
#'   For the Gompertz curve, vector \code{w} needs to be in the form of
#'   \code{("m", "a", "b")}. Where "a" is the x-axis displacement coefficient, "b"
#'   determines the growth rate and "m" sets, similarly to the Bass curve, the
#'   market potential (saturation point).
#'   
#'   For the Shifted-Gompertz curve, vector \code{w} needs to be in the form of 
#'   \code{("m", "a", "b", "c")}. Where "a" is the x-axis displacement
#'   coefficient, "b" determines the growth rate, "c" is the shifting parameter
#'   and "m" sets, similarly to the Bass curve, the market potential (saturation
#'   point).
#'   
#'   For the Weibull curve, vector \code{w} needs to be in the form of
#'   \code{("m", "a", "b")}. Where "a" is the scale parameter, "b" determines the
#'   shape. Together, "a" and "b" determine the steepness of the curve. The "m"
#'   parameter sets the market potential (saturation point).
#'   
#' @examples 
#'  fitbass <- diffusion(diff(tsChicken[, 2]), type = "bass")
#'  fitgomp <- diffusion(diff(tsChicken[, 2]), type = "gompertz")
#'  fitgsg <- diffusion(diff(tsChicken[, 2]), type = "gsgompertz")
#'  fitgwb <- diffusion(diff(tsChicken[, 2]), type = "weibull")
#'  
#'  # Produce some plots
#'  plot(fitbass)
#'  plot(fitgomp)
#'  plot(fitgsg)
#'  plot(fitgwb)
#' 
#' @references
#' \itemize{
#' \item{For an introduction to diffusion curves see Ord K., Fildes R., Kourentzes N. (2017) \href{http://kourentzes.com/forecasting/2017/10/16/new-forecasting-book-principles-of-business-forecasting-2e/}{Principles of Business Forecasting 2e}. \emph{Wessex Press Publishing Co.}, Chapter 12.}
#' \item{Bass, F.M., 1969. A new product growth for model consumer durables. Management Science 15(5), 215-227.}
#' \item{Bemmaor, A. 1994. Modeling the Diffusion of New Durable Goods: Word-of-Mouth Effect versus Consumer Heterogeneity. In G. Laurent, G.L. Lilien and B. Pras (Eds.). Research Traditions in Marketing. Boston: Kluwer, pp. 201-223.}
#' \item{Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares fitting Gompertz curve. Journal of Computational and Applied Mathematics, 169, 359-375.}
#' \item{Sharif, N.M. and Islam, M.N. 1980. The Weibull Distribution as a General Model for Forecasting Technological Change. Technological Forecasting and Social Change, 18, 247-256.}
#' }
#'   
#' @seealso \code{\link{predict.diffusion}}, \code{\link{plot.diffusion}} and \code{\link{print.diffusion}}.   
#'   
#' @note Parameters are estimated by 
#' minimising the Mean Squared Error with a Subplex algorithm from the optimx package. 
#' Optionally p-values of the coefficients can be determined via
#' bootstraping. Furthermore, the bootstrapping allows removing insignificant
#' parameters from the optimization process.   
#'   
#' @seealso \code{\link{seqdiffusion}} for sequential diffusion model fitting
#'  across product generations.
#'   
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' 
#' @rdname diffusion  
#' @export diffusion
diffusion <- function(y, w = NULL, cleanlead = c(TRUE, FALSE),
                      loss = 2, cumulative = c(TRUE, FALSE), verbose = c(FALSE, TRUE),
                      type = c("bass", "gompertz", "gsgompertz", "weibull"),
                      optim = c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa"),
                      maxiter = 500, opttol = 1.e-06, optsol = c("single", "multi"),
                      initpar = c("linearize","preset"), mscal = c(TRUE, FALSE), ...) {

  type <- match.arg(type[1], c("bass", "gompertz", "gsgompertz", "weibull"))
  optim <- match.arg(optim[1], c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa", "nm", "hj"))
  optsol <- match.arg(optsol[1], c("single", "multi"))
  if (!is.numeric(initpar)){
    initpar <- match.arg(initpar[1], c("preset", "linearize", "linearise"))
  }
  
  # Check arguments in ellipsis
  el <- list(...)
  nel <- names(el)
  # First check for arguments used by seqdiffusion
  if ("pvalreps" %in% nel){
    pvalreps <- el$pvalreps
  } else {
    pvalreps <- 0
  }
  if ("eliminate" %in% nel){
    eliminate <- el$eliminate
  } else {
    eliminate <- FALSE
  }
  if ("sig" %in% nel){
    sig <- el$sig
  } else {
    sig <- 0.05
  }
  if ("prew" %in% nel){
    prew <- el$prew
  } else {
    prew <- NULL
  }
  # Then check for deprecated arguments
  if ("l" %in% nel) {
    warning("Argument \"l\" has been deprecated and replaced by \"loss\"")
    loss <- el$l
  } 
  if("x" %in% nel) {
    warning("Argument \"x\" has been deprecated and replaced by \"y\"")
    y <- el$x
  }
  
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  cleanlead <- cleanlead[1] # note dependency in seqdiffusion plot
  mscal <- mscal[1]
  
  if (cleanlead == TRUE) {
    y <- cleanzero(y)$x
  }
  y <- cleanna(y)$x
  
  # Check which parameters to estimate
  
  ## Put this in auxiliary
  # determine how many paramters needed
  if (type == "bass" | type == "gompertz" | type == "weibull"){
    noW <- 3
  } else if (type == "gsgompertz"){
    noW <- 4
  }
  
  if (is.null(w)){
    # If no parameters given then everything is estimated
    wFix <- NULL
    optPar <- TRUE
  } else {
    # Otherwise only NA parameters are estimated
    wFix <- is.na(w)
    if (all(wFix == FALSE)){
      optPar <- FALSE
    } else {
      optPar <- TRUE
      wFix <- w
    }
  }
  
  # Optimise parameters
  if (optPar) {
    opt <- diffusionEstim(y, loss, cumulative, prew, pvalreps, eliminate,
                          sig, verbose, type = type, optim  = optim,
                          maxiter = maxiter, optsol = optsol, initpar = initpar,
                          mscal = mscal, wFix = wFix)

    w <- opt$w
    pval <- opt$pval
    init <- opt$init
    warScal <- opt$warScal
  } else {
    pval <- rep(NA, length(w))
    warScal <- FALSE
    init <- rep(NA, length(w))
  }
  
  n <- length(y)
  switch(type,
         "bass" = fit <- bassCurve(n, w),
         "gompertz" = fit <- gompertzCurve(n, w),
         "gsgompertz" = fit <- gsgCurve(n, w),
         "weibull" = fit <- weibullCurve(n, w),
         "gompertz2" = fit <- gompertzCurve(n, w),
         "bass2" = fit <- bassCurve2(n, w))
  
  mse <- mean((y - fit[, 2])^2)
  
  if (warScal == TRUE & mscal == FALSE) {
    warning('Initalisation parameters are of different scale. Consider argument "mscal" for better optimsation results')
  } else if (warScal == TRUE & mscal == TRUE) {
    warning("Initalisation parameters are of different scale. Optimisation might be impacted")
  }
  
  out <- structure(list("type" = type, "call" = sys.call(),
                        "w" = w, "y" = y, "fit" = fit, "frc" = NULL, 
                        "mse" = mse, "pval" = pval, "init" = init), class="diffusion")
  return(out)
}


diffusionEstim <- function(y, loss = 2, cumulative = c(FALSE, TRUE),
                           prew=NULL, pvalreps = 0,
                           eliminate = c(FALSE, TRUE), sig = 0.05,
                           verbose = c(FALSE, TRUE),
                           type = c("bass", "gompertz", "gsgompertz", "weibull"),
                           optim = c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa"), maxiter = 500, opttol = 1.e-06,
                           optsol = c("single", "multi"), initpar = c("preset", "linearize"),
                           mscal = c(TRUE, FALSE), wFix = NULL) {
  # Internal function: estimate bass parameters 
  # y, adoption per period
  # loss, the l-norm (1 is absolute errors, 2 is squared errors)
  # cumulative, if TRUE optimise on cumulative adoption. 
  # prew, the w of the previous generation - this is used for sequential fitting
  # pvalreps, bootstrap repetitions to estimate (marginal) p-values
  # eliminate, if TRUE eliminates insignificant parameters from estimation.
  # Forces pvalreps = 1000 if left to 0.
  # sig, significance level used to eliminate parameters
  # verbose, if TRUE provide console output during estimation
  # type, diffusion model to use
  # optim, optimisation algorithm, "nm" is nelder-mead, "hj" is hooke-jeeves
  # maxiter, numbers of iterations the optimsation algorithm is allowed to take
  # opttol, convergence tolerance for nm and hj algorithm
  # optsol, run multiple initialisation or just single
  # initpar, aprx uses linear approximation, fix has set initalisation parameters
  # mscal, TRUE scales market potential times the maximum
  # wFix, used to control user fixed parameters
  
  type <- match.arg(type[1], c("bass", "gompertz", "gsgompertz", "weibull"))
  optim <- match.arg(optim[1], c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa", "nm", "hj"))
  optsol <- match.arg(optsol[1], c("single", "multi"))
  if (!is.numeric(initpar)){
    initpar <- match.arg(initpar[1], c("preset", "linearize", "linearise"))
  }
  
  # Defaults 
  mscal <- mscal[1]
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  
  ## Put this in auxiliary
  # determine how many paramters needed
  if (type == "bass" | type == "gompertz" | type == "weibull"){
    noW <- 3
  } else if (type == "gsgompertz"){
    noW <- 4
  }
  
  if (is.numeric(initpar) & length(initpar) != noW) {
    stop(sprintf("%s requires vector of %i paramters for initpar ", type, noW))
  }
  
  # backward compability to old optimisation paramters
  if (optim == "nm") {
    optim <- "Nelder-Mead"
  }
  
  if (optim == "hj") {
    optim <- "hjkb"
  }
  
  # Check maxiter argument
  if (optim == "Nelder-Mead" & maxiter < 500) {
    message("It is recommend to set \"maxiter\" to 500 or more for better results with Nelder-Mead optimiser")
  } else if (maxiter == Inf) {
    maxiter <- 100000
    message("Set \"maxiter\" to 100 000")
  } else if (optim == "hjkb" & maxiter < 1000) {
    message("It is recommend to set \"maxiter\" to 1000 or more for better results with HJKB optimiser")
  }
  
  if (eliminate == TRUE & pvalreps == 0){
    pvalreps <- 1000
    warning("To eliminate parameters p-values must be estimated. Setting pvalreps = 1000.")
  }
  
  # Check bootstrap repetitions (pvalreps)
  if (pvalreps < 0 | !is.numeric(pvalreps)) {
    stop("pvalreps must be positive number.")
  }

  ## Let prew proceed as NULL, this is needed to distinguish between fixed parameters
  ## and previous generations
  # # For sequential generations
  # if (is.null(prew)) {
  #   # no values from previous generation
  #   prew <- rep(0, noW)
  # }
  
  # # Initialise --> see commented out part for the fixing parameter
  #   if (is.null(prew)) {
  #   # no values from previous generation
  #   prew <- rep(0, noW)
  # } # else if (anyNA(prew)) {
  #   # partially fixed parameters
  #   wIdx[!is.na(prew)] <- FALSE # disable parameters
  #   prew[is.na(prew)] <- 0 # set NA to 0 in order to estimated
  # }
  
  if (is.numeric(initpar)) { # use provided initalisation values
    init <- initpar
  }
  
  if (initpar[1] == "linearize") {
    # make sure linearization does not break down the process
    tryCatch( {switch(type,
                      "bass" = init <- bassInit(y),
                      "gompertz" = init <- gompertzInit(y, loss, optim, optsol, initpar, mscal),
                      "gsgompertz" = init <- gsgInit(y, loss, optim, optsol, initpar, mscal),
                      "weibull" = init <- weibullInit(y))
      
      # Check validity of initials
      if (init[1] < max(y)){init[1] <- max(y)}
      
    }, error = function(err) {
      warning('Not able to run linearization. Reverted to "preset" values.')
      initpar <- "preset"
    })
  }
  
  if (initpar[1] == "preset") {
    
    switch(type,
           "bass" = init <- c(0.5, 0.5, 0.5),
           "gompertz" = init <- c(1, 1, 1),
           "gsgompertz" = init <- c(0.5, 0.5, 0.5, 0.5),
           "weibull" = init <- c(0.5, 0.5, 0.5)
           )
    
    # Add scale to first parameter
    if (mscal == TRUE){
      init[1] <- init[1]*(10*sum(y))
    }
  }
  
  # Check which parameters are estimated
  wIdx <- rep(TRUE, noW)         # Which parameters to estimate 
  if (!is.null(wFix)){
    if (any(!is.na(wFix))){
      # And which to fix
      fIdx <- !is.na(wFix)
      wIdx[fIdx] <- FALSE
      init[fIdx] <- wFix[fIdx]
    }
  }
  
  # If prew then adjust initials to be difference from it
  # We are estimating how parameters change from prew
  if (!is.null(prew)){
    init <- init - prew
    init[(init + prew) <= 0] <- 1e-9
  }
  
  switch(type,
         "bass" = names(init) <- c("m", "p", "q"),
         "gompertz" = names(init) <- c("m", "a", "b"),
         "gsgompertz" = names(init) <- c("m", "a", "b", "c"),
         "weibull" = names(init) <- c("m", "a", "b")
  )
  
  # check initalisation
  initval <- checkInit(init, optim, prew)
  init <- initval$init
  lbound <- initval$lbound
  ibound <- initval$ibound
  warScal <- initval$warScal
  
  n <- length(y)
  
  # Iterate until all p-values are < sig
  # If eliminate is not requested it will only iterate once
  elim <- TRUE
  it <- 1
  
  while (elim == TRUE) {
    
    # Optimise
    w <- rep(0, noW)
    
    if (sum(wIdx) > 1) {
      # These optimisation algorithms are multidimensional, so revert to BFGS if needed
      wNew <- callOptim(y, loss, optim, maxiter, type, init,
                         wIdx, prew, cumulative, optsol, mscal, ibound, lbound)

    } else {
      # Revert to L-BFGS-B if only one parameter is required
      # Max iterations included in the BFGS

      wNew <-  callOptim(y, loss, optim = "L-BFGS-B", maxiter, type, init,
                         wIdx, prew, cumulative, optsol, mscal, ibound = F, lbound=lbound)
        
    }  
      
    ## When wNew has 1e-9 values, it means we have hit the lbound
    ## Perhaps we should consider replacing those with zero in the sequential case.
    # The resulting w contains the differences from prew. Final parameters are prew+w
    w <- wNew
    # browser()
    # Bootstrap p-values
    if (pvalreps > 0){
      
      switch(type,
             "bass" = yhat <- bassCurve(n, prew+w)[, 2],
             "gompertz" = yhat <- gompertzCurve(n, prew+w)[, 2],
             "sgompertz" = yhat <- gsgCurve(n, prew+w)[, 2],
             "weibull" = yhat <- weibullCurve(n, prew+w)[, 2])
      
      sigma <- sqrt(mean((y - yhat)^2))
      
      ## This can be improved to be a non-parametric bootstrap. Now we impose a severe assumption
      # Construct bootstraps
      wboot <- array(0, c(pvalreps, noW))
      yboot <- matrix(stats::rnorm(n*pvalreps, 0, sigma), nrow = n) + matrix(rep(yhat, pvalreps), ncol = pvalreps)
      
      # This needs to become multiplicative
      yboot[yboot < 0] <- 0.001
      
      # Estimate model
      for (i in 1:pvalreps){
        
        # Step 1: Estimate parameters on the bootstrapped curve, starting from prew
        #         Note that prew=NULL so the optimiser will output the values including prew (i.e. w = new w + prew)
        wboot[i,] <- callOptim(yboot[,i], loss, optim, maxiter, type, init=prew,
                                   wIdx, prew=NULL, cumulative, optsol, mscal, ibound, lbound)
        # Step 2: Find differences from prew, as we want to find significant deviations from 0 for the pvalues
        wboot[i,] <- wboot[i,] - prew
        
        # for debugging NA error
        # if (any(is.na(wboot[i,]))) {
        #   browser()
        # }
        
        ## Old code, replaced diffusionEstim with callOptim above as this is cleaner
        # wboot[i,] <-  diffusionEstim(y=yboot[,i], loss=loss, cumulative=cumulative, pvalreps=0, eliminate=FALSE, type=type,
        #                             optim=optim, maxiter=maxiter, optsol=optsol, initpar=prew, mscal=mscal)$w - prew
  
      }

      # Calculation of the p-values
      # wboot contains only the additional bit over prew
      # http://www.inference.org.uk/mackay/itila/ pp. 457-466
      # https://stats.stackexchange.com/questions/83012/how-to-obtain-p-values-of-coefficients-from-bootstrap-regression
      # remove NA for failed estimations
      wboot0m <- abs(wboot - matrix(rep(colMeans(wboot, na.rm = T), pvalreps), ncol = noW, byrow = T))
      pval <- colMeans(wboot0m > abs(matrix(rep(w, pvalreps), ncol = noW, byrow = T)), na.rm = T)
      
    } else {
      pval <- rep(NA, noW)
    }
    
    # Elimination process
    if (eliminate == TRUE & any(pval[wIdx] > sig)){
      # Find most insignificant
      pvalTemp <- pval
      pvalTemp[pvalTemp < sig] <- sig
      pvalTemp <- pvalTemp - sig
      pvalTemp[!wIdx] <- 0
      loc <- which(pvalTemp == max(pvalTemp))[1]
      wIdx[loc] <- FALSE
      
      if (all(wIdx == FALSE)){ # check if any variable is still left
        elim <- FALSE
      }
    } else {
      # Stop elimination iterations
      elim <- FALSE
      loc <- NA
    }
    
    # Provide console output
    if (verbose == TRUE){
      writeLines(paste0("Estimation iteration: ", it))
      it <- it + 1
      
      if (!is.na(loc)){
        locv <- rep("", 3)
        locv[loc] <- "X"
      } else {
        locv <- NULL
      }
      
      if (!is.null(prew)){
        temp <- cbind(round(cbind(w+prew, pval), 4), locv)
      } else {
        temp <- cbind(round(cbind(w, pval), 4), locv)
      }
      
      switch(type,
             "bass" = rownames(temp) <- c("m", "p", "q"),
             "gompertz" = rownames(temp) <- c("m", "a", "b"),
             "gsgompertz" = rownames(temp) <- c("m", "a", "b", "c"),
             "weibull" = rownames(temp) <- c("m", "a", "b")
             )
      
      colnames(temp) <- c("Estimate", "p-value", "")[1:(2+!is.na(loc))]
      print(temp, quote = FALSE)
      
      if (elim == FALSE){
        writeLines("Estimation completed")
      }
      
      writeLines("")
    }
  }
  
  # w so far is the difference over prew. We output the final parameters.
  if (!is.null(prew)){
    w <- w + prew  
  }
  names(w) <- names(init)
  names(pval) <- names(w)
  
  return(list("w" = w, "pval" = pval, "init" = init, "warScal" = warScal))
  
}

#' Plot a fitted diffusion curve.
#'
#' Produces a plot of a fitted diffusion curve.
#'
#' @param x \code{diffusion} object, produced using \code{\link{diffusion}}.
#' @param cumulative If TRUE plot cumulative adoption.
#' @param ... Unused argument.
#'
#' @return None. Function produces a plot.
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' @seealso \code{\link{diffusion}}.
#' @examples
#'  fit <- diffusion(tsChicken[, 2])
#'  plot(fit)
#'
#' @export
#' @method plot diffusion
plot.diffusion <- function(x, cumulative = c(FALSE, TRUE), ...){
  diffusionPlot(x, cumulative = cumulative, ...)
}

diffusionPlot <- function(x, cumulative = c(FALSE, TRUE), ...){
  # Internal function: plot diffusion curves
  # x, object estimated using diffusion
  # cumulative, if TRUE plot cumulative adoption
  
  type <- tolower(x$type)
  # set numbers of elements to be plotted, i.e. including innov. and immiat.
  switch(type,
         "bass" = elmt <- 3,
         "gompertz" = elmt <- 1,
         "gsgompertz" = elmt <- 1,
         "weibull" = elmt <- 1
         )
  
  cumulative <- cumulative[1]
  
  # Colorbrewer colours
  cmp <- c("#E41A1C", "#377EB8", "#4DAF4A")
  # Check if forecasts exist and construct xx
  if (!is.null(x$frc)){
    xx <- c(1, (length(x$y) + dim(x$frc)[1]))
  } else {
    xx <- c(1, (length(x$y)))
  }
  
  if (cumulative == FALSE){
    
    # Get yy min-max
    if (!is.null(x$frc)){
      yy <- range(c(x$y, x$fit[, 2:(1 + elmt)], x$frc[, 2:(1 + elmt)]))
    } else {
      yy <- range(cbind(x$y, x$fit[, 2:(1 + elmt)]))
    }
    yy <- yy + c(-1, 1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    graphics::plot(as.vector(x$y), type="p", pch = 20, ylab = "Adoption", xlab = "Period",
         ylim = yy, xlim = xx, main = x$type)
    for (i in 1:elmt){
      graphics::lines(x$fit[, 1+i], col = cmp[i])
    }
    # Check if forecasts exist and plot
    if (!is.null(x$frc)){
      for (i in 1:elmt){
        graphics::lines((length(x$y)+1):xx[2], x$frc[, i+1], col=cmp[i])
      }
    }
    graphics::legend("topleft", c("Adoption", "Innovators", "Imitators")[1:elmt],
           col = cmp, lty = 1, bty = "n")
    
  } else {
    # Cumulative plot
    
    # Get yy min-max
    if (!is.null(x$frc)){
      yy <- range(c(cumsum(x$y), x$frc[, 1]))
    } else {
      yy <- range(cbind(cumsum(x$y), x$fit[, 1]))
    }
    yy <- yy + c(-1,1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    graphics::plot(cumsum(x$y), type = "p", pch = 20, ylab = "Cumulative Adoption",
         xlab="Period", ylim = yy, xlim = xx, main = x$method)
    graphics::lines(x$fit[, 1], col = cmp[1])
    
    if (type == "bass"){
      for (i in 1:2){
        graphics::lines(cumsum(x$fit[, 2+i]), col = cmp[i+1])
      }
    }
    
    # Check if forecasts exist and plot
    if (!is.null(x$frc)){
      fstart <- apply(x$fit, 2, cumsum)[length(x$y), 2:(1+elmt)]
      for (i in 1:elmt){
        graphics::lines((length(x$y)+1):xx[2],
              cumsum(x$frc[, i+1]) + fstart[i], col = cmp[i])
      }
    }
    graphics::legend("bottomright", c("Adoption", "Innovators", "Imitators")[1:elmt],
           col = cmp, lty = 1, bty = "n")
  }
}

#' Print a fitted diffusion curve.
#'
#' Outputs the result of a fitted diffusion curve.
#'
#' @param x \code{diffusion} object, produced using \code{\link{diffusion}}.
#' @param ... Unused argument.
#'
#' @return None. Console output only. 
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' @seealso \code{\link{diffusion}}.
#' @examples
#'  fit <- diffusion(tsChicken[, 2])
#'  print(fit)
#'
#' @export
#' @method print diffusion
print.diffusion <- function(x, ...){
  diffusionPrint(x, ...)
}

diffusionPrint <- function(x, ...){
  # Internal function: print console output for diffusion models
  # x, object estimated using diffusion
  
  type <- tolower(x$type)
  
  writeLines(paste(x$type, "model"))
  writeLines("")
  writeLines("Parameters:")

  if (is.null(x$prew)){
    # If fixed parameter solution will be activated NA will cause to display
    # 2nd condition with marginals fix below should handle it 
    # if (is.null(x$prew) | anyNA(x$prew == 0)){
    temp <- round(cbind(x$w, x$pval), 4)    
    colnames(temp) <- c("Estimate", "p-value")
  } else {
    temp <- round(cbind(x$w, x$w-x$prew, x$pval), 4)    
    colnames(temp) <- c("Estimate", "Marginal", " Marginal p-value")
  }
  
  switch(type,
         "bass" = rownames(temp) <- c("m - Market potential",
                                      "p - Coefficient of innovation",
                                      "q - Coefficient of imitation"),
         "gompertz" = rownames(temp) <- c("m - Market potential",
                                          "a - displacement",
                                          "b - growth"),
         "gsgompertz" = rownames(temp) <- c("m - Market potential",
                                            "a - displacement",
                                            "b - growth",
                                            "c - shift"),
         "weibull" = rownames(temp) <- c("m - Market potential",
                                         "a - scale",
                                         "b - shape")
         )
  
  print(temp)
  writeLines("")
  writeLines(paste("sigma:", round(sqrt(x$mse), 4)))
}

#' Calculates the values for various diffusion curves, given some parameters.
#' 
#' This function calculates the values of diffusion curves that can be of \code{"bass"}, 
#' \code{"gompertz"}, \code{"gsgompertz"} or \code{"weibull"} type, given some parameters. 
#' 
#' @param n number of periods to calculate values for.
#' @param w vector of curve parameters (see note). If argument curve is used, this is ignored.
#' @param type diffusion curve to use. This can be "bass", "gompertz" and "gsgompertz". If argument curve is used, this is ignored.
#' @param curve if provided \code{w} and \code{type} are taken from an object of class \code{diffusion}, the output of \code{\link{diffusion}}.
#' 
#' @return Returns a matrix of values with each row being a period.
#' 
#' @note \code{w} needs to be provided for the Bass curve in the order of
#'   \code{("m", "p", "q")}, where "p" is the coefficient of innovation, "q" is the
#'   coefficient of imitation and "m" is the market size coefficient.
#'   
#'   For the Gompertz curve, vector \code{w} needs to be in the form of
#'   \code{("m", "a", "b")}. Where "a" is the x-axis displacement coefficient, "b"
#'   determines the growth rate and "m" sets, similarly to Bass model, the
#'   market potential (saturation point).
#'   
#'   For the Shifted-Gompertz curve, vector \code{w} needs to be in the form of 
#'   \code{("m", "a", "b", "c")}. Where "a" is the x-axis displacement
#'   coefficient, "b" determines the growth rate, "c" is the shifting parameter
#'   and "m" sets, similarly to Bass model, the market potential (saturation
#'   point).
#'   
#'   For the Weibull curve, vector \code{w} needs to be in the form of
#'   \code{("m", "a", "b")}. Where "a" is the scale parameter, "b" determines the
#'   shape. Together, "a" and "b" determine the stepness of the curve. The "m"
#'   parameter sets the market potential (saturation point).
#'   
#' @examples 
#'   difcurve(w=c(0.01,0.1,10),20)
#'   
#' @seealso \code{\link{diffusion}} for fitting a diffusion curve.
#'   
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' 
#' @rdname difcurve 
#' @export difcurve
difcurve <- function(n, w = c(0.01, 0.1, 10),
                     type = c("bass", "gompertz", "gsgompertz", "weibull"),
                     curve = NULL){
  
  # Check inputs
  if (!is.null(curve)){
    if (is.diffusion(curve)){
      type <- curve$type
      w <- curve$w
    }
  } else {
    type <- match.arg(type, c("bass", "gompertz", "gsgompertz", "weibull"))
    if (type == "gsgompertz"){
      if (length(w) != 4){
        stop("gsgompertz requires 4 parameters.")
      }
    } else {
      if (length(w) != 3){
        stop("bass, gompertz and weibull require 3 parameters.")
      }
    }
  }
  
  if (n < 1){
    stop("At least 1 point must be generated!")
  }
  
  switch(type,
         "bass" = {y <- bassCurve(n, w)},
         "gompertz" = {y <- gompertzCurve(n, w)},
         "gsgompertz" = {y <- gsgCurve(n, w)},
         "weibull" = {y <- weibullCurve(n, w)}
         )
  
  return(y)
  
}

#' Predict future periods of a fitted diffusion curve.
#'
#' Calculates the values for h future periods of a fitted diffusion curve.
#'
#' @param object \code{diffusion} object, produced using \code{\link{diffusion}}.
#' @param h Forecast horizon. 
#' @param ... Unused argument.
#'
#' @return Returns an object of class \code{diffusion}, which contains:
#' \itemize{
#' \item \code{type} diffusion curve type used
#' \item \code{call} calls function fitted
#' \item \code{w} named vector of fitted parameters
#' \item \code{y} actuals
#' \item \code{fit} fitted values of model
#' \item \code{frc} forecasts for future periods.
#' \item \code{mse} insample Mean Squared Error
#' \item \code{prew} the \code{w} of the previous generation
#' \item \code{pval} p-values for \code{w}
#' }
#' 
#' @note This function populates the matrix frc of the \code{diffusion} object used as input.
#' 
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' @seealso \code{\link{diffusion}}.
#' @examples
#'  fit <- diffusion(tsChicken[, 2])
#'  fit <- predict(fit, 20)
#'  plot(fit)
#'
#' @export
#' @method predict diffusion
predict.diffusion <- function(object,h=10,...){
  # Calculate forecasts for fitted diffusion curves
  
  if (h < 1){
    stop("Horizon h must be positive integer.")
  }
  
  type <- object$type
  w <- object$w
  n <- length(object$y) + h
  
  switch(type,
         "bass" = {y <- bassCurve(n, w)},
         "gompertz" = {y <- gompertzCurve(n, w)},
         "gsgompertz" = {y <- gsgCurve(n, w)},
         "weibull" = {y <- weibullCurve(n, w)}
         )
  
  y <- y[(n-h+1):n, , drop = FALSE]
  
  object$frc <- y
  
  return(object)
}