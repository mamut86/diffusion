#' diffusion
#' 
#' \code{diffusion} enables to fit various diffusion models.
#' 
#' This function fits diffusion models of the type \code{"bass"}, 
#' \code{"gompertz"} or \code{"sgompertz"}. Parameters are estimated by 
#' minimising the Mean Squared Error with a Subplex algorithm from the nloptr
#' package. Optionally p-values of the coefficients can be determined via
#' bootstraping. Furthermore, the bootstrapping allows to remove insignificant
#' parameters from the optimisation process.
#' 
#' @section Bass model:
#' The optimisation of the Bass model is initialisated by the linear
#' aproximation suggested in Bass (1969).
#' 
#' @section Gompertz model:
#' The initialisation of the Gompertz model uses the approach suggested by Jukic
#' et al. (2004) but is adopted to allow for the non-exponential version of
#' Gompertz curve. This allows that m becomes Bass model equivalent. Hence for
#' the market potential the Bass model is used as an initialisation.
#' 
#' @section Shifted-Gompertz model:
#' The model is initialised by assuming the shift operator to be 1. At this 
#' point the model becomes Bass equivalent as shown in Bemmaor (1994). A Bass
#' model is therefore used as an estimator for the remaining parameters.
#' 
#' @param x vector with adoption per period
#' @param w vector of model parameters (see note). If provided no estimation
#'   is done.
#' @param cleanlead removes leading zeros for fitting purposes (default == T)
#' @param prew the \code{w} of the previous generation. This is used for
#'   sequential fitting.
#' @param l the l-norm (1 is absolute errors, 2 is squared errors)
#' @param cumulative If TRUE optimisation is done on cumulative adoption.
#' @param pvalreps bootstrap repetitions to estimate (marginal) p-values
#' @param eliminate if TRUE eliminates insignificant parameters from the
#'   estimation. Forces \code{pvalreps = 1000} if left to 0.
#' @param sig significance level used to eliminate parameters
#' @param verbose if TRUE console output is provided during estimation (default
#'   == F)
#' @param type diffusion model to use. This can be "bass", "gompertz" and "sgompertz"
#' @param optim optimization method to use. This can be "nm" for Nelder-Meade or "hj" for Hooke-Jeeves.
#' @param maxiter number of iterations the optimser takes (default ==
#'   \code{10000} for "nm" and \code{Inf} for "hj")
#' @param opttol Tolerance for convergence (default == 1.e-06)
#' 
#' @return returns list of:
#' \itemize{
#' \item \code{type} diffusion model type used
#' \item \code{call} calls function fitted
#' \item \code{w} named vector of fitted parameters
#' \item \code{x} actuals
#' \item \code{fit} fitted values of model
#' \item \code{mse} insample Mean Squared Error
#' \item \code{prew} the \code{w} of the previous generation
#' \item \code{pval} p-values for \code{w}
#' }
#' 
#' @note vector \code{w} needs to be provided for the Bass model in the order of
#'   \code{"p", "q", "m"}, where "p" is the coefficient of innovation, "q" is the
#'   coeficient of imitation and "m" is the market size coefficient.
#'   
#'   For the Gompertz model vector \code{w} needs to be in the form of
#'   \code{("a", "b", "m")}. Where "a" is the x-axis displacement coefficient, "b"
#'   determines the growth rate and "m" sets, similarly to Bass model, the
#'   market potential (saturation point).
#'   
#'   For the Shifted-Gompertz model vector \code{w} needs to be in the form of 
#'   \code{("a", "b", "c", "m")}. Where "a" is the x-axis displacement
#'   coefficient, "b" determines the growth rate, "c" is the shifting parameter
#'   and "m" sets, similarly to Bass model, the market potential (saturation
#'   point).
#'   
#' @example examples/example_diffusion.R
#' 
#' @references Bass, F.M., 1969. A new product growth for model consumer
#'   durables. Management Science 15(5), 215-227.
#'   
#' @references Bemmaor, A. 1994. Modeling the Diffusion of New Durable Goods:
#'   Word-of-Mouth Effect versus Consumer Heterogeneity. In G. Laurent, G.L.
#'   Lilien and B. Pras (Eds.). Research Traditions in Marketing. Boston:
#'   Kluwer, pp. 201-223.
#' 
#' @references Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares
#'   fitting Gompertz curve. Journal of Computational and Applied Mathematics,
#'   169, 359-375.
#'   
#' @seealso \code{\link{seqdiffusion}} for sequential diffusion model fitting
#'   across product generations.
#'   
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' 
#' @rdname diffusion  
#' @export diffusion
diffusion <- function(x, w = NULL, cleanlead = c(TRUE, FALSE), prew = NULL,
                      l = 2, cumulative = c(TRUE, FALSE), pvalreps = 0, 
                      eliminate = c(FALSE, TRUE), sig = 0.05, verbose = c(FALSE, TRUE),
                      type = c("bass", "gompertz", "sgompertz"),
                      optim = c("nm", "hj"), maxiter = Inf, opttol = 1.e-06){

  type <- match.arg(type, c("bass", "gompertz", "sgompertz"))
  optim <- match.arg(optim, c("nm", "hj"))
  
  cleanlead <- cleanlead[1]
  if (cleanlead == TRUE){
    x <- cleanzero(x)$x
  }
  
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  
  # Optimise parameters
  if (is.null(w)){

    opt <- diffusionEstim(x, l, cumulative, prew, pvalreps, eliminate,
                          sig, verbose, type = type, optim  = optim,
                          maxiter = maxiter)

    w <- opt$w
    pval <- opt$pval
  } else {
    pval <- rep(NA, length(w))
  }
  
  n <- length(x)
  switch(type,
         "bass" = fit <- bassCurve(n, w),
         "gompertz" = fit <- gompertzCurve(n, w),
         "sgompertz" = fit <- sgompertzCurve(n, w))
  
  mse <- mean((x - fit[,2])^2)
  
  switch(type,
         "bass" = out <- structure(list("type" = "Bass", "call" = sys.call(),
                                      "w" = w, "x" = x, "fit" = fit,
                                      "mse" = mse, "prew" = prew, "pval" = pval),
                                 class = "bass"),
         "gompertz" = out <- structure(list("type" = "Gompertz", "call" = sys.call(),
                                          "w" = w, "x" = x, "fit" = fit,
                                          "mse" = mse, "prew" = prew, "pval" = pval),
                                     class = "gompertz"),
         "sgompertz" = out <- structure(list("type" = "SGompertz", "call" = sys.call(),
                                           "w" = w, "x" = x, "fit" = fit,"mse" = mse,
                                           "prew" = prew, "pval" = pval),
                                      class = "sgompertz"))
  return(out)
}


diffusionEstim <- function(x, l = 2, cumulative = c(FALSE, TRUE),
                           prew = NULL, pvalreps = 0,
                           eliminate = c(FALSE, TRUE), sig = 0.05,
                           verbose = c(FALSE, TRUE),
                           type = c("bass", "gompertz", "sgompertz"),
                           optim = c("nm", "hj"), maxiter = Inf, opttol = 1.e-06)
                          {
  # Internal function: estimate bass parameters 
  # x, adoption per period
  # l, the l-norm (1 is absolute errors, 2 is squared errors)
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
  
  type <- match.arg(type,c("bass", "gompertz", "sgompertz"))
  optim <- match.arg(optim,c("nm", "hj"))
  
  # Defaults 
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  
  # determine how many paramters needed
  if (type == "bass" | type == "gompertz"){
    no.w <- 3
  } else if (type == "sgompertz"){
    no.w <- 4
  }
  
  if (eliminate == TRUE & pvalreps == 0){
    pvalreps <- 1000
    warning("To eliminate parameters p-values must be estimated. Setting pvalreps = 1000.")
  }
  
  # Initially all parameters are estimated
  w.idx <- rep(TRUE, no.w)         # Which parameters to estimate 
  
  # Check botstrap repetitions (pvalreps)
  if (pvalreps > 0 & pvalreps < 500){
    warning("Very few bootstraps, unreliable p-values.")
  }
  if (pvalreps < 0){
    stop("pvalreps must be positive.")
  }
  if (pvalreps == 0 & eliminate == TRUE){
    stop("To eliminate coefficients from the estimation p-values need to be calculated. Use positive pvalreps.")
  }
  
  # Initialise
  if (is.null(prew)){
    prew <- rep(0, no.w)
  }
  
  switch(type,
         "bass" = init <- bassInit(x),
         "gompertz" = init <- gompertzInit(x, l),
         "sgompertz" = init <- sgompertzInit(x, l))
  
  init <- init - prew
  init[(init + prew) <= 0] <- 0.00001
  n <- length(x)
  
  # Iterate until all p-values are < sig
  # If eliminate is not requested it will only iterate once
  elim <- TRUE
  it <- 1
  
  while (elim == TRUE){
    
    # Optimise
    w <- rep(0, no.w)
    
    if (sum(w.idx) > 1){
      # This optimisation algorithms are multidimensional, so revert to BFGS if needed
      
      if (optim == "nm"){
        
        # set maximum iterations (see documentation of dfotpim::nmk)
        if (maxiter < 5000){
          #if (20*length(w.idx)^2 > 1500) {
          maxiter <- 5000
          warning("Set maxiter to 5'000 for Naelder-Maed optimiser")
        } else if (maxiter == Inf) {
          maxiter <- 10000
        }
          
        # Nelder-Meade works typically quite well
        switch(type,
               "bass" = w.new <- dfoptim::nmk(par = init[w.idx], fn = bassCost,
                                            control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                            x = x, l = l, prew = prew,
                                            cumulative = cumulative, w.idx = w.idx)$par,
               "gompertz" = w.new <- dfoptim::nmk(par = init[w.idx], fn = gompertzCost,
                                                control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                                x = x, l = l, prew = prew,
                                                cumulative=cumulative, w.idx = w.idx)$par,
               "sgompertz" = w.new <- dfoptim::nmk(par = init[w.idx], fn = sgompertzCost,
                                                 control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                                 x = x, l = l, prew = prew,
                                                 cumulative = cumulative, w.idx = w.idx)$par)
      } else {
        
        # there is some problem that s gompertz switches to very large variables
        # one way would be to set bounds but this does necessarliy mean that the
        # bounds are correct. Use hjkb function for this but it also needs lower
        # value to be set as there seems to be an error
        # up <- c(5000, 100, 1000, 1e20)
        # lo <- -Inf
        # print(init[w.idx])
        
        if (maxiter < 100000){
          maxiter <- 100000
          warning("Set maxiter to 100'000 for hj optimiser")
        }
        
        # Hooker-Jeeves is slow but optimises tough stuff
        switch(type,

               "bass" = w.new <- dfoptim::hjk(par = init[w.idx], fn = bassCost,
                                            control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                            x = x, l = l, prew = prew,
                                            cumulative = cumulative, w.idx = w.idx)$par,
               "gompertz" = w.new <- dfoptim::hjk(par = init[w.idx],
                                                fn = gompertzCost,
                                                control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                                x = x, l = l, prew = prew,
                                                cumulative = cumulative, w.idx = w.idx)$par,
               "sgompertz" = w.new <- dfoptim::hjk(par = init[w.idx],
                                                 fn = sgompertzCost,
                                                  control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                                  x = x, l = l, prew = prew,
                                                 cumulative = cumulative, w.idx = w.idx)$par
               # "sgompertz" = opt <- dfoptim::hjkb(par = init[w.idx], fn = sgompertzCost, upper = up, lower = lo,
               #                                   control = list(maxfeval = maxiter, tol = opttol, info = T),
               #                                   x = x, l = l, prew = prew,
               #                                   w.idx = w.idx)
        )

      }
        
    } else {
      # Revert to BFGS if only one parameter is required
      # Max iterations included in the BFGS
      
      switch(type,
             "bass" = w.new <- optim(init[w.idx], bassCost, method = "BFGS", x = x, l = l,
                                   cumulative = cumulative, prew = prew, w.idx = w.idx)$par,
             "gompertz" = w.new <- optim(init[w.idx], gompertzCost, method = "BFGS", x = x, l = l,
                                       cumulative = cumulative, prew = prew, w.idx = w.idx)$par,
             "sgompertz" = w.new <- optim(init[w.idx], sgompertzCost, method = "BFGS", x = x, l = l,
                                        cumulative = cumulative, prew = prew, w.idx = w.idx)$par)

    }
    
    w[w.idx] <- w.new
    
    # Bootstrap p-values
    if (pvalreps > 0){
      
      switch(type,
             "bass" = yhat <- bassCurve(n, prew+w)[, 2],
             "gompertz" = yhat <- gompertzCurve(n, prew+w)[, 2],
             "sgompertz" = yhat <- sgompertzCurve(n, prew+w)[, 2])
      
      sigma <- sqrt(mean((x - yhat)^2))
      
      # Construct bootstraps
      wboot <- array(NA, c(pvalreps, no.w))
      yboot <- matrix(rnorm(n*pvalreps, 0, sigma), nrow = n) +
        matrix(rep(yhat, pvalreps), ncol = pvalreps)
      
      # This needs to become multiplicative
      yboot[yboot < 0] <- 0.001
      
      # Estimate model
      for (i in 1:pvalreps){
        switch(type,
               "bass" = wboot[i,] <- diffusionEstim(yboot[, i], l, pvalreps = 0,
                                                    type = "bass")$w - prew,
               "gompertz" = wboot[i,] <- diffusionEstim(yboot[, i], l, pvalreps = 0,
                                                        type = "gompertz")$w - prew,
               "sgompertz" = wboot[i,] <- diffusionEstim(yboot[, i], l, pvalreps = 0,
                                                         type = "sgompertz")$w - prew)
      }

      pval <- colMeans((abs(wboot - 
                              matrix(rep(colMeans(wboot), pvalreps),
                                     ncol = no.w, byrow = T))) > 
                         abs(matrix(rep(w, pvalreps), ncol = no.w, byrow = T)))
    } else {
      pval <- rep(NA, no.w)
    }
    
    # Elimination process
    if (eliminate == TRUE & any(pval[w.idx] > sig)){
      # Find most insignificant
      pval.temp <- pval
      pval.temp[pval.temp < sig] <- sig
      pval.temp <- pval.temp - sig
      pval.temp[!w.idx] <- 0
      loc <- which(pval.temp == max(pval.temp))[1]
      w.idx[loc] <- FALSE
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
      
      temp <- cbind(round(cbind(w, pval), 4), locv)
      
      switch(type,
             "bass" = rownames(temp) <- c("p", "q", "m"),
             "gompertz" = rownames(temp) <- c("a", "b", "m"),
             "sgompertz" = rownames(temp) <- c("a", "b", "c", "m"))
      
      colnames(temp) <- c("Estimate", "p-value", "")[1:(2+!is.na(loc))]
      print(temp, quote = FALSE)
      
      if (elim == FALSE){
        writeLines("Estimation completed")
      }
      
      writeLines("")
    }
  }
  
  w <- w + prew
  names(w) <- names(init)
  
  names(pval) <- names(w)
  return(list("w" = w, "pval" = pval))
  
}

diffusionPlot <- function(x, cumulative = c(FALSE, TRUE), ...){
  # Plot diffusion curves
  # x, object estimated using diffusion
  # cumulative, if TRUE plot cumulative adoption
  
  type <- class(x)
  # set numbers of elements to be plotted, i.e. including innov. and immiat.
  switch(type,
         "bass" = elmt <- 3,
         "gompertz" = elmt <- 1,
         "sgompertz" = elmt <- 1)
  
  cumulative <- cumulative[1]
  
  # Colorbrewer colours
  cmp <- c("#E41A1C", "#377EB8", "#4DAF4A")
  # Check if forecasts exist and construct xx
  if (exists("xhat", where = x)){
    xx <- c(1, (length(x$x) + length(x$mean)))
  } else {
    xx <- c(1, (length(x$x)))
  }
  
  if (cumulative == FALSE){
    
    # Get yy min-max
    yy <- range(cbind(x$x, x$fit[, 2:(1 + elmt)]))
    yy <- yy + c(-1, 1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    plot(x$x,type="p", pch = 20, ylab = "Adoption", xlab = "Period",
         ylim = yy, xlim = xx, main = x$method)
    for (i in 1:elmt){
      lines(x$fit[, 1+i], col = cmp[i])
    }
    # Check if forecasts exist and plot
    if (exists("xhat", where = x)){
      for (i in 1:elmt){
        lines((length(x$x)+1):xx[2], x$xhat[, i+1], col=cmp[i])
      }
    }
    legend("topleft", c("Adoption", "Innovators", "Imitators")[1:elmt],
           col = cmp, lty = 1, bty = "n")
    
  } else {
    # Cumulative plot
    
    # Get yy min-max
    yy <- range(cbind(cumsum(x$x), x$fit[, 1]))
    yy <- yy + c(-1,1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    plot(cumsum(x$x), type = "p", pch = 20, ylab = "Cumulative Adoption",
         xlab="Period", ylim=yy, xlim = xx, main = x$method)
    lines(x$fit[, 1], col=cmp[1])
    
    if (type == "bass"){
      for (i in 1:2){
        lines(cumsum(x$fit[, 2+i]), col = cmp[i+1])
      }
    }
    
    # Check if forecasts exist and plot
    if (exists("xhat", where = x)){
      fstart <- apply(x$fit, 2, cumsum)[length(x$x), 2:(1+elmt)]
      for (i in 1:elmt){
        lines((length(x$x)+1):xx[2],
              cumsum(x$xhat[, i+1]) + fstart[i], col = cmp[i])
      }
    }
    legend("bottomright", c("Adoption", "Innovators", "Imitators")[1:elmt],
           col = cmp, lty = 1, bty = "n")
  }
}

diffusionPrint <- function(x, ...){
  # Print console output for diffusion models
  # x, object estimated using diffusion
  
  type <- tolower(x$type)
  
  writeLines(paste(x$type, "model"))
  writeLines("")
  writeLines("Parameters:")
  
  if (is.null(x$prew)){
    temp <- round(cbind(x$w, x$pval), 4)    
    colnames(temp) <- c("Estimate", "p-value")
  } else {
    temp <- round(cbind(x$w, x$w-x$prew, x$pval), 4)    
    colnames(temp) <- c("Estimate", "Marginal", " Marginal p-value")
  }
  
  switch(type,
         "bass" = rownames(temp) <- c("p - Coefficient of innovation",
                                      "q - Coefficient of imitation",
                                      "m - Market potential"),
         "gompertz" = rownames(temp) <- c("a - displacement",
                                          "b - growth",
                                          "m - Market potential"),
         "sgompertz" = rownames(temp) <- c("a - displacement",
                                           "b - growth",
                                           "c - shift",
                                           "m - Market potential"))
  
  print(temp)
  writeLines("")
  writeLines(paste("sigma:", round(sqrt(x$mse), 4)))
}
