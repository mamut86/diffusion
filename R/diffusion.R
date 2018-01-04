#' Fit various diffusion curves.
#' 
#' This function fits diffusion curves that can be of \code{"bass"}, 
#' \code{"gompertz"} or \code{"gsgompertz"} type. 
#' 
#' @section Bass curve:
#' The optimisation of the Bass curve is initialisated by the linear
#' aproximation suggested in Bass (1969).
#' 
#' @section Gompertz curve:
#' The initialisation of the Gompertz curve uses the approach suggested by Jukic
#' et al. (2004), but is adapted to allow for the non-exponential version of
#' Gompertz curve. This makes the market potential parameter equivalent to the Bass curves's 
#' and the market potential from Bass curve is used for initialisation.
#' 
#' @section Gamma/Shifted Gompertz (G/SG):
#' The curve is initialised by assuming the shift operator to be 1 and 
#' becomes equivalent to the Bass curve, as shown in Bemmaor (1994). A Bass
#' curve is therefore used as an estimator for the remaining initial parameters.
#' 
#' @param x vector with adoption per period
#' @param w vector of curve parameters (see note). If provided no estimation
#'   is done.
#' @param cleanlead removes leading zeros for fitting purposes (default == TRUE)
#' @param prew Experimental. Ignore!
# #'  the \code{w} of the previous generation. This is used for
# #'  sequential fitting.
#' @param l the l-norm (1 is absolute errors, 2 is squared errors).
#' @param cumulative If TRUE optimisation is done on cumulative adoption.
#' @param pvalreps Experimental. Ignore!
# #' bootstrap repetitions to estimate (marginal) p-values
#' @param eliminate Experimental. Ignore!
# #'   if TRUE eliminates insignificant parameters from the
# #'   estimation. Forces \code{pvalreps = 1000} if left to 0.
#' @param sig Experimental. Ignore! 
# #' significance level used to eliminate parameters
#' @param verbose if TRUE console output is provided during estimation (default
#'   == FALSE)
#' @param type diffusion curve to use. This can be "bass", "gompertz" and "gsgompertz"
#' @param optim optimization method to use. This can be "nm" for Nelder-Meade or "hj" for Hooke-Jeeves.
#' @param maxiter number of iterations the optimser takes (default ==
#'   \code{10000} for "nm" and \code{Inf} for "hj")
#' @param opttol Tolerance for convergence (default == 1.e-06)
#' 
#' @return Returns an object of class \code{diffusion}, which contains:
#' \itemize{
#' \item \code{type} diffusion curve type used
#' \item \code{call} calls function fitted
#' \item \code{w} named vector of fitted parameters
#' \item \code{x} actuals
#' \item \code{fit} fitted values of model
#' \item \code{frc} forecasts for future periods. This is \code{NULL} until \code{\link{predict.diffusion}} is called.
#' \item \code{mse} insample Mean Squared Error
#' \item \code{prew} the \code{w} of the previous generation
#' \item \code{pval} p-values for \code{w}
#' }
#' 
#' @note vector \code{w} needs to be provided for the Bass curve in the order of
#'   \code{"p", "q", "m"}, where "p" is the coefficient of innovation, "q" is the
#'   coefficient of imitation and "m" is the market size coefficient.
#'   
#'   For the Gompertz curve, vector \code{w} needs to be in the form of
#'   \code{("a", "b", "m")}. Where "a" is the x-axis displacement coefficient, "b"
#'   determines the growth rate and "m" sets, similarly to Bass model, the
#'   market potential (saturation point).
#'   
#'   For the Shifted-Gompertz curve, vector \code{w} needs to be in the form of 
#'   \code{("a", "b", "c", "m")}. Where "a" is the x-axis displacement
#'   coefficient, "b" determines the growth rate, "c" is the shifting parameter
#'   and "m" sets, similarly to Bass model, the market potential (saturation
#'   point).
#'   
#' @example examples/example_diffusion.R
#' 
#' @references
#' \itemize{
#' \item{For an introduction to diffusion curves see: Ord K., Fildes R., Kourentzes N. (2017) \href{http://kourentzes.com/forecasting/2017/10/16/new-forecasting-book-principles-of-business-forecasting-2e/}{Principles of Business Forecasting 2e}. \emph{Wessex Press Publishing Co.}, Chapter 12.}
#' \item{Bass, F.M., 1969. A new product growth for model consumer durables. Management Science 15(5), 215-227.}
#' \item{Bemmaor, A. 1994. Modeling the Diffusion of New Durable Goods: Word-of-Mouth Effect versus Consumer Heterogeneity. In G. Laurent, G.L. Lilien and B. Pras (Eds.). Research Traditions in Marketing. Boston: Kluwer, pp. 201-223.}
#' \item{Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares fitting Gompertz curve. Journal of Computational and Applied Mathematics, 169, 359-375.}
#' }
#'   
#' @seealso \code{\link{predict.diffusion}}, \code{\link{plot.diffusion}} and \code{\link{print.diffusion}}.   
#'   
#' @note Parameters are estimated by 
#' minimising the Mean Squared Error with a Subplex algorithm from the nloptr
#' package. 
# #' Optionally p-values of the coefficients can be determined via
# #' bootstraping. Furthermore, the bootstrapping allows to remove insignificant
# #' parameters from the optimisation process.   
#'   
# #' @seealso \code{\link{seqdiffusion}} for sequential diffusion model fitting
# #'   across product generations.
#'   
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' 
#' @rdname diffusion  
#' @export diffusion
diffusion <- function(x, w = NULL, cleanlead = c(TRUE, FALSE), prew = NULL,
                      l = 2, cumulative = c(TRUE, FALSE), pvalreps = 0, 
                      eliminate = c(FALSE, TRUE), sig = 0.05, verbose = c(FALSE, TRUE),
                      type = c("bass", "gompertz", "gsgompertz"),
                      optim = c("nm", "hj"), maxiter = Inf, opttol = 1.e-06){

  type <- match.arg(type, c("bass", "gompertz", "gsgompertz"))
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
         "gsgompertz" = fit <- gsgCurve(n, w))
  
  mse <- mean((x - fit[,2])^2)
  
  out <- structure(list("type" = type, "call" = sys.call(),
                        "w" = w, "x" = x, "fit" = fit, "frc" = NULL, 
                        "mse" = mse, "prew" = prew, "pval" = pval), class="diffusion")
  return(out)
}


diffusionEstim <- function(x, l = 2, cumulative = c(FALSE, TRUE),
                           prew = NULL, pvalreps = 0,
                           eliminate = c(FALSE, TRUE), sig = 0.05,
                           verbose = c(FALSE, TRUE),
                           type = c("bass", "gompertz", "gsgompertz"),
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
  
  type <- match.arg(type,c("bass", "gompertz", "gsgompertz"))
  optim <- match.arg(optim,c("nm", "hj"))
  
  # Defaults 
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  
  # determine how many paramters needed
  if (type == "bass" | type == "gompertz"){
    no.w <- 3
  } else if (type == "gsgompertz"){
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
         "gsgompertz" = init <- gsgInit(x, l))
  
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
                                            control = list(tol = opttol, maxfeval = maxiter),
                                            x = x, l = l, prew = prew,
                                            cumulative = cumulative, w.idx = w.idx)$par,
               "gompertz" = w.new <- dfoptim::nmk(par = init[w.idx], fn = gompertzCost,
                                                control = list(maxfeval = maxiter, tol = opttol),
                                                x = x, l = l, prew = prew,
                                                cumulative=cumulative, w.idx = w.idx)$par,
               "gsgompertz" = w.new <- dfoptim::nmk(par = init[w.idx], fn = gsgCost,
                                                 control = list(maxfeval = maxiter, tol = opttol),
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
               "gsgompertz" = w.new <- dfoptim::hjk(par = init[w.idx],
                                                 fn = gsgCost,
                                                  control = list(maxfeval = maxiter, tol = opttol, info = verbose),
                                                  x = x, l = l, prew = prew,
                                                 cumulative = cumulative, w.idx = w.idx)$par
               # "gsgompertz" = opt <- dfoptim::hjkb(par = init[w.idx], fn = gsgCost, upper = up, lower = lo,
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
             "gsgompertz" = w.new <- optim(init[w.idx], gsgCost, method = "BFGS", x = x, l = l,
                                        cumulative = cumulative, prew = prew, w.idx = w.idx)$par)

    }
    
    w[w.idx] <- w.new
    
    # Bootstrap p-values
    if (pvalreps > 0){
      
      switch(type,
             "bass" = yhat <- bassCurve(n, prew+w)[, 2],
             "gompertz" = yhat <- gompertzCurve(n, prew+w)[, 2],
             "sgompertz" = yhat <- gsgCurve(n, prew+w)[, 2])
      
      sigma <- sqrt(mean((x - yhat)^2))
      
      # Construct bootstraps
      wboot <- array(NA, c(pvalreps, no.w))
      yboot <- matrix(stats::rnorm(n*pvalreps, 0, sigma), nrow = n) +
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
               "gsgompertz" = wboot[i,] <- diffusionEstim(yboot[, i], l, pvalreps = 0,
                                                         type = "gsgompertz")$w - prew)
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
             "gsgompertz" = rownames(temp) <- c("a", "b", "c", "m"))
      
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
#'  fit <- diffusion(tschicken[, 2])
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
         "gsgompertz" = elmt <- 1)
  
  cumulative <- cumulative[1]
  
  # Colorbrewer colours
  cmp <- c("#E41A1C", "#377EB8", "#4DAF4A")
  # Check if forecasts exist and construct xx
  if (!is.null(x$frc)){
    xx <- c(1, (length(x$x) + dim(x$frc)[1]))
  } else {
    xx <- c(1, (length(x$x)))
  }
  
  if (cumulative == FALSE){
    
    # Get yy min-max
    if (!is.null(x$frc)){
      yy <- range(c(x$x, x$fit[, 2:(1 + elmt)], x$frc[, 2:(1 + elmt)]))
    } else {
      yy <- range(cbind(x$x, x$fit[, 2:(1 + elmt)]))
    }
    yy <- yy + c(-1, 1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    graphics::plot(x$x,type="p", pch = 20, ylab = "Adoption", xlab = "Period",
         ylim = yy, xlim = xx, main = x$method)
    for (i in 1:elmt){
      graphics::lines(x$fit[, 1+i], col = cmp[i])
    }
    # Check if forecasts exist and plot
    if (!is.null(x$frc)){
      for (i in 1:elmt){
        graphics::lines((length(x$x)+1):xx[2], x$frc[, i+1], col=cmp[i])
      }
    }
    graphics::legend("topleft", c("Adoption", "Innovators", "Imitators")[1:elmt],
           col = cmp, lty = 1, bty = "n")
    
  } else {
    # Cumulative plot
    
    # Get yy min-max
    if (!is.null(x$frc)){
      yy <- range(c(cumsum(x$x), x$frc[, 1]))
    } else {
      yy <- range(cbind(cumsum(x$x), x$fit[, 1]))
    }
    yy <- yy + c(-1,1) * 0.04 * diff(yy)
    yy[1] <- max(0, yy[1])
    
    # Plot fit
    graphics::plot(cumsum(x$x), type = "p", pch = 20, ylab = "Cumulative Adoption",
         xlab="Period", ylim=yy, xlim = xx, main = x$method)
    graphics::lines(x$fit[, 1], col=cmp[1])
    
    if (type == "bass"){
      for (i in 1:2){
        graphics::lines(cumsum(x$fit[, 2+i]), col = cmp[i+1])
      }
    }
    
    # Check if forecasts exist and plot
    if (!is.null(x$frc)){
      fstart <- apply(x$fit, 2, cumsum)[length(x$x), 2:(1+elmt)]
      for (i in 1:elmt){
        graphics::lines((length(x$x)+1):xx[2],
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
#'  fit <- diffusion(tschicken[, 2])
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
         "gsgompertz" = rownames(temp) <- c("a - displacement",
                                           "b - growth",
                                           "c - shift",
                                           "m - Market potential"))
  
  print(temp)
  writeLines("")
  writeLines(paste("sigma:", round(sqrt(x$mse), 4)))
}

#' Calculates the values for various diffusion curves, given some parameters.
#' 
#' This function calculates the values of diffusion curves that can be of \code{"bass"}, 
#' \code{"gompertz"} or \code{"gsgompertz"} type, given some parameters. 
#' 
#' @param n number of periods to calculate values for.
#' @param w vector of curve parameters (see note). If argument curve is used, this is ignored.
#' @param type diffusion curve to use. This can be "bass", "gompertz" and "gsgompertz". If argument curve is used, this is ignored.
#' @param curve if provided \code{w} and \code{type} are taken from an object of class \code{diffusion}, the output of \code{\link{diffusion}}.
#' 
#' @return Returns a matrix of values with each row being a period.
#' 
#' @note \code{w} needs to be provided for the Bass model in the order of
#'   \code{"p", "q", "m"}, where "p" is the coefficient of innovation, "q" is the
#'   coefficient of imitation and "m" is the market size coefficient.
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
difcurve <- function(n, w=c(0.01,0.1,10), type=c("bass", "gompertz", "gsgompertz"),curve=NULL){
  
  # Check inputs
  if (!is.null(curve)){
    if (class(curve) == "diffusion"){
      type <- curve$type
      w <- curve$w
    }
  } else {
    type <- match.arg(type, c("bass", "gompertz", "gsgompertz"))
    if (type == "gsgompertz"){
      if (length(w) != 4){
        stop("gsgompertz requires 4 parameters.")
      }
    } else {
      if (length(w) != 3){
        stop("bass and gompertz require 3 parameters.")
      }
    }
  }
  
  if (n < 1){
    stop("At least 1 point must be generated!")
  }
  
  switch(type,
         "bass" = {y <- bassCurve(n, w)},
         "gompertz" = {y <- gompertzCurve(n, w)},
         "gsgompertz" = {y <- gsgCurve(n, w)})
  
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
#' \item \code{x} actuals
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
#'  fit <- diffusion(tschicken[, 2])
#'  fti <- predict(fit,20)
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
  n <- length(object$x) + h
  
  switch(type,
         "bass" = {y <- bassCurve(n, w)},
         "gompertz" = {y <- gompertzCurve(n, w)},
         "gsgompertz" = {y <- gsgCurve(n, w)})
  
  y <- y[(n-h):n,]
  
  object$frc <- y
  
  return(object)
  
}
