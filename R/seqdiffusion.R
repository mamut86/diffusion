#' Enables fitting various sequential diffusion curves.
#' 
#' This function fits diffusion curves of the type \code{"bass"},
#' \code{"gompertz"}, \code{gsgompertz} or \code{weibull} across generations.
#' Parameters are estimated for each generation individually by minimizing the
#' Mean Squared Error with subplex algorithms from the optimx package.
#' Optionally p-values of the coefficients can be determined via bootstraping.
#' Furthermore, the bootstrapping allows removing insignificant parameters from
#' the optimization process.
#' 
#' @inheritSection diffusion Bass curve
#' @inheritSection diffusion Gompertz curve
#' @inheritSection diffusion Gamma/Shifted Gompertz
#' @inheritSection diffusion Weibull
#' 
#' @param y matrix containing in each column the adoption per period for generation k
#' @param w vector of curve parameters (see note). Parameters set to NA will be
#'   optimized. If \code{w = NULL} (default) all paramters are optimized.
#' @param cleanlead removes leading zeros for fitting purposes (default == T)
#' @param loss the l-norm (1 is absolute errors, 2 is squared errors)
#' @param cumulative If TRUE optimisation is done on cumulative adoption.
#' @param pvalreps bootstrap repetitions to estimate (marginal) p-values
#' @param eliminate if TRUE eliminates insignificant parameters from the
#'   estimation. Forces \code{pvalreps = 1000} if left to 0.
#' @param sig significance level used to eliminate parameters
#' @param verbose if TRUE console output is provided during estimation (default
#'   == F)
#' @param type of diffusion curve to use. This can be "bass", "gompertz" and
#'   "gsgompertz"
#' @param optim optimization method to use. This can be "nm" for Nelder-Meade or
#'   "hj" for Hooke-Jeeves. #' @param maxiter number of iterations the optimser
#'   takes (default == \code{10000} for "nm" and \code{Inf} for "hj")
#' @param opttol Tolerance for convergence (default == 1.e-06)
#' @param optsol when \code{"multi"} multiple optmisation solutions from different initialisations of the market parameter are used (default == \code{"single"})
#' @param initpar vector of initalisation parameters. If set to \code{preset} a predfined set of internal initalisation parameters is used while \code{"linearize"} uses linearized initalisation methods (default == \code{"linearize"}.
#' @param mscal scales market potential at initalisation with the maximum of the observed market potential for better optimisation results (default == \code{TRUE})
#' 
#' 
#' @return Returns an object of class \code{seqdiffusion}, which contains:
#' \itemize{
#' \item \code{type} diffusion model type used
#' \item \code{diffusion} returns model specification for each generation (see
#' \code{\link{diffusion}} for details)
#' \item \code{call} calls function fitted
#' \item \code{w} named matrix of fitted parameters for each generation
#' \item \code{y} matrix of actuals
#' \item \code{mse} insample Mean Squared Error for each generation
#' \item \code{pval} all p-values for \code{w} at each generation
#' }
#' 
# #' @inherit diffusion note
#'   
#' @examples 
#'   fit <- seqdiffusion(tsIbm)
#'   plot(fit)
#' 
#' @inherit diffusion references
#' 
#' @seealso \code{\link{plot.seqdiffusion}} and \code{\link{print.seqdiffusion}}.   
#'   
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' 
#' @keywords internal
#' 
#' @rdname seqdiffusion  
#' @export seqdiffusion
seqdiffusion <- function(y, cleanlead = c(TRUE, FALSE), loss = 2,
                         cumulative = c(TRUE, FALSE),
                         pvalreps = 0, eliminate = c(FALSE, TRUE), sig = 0.05, 
                         verbose = c(FALSE, TRUE),
                         type = c("bass", "gompertz", "gsgompertz", "weibull"),
                         optim = c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa"),
                         maxiter = 500, opttol = 1.e-06, optsol = c("single", "multi"),
                         initpar = c("linearize", "preset"), mscal = c(TRUE, FALSE), ...) {
  
  type <- match.arg(type[1], c("bass", "gompertz", "gsgompertz", "weibull"))
  optim <- match.arg(optim[1], c("L-BFGS-B", "Nelder-Mead", "BFGS", "hjkb", "Rcgmin", "bobyqa", "nm", "hj"))
  optsol <- match.arg(optsol[1], c("single", "multi"))
  if (!is.numeric(initpar)){
    initpar <- match.arg(initpar[1], c("preset", "linearize", "linearise"))
  }
  
  # check deprecated arguments doesn't work somehow
  el <- list(...)
  if (!is.null(el$l)) {
    warning("Argument \"l\" has been deprecated and replaced by \"loss\"")
    loss <- l
  } else if(!is.null(el$x)) {
    warning("Argument \"x\" has been deprecated and replaced by \"y\"")
    y <- x
  }
  
  cumulative <- cumulative[1]
  eliminate <- eliminate[1]
  verbose <- verbose[1]
  cleanlead <- cleanlead[1] # note dependency in seqdiffusion plot
  mscal <- mscal[1]

  # Number of curves
  k <- dim(y)[2]
  
  fit <- vector("list", k)
  names(fit) <- paste0("Gen", 1:k)
  
  # Fit iteratively across generations
  for (i in 1:k) {
    
    if (verbose == TRUE) {
      writeLines(paste0("Generation ", i))
    }
    if (i > 1) {
      prew <- fit[[i-1]]$w
      elimin <- eliminate
      pvalr <- pvalreps
    } else {
      elimin <- FALSE
      pvalr <- 0
      prew <- c(0,0,0)
    }
    
    fit[[i]] <- diffusion(y[, i], w = NULL, cleanlead, loss, cumulative,
                          verbose, type, optim, maxiter,
                          opttol, optsol, initpar=initpar, mscal, 
                          pvalreps=pvalr, eliminate=elimin, sig=sig, prew=prew)
  
  }
  
  allw <- do.call(rbind, lapply(fit, function(x) {x$w}))
  allmse <- do.call(rbind, lapply(fit, function(x) {x$mse}))
  allpval <- do.call(rbind, lapply(fit, function(x) {x$pval}))
  
  type <- paste("Sequential", fit[[1]]$type)
  
  return(structure(list("type" = type, "call" = sys.call(), "diffusion" = fit,
                        "y" = y, "w" = allw, "mse" = allmse, "pval" = allpval), 
                   class = "seqdiffusion"))
}

#' Print sequentially fitted diffusion curves.
#'
#' Outputs the result of sequentially fitted diffusion curves.
#'
#' @param x \code{seqdiffusion} object, produced using \code{\link{seqdiffusion}}.
#' @param ... Unused argument.
#'
#' @return None. Console output only. 
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' @seealso \code{\link{seqdiffusion}}.
#' @keywords internal
#' @examples
#'  fit <- seqdiffusion(tsIbm)
#'  print(fit)
#'
#' @export
#' @method print seqdiffusion
print.seqdiffusion <- function(x,...){
  # Print console output for bass
  # x, object estimated using bass
  
  type <- tolower(x$diffusion[[1]]$type)
  
  # create selector of paramters and p-values
  no.w <- ncol(x$w)
  sel <- NULL
  for (i in 1:no.w){
    sel <- c(sel, c(i, (i+no.w)))
  }
  
  writeLines(paste(x$type, "model"))
  writeLines("")
  writeLines("Parameters:")
  temp <- round(cbind(cbind(x$w, x$pval)[, sel],
                      sqrt(x$mse)), 4)
  
  switch(type,
         bass = colnames(temp) <- c("m coef.", "pval.",
                                    "p coef.", "pval.",
                                    "q coef.", "pval.",  "sigma"),
         gompertz = colnames(temp) <- c("m coef.", "pval.",
                                        "p coef.", "pval.",
                                        "q coef.", "pval.",
                                        "sigma"),
         gsgompertz = colnames(temp) <- c("m coef", "pval.",
                                          "a coef.", "pval.",
                                          "b coef.", "pval.",
                                          "c coef", "pval.", "sigma"),
         weibull = colnames(temp) <- c("m coef.", "pval.",
                                       "a coef.", "pval.",
                                       "b coef.", "pval.",
                                       "sigma"))
  print(temp)
  writeLines("")
  
}

#' Plot sequentially fitted diffusion curves.
#'
#' Produces a plot of sequentially fitted diffusion curves.
#'
#' @param x \code{seqdiffusion} object, produced using \code{\link{seqdiffusion}}.
#' @param cumulative If TRUE plot cumulative adoption.
#' @param ... Unused argument.
#'
#' @return None. Function produces a plot.
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}, 
#' @author Nikoloas Kourentzes, \email{nikoloas@@kourentzes.com}
#' @seealso \code{\link{seqdiffusion}}.
#' @keywords internal
#' @examples
#'  fit <- seqdiffusion(tsIbm)
#'  plot(fit)
#'
#' @export
#' @method plot seqdiffusion
plot.seqdiffusion <- function(x, cumulative = c(FALSE, TRUE),...){
  # Plot sequential bass curves
  # x, object estimated using bass
  # cumulative, if TRUE plot cumulative adoption
  
  cumulative <- cumulative[1]
  if ("cleanlead" %in% names(x$call)) {
    cleanlead <- any(x$call$cleanlead == c("T", "TRUE"))
  } else {
    cleanlead <- TRUE
  }
  
  
  cmp <- c("#B2182B", "#EF8A62", "#67A9CF", "#2166AC")
  k <- dim(x$y)[2]
  cmp <- grDevices::colorRampPalette(cmp)(k)
  N <- dim(x$y)[1]
  
  if (cumulative == FALSE){
    Y <- x$y
    ll <- 2
  } else {
    Y <- apply(x$y, 2, cumsum)
    ll <- 1
  }
  
  yy <- range(Y, na.rm = T)
  yy <- yy + c(-1, 1)*0.04*diff(yy)
  yy[1] <- max(0, yy[1])
  
  graphics::plot(NA, NA, xlim = c(1, N), ylim = yy,
       xlab = "Period", ylab = "Adoption", main = x$type)
  for (i in 1:k){
    y.temp <- Y[, i]
    
    if (cleanlead == TRUE) {
      y.temp <- cleanzero(y.temp)
      l <- y.temp$loc
      y.temp <- y.temp$x
    } else {
      l <- 1
    }
    
    y.temp <- cleanna(y.temp, silent = T)
    l <- y.temp$locLead+l-1
    n <- length(y.temp$x)
    xx <- l:(l+n-1)
    graphics::points(xx, y.temp$x, col = "black", pch = 21, bg = cmp[i], cex = 0.7)
    graphics::lines(xx, x$diffusion[[i]]$fit[, ll], col = cmp[i], lwd = 2)
  }
}
