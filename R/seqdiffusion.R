#' Enables fitting various sequential diffusion curves.
#' 
#' This function fits diffusion curves of the type \code{"bass"}, 
#' \code{"gompertz"} or \code{sgompertz} across generations. Parameters are 
#' estimated for each generation individually by minimising the Mean Squared 
#' Error with the subplex algorithm from the nloptr package. Optionally p-values
#' of the coefficients can be determined via bootstraping. Furthermore, the
#' bootstrapping allows to remove insignificant parameters from the optimisation
#' process.
#' 
#' @inheritSection diffusion Bass curve
#' @inheritSection diffusion Gompertz curve
#' @inheritSection diffusion Shifted-Gompertz curve
#' 
#' @param x matrix containing in each column the adoption per period for generation k
#' @param w vector of curve parameters (see note). If provided no estimation
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
#' @param type of diffusion curve to use. This can be "bass", "gompertz" and
#'   "sgompertz"
#' @param optim optimization method to use. This can be "nm" for Nelder-Meade or
#'   "hj" for Hooke-Jeeves. #' @param maxiter number of iterations the optimser
#'   takes (default == \code{10000} for "nm" and \code{Inf} for "hj")
#' @param opttol Tolerance for convergence (default == 1.e-06)
#' 
#' @return Returns an object of class \code{seqdiffusion}, which contains:
#' \itemize{
#' \item \code{type} diffusion model type used
#' \item \code{diffusion} returns model specification for each generation (see
#' \code{\link{diffusion}} for details)
#' \item \code{call} calls function fitted
#' \item \code{w} named matrix of fitted parameters for each generation
#' \item \code{x} matrix of actuals
#' \item \code{mse} insample Mean Squared Error for each generation
#' \item \code{pval} all p-values for \code{w} at each generation
#' }
#' 
#' @note vector \code{w} needs to be provided for the Bass curve in the order of
#'   \code{"p", "q", "m"}, where "p" is the coefficient of innovation, "q" is the
#'   coeficient of imitation and "m" is the market size coefficient.
#'   
#'   For the Gompertz curve vector \code{w} needs to be in the form of
#'   \code{("a", "b", "m")}. Where "a" is the x-axis displacement coefficient, "b"
#'   determines the growth rate and "m" sets, similarly to Bass model, the
#'   market potential (saturation point).
#'   
#'   For the Shifted-Gompertz curve vector \code{w} needs to be in the form of 
#'   \code{("a", "b", "c", "m")}. Where "a" is the x-axis displacement
#'   coefficient, "b" determines the growth rate, "c" is the shifting parameter
#'   and "m" sets, similarly to Bass model, the market potential (saturation
#'   point).
#'   
#' @example examples/example_seqdiffusion.R
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
seqdiffusion <- function(x, cleanlead = c(TRUE, FALSE), prew = NULL, l = 2,
                         cumulative = c(TRUE, FALSE),
                         pvalreps = 0, eliminate = c(FALSE, TRUE), sig = 0.05, 
                         verbose = c(FALSE, TRUE),
                         type = c("bass", "gompertz", "sgompertz"),
                         optim = c("nm", "hj"), maxiter = Inf, opttol = 1.e-06){
  
  type <- match.arg(type, c("bass", "gompertz", "sgompertz"))
  optim <- match.arg(optim, c("nm", "hj"))
  verbose <- verbose[1]
  eliminate <- eliminate[1]
  cumulative <- cumulative[1]
  
  # Number of curves
  k <- dim(x)[2]
  
  fit <- vector("list", k)
  names(fit) <- paste0("Gen", 1:k)
  
  # Fit iteratively across generations
  for (i in 1:k){
    
    if (verbose == TRUE){
      writeLines(paste0("Generation ", i))
    }
    if (i > 1){
      prew <- fit[[i-1]]$w
      elimin <- eliminate
    } else {
      elimin <- FALSE
    }
    
    fit[[i]] <- diffusion(x[, i], w = NULL, cleanlead, prew, l, cumulative, pvalreps, 
                          elimin, sig, verbose, type = type, optim = optim,
                          maxiter, opttol)
    
  }
  
  allw <- do.call(rbind, lapply(fit, function(x) {x$w}))
  allmse <- do.call(rbind, lapply(fit, function(x) {x$mse}))
  allpval <- do.call(rbind, lapply(fit, function(x) {x$pval}))
  
  typ <- paste("Sequential", fit[[1]]$type)
  
  return(structure(list("type" = typ, "diffusion" = fit, "x" = x, "w" = allw,
                        "mse" = allmse, "pval" = allpval), 
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
#'  fit <- seqdiffusion(tsibm)
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
                      sqrt(x$mse)),
                      4)
  
  switch(type,
         bass = colnames(temp) <- c("p coef.", "pval.", "q coef.", "pval.",
                                    "M size", "pval.", "sigma"),
         gompertz = colnames(temp) <- c("p coef.", "pval.", "q coef.", "pval.",
                                        "M size", "pval.", "sigma"),
         sgompertz = colnames(temp) <- c("a coef.", "pval.", "b coef.",
                                         "pval.", "c coef", "pval.", "M size",
                                         "pval.", "sigma"))
  
#   if (type == "bass"){
#     colnames(temp) <- c("p coef.", "pval.", "q coef.", "pval.",
#                         "M size", "pval.", "sigma")
#   } else if (type == "gompertz"){
#     colnames(temp) <- c("a coef.", "pval.", "b coef.",
#                         "pval.", "M size", "pval.", "sigma")
#   } else if (type == "sgompertz"){
#     colnames(temp) <- c("a coef.", "pval.", "b coef.",
#                         "pval.", "c coef", "pval.", "M size", "pval.", "sigma")
#   }
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
#'  fit <- seqdiffusion(tsibm)
#'  plot(fit)
#'
#' @export
#' @method plot seqdiffusion
plot.seqdiffusion <- function(x, cumulative = c(FALSE, TRUE),...){
  # Plot sequential bass curves
  # x, object estimated using bass
  # cumulative, if TRUE plot cumulative adoption
  
  cumulative <- cumulative[1]
  
  cmp <- c("#B2182B", "#EF8A62", "#67A9CF", "#2166AC")
  k <- dim(x$x)[2]
  cmp <- grDevices::colorRampPalette(cmp)(k)
  N <- dim(x$x)[1]
  
  if (cumulative == FALSE){
    X <- x$x
    ll <- 2
  } else {
    X <- apply(x$x, 2, cumsum)
    ll <- 1
  }
  
  yy <- range(X)
  yy <- yy + c(-1, 1)*0.04*diff(yy)
  yy[1] <- max(0,yy[1])
  
  graphics::plot(NA, NA, xlim = c(1, N), ylim = yy,
       xlab = "Period", ylab = "Adoption", main = x$type)
  for (i in 1:k){
    x.temp <- X[, i]
    x.temp <- cleanzero(x.temp)
    n <- length(x.temp$x)
    l <- x.temp$loc
    xx <- l:(l+n-1)
    graphics::points(xx, x.temp$x, col = "black", pch = 21, bg = cmp[i], cex = 0.7)
    graphics::lines(xx, x$diffusion[[i]]$fit[,ll], col = cmp[i], lwd = 2)
  }
}
