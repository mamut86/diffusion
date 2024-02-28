#' Statistical Bass Model
#'
#' Function estimates the statistical Bass model
#'
#' The function accepts the data and applies the Bass model with
#' assumed distribution and defined components to it. This is based on
#' the paper by Svetunkov et al. (202??). The model assumes that the market
#' size variable m is stochastic and might change over time.
#'
#' @author Ivan Svetunkov, \email{ivan@svetunkov.ru}
#' @keywords univar ts models nonlinear
#'
#' @references
#' \itemize{
#' \item{Bass, F.M., 1969. A new product growth for model consumer durables. Management Science 15(5), 215-227.}
#' }
#'
#' @param data A vector or a matrix with variables. In the latter case if
#' \code{formula} is not provided then it is assumed that the response variable
#' is in the first column of the data.
#' @param lags The vector of seasonal lags. Only required if \code{seasonality=TRUE}.
#' @param seasonality The logical variable. If \code{TRUE} then the seasonal model is
#' estimated. The seasonality is modelled in the style of ETS model.
#' @param distribution The type of distribution to assume for the error term. Can be
#' \code{"dlnorm"} - Log-Normal, \code{"dgamma"} - Gamma or \code{"dinvgauss"} -
#' Inverse Gaussian. "d" in the name of distribution specifies that we use density
#' function.
#' @param formula R formula specifying which variable to include in the model and
#' in what form.
#' @param ... Parameters passed to the optimiser (nloptr):
#' \itemize{
#' \item \code{maxeval} - maximum number of evaluations to carry out. The default is 40 per
#' estimated parameter for ETS and / or ARIMA and at least 1000 if explanatory variables
#' are introduced in the model (100 per parameter for explanatory variables, but not less
#' than 1000);
#' \item \code{maxtime} - stop, when the optimisation time (in seconds) exceeds this;
#' \item \code{xtol_rel} - the relative precision of the optimiser (the default is 1E-6);
#' \item \code{xtol_abs} - the absolute precision of the optimiser (the default is 1E-8);
#' \item \code{ftol_rel} - the stopping criterion in case of the relative change in the loss
#' function (the default is 1E-8);
#' \item \code{ftol_abs} - the stopping criterion in case of the absolute change in the loss
#' function (the default is 0 - not used);
#' \item \code{algorithm} - the algorithm to use in optimisation
#' (by default, \code{"NLOPT_LN_SBPLX"} is used);
#' \item \code{print_level} - the level of output for the optimiser (0 by default).
#' If equal to 41, then the detailed results of the optimisation are returned.
#' }
#' You can read more about these parameters by running the function
#' \link[nloptr]{nloptr.print.options}.
#' Starting values of parameters can be passed via \code{B}, while the upper and lower
#' bounds should be passed in \code{ub} and \code{lb} respectively. In this case they
#' will be used for optimisation.
#' @return Object of class "bass" is returned. It contains the list of the
#' following values:
#'
#' \itemize{
#' \item \code{data} - the provided data.
#' \item \code{timeElapsed} - time elapsed for the construction of the model.
#' \item \code{fitted} - the fitted values, shifted in time.
#' \item \code{residuals} - the residuals of the model.
#' \item \code{B} - the vector of estimated parameters.
#' \item \code{curves} - adoption, innovators and imitators curves.
#' \item \code{scale} - the value of the estimated scale parameter.
#' \item \code{distribution} - the assumed distribution.
#' \item \code{loss} - type of loss function used in the estimation.
#' \item \code{lossValue} - loss function value.
#' \item \code{logLik} - log-likelihood of the model.
#' }
#'
#' @seealso \code{\link[smooth]{es}, \link[smooth]{ssarima}}
#'
#' @examples
#'
#' # CMA of specific order
#' ourModel <- bass(tsWindows[,6])
#'
#' @importFrom nloptr nloptr
#' @importFrom statmod dinvgauss qinvgauss
#' @importFrom stats dlnorm dgamma qlnorm qgamma
#' @importFrom stats AIC BIC fitted frequency lm time
#' @importFrom utils head
#' @importFrom graphics legend lines
#' @importFrom greybox AICc BICc
#' 
#' @rdname bass
#' @export
bass <- function(data, lags=frequency(data), seasonality=FALSE,
                 distribution=c("dlnorm","dgamma","dinvgauss"), formula=NULL, ...){
  # Function implements a statistical Bass model
  distribution <- match.arg(distribution);
  
  # Process the provided data
  # Remove consecutive zeroes from the data first...
  # This should also use formula
  if(is.matrix(data)){
    y <- data[,1];
  }
  else if(is.data.frame(data)){
    y <- data[[1]];
  }
  else{
    y <- data;
  }
  
  # Technical parameters
  yIndex <- try(time(y), silent=TRUE);
  yFrequency <- frequency(y);
  
  # Remove consecutive zeroes
  zeroesStart <- which(y!=0)[1];
  zeroesEnd <- tail(which(y!=0),1);
  if(length(zeroesStart)!=0 || length(zeroesEnd)!=0){
    y <- y[zeroesStart:zeroesEnd];
    yIndex <- yIndex[zeroesStart:zeroesEnd];
    warning("Data had some zeroes in the beginning/end. We dropped them.",
            call.=FALSE);
  }
  yStart <- yIndex[1];
  obsInsample <- length(y);
  
  ellipsis <- list(...);
  
  #### Process ellipsis ####
  # Parameters for the optimiser
  if(is.null(ellipsis$maxeval)){
    maxeval <- NULL;
  }
  else{
    maxeval <- ellipsis$maxeval;
  }
  if(is.null(ellipsis$maxtime)){
    maxtime <- -1;
  }
  else{
    maxtime <- ellipsis$maxtime;
  }
  if(is.null(ellipsis$xtol_rel)){
    xtol_rel <- 1E-6;
  }
  else{
    xtol_rel <- ellipsis$xtol_rel;
  }
  if(is.null(ellipsis$xtol_abs)){
    xtol_abs <- 1E-8;
  }
  else{
    xtol_abs <- ellipsis$xtol_abs;
  }
  if(is.null(ellipsis$ftol_rel)){
    ftol_rel <- 1E-8;
  }
  else{
    ftol_rel <- ellipsis$ftol_rel;
  }
  if(is.null(ellipsis$ftol_abs)){
    ftol_abs <- 0;
  }
  else{
    ftol_abs <- ellipsis$ftol_abs;
  }
  if(is.null(ellipsis$algorithm)){
    algorithm <- "NLOPT_LN_SBPLX";
  }
  else{
    algorithm <- ellipsis$algorithm;
  }
  if(is.null(ellipsis$print_level)){
    print_level <- 0;
  }
  else{
    print_level <- ellipsis$print_level;
  }
  if(is.null(ellipsis$B)){
    B <- abs(bassInit(y));
  }
  else{
    B <- ellipsis$B;
  }
  names(B) <- c("p","q","m");
  
  # Upper and lower bounds for the parameters
  if(is.null(ellipsis$lb)){
    lb <- rep(0, length(B));
  }
  else{
    lb <- ellipsis$lb;
  }
  if(is.null(ellipsis$ub)){
    ub <- c(1, 1, Inf);
  }
  else{
    ub <- ellipsis$ub;
  }
  
  # Loss/cost function
  CF <- function(B){
    yFitted <- bassCurve(obsInsample, B)[,"Adoption"];
    scale <- switch(distribution,
                    "dlnorm"=sqrt(mean((log(y) - log(yFitted))^2)),
                    "dgamma"=mean((y - yFitted)^2),
                    "dinvgauss"=mean((y - yFitted)^2),
                    mean((y - yFitted)^2/(1 + y - yFitted)));
    CFValue <- -sum(switch(distribution,
                           "dlnorm"=dlnorm(y, log(yFitted), scale, log=TRUE),
                           "dgamma"=dgamma(y, shape=1/scale, scale=scale*yFitted, log=TRUE),
                           "dinvgauss"=dinvgauss(y, mean=yFitted, dispersion=scale/yFitted, log=TRUE)));
    
    return(CFValue);
  }
  
  print_level_hidden <- print_level;
  if(print_level==41){
    cat("Initial parameters:",B,"\n");
    print_level[] <- 0;
  }
  
  maxevalUsed <- maxeval;
  if(is.null(maxeval)){
    maxevalUsed <- 1000;
  }
  
  res <- nloptr(B, CF, lb=lb, ub=ub,
                opts=list(algorithm=algorithm, xtol_rel=xtol_rel, xtol_abs=xtol_abs,
                          ftol_rel=ftol_rel, ftol_abs=ftol_abs,
                          maxeval=maxevalUsed, maxtime=maxtime, print_level=print_level))
  
  if(print_level_hidden>0){
    print(res);
  }
  
  B[] <- res$solution;
  CFValue <- res$objective;
  logLikBass <- structure(-CFValue,nobs=obsInsample,df=length(B)+1,class="logLik");
  
  bassValues <- bassCurve(obsInsample, B);
  yFitted <- bassValues[,"Adoption"];
  scale <- sqrt(mean((log(y) - log(yFitted))^2));
  
  modelReturned <- list(data=y, fitted=yFitted, residuals=log(y) - log(yFitted),
                        B=B, curves=bassValues, scale=scale, distribution=distribution,
                        loss="likelihood", lossValue=CFValue, logLik=logLikBass);
  
  return(structure(modelReturned, class="bass"));
}

#' @importFrom stats nobs
#' @export
nobs.bass <- function(object, ...){
  return(length(fitted(object)));
}

#' @importFrom stats logLik
#' @export
logLik.bass <- function(object, ...){
  return(object$logLik);
}

#' @importFrom greybox actuals
#' @export
actuals.bass <- function(object, ...){
  return(object$data);
}

#' @export
plot.bass <- function(x, level=0.95, ...){
  ellipsis <- list(...);
  obsInsample <- nobs(x);
  yFitted <- fitted(x);
  
  quantileValues <- matrix(switch(x$distribution,
                                  "dlnorm"=qlnorm(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                  log(yFitted), x$scale),
                                  "dgamma"=qgamma(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                  shape=1/x$scale, scale=x$scale*yFitted),
                                  "dinvgauss"=qinvgauss(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                        mean=yFitted, dispersion=x$scale/yFitted)),
                           obsInsample, 2);
  
  # Set default plot parameters
  if(!any(names(ellipsis)=="main")){
    ellipsis$main <- paste0("Statistical Bass model with ",
                        switch(x$distribution,
                               "dlnorm"="Log-Normal",
                               "dgamma"="Gamma",
                               "dinvgauss"="Inverse Gaussian",
                               "Normal"),
                        " distribution");
  }
  
  if(!any(names(ellipsis)=="ylim")){
    ellipsis$ylim <- range(c(x$data, fitted(x)), quantileValues);
  }

  if(!any(names(ellipsis)=="ylab")){
    ellipsis$ylab <- "Adoption";
  }

  if(!any(names(ellipsis)=="xlab")){
    ellipsis$xlab <- "Time";
  }

  # The actuals
  ellipsis$x <- actuals(x);
  
  do.call("plot", ellipsis);
  lines(fitted(x), col="darkred");
  lines(x$curves[,3], col="darkgreen");
  lines(x$curves[,4], col="darkblue");
  lines(quantileValues[,1], col="darkgrey", lty=2);
  lines(quantileValues[,2], col="darkgrey", lty=2);
  legend("topright", legend=c("Adoption","Innovators","Imitators",paste0(round(level*100,0),"% Prediction level")),
         col=c("darkred","darkgreen","darkblue","darkgrey"), lwd=1, lty=c(1,1,1,2))
}

#' @export
print.bass <- function(x, digits=4, ...){
  distribution <- switch(x$distribution,
                         "dlnorm"="Log-Normal",
                         "dgamma"="Gamma",
                         "dinvgauss"="Inverse Gaussian",
                         "Normal");
  cat("Statistical Bass model with", distribution, "distribution.\n");
  cat("Parameters:\n");
  print(round(x$B,digits));
  ICs <- c(AIC(x),AICc(x),BIC(x),BICc(x));
  names(ICs) <- c("AIC","AICc","BIC","BICc");
  cat("\nInformation criteria:\n");
  print(round(ICs,digits));
}


# coef.bass
# vcov.bass
# confint.bass
# summary.bass
# predict.bass
# forecast.bass