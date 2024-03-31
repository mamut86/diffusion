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
#' @importFrom stats AIC BIC fitted frequency lm time lowess
#' @importFrom utils head
#' @importFrom graphics legend lines
#' @importFrom greybox AICc BICc
#' @importFrom smooth msdecompose
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
  if(zeroesStart!=1 || zeroesEnd!=length(y)){
    yInSample <- y[zeroesStart:zeroesEnd];
    yIndex <- yIndex[zeroesStart:zeroesEnd];
    warning("Data had some zeroes in the beginning/end. We dropped them.",
            call.=FALSE);
  }
  else{
    yInSample <- y;
  }
  yStart <- yIndex[1];
  obsInsample <- length(yInSample);
  
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
    BNotProvided <- TRUE;
    B <- abs(bassInit(y));
  }
  else{
    BNotProvided <- FALSE;
    B <- ellipsis$B;
  }
  
  # Estimate parameters
  if(BNotProvided){
    # Deal with seasonality
    if(seasonality){
      if(all(lags==1)){
        warning("Cannot build seasonal model with lags=1",
                call.=FALSE);
        BSeasonal <- NULL;
        B <- abs(bassInit(lowess(yInSample)$y));
      }
      else{
        yDecomposition <- msdecompose(yInSample, lags=lags[lags!=1],
                                      type="multiplicative");
        BSeasonal <- unlist(yDecomposition$seasonal)[-lags];
        names(BSeasonal) <- paste0("seasonal",c(1:(lags-1)));
        B <- c(abs(bassInit(yDecomposition$trend[!is.na(yDecomposition$trend)])),
               BSeasonal);
      }
    }
    else{
      B <- abs(bassInit(lowess(yInSample)$y));
      # B <- abs(bassInit(yInSample));
      BSeasonal <- NULL;
    }
  }
  nParamSeasonal <- length(BSeasonal);
  
  # Upper and lower bounds for the parameters
  if(is.null(ellipsis$lb)){
    lb <- rep(0, length(B));
  }
  else{
    lb <- ellipsis$lb;
  }
  if(is.null(ellipsis$ub)){
    ub <- c(Inf, 1, 1, rep(Inf, nParamSeasonal));
  }
  else{
    ub <- ellipsis$ub;
  }
  
  # Loss/cost function
  CF <- function(B){
    yFitted <- bassCurve(obsInsample, B[c(1:3)])[,"Adoption"];
    if(seasonality){
      BSeasonal <- c(B[-c(1:3)],1);
      BSeasonal[length(BSeasonal)] <- 1/prod(BSeasonal);
      yFitted[] <- (yFitted * rep(BSeasonal,ceiling(obsInsample/lags))[1:obsInsample]);
    }
    scale <- switch(distribution,
                    "dlnorm"=sqrt(mean((log(yInSample) - log(yFitted))^2)),
                    "dgamma"=mean((yInSample - yFitted)^2),
                    "dinvgauss"=mean((yInSample - yFitted)^2),
                    mean((yInSample - yFitted)^2/(1 + yInSample - yFitted)));
    CFValue <- -sum(switch(distribution,
                           "dlnorm"=dlnorm(yInSample, log(yFitted), scale, log=TRUE),
                           "dgamma"=dgamma(yInSample, shape=1/scale, scale=scale*yFitted, log=TRUE),
                           "dinvgauss"=dinvgauss(yInSample, mean=yFitted, dispersion=scale/yFitted, log=TRUE)));
    
    return(CFValue);
  }
  
  print_level_hidden <- print_level;
  if(print_level==41){
    cat("Initial parameters:",B,"\n");
    print_level[] <- 0;
  }
  
  maxevalUsed <- maxeval;
  if(is.null(maxeval)){
    maxevalUsed <- 2000;
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
  if(seasonality){
    BSeasonal <- c(B[-c(1:3)],1);
    BSeasonal[length(BSeasonal)] <- 1/prod(BSeasonal);
    yFitted[] <- yFitted * rep(BSeasonal,ceiling(obsInsample/lags))[1:obsInsample];
  }
  scale <- sqrt(mean((log(yInSample) - log(yFitted))^2));
  
  modelReturned <- list(data=yInSample, fitted=yFitted, residuals=log(yInSample) - log(yFitted),
                        B=B, curves=bassValues, scale=scale, distribution=distribution,
                        lags=lags, seasonality=seasonality,
                        loss="likelihood", lossValue=CFValue, logLik=logLikBass, res=res);
  
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
    ellipsis$ylim <- range(c(actuals(x), yFitted), quantileValues);
  }

  if(!any(names(ellipsis)=="ylab")){
    ellipsis$ylab <- "Adoption";
  }

  if(!any(names(ellipsis)=="xlab")){
    ellipsis$xlab <- "Time";
  }

  # The actuals
  ellipsis$x <- as.vector(actuals(x));
  
  do.call("plot", ellipsis);
  lines(yFitted, col="darkorange");
  lines(x$curves[,2], col="darkred");
  lines(x$curves[,3], col="darkgreen");
  lines(x$curves[,4], col="darkblue");
  lines(quantileValues[,1], col="darkgrey", lty=2);
  lines(quantileValues[,2], col="darkgrey", lty=2);
  legend("topright", legend=c("Fitted","Adoption","Innovators","Imitators",paste0(round(level*100,0),"% Prediction level")),
         col=c("darkorange","darkred","darkgreen","darkblue","darkgrey"), lwd=1, lty=c(1,1,1,1,2))
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

#' @importFrom stats coef
#' @export
coef.bass <- function(object, ...){
  return(object$B);
}

#' @export
predict.bass <- function(object, h=10, interval="prediction",
                         level=0.95, ...){
  # Calculate forecasts for fitted diffusion curves
  
  if (h <= 1){
    stop("Horizon h must be positive integer.")
  }
  
  obsAll <- nobs(object) + h;
  
  B <- coef(object);
  
  yForecast <- tail(bassCurve(obsAll, B[1:3])[,"Adoption"], h);
  bassValues <- bassCurve(obsAll, B);
  if(object$seasonality){
    BSeasonal <- c(B[-c(1:3)],1);
    BSeasonal[length(BSeasonal)] <- 1/prod(BSeasonal);
    yForecast[] <- yForecast * tail(rep(BSeasonal,ceiling(obsAll/object$lags)), h);
  }

  quantileValues <- matrix(switch(object$distribution,
                                  "dlnorm"=qlnorm(rep(c((1-level)/2,(1+level)/2),each=h),
                                                  log(yForecast), object$scale),
                                  "dgamma"=qgamma(rep(c((1-level)/2,(1+level)/2),each=h),
                                                  shape=1/object$scale, scale=object$scale*yForecast),
                                  "dinvgauss"=qinvgauss(rep(c((1-level)/2,(1+level)/2),each=h),
                                                        mean=yForecast, dispersion=object$scale/yForecast)),
                           h, 2,
                           dimnames=list(NULL, c(paste0("Lower bound (",(1-level)/2*100,"%)"),
                                                 paste0("Upper bound (",(1+level)/2*100,"%)"))));
  
  
  return(structure(list(mean=yForecast, lower=quantileValues[,1,drop=FALSE], upper=quantileValues[,2,drop=FALSE], model=object,
                        curves=bassValues, level=level, interval=interval, h=h),
                   class=c("bass.forecast")));
}

#' @export
print.bass.forecast <- function(x, ...){
    if(x$interval!="none"){
        returnedValue <- cbind(x$mean,x$lower,x$upper);
        colnames(returnedValue) <- c("Point forecast",colnames(x$lower),colnames(x$upper))
    }
    else{
        returnedValue <- x$mean;
    }
    print(returnedValue);
}

#' @importFrom graphics abline
#' @export
plot.bass.forecast <- function(x, ...){
  ellipsis <- list(...);
  obsInsample <- nobs(x$model);
  h <- x$h;
  obsAll <- obsInsample + h;
  yFitted <- fitted(x$model);
  level <- x$level;
  
  quantileValues <- matrix(switch(x$model$distribution,
                                  "dlnorm"=qlnorm(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                  log(yFitted), x$model$scale),
                                  "dgamma"=qgamma(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                  shape=1/x$model$scale, scale=x$model$scale*yFitted),
                                  "dinvgauss"=qinvgauss(rep(c((1-level)/2,(1+level)/2),each=obsInsample),
                                                        mean=yFitted, dispersion=x$model$scale/yFitted)),
                           obsInsample, 2);
  
  yForecast <- x$mean;
  yLower <- x$lower;
  yUpper <- x$upper;
  
  # Set default plot parameters
  if(!any(names(ellipsis)=="main")){
    ellipsis$main <- paste0("Statistical Bass model with ",
                        switch(x$model$distribution,
                               "dlnorm"="Log-Normal",
                               "dgamma"="Gamma",
                               "dinvgauss"="Inverse Gaussian",
                               "Normal"),
                        " distribution");
  }
  
  if(!any(names(ellipsis)=="ylim")){
    ellipsis$ylim <- range(c(x$data, yFitted, yForecast), quantileValues, yLower, yUpper);
  }

  if(!any(names(ellipsis)=="xlim")){
    ellipsis$xlim <- c(1, obsAll);
  }
  
  if(!any(names(ellipsis)=="ylab")){
    ellipsis$ylab <- "Adoption";
  }

  if(!any(names(ellipsis)=="xlab")){
    ellipsis$xlab <- "Time";
  }

  # The actuals
  ellipsis$x <- as.vector(actuals(x$model));
  
  do.call("plot", ellipsis);
  lines(yFitted, col="darkorange");
  lines(obsInsample+c(1:h), yForecast, col="darkorange", lwd=2);
  lines(obsInsample+c(1:h), yLower, col="darkgrey", lty=2, lwd=2);
  lines(obsInsample+c(1:h), yUpper, col="darkgrey", lty=2, lwd=2);
  lines(x$curves[,2], col="darkred");
  lines(x$curves[,3], col="darkgreen");
  lines(x$curves[,4], col="darkblue");
  lines(quantileValues[,1], col="darkgrey", lty=2);
  lines(quantileValues[,2], col="darkgrey", lty=2);
  abline(v=obsInsample, col="red", lwd=2)
  legend("topleft", legend=c("Fitted","Adoption","Innovators","Imitators",paste0(round(level*100,0),"% Prediction level")),
         col=c("darkorange","darkred","darkgreen","darkblue","darkgrey"), lwd=1, lty=c(1,1,1,1,2))
}

# vcov.bass
# confint.bass
# summary.bass
# predict.bass
# forecast.bass