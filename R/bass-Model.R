#' @importFrom nloptr nloptr
#' @importFrom statmod dinvgauss qinvgauss
#' @importFrom stats dlnorm dgamma qlnorm qgamma
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
  obsInsample <- length(y);
  yIndex <- try(time(y), silent=TRUE);
  yFrequency <- frequency(y);
  yStart <- yIndex[1];
  
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
    B <- bassInit(y);
  }
  else{
    B <- ellipsis$B;
  }
  names(B) <- c("p","q","m");
  
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
  
  lb <- rep(0, length(B));
  ub <- c(1, 1, Inf);
  
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
  
  modelReturned <- list(data=y, fitted=yFitted, B=B, curves=bassValues, scale=scale, distribution=distribution,
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