bass <- function(y, lags=frequency(y), seasonality=FALSE,
                 distribution=c("dlnorm"), xreg=NULL, ...){
  # Function implements a statistical Bass model
  obsInsample <- length(y);
  
  yIndex <- try(time(y),silent=TRUE);
  
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
    scale <- sqrt(mean((log(y) - log(yFitted))^2));
    CFValue <- -sum(dlnorm(y, log(yFitted), scale, log=TRUE));
    
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
  logLikBass <- -CFValue;
  
  bassValues <- bassCurve(obsInsample, B);
  yFitted <- bassValues[,"Adoption"];
  scale <- sqrt(mean((log(y) - log(yFitted))^2));
  
  modelReturned <- list(data=y, fitted=yFitted, B=B, curves=bassValues, scale=scale, distribution=distribution,
                        loss="likelihood", lossValue=CFValue, logLik=logLikBass);
  
  return(structure(modelReturned, class="bass"));
}

plot.bass <- function(x, level=0.95, ...){
  plot(x$data, ylim=range(c(x$data, fitted(x)),
                          qlnorm((1+level)/2, log(x$fitted), x$scale)),
       ylab="Adoption", xlab="Time");
  lines(fitted(x), col="darkred");
  lines(x$curves[,3], col="darkgreen");
  lines(x$curves[,4], col="darkblue");
  lines(qlnorm((1-level)/2, log(x$fitted), x$scale), col="darkgrey", lty=2);
  lines(qlnorm((1+level)/2, log(x$fitted), x$scale), col="darkgrey", lty=2);
  legend("topright", legend=c("Adoption","Innovators","Imitators",paste0(round(level*100,0),"% Prediction level")),
         col=c("darkred","darkgreen","darkblue","darkgrey"), lwd=1, lty=c(1,1,1,2))
}

print.bass <- function(x, ...){
  distribution <- switch(x$distribution,
                         "dlnorm"="Log-Normal",
                         "Normal");
  cat("Statistical bass model with", distribution, "distribution.\n");
  cat("Parameters:\n");
  print(x$B);
}