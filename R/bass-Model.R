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
  
  # Loss/cost function
  CF <- function(B){
    bassValues <- bassCurve(obsInsample, B);
    scale <- sqrt(mean((log(y) - log(bassValues[,"Adoption"]))^2));
    CFValue <- -sum(dlnorm(y, log(bassValues[,"Adoption"]), scale, log=TRUE));
    
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

  modelReturned <- list(data=y, fitted=yFitted, B=B, curves=bassValues,
                        loss="likelihood", lossValue=CFValue, logLik=logLikBass);
  
  return(modelReturned);
}