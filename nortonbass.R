
# This is the function starting
nortonBass <- function(x, startval.met = c("2ST", "BB", "iBM"),
                       startval = NULL, flexpq = F,
                       estim.met = c("OLS", "SUR", "2SLS", "3SLS"),
                       gstart = NULL){
  # Inputs
  # x                       matrix or dataframe containing demand for each generation
  #                         in non-cumulative form
  # startval.met            Different ways of obtaining start values
  #                         "2ST" (default) Two stage approach taking "BB" method first
  #                         and then re-estimate if flexpq == T
  #                         "BB" Bass and Bass 2004 method sets p = 0.003, q = 0.05
  #                         and m is the maximum observed value for generation j
  #                         "iBM" Fits individual Bass models and uses this as
  #                         estimators in case flexpq == F median of p and q is used
  # startval                Vector with start values needed for manual estimation.
  #                         Vector values need to be named in the form of
  #                         case flexpq == T  --> (p1,...,pj,q1,...,qj,m1,..,mj)
  #                         case flexpq == F  --> (p1,q1,m1,..,mj)
  # flexqp[default = F]     Allows for flexible p and q for each generation as
  #                         described by Islam and Maede 1997
  # estim.met["OLS" default]Estimation method for nlsystemfit() function --> OLS default
  
  # gstart                  manually defne starting points of generation
  #
  
  # Set some basic variables
  gn <- ncol(x)
  startval.met <- startval.met[1]
  estim.met <- estim.met[1]
  
  # Input validation
  if(!is.matrix(x) || is.data.frame(x)){
    stop("x needs to be matrix or data.frame")
  }
  
  if(!is.null(startval)){
    if(flexpq == T){
      parnum <- gn*3
    }else{
      parnum <- gn+2
    }
    
    if(flexpq == T){
      startvalNames <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
    }else{
      startvalNames <- c("p1","q1", paste0("m", 1:gn))
    }
    
    # Error handling
    if(length(startval) != parnum){
      stop("Lenght of startvalues not correct")
    }
    
    if(all(names(startval), startvalNames)){
      if(flexpq == T){
        stop("Startvalue vector need to be named in
             (p1,...,pj,q1,...,qj,m1,..,mj) form")
      }else{
        stop("Startvalues vector need to be named in (p1,q1,m1,..,mj) form")
      }
    }
    }
  
  if(!is.null(gstart)){
    if(ncol(x) != length(gstart)){
      stop("Number of generations in data does not match number of starting values")
    }
  }
  
  # get generations starting points
  if(is.null(gstart)){
    gstart <- apply(x, 2, function(x) which(x > 0)[1])
  }
  
  # add time values to the x matrix
  for(i in 1:gn){
    ti <- c(rep(0, (gstart[i]-1)), 1:(nrow(x)-(gstart[i]-1)))
    x <- cbind(x, ti)
  }
  
  # set name for later equation
  colnames(x) <- c(paste0("y", 1:gn), paste0("t", 1:gn))
  x <- as.data.frame(x) # nlsystemfit requires dataframe format
  
  # Obtain parameter starting values
  if(is.null(startval)){
    
    # 2 Stage method
    if(startval.met == "2ST" & flexpq == T){
      startval <- startvalgen(x, gn, flexpq = F, startval.met = "BB")
      
      param <- estimNB(x, gn, gstart, startval, flexpq = F, estim.met)$param
      
      startval <- c(rep(param[1], gn), rep(param[2], gn), param[3:(gn+2)])
      names(startval) <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
      
    }else{
      startval <- startvalgen(x, gn, flexpq, startval.met)
    }
  }
  
  
  # estimate Norton Bass
  fitNB <- estimNB(x, gn, gstart, startval, flexpq, estim.met)
  
  return(fitNB)
  }


eqnNB <- function(gn, flexpq = T){
  # Creates the Norton-Bass model equation
  
  # return values
  # eqns          Norton-Bass equation for gn number of generations
  # instr         Istruments required for three and two stage least squares
  
  # case p and q flexible
  if(flexpq == T){
    pqmt <- cbind(p=1:gn, q = 1:gn, m = 1:gn, t = 1:gn)
  }else{
    pqmt <- cbind(p = rep(1, gn), q = rep(1, gn), m = 1:gn, t = 1:gn)
  }
  
  # store instruments --> needed for the 3SLS estimation
  inst <- c(list(paste(c("~ t%d", rep(" + t%d", gn-1)), collapse = "")), pqmt[,4])
  inst <- as.formula(do.call(sprintf, inst))
  
  # create Norton-Bass 1987 forumla using their naming convention
  a <- sprintf("(q%d/p%d)", pqmt[, 2], pqmt[, 1])
  b <- sprintf("(q%d+p%d)*t%d", pqmt[, 2], pqmt[, 1], pqmt[, 4])
  m <- sprintf("m%d", pqmt[,3])
  
  ft <- sprintf("((1-exp(-%s)) / (1+%s*exp(-%s)))", b, a, b)
  
  # storage for equations
  eqns <- list()
  
  # loop across all generations
  for(g in 1:gn){
    
    if(g == 1){
      pg <- sprintf("%s * %s", ft[g], m[g])
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s * (1-%s)", g, pg, ft[g+1]))
    }else if(g == gn){
      pg <- sprintf("%s * (%s + %s)", ft[g], m[g], pg)
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s", g, pg))
    }else{
      pg <- sprintf("%s * (%s + %s)", ft[g], m[g], pg)
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s * (1-%s)", g, pg, ft[g+1]))
    }
  }
  return(list("eqns" = eqns, "inst" = inst))
}



estimNB <- function(x, gn, gstart, startval, flexpq, estim.met){
  
  # create devilishly nonlinear model equation
  mod <- eqnNB(gn, flexpq)
  
  print(startval)
  
  # Fit devilishly nonlinear model
  if(estim.met == "OLS" | estim.met == "SUR"){
    nbFit <- systemfit::nlsystemfit(method = estim.met, eqns = mod$eqns, startvals = startval,
                                    data = x, maxiter = 1000)
  }else{
    nbFit <- systemfit::nlsystemfit(method = estim.met, eqns = mod$eqns, startvals = startval,
                                    data = x, inst = mod$inst, maxiter = 1000)
  }
  
  return(list("fitted" = nbFit$eq, "param" = nbFit$b))
}


startvalgen <- function(x, gn, flexpq, startval.met){
  # function to guess starting values to be past into the nonlinear optimiser
  # methods considered are:
  # i) "BB" --> Bass and Bass (2004) approach
  # ii) "iBM" --> indivudual Bass models
  
  if(startval.met == "iBM"){
    
    for(i in 1:gn){
      fitBass <- bass.est(x[gstart[i]:nrow(x),i], estim = "nls")
      print(fitBass)
      names(fitBass) <- c(paste0("p", i), paste0("q", i), paste0("m", i))
      startval <- c(startval, fitBass)
    }
    
    if(gn > 1){
      if(flexpq == F){
        startval[1] <- median(startval[seq(1, (3*gn), 3)])
        startval[2] <- median(startval[seq(2, (3*gn), 3)])
        
        startval <- startval[-c(seq(4, (3*gn), 3), seq(5,(3*gn), 3))]
      }
    }
  }
  
  if(startval.met == "BB" || startval.met == "2ST"){
    # Bass, P.I. and Bass, F.M. (2004). IT Waves. Two Completed Generational
    # Diffusion Models. Working Paper - basseconomics
    # set p and q to fixed values p = 0.003, q = 0.5 m = max(x_g)
    
    p <- 0.003
    q <- 0.5
    
    if(flexpq == F){
      startval <- c(p, q, apply(x, 2, max)[1:gn])
      names(startval) <- c(paste0("p", 1), paste0("q", 1), paste0("m", 1:gn))
    }else{
      startval <- c(rep(p, gn), rep(q, gn), apply(x, 2, max)[1:gn])
      names(startval) <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
    }
  }
  
  return(startval)
}
