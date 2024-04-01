#' Norton-Bass model
#' 
#' \code{Nortonbass} fits a generational Bass model proposed by Norton and Bass 
#' (1987). Each subsequent generation influences the sales of the previous 
#' generation. The set of equation is estimated simulataneously.
#' 
#' @param x matrix or dataframe containing demand for each generation in 
#'   non-cumulative form.
#' @param startval.met Different methods of obtaining starting values. 
#'   \describe{ \item{\code{"2ST"}}{Two stage approach taking \code{"BB"} method
#'   first and then re-estimate if \code{flexpq == T} (default)} 
#'   \item{\code{"BB"}}{Bass and Bass (2004) method which sets
#'   \eqn{p_{1,\dots,j} = 0.003, q_{1,\dots,j} = 0.05}{pj = 0.003, qj = 0.05}
#'   and \eqn{m_j}{mj} is the maximum observed value for generation \eqn{j}{j}} 
#'   \item{\code{"iBM"}}{Fits individual Bass models and uses this as
#'   estimators. In case \code{flexpq == F} the median of p and q is used }}
#' @param estim.met Estimation method, \code{"BOBYQA"} see
#'   \code{\link[systemfit]{nlsystemfit}} (\code{BOBYQA} default)
#' @param gstart optional vector with starting points of generations#'
#' @param startval an optional Vector with starting for manual estimation
#' @param flexpq If \code{TRUE}, generations will have independent p and q 
#'   values as suggested by Islam and Maed (1997). Note that model might
#'   not converge.
#'   
#' @return \code{coef}: coefficients for p, q and m
#'   
#' @details For starting values the Vector values need to be named in the case 
#'   \code{flexpq == T} 
#'   \eqn{p_1,\dots,p_j,q_1,\dots,q_j,m_1,\dots,m_j}{p1,..,pj,q1,...,qj,m1,...,mj}.
#'    In the case of \code{flexpq == F} \eqn{p_1, q_1, m_1,\dots, m_j}{p1, 
#'   q1,m1,..., mj}.
#'   
#'   If \code{gstart} is not provided, the generation starting points will be 
#'   detected automatically selecting the first value that is non-zero.
#'   
#' @references Norton, J.A. and Bass, F.M., 1987. A Diffusion Theory Model of 
#'   Adoption and Substitution for Successive Generations of High-Technology 
#'   Products.
#' @references Islam, T. and Meade, N., 1997. The Diffusion of Successive 
#'   Generations of a Technology: A More General Model. Technological 
#'   Forecasting and Social Change, 56, 49-60.
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}
#'   
#' @keywords internal
#'   
#' @examples 
#'  \dontrun{
#'    fitNB1 <- Nortonbass(tsIbm, startval.met = "2ST", estim.met = "OLS",
#'                         startval = NULL, flexpq = F, gstart = NULL)
#'    fitNB2 <- Nortonbass(tsIbm, startval.met = "2ST", estim.met = "SUR",
#'                         startval = NULL, flexpq = F, gstart = NULL)
#'    # using BOBYQA algorithm
#'    fitNB3 <- Nortonbass(tsIbm, startval.met = "2ST", estim.met = "BOBYQA",
#'                         startval = NULL, flexpq = F, gstart = NULL)
#'    # Create some plots
#'    plot(tsibm[, 1],type = "l", ylim=c(0,35000))
#'    lines(tsibm[, 2],col ="blue")
#'    lines(tsibm[, 3],col ="green")
#'    lines(tsibm[, 4],col ="pink")
#'    lines(fitNB1$fit$fitted[[1]], col = "black", lty = 2)
#'    lines(fitNB1$fit$fitted[[2]], col = "blue", lty = 2)
#'    lines(fitNB1$fit$fitted[[3]], col = "green", lty = 2)
#'    lines(fitNB1$fit$fitted[[4]], col = "pink", lty = 2)
#'    lines(fitNB2$fit$fitted[[1]], col = "black", lty = 3)
#'    lines(fitNB2$fit$fitted[[2]], col = "blue", lty = 3)
#'    lines(fitNB2$fit$fitted[[3]], col = "green", lty = 3)
#'    lines(fitNB2$fit$fitted[[4]], col = "pink", lty = 3)
#'    lines(fitNB3$fit$fitted[[1]], col = "black", lty = 4)
#'    lines(fitNB3$fit$fitted[[2]], col = "blue", lty = 4)
#'    lines(fitNB3$fit$fitted[[3]], col = "green", lty = 4)
#'    lines(fitNB3$fit$fitted[[4]], col = "pink", lty = 4)
#'    # read out RMSE
#'    fitNB1$fit$RMSE[[1]]
#'    fitNB1$fit$RMSE[[2]]
#'    fitNB1$fit$RMSE[[3]]
#'    fitNB1$fit$RMSE[[4]]
#'    fitNB2$fit$RMSE[[1]]
#'    fitNB2$fit$RMSE[[2]]
#'    fitNB2$fit$RMSE[[3]]
#'    fitNB2$fit$RMSE[[4]]
#'    fitNB3$fit$RMSE[[1]]
#'    fitNB3$fit$RMSE[[2]]
#'    fitNB3$fit$RMSE[[3]]
#'    fitNB3$fit$RMSE[[4]]
#'  }
#'  \dontshow{
#'    Nortonbass(tsIbm, startval.met = "2ST", estim.met = "OLS", startval = NULL, flexpq = FALSE, gstart = NULL)
#'  }
#'   
#' @rdname Nortonbass
#' @importFrom optimx optimx scalechk
#' @importFrom systemfit nlsystemfit
#' @export Nortonbass

Nortonbass <- function(x, startval.met = c("2ST", "BB", "iBM"),
                       estim.met = c("BOBYQA", "OLS", "SUR", "2SLS", "3SLS"),
                       gstart = NULL, startval = NULL, flexpq = F){
  
  # Set some basic variables
  gn <- ncol(x)
  startval.met <- startval.met[1]
  estim.met <- estim.met[1]
  
  # Input validation
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x needs to be matrix or data.frame")
  }
  
  if (!is.null(startval)) {
    if (flexpq == T) {
      parnum <- gn*3
    }else{
      parnum <- gn+2
    }
    
    if (flexpq == T) {
      startvalNames <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
    }else{
      startvalNames <- c("p1","q1", paste0("m", 1:gn))
    }
    
    # Error handling
    if (length(startval) != parnum) {
      stop("Lenght of startvalues not correct")
    }
    
    if (all(names(startval), startvalNames)) {
      if (flexpq == T) {
        stop("Startvalue vector need to be named in
             (p1,...,pj,q1,...,qj,m1,..,mj) form")
      }else{
        stop("Startvalues vector need to be named in (p1,q1,m1,..,mj) form")
      }
    }
    }
  
  if (!is.null(gstart)) {
    if (ncol(x) != length(gstart)) {
      stop("Number of generations in data does not match number of starting values")
    }
  }
  
  # get generations starting points
  if (is.null(gstart)) {
    gstart <- apply(x, 2, function(x) which(x > 0)[1])
  }
  
  x <- as.data.frame(x) # nlsystemfit requires dataframe format
  
  # Obtain parameter starting values
  if(is.null(startval)){
    
    # 2 Stage method
    if (startval.met == "2ST" & flexpq == T) {
      startval <- Nortonbass_startvalgen(x, gstart, flexpq, startval.met = "BB")
      
      param <- Nortonbass_estim(x, gn, gstart, startval, flexpq = F, estim.met)
      
      startval <- c(rep(param[1], gn), rep(param[2], gn), param[3:(gn+2)])
      names(startval) <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
      
    }else{
      startval <- Nortonbass_startvalgen(x, gstart, flexpq, startval.met)
    }
  }
  
  # estimate parameters of Norton Bass
  param <- Nortonbass_estim(x, gn, gstart, startval, flexpq, estim.met)
  
  # get error
  error <- Nortonbass_error(x, param, gstart, flexpq)
  
  return (list("param" = param, "fit" = error))
  }


Nortonbass_eqngen <- function(gn, flexpq = T){
  # Creates the Norton-Bass model equation
  
  # return values
  # eqns          Norton-Bass equation for gn number of generations
  # instr         Istruments required for three and two stage least squares
  
  # case p and q flexible
  if (flexpq == T) {
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
  eqnshat <- list()
  
  # loop across all generations
  for(g in 1:gn){
    
    if (g == 1) {
      pg <- sprintf("%s * %s", ft[g], m[g])
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s * (1-%s)", g, pg, ft[g+1]))
      eqnshat[[g]] <- sprintf("yhat[[%d]] <-  %s * (1-%s)", g, pg, ft[g+1])
    }else if (g == gn) {
      pg <- sprintf("%s * (%s + %s)", ft[g], m[g], pg)
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s", g, pg))
      eqnshat[[g]] <- sprintf("yhat[[%d]] <-  %s", g, pg)
    }else{
      pg <- sprintf("%s * (%s + %s)", ft[g], m[g], pg)
      eqns[[g]] <- as.formula(sprintf("y%d ~ %s * (1-%s)", g, pg, ft[g+1]))
      eqnshat[[g]] <- sprintf("yhat[[%d]] <-  %s * (1-%s)", g, pg, ft[g+1])
    }
  }
  return (list("eqns" = eqns, "inst" = inst, "eqnshat" = eqnshat))
}


Nortonbass_estim <- function(x, gn, gstart, startval, flexpq, estim.met){
  # estimates nortonbass parameters using systemfit() package at the moment
  # returns
  # fitted      list of each equation provided from including fitted values, SSE
  # param       obtained parameters
  
  # create devilishly nonlinear model equation
  mod <- Nortonbass_eqngen(gn, flexpq)
  
  # Fit devilishly nonlinear model
  if (estim.met == "BOBYQA") {
    
    param <- Nortonbass_optim(param = startval, x, gstart, gn, flexpq)
    
  }else if (estim.met == "OLS" | estim.met == "SUR") {
    
    # add time values to the x matrix
    for (i in 1:gn) {
      ti <- c(rep(0, (gstart[i]-1)), 1:(nrow(x)-(gstart[i]-1)))
      x <- cbind(x, ti)
    }
    
    colnames(x) <- c(paste0("y", 1:gn), paste0("t", 1:gn))
    x <- as.data.frame(x) # nlsystemfit requires dataframe format
    
    nbFit <- nlsystemfit(method = estim.met, eqns = mod$eqns, startvals = startval,
                                    data = x, maxiter = 1000)
    param <- nbFit$b
    
  }else{
    
    # add time values to the x matrix
    for (i in 1:gn) {
      ti <- c(rep(0, (gstart[i]-1)), 1:(nrow(x)-(gstart[i]-1)))
      x <- cbind(x, ti)
    }
    
    colnames(x) <- c(paste0("y", 1:gn), paste0("t", 1:gn))
    x <- as.data.frame(x) # nlsystemfit requires dataframe format
    
    nbFit <- nlsystemfit(method = estim.met, eqns = mod$eqns, startvals = startval,
                                    data = x, inst = mod$inst, maxiter = 1000)
    
    param <- nbFit$b
  }
  return(param)
}

#' Fits Norton Bass curve and estimated RMSE
#' 
#' @param x matrix with generations
#' @param gstart optional vector of starting points for the generations
#' @param flexpq For \code{startvalgen="BB"}. Allows parameters p and q to be flexible if set \code{TRUE}. 
#' @param startvalgen \code{"iBM"} fits individual Bass model to each generation;
#'   \code{"BB"} uses the approach described in Bass and Bass (2004).
#'   
#' @return starting values for all parameters
#' 
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}
#' 
#' @keywords internal
#' 
#' @rdname Nortonbass_startvalgen
#' @importFrom stats as.formula median
#' @export Nortonbass_startvalgen
Nortonbass_startvalgen <- function(x, gstart, flexpq, startval.met){
  # function to guess starting values to be past into the nonlinear optimiser
  # methods considered are:
  # i) "BB" --> Bass and Bass (2004) approach
  # ii) "iBM" --> indivudual Bass models
  
  gn <- length(gstart)
  n <- nrow(x)
  startval <- NULL
  
  if (startval.met == "iBM") {
    
    for (i in 1:gn) {
      # fitBass <- Bass_estim(x, estim = "nls")
      fitBass <- diffusion(x[gstart[i]:n, i], type = "bass", optim = "nm")$w
      
      # print(fitBass)
      names(fitBass) <- c(paste0("p", i), paste0("q", i), paste0("m", i))
      startval <- c(startval, fitBass)
    }
    
    if (gn > 1) {
      if (flexpq == F) {
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
    
    if (flexpq == F) {
      startval <- c(p, q, apply(x, 2, max)[1:gn])
      names(startval) <- c(paste0("p", 1), paste0("q", 1), paste0("m", 1:gn))
    }else{
      startval <- c(rep(p, gn), rep(q, gn), apply(x, 2, max)[1:gn])
      names(startval) <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
    }
  }
  return (startval)
}


#' Fits Norton Bass curve and estimated RMSE
#' 
#' @param x matrix with generations
#' @param param the parameters for curve to estimated
#' @param gstart optional vector of starting points for the generations
#' @param flexpq flexible p and q
#'   
#' @return yhat, the predicted values
#' @return actuals, the actual values
#' @return RMSE, the root mean squared error for each generation
#' 
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}
#' 
#' @keywords internal
#' 
#' @rdname Nortonbass_error
#' @export Nortonbass_error

Nortonbass_error <- function(x, param, gstart = NULL, flexpq = F){
  # calculates the insample errors
  #
  # returns
  # fitted      fitted values
  # actuals     actual values
  # RMSE        Root Mean Squared Error
  
  # the length and numbers of generations of series
  n <- nrow(x)
  gn <- ncol(x)
  
  # get generations starting points if not provided
  if (is.null(gstart)) {
    gstart <- apply(x, 2, function(x) which(x > 0)[1])
  }
  
  # get fitted values
  yhat <- Nortonbass_curve(gstart, n, param, flexpq)
  
  # prepare
  rmse <- list()
  
  # Insample Performance Measurement
  for (g in 1:gn) {
    rmse[[g]] <-  sqrt(mean((x[gstart[g]:n, g] - yhat[[g]][gstart[g]:n])^2))
  }
  
  #an alternative to get the errros
#   # get sum squared errors
#   sse <- mapply(function(x, xhat) sum((x - xhat)^2),
#                 x = as.list(x), xhat =  yhat, SIMPLIFY = F)
  
#   # optimise on the total sum squared errors
#   tsse <-  Reduce("+", sse)
  
  return(list("fitted" = yhat, "actuals" = x, "RMSE" = rmse))
}

Nortonbass_curve <- function(gstart, n, param, flexpq = F){
  # creates predicted values for each generation
  # 
  # returns
  # yhat        list with point forecasts for each generation
  
  # how many generations
  gn <- length(gstart)
  
  if (is.null(names(param))) {
    names(param) <- Nortonbass_paraname(gn, flexpq)
  }
  
  
  # create time values
  ti <- list()
  for (i in 1:gn) {
    ti[[i]] <- c(rep(0, (gstart[i]-1)), 1:(n-(gstart[i]-1)))
  }
  names(ti) <- paste0("t", 1:gn)
  
  # combine to list to be used as environment in the eval()
  estimators <- c(as.list(param), ti)

  # create devilishly nonlinear model equation
  mod <- Nortonbass_eqngen(gn, flexpq = flexpq)
  
  # estimate yhat
  yhat <- list()
  for (i in 1:gn){
    yhat[[i]] <- eval(parse(text = mod$eqns[[i]]), envir = estimators)
  }
  names(yhat) <- paste0("yhat", 1:gn)
  
  return(yhat)
}

#' @importFrom nloptr bobyqa
Nortonbass_optim <- function(param, x, gstart, gn, flexpq){
  
  paraminit <- param
  
  opt <- bobyqa(x0 = paraminit, fn = Nortonbass_costfun,
                lower = rep(0, length(param)),
                upper = rep(Inf, length(param)),
                x = x, gn = gn, gstart = gstart, flexpq = flexpq)
  
  optparam <- opt$par
  names(optparam) <- Nortonbass_paraname(gn, flexpq)
  return(optparam)
  
}

Nortonbass_costfun <- function(param, x, gn, gstart, flexpq){
  
  n <- nrow(x)

  # get predictions
  yhat <- Nortonbass_curve(gstart, n, param, flexpq)
  
  # get sum squared errors
  sse <- mapply(function(y, yhat) sum((y - yhat)^2)/(mean(y)^2),
         y = as.list(x), yhat =  yhat)
  
  # optimise on the total sum squared errors
  tsse <-  sum(sse)
  
  # get numbers of parameters p and q
  if (flexpq == F) {
    gn <- 1
  }
  
  if (any(param < 0)) {
    tsse <- 1e200
  }
  return(tsse)
}

Nortonbass_paraname <- function(gn, flexpq){
  # helper function for generating the parameter names depending on numbers of
  # generations and if flexible paramters or not
  # returns
  # paraname
  
  if (flexpq == F) {
    paraname <- c(paste0("p", 1), paste0("q", 1), paste0("m", 1:gn))
  }else{
    paraname <- c(paste0("p", 1:gn), paste0("q", 1:gn), paste0("m", 1:gn))
  }
  
  return(paraname)
}