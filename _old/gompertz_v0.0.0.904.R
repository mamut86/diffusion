# #' Gompertz model
# #'
# #' \code{Gompertz} fits a Gompertz model using non-linear least square
# #' estimation.
# #' 
# #' @param x vector of cumulative observations
# #' @param startval an optional Vector with starting for manual estimation
# #'   
# #' @return coefficients for a, b and c
# #' 
# #' @details For \code{startval} the Vector values needs to be named in the form
# #'   of (a, b, c). Else automated approximation of NLS is taking place using
# #'   Jukic et al. (2004) approach.
# #' 
# #' @references Jukic, D., Kralik, G. and Scitovski, R., 2004. Least-squares
# #'   fitting Gompertz curve. Journal of Computational and Applied Mathematics,
# #'   169, 359-375.
# #' @author Oliver Schaer, \email{info@@oliverschaer.ch}
# #' 
# #' @example examples/example_gompertz.R
# #'  
# #' @rdname Gompertz 
# #' @export Gompertz
# 
# Gompertz <- function(x, startval = NULL){
#   
#   # some error handling
#   startvalNames <- c("a","b", "c")
#   parnum <- 3
#   
#   # Error handling
#   if(!is.null(startval)){
#     if(length(startval) != parnum){
#       stop("Lenght of startvalues not correct")
#     }
#     
#     if(all(names(startval), startvalNames)){
#       stop("Startvalues vector need to be named in (a, b, c) form")
#       }
#   }
#   
#   if(any(diff(x) < 0)){
#     stop("x needs to be in strictly positive cumulative form (monotonic)")
#   }
#   
#   # get starting values if needed
#   if(is.null(startval)){
#     startval <- Gompertz_startvalgen(x)
#   }
#   
#   # estimate curve
#   param <- Gompertz_estim(x, startval)
#   
#   return(list("param" = param))
#   
# }
# 
# 
# Gompertz_estim <- function(x, startval){
#   
#   t0 <- 1:length(x)
#   fitGomp <- nls(x ~ exp(a - b*exp(-c*t0)), start = startval)
# 
#   return(coef(fitGomp))
# }
# 
# Gompertz_startvalgen <- function(x){
#   # get approximation of initial values using Jukic et al. 2004 approach
#   # get largest distance between t1 and t3 possible, t2 = (t1 + t3)/2
#   t0 <- c(1, floor((1+length(x))/2), length(x))
#   x0 <- x[t0]
#   
#   a <- log(x0[1]) - (((log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1])))
#   
#   b <- ((-(log(x0[2]) - log(x0[1]))^2) / (log(x0[3]) - (2*log(x0[2])) + log(x0[1]))) *
#     ((log(x0[2]) - log(x0[1])) / (log(x0[3]) - log(x0[2])))^(2*t0[1] / (t0[3]-t0[1]))
#   
#   c <- (-2 / (t0[3] - t0[1])) * log((log(x0[3]) - log(x0[2])) / (log(x0[2]) - log(x0[1])))
#   
#   startval <- c(a=a, b=b, c=c)
#   
#   return(startval)
# }
# 
# Gompertz_curve <- function(n, param){
#   # creates predicted values for each generation
#   # 
#   # returns
#   # yhat        vector with point forecasts
#   
#   # create time values
#   t0 <- 1:n
#   
#   a <- as.list(param)$a
#   b <- as.list(param)$b
#   c <- as.list(param)$c
#   
#   yhat <- exp(a - b*exp(-c*t0))
#   
#   return(yhat)
# }
# 
# Gompertz_error <- function(x, param){
#   # calculates the insample errors
#   #
#   # returns
#   # fitted      fitted values
#   # actuals     actual values
#   # RMSE        Root Mean Squared Error
#   
#   # the length and numbers of generations of series
#   n <- length(x)
#   
#   # get fitted values
#   yhat <- Gompertz_curve(n, param)
#   
#   # Insample Performance Measurement
#   rmse <-  sqrt(mean((x - yhat)^2))
# 
#   return(list("fitted" = yhat, "actuals" = x, "RMSE" = rmse))
# }