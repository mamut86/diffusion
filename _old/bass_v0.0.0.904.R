# #' Bass model
# #'
# #' \code{Bass} fits a Bass model (Bass 1969)
# #' 
# #' @param x vector of non-cumulative observed data to fit Bass model on
# #' @param h desired step-ahead forecast (to become depreciated) [default = 0]
# #' @param optim additional optimisation on the insample MSE
# #' @param estim.met non-linear least squares (\code{"nls"}) or ordinary least
# #'   squares (\code{"ols"}) [default "nls"]
# #'   
# #' @return \code{coef}: coefficients p, q and m
# #' @return list of fitted values:
# #' \itemize{
# #' \item \code{fit} fitted values
# #' \item \code{actuals} actual values
# #' \item \code{out} predicted values
# #' \item \code{RMSE} insample root mean squared error
# #' }
# #' 
# #' @details The Coefficients are estimated by first fitting a linear regression 
# #'   as proposed by Bass (1969). This then serves as starting values for the 
# #'   non-linear least square estimation (Srinivasan and Mason 1986).
# #'   Furthermore, parameters can be additionally optimised on the MSE of the
# #'   insample following the suggestion of Lilien et al. (2000) using BOBYQA
# #'   algorithm (Powell 2009). If p and q can not be determined optimal region,
# #'   they are manually set to values of either 0.00000001 or 0.99999999 for p
# #'   and 0.00000001 or 1.49999999 for q, depending whether the proposed values
# #'   of the optim()-function are above or below of 0 to 1 or 0 to 1.5,
# #'   respectively.
# #'   
# #' @references Bass, F.M., 1969. A new product growth for model consumer 
# #'   durables. Management Science 15(5), 215-227.
# #' @references Srinivasan, V., and Mason, C.H., 1986. Nonlinear Least Squares
# #'   Estimation of New Product Diffusion MOdels. Marketing Science, 5(2),
# #'   169-178.
# #' @references Lilien, G.Y., Rangaswamy, A. and Van den Bulte, C., 2000. 
# #'   Diffusion models: Managerial applications and software. In: Mahajan, V., 
# #'   Muller, E., Wind, Y. (Eds.), New-Product Diffusion Models. Kluwer, 
# #'   Massachusetts, 295-311.
# #' @references Powell, M.J.D., 2009. The QOBYQA algorithm for bound constrained 
# #'   optimization witouth derivatives. Department of Applied Mathematics and 
# #'   Theorteical Physics, Cambridge University. DAMTP 2009/NA06.
# #' @author Oliver Schaer, \email{info@@oliverschaer.ch} 
# #'  
# #' @rdname Bass  
# #' @export Bass
# 
# Bass <- function(x, h = 0, optim = F, estim.met = c("nls", "ols")){
# 
#   estim.met <- estim.met[1]
#     
#   pqm <- Bass_estim(x, estim.met = estim.met)
#   
#   if(optim==T){
#     pqm <- Bass_optim(pqm, x)
#   }
#   
#   fit <- Bass_fit(pqm, x, h)
#   return(c(list("coef"= pqm), fit))
#   
# }
# 
# Bass_estim <- function(x, estim.met = c("nls", "ols")){
#   
#   estim.met = estim.met[1]
#   
#   #get LM set up
#   y <- x
#   x <- cumsum(x)
#   x2 = x^2
#   
#   #OLS fitting
#   #Regression fit
#   fit.lm <-  lm(y~x+x2) 
#   cf.lm <- fit.lm$coefficients
#   names(cf.lm) <- c("c_0","c_1","c_2")
#   
#   #solving the quadratic and uncomplexing
#   m <- polyroot(cf.lm) 
#   m <- Re(m)
#   
#   #Derriving p, q, m
#   m <- max(m)
#   p <- cf.lm[1]/m
#   q <- cf.lm[2]+p
#   
#   #NLS version
#   if(estim.met == "nls"){
#     
#     T79 <- 1:length(y)
# 
#     # Method used by Nikos --> I think it has an error    
# #     fit.nls <- nls(y ~ m * ( ((p+q)^2 / p) * exp(-(p+q) * T79) ) / (1 + (q/p) * exp(-(p+q)*T79))^2, 
# #                    start = list(m=m, p=p, q=q), control = nls.control(warnOnly = T, minFactor = 0.000001))
# #   
#     # using Formula from Srinivasan & Mason 1986
#     
#     fit.nls <- nls(y ~ m*( (1-exp(-(p+q)*T79))/(1+(q/p)*exp(-(p+q)*T79)) - (1-exp(-(p+q)*(T79-1)))/(1+(q/p)*exp(-(p+q)*(T79-1))) ), 
#                    start = list(p=p, q=q, m=m), control = nls.control(warnOnly = T, minFactor = 0.000001),
#                    lower = c(0, 0, 0), algorithm = "port")
#     
# 
#     cf.nls <- coef(fit.nls)
#     m <- cf.nls[3]
#     q <- cf.nls[2]
#     p <- cf.nls[1]
#     
#   }
#   
#   # And return the output
#   fit <- c(p, q, m)
#   names(fit) <- c("p", "q", "m")
#   return(fit)
# }
# 
# 
# Bass_fit <- function(pqm, x, h=0){
#   
#   n <- length(x)
#   fc <- Bass_curve(pqm, n, h)
#   
#   #Creat insample and out-of-sample
#   fc.in <- fc[1:n,]
#   if(h!=0) fc.out <- fc[(n+1):nrow(fc)]
#   
#   #Insample Performance Measurement
#   rmse = sqrt(mean((x - fc.in[,1])^2))
#   names(rmse) = c("RMSE")
#   
#   
#   if(h !=0){
#     return(list("fit" = fc.in, "actuals" = x, "out" = fc.out, "RMSE" = rmse))
#   }else{
#     return(list("fit" = fc.in, "actuals" = x, "RMSE" = rmse))
#   }
# }
# 
# Bass_curve <- function(pqm, n, h=0){
#   
#   p <- pqm[1]
#   q <- pqm[2]
#   m <- pqm[3]
#   
#   fc <- array(0, c(n+h, 4), dimnames = list(NULL, c("Adoption", "Cumulative Adoption", "Innovators", "Imitators")))
#   
#   #Point forecast
#   for(t in 1:(n+h)){
#     # The first cumulative is assumed to be zero = no sales before launch
#     if (t>1){
#       fc[t,2] <- sum(fc[1:t,1])
#     }
#     fc[t,1] <- p*(m - fc[t,2])+q*(fc[t,2]*(m-fc[t,2]))/m
#     fc[t,3] <- p*(m - fc[t,2])
#     fc[t,4] <- fc[t,1] - fc[t,3]
#   }
#   
#   return(fc)
# }
# 
# 
# Bass_optim <- function(pqm, x){
#   
#   #only submit positive numbers to 
#   if(pqm[1]<0) pqm[1] <- 0.0000000001
#   if(pqm[2]<0) pqm[2] <- 0.0000000001
#   if(pqm[1]>5) pqm[1] <- 4.9999999999
#   if(pqm[2]>5) pqm[2] <- 4.9999999999
# 
#   pqm.init <- pqm
#   
#   opt.pqm <- nloptr::bobyqa(pqm.init, Bass_costfun, lower = c(0, 0, 0), upper = c(5, 5, Inf), x = x)
#   
#   opt.pqm <- opt.pqm$par
#   names(opt.pqm) <- c("p", "q", "m")
#   return(opt.pqm)
#   
# }
# 
# 
# Bass_costfun <- function(pqm, x){
#   
#   sse <-  sum((x - Bass_fit(pqm, x)$fit[,1])^2)
#   
#   if (sum(pqm < 0) > 0 | pqm[1] > 1.5 | pqm[2] > 1.5 | pqm[3] == 0){
#     sse <- 1e200
#   }
#   
#   return(sse)
# }