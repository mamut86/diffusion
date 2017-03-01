
# Bass model --------------------------------------------------------------

Bass <- function(x, h = 0, optim = F, estim.met = c("nls", "ols")){
  # Input
  # 
  
  # The Coefficients are estimated in x first step by the linear regression as proposed by Bass
  # Since the linear regression does not always provide satisfying or feasible estimates, the
  # function is further optimised on the MSE of the insample. If p and q can not be found in x
  # optimal region, they are manually set to values of either 0.00000001 or 0.99999999 for p and
  # 0.00000001 or 1.49999999 for q, depending whether the proposed values of the optim()-function
  # are above or below of 0 to 1 or 0 to 1.5, respectively.
  # Function requires only the dataset as input variable and will automatically become the origin.
  # Moreover horizon (h=) can be set if desired. The default value, however, is 100 days.
  # In case the datapoints are of cumulative basis this can be indicated by setting cum=True.
  # Default is False. If set to True forecasts are also given cumulated though.
  
  estim.met <- estim.met[1]
    
  pqm <- EstimBass(x, estim.met = estim.met)
  
  if(optim==T){
    pqm <- OptimBass(pqm, x)
  }
  
  fit <- FitBass(pqm, x, h)
  return(c(list("coef"= pqm), fit))
  
}

EstimBass = function(x, estim.met = c("nls", "ols")){
  
  estim.met = estim.met[1]
  
  #get LM set up
  y <- x
  x <- cumsum(x)
  x2 = x^2
  
  #OLS fitting
  #Regression fit
  fit.lm <-  lm(y~x+x2) 
  cf.lm <- fit.lm$coefficients
  names(cf.lm) <- c("c_0","c_1","c_2")
  
  #solving the quadratic and uncomplexing
  m <- polyroot(cf.lm) 
  m <- Re(m)
  
  #Derriving p, q, m
  m <- max(m)
  p <- cf.lm[1]/m
  q <- cf.lm[2]+p
  
  #NLS version
  if(estim.met == "nls"){
    
    T79 <- 1:length(y)

    # Method used by Nikos --> I think it has an error    
#     fit.nls <- nls(y ~ m * ( ((p+q)^2 / p) * exp(-(p+q) * T79) ) / (1 + (q/p) * exp(-(p+q)*T79))^2, 
#                    start = list(m=m, p=p, q=q), control = nls.control(warnOnly = T, minFactor = 0.000001))
#   
    # using Formula from Srinivasan & Mason 1986
    
    fit.nls <- nls(y ~ m*( (1-exp(-(p+q)*T79))/(1+(q/p)*exp(-(p+q)*T79)) - (1-exp(-(p+q)*(T79-1)))/(1+(q/p)*exp(-(p+q)*(T79-1))) ), 
                   start = list(p=p, q=q, m=m), control = nls.control(warnOnly = T, minFactor = 0.000001),
                   lower = c(0, 0, 0), algorithm = "port")
    

    cf.nls <- coef(fit.nls)
    m <- cf.nls[3]
    q <- cf.nls[2]
    p <- cf.nls[1]
    
  }
  
  # And return the output
  fit <- c(p, q, m)
  names(fit) <- c("p", "q", "m")
  return(fit)
}


FitBass <- function(pqm, x, h=0){
  
  n <- length(x)
  fc <- CurveBass(pqm, n, h)
  
  #Creat insample and out-of-sample
  fc.in <- fc[1:n,]
  if(h!=0) fc.out <- fc[(n+1):nrow(fc)]
  
  #Insample Performance Measurement
  rmse = sqrt(mean((x - fc.in[,1])^2))
  names(rmse) = c("RMSE")
  
  
  if(h !=0){
    return(list("fit" = fc.in, "actuals" = x, "out" = fc.out, "RMSE" = rmse))
  }else{
    return(list("fit" = fc.in, "actuals" = x, "RMSE" = rmse))
  }
}

CurveBass <- function(pqm, n, h=0){
  
  p <- pqm[1]
  q <- pqm[2]
  m <- pqm[3]
  
  fc <- array(0, c(n+h, 4), dimnames = list(NULL, c("Adoption", "Cumulative Adoption", "Innovators", "Imitators")))
  
  #Point forecast
  for(t in 1:(n+h)){
    # The first cumulative is assumed to be zero = no sales before launch
    if (t>1){
      fc[t,2] <- sum(fc[1:t,1])
    }
    fc[t,1] <- p*(m - fc[t,2])+q*(fc[t,2]*(m-fc[t,2]))/m
    fc[t,3] <- p*(m - fc[t,2])
    fc[t,4] <- fc[t,1] - fc[t,3]
  }
  
  return(fc)
}


OptimBass <- function(pqm, x){
  
  require(nloptr)
  
  #only submit positive numbers to 
  if(pqm[1]<0) pqm[1] <- 0.0000000001
  if(pqm[2]<0) pqm[2] <- 0.0000000001
  if(pqm[1]>5) pqm[1] <- 4.9999999999
  if(pqm[2]>5) pqm[2] <- 4.9999999999

  pqm.init <- pqm
  
  opt.pqm <- bobyqa(pqm.init, CostBass, lower = c(0, 0, 0), upper = c(5, 5, Inf), x = x)
  
  opt.pqm <- opt.pqm$par
  names(opt.pqm) <- c("p", "q", "m")
  return(opt.pqm)
  
}


CostBass <- function(pqm, x){
  
  sse <-  sum((x - FitBass(pqm, x)$fit[,1])^2)
  
  if (sum(pqm < 0) > 0 | pqm[1] > 1.5 | pqm[2] > 1.5 | pqm[3] == 0){
    sse <- 1e200
  }
  
  return(sse)
}
