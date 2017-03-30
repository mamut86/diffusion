# Implemented since v.0.0.0.905
#
## ---- Bass functions ----
# 
# bassCurve <- function(n, pqm){
#   # Generate bass curve
#   # n, sample size
#   # pqm, vector of parameters
#   
#   # Distribute parameters
#   p <- pqm[1]
#   q <- pqm[2]
#   m <- pqm[3]
#   
#   # Cumulative adoption
#   t <- 1:n
#   At <- m*(1-exp(-(p+q)*t))/(1+(q/p)*exp(-(p+q)*t))
#   # Adoption
#   at <- diff(c(0,At))
#   # Separate into innovator and imitators
#   innov <- p*(m - At)
#   imit <- at - innov
#   # Merge
#   Y <- cbind(At,at,innov,imit)
#   colnames(Y) <- c("Cumulative Adoption","Adoption","Innovators","Imitators")
#   
#   return(Y)
#   
# }
# 
# bassInit <- function(x){
#   # Internal function: get initial values using linear regression
#   # x in adoption per period
#   
#   # Estimate via linear regression
#   X <- cumsum(x)
#   X2 <- X^2
#   cf <- lm(x ~ X + X2)$coefficients
#   
#   # Solve the quadratic and get all p, q, m
#   m <- polyroot(cf) 
#   m <- Re(m)        
#   m <- max(m)       
#   p <- cf[1]/m
#   q <- cf[2]+p
#   
#   init <- c(p,q,m)
#   names(init) <- c("p","q","m")
#   
#   return(init)
#   
# }
# 
# bass <- function(x,pqm=NULL,cleanlead=c(TRUE,FALSE),prepqm=NULL,l=2,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE)){
#   # Bass model
#   # x, adoption per period
#   # pqm, vector of model parameters. If provided no estimation is done.
#   # cleanlead, removes leading zeros
#   # prepqm, the pqm of the previous generation - this is used for sequential fitting
#   # l, the l-norm (1 is absolute errors, 2 is squared errors)
#   # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#   # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#   # sig, significance level used to eliminate parameters
#   # verbose, if TRUE provide console output during estimation
#   
#   cleanlead <- cleanlead[1]
#   if (cleanlead == TRUE){
#     x <- cleanzero(x)$x
#   }
#   
#   eliminate <- eliminate[1]
#   verbose <- verbose[1]
#   
#   # Optimise parameters
#   if (is.null(pqm)){
#     opt <- bassEstim(x,l,prepqm,pvalreps,eliminate,sig,verbose)
#     pqm <- opt$pqm
#     pval <- opt$pval
#   } else {
#     pval <- rep(NA,3)
#   }
#   
#   n <- length(x)
#   fit <- bassCurve(n,pqm)
#   
#   mse <- mean((x - fit[,2])^2)
#   
#   return(structure(list("method"="Bass","pqm"=pqm,"x"=x,"fit"=fit,"mse"=mse,"prepqm"=prepqm,"pval"=pval),class="bass"))
#   
# }
# 
# bassEstim <- function(x,l=2,prepqm=NULL,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE)){
#   # Internal function: estimate bass parameters 
#   # x, adoption per period
#   # l, the l-norm (1 is absolute errors, 2 is squared errors)
#   # prepqm, the pqm of the previous generation - this is used for sequential fitting
#   # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#   # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#   # sig, significance level used to eliminate parameters
#   # verbose, if TRUE provide console output during estimation
#   
#   # Defaults for eliminating parameters
#   eliminate <- eliminate[1]
#   verbose <- verbose[1]
#   if (eliminate == TRUE & pvalreps == 0){
#     pvalreps <- 1000
#     warning("To eliminate parameters p-values must be estimated. Setting pvalreps=1000.")
#   }
#   
#   # Initially all parameters are estimated
#   pqm.idx=rep(TRUE,3)         # Which parameters to estimate 
#   
#   # Check botstrap repetitions (pvalreps)
#   if (pvalreps>0 & pvalreps < 500){
#     warning("Very few bootstraps, unreliable p-values.")
#   }
#   if (pvalreps<0){
#     stop("pvalreps must be positive.")
#   }
#   if (pvalreps==0 & eliminate == TRUE){
#     stop("To eliminate coefficients from the estimation p-values need to be calculated. Use positive pvalreps.")
#   }
#   
#   # Initialise
#   if (is.null(prepqm)){
#     prepqm <- rep(0,3)
#   }
#   init <- bassInit(x)
#   init <- init - prepqm
#   init[(init + prepqm)<=0] <- 0.00001
#   n <- length(x)
#   
#   # Iterate until all p-values are < sig
#   # If eliminate is not requested it will only iterate once
#   elim <- TRUE; it <- 1
#   while (elim == TRUE){
#     
#     # Optimise
#     pqm <- rep(0,3)
#     if (sum(pqm.idx)>0){
#       if (sum(pqm.idx)>1){
#         pqm.new <- optim(init[pqm.idx],bassCost,x=x,l=l,prepqm=prepqm,pqm.idx=pqm.idx)$par    
#       } else {
#         pqm.new <- optim(init[pqm.idx],bassCost,x=x,l=l,prepqm=prepqm,pqm.idx=pqm.idx,method="BFGS")$par    
#       }
#       pqm[pqm.idx] <- pqm.new
#     }
#     
#     # Bootstrap p-values
#     if (pvalreps > 0){
#       # Calculate sigma
#       yhat <- bassCurve(n,prepqm+pqm)[,2]
#       sigma <- sqrt(mean((x - yhat)^2))
#       # Construct bootstraps
#       pqmboot <- array(NA,c(pvalreps,3))
#       yboot <- matrix(rnorm(n*pvalreps,0,sigma),nrow=n) + matrix(rep(yhat,pvalreps),ncol=pvalreps)
#       # Estimate model
#       for (i in 1:pvalreps){
#         pqmboot[i,] <- bassEstim(yboot[,i],l,pvalreps=0)$pqm - prepqm
#       }
#       pval <- colMeans((abs(pqmboot - matrix(rep(colMeans(pqmboot),pvalreps),ncol=3,byrow=TRUE))) > abs(matrix(rep(pqm,pvalreps),ncol=3,byrow=TRUE)))
#     } else {
#       pval <- rep(NA,3)
#     }
#     # Elimination process
#     if (eliminate == TRUE & any(pval[pqm.idx]>sig)){
#       # Find most insignificant
#       pval.temp <- pval
#       pval.temp[pval.temp<sig] <- sig
#       pval.temp <- pval.temp - sig
#       pval.temp[!pqm.idx] <- 0
#       loc <- which(pval.temp == max(pval.temp))[1]
#       pqm.idx[loc] <- FALSE
#     } else {
#       # Stop elimination iterations
#       elim <- FALSE
#       loc <- NA
#     }
#     # Provide console output
#     if (verbose == TRUE){
#       writeLines(paste0("Estimation iteration: ",it))
#       it <- it + 1
#       if (!is.na(loc)){
#         locv <- rep("",3)
#         locv[loc] <- "X"
#       } else {
#         locv <- NULL
#       }
#       temp <- cbind(round(cbind(pqm,pval),4),locv)
#       rownames(temp) <- c("p","q","M")
#       colnames(temp) <- c("Estimate","p-value","")[1:(2+!is.na(loc))]
#       print(temp,quote=FALSE)
#       if (elim == FALSE){
#         writeLines("Estimation completed")
#       } 
#       writeLines("")
#     }
#     
#   }
#   
#   pqm <- pqm + prepqm
#   
#   names(pval) <- names(pqm)
#   return(list("pqm"=pqm,"pval"=pval))
#   
# }
# 
# bassCost <- function(pqm,x,l,pqm.idx=rep(TRUE,3),prepqm=NULL){
#   # Internal function: cost function for numerical optimisation
#   # pqm, current parameters
#   # x, adoption per period
#   # l, the l-norm (1 is absolute errors, 2 is squared errors)
#   # pqm.idx, logical vector with three elements. Use FALSE to not estimate respective parameter
#   # prepqm, the pqm of the previous generation - this is used for sequential fitting
#   
#   n <- length(x)
#   
#   # If some elements of pqm are not optimised, sort out vectors
#   pqm.all <- rep(0,3)
#   pqm.all[pqm.idx] <- pqm
#   # If sequential construct total parameters
#   if (is.null(prepqm)){
#     basspqm <- pqm.all    
#   } else {
#     basspqm <- pqm.all + prepqm
#   }
#   
#   fit <- bassCurve(n,basspqm)
#   
#   if (l == 1){
#     se <- sum(abs(x-fit[,2]))
#   } else if (l == 2){
#     se <- sum((x-fit[,2])^2)
#   } else {
#     se <- sum(abs(x-fit[,2])^l)
#   }
#   
#   # Ensure positive coefficients
#   if (any(basspqm<=0)){
#     se <- 10e200
#   }
#   
#   return(se)
#   
# }
# 
# forecast.bass <- function(object,h){
#   # Produce forecasts for Bass
#   # object, estimated bass model using bass
#   # h, forecast horizon
#   
#   n <- length(object$x)
#   xhat <- bassCurve(n+h,object$pqm)[(n+1):(n+h),]
#   
#   # Append forecasts to bass object
#   return(structure(c(object,list("mean"=xhat[,2],"xhat"=xhat)),class="bass"))
#   
# }
# 
# print.bass <- function(x, ...){
#   # Print console output for bass
#   # x, object estimated using bass
#   
#   writeLines(paste(x$method,"model"))
#   writeLines("")
#   writeLines("Parameters:")
#   if (is.null(x$prepqm)){
#     temp <- round(cbind(x$pqm,x$pval),4)    
#     colnames(temp) <- c("Estimate","p-value")
#   } else {
#     temp <- round(cbind(x$pqm,x$pqm-x$prepqm,x$pval),4)    
#     colnames(temp) <- c("Estimate","Marginal"," Marginal p-value")
#   }
#   rownames(temp) <- c("p - Coefficient of innovation","q - Coefficient of imitation","M - Market potential")
#   print(temp)
#   writeLines("")
#   writeLines(paste("sigma:",round(sqrt(x$mse),4)))
#   
# }
# 
# plot.bass <- function(x, cumulative=c(FALSE,TRUE),...){
#   # Plot bass curves
#   # x, object estimated using bass
#   # cumulative, if TRUE plot cumulative adoption
#   
#   cumulative <- cumulative[1]
#   
#   # Colorbrewer colours
#   cmp <- c("#E41A1C","#377EB8","#4DAF4A")
#   # Check if forecasts exist and construct xx
#   if (exists("xhat",where=x)){
#     xx <- c(1,(length(x$x)+length(x$mean)))
#   } else {
#     xx <- c(1,(length(x$x)))
#   }
#   
#   if (cumulative == FALSE){
#     
#     # Get yy min-max
#     yy <- range(cbind(x$x,x$fit[,2:4]))
#     yy <- yy + c(-1,1)*0.04*diff(yy)
#     yy[1] <- max(0,yy[1])
#     
#     # Plot fit
#     plot(x$x,type="p",pch=20,ylab="Adoption",xlab="Period",ylim=yy,xlim=xx,main=x$method)
#     for (i in 1:3){
#       lines(x$fit[,1+i],col=cmp[i])
#     }
#     # Check if forecasts exist and plot
#     if (exists("xhat",where=x)){
#       for (i in 1:3){
#         lines((length(x$x)+1):xx[2],xhat[,i+1],col=cmp[i])
#       }
#     }
#     legend("topleft",c("Adoption","Innovators","Imitators"),col=cmp,lty=1,bty="n")
#     
#   } else {
#     # Cumulative plot
#     
#     # Get yy min-max
#     yy <- range(cbind(cumsum(x$x),x$fit[,1]))
#     yy <- yy + c(-1,1)*0.04*diff(yy)
#     yy[1] <- max(0,yy[1])
#     
#     # Plot fit
#     plot(cumsum(x$x),type="p",pch=20,ylab="Cumulative Adoption",xlab="Period",ylim=yy,xlim=xx,main=x$method)
#     lines(x$fit[,1],col=cmp[1])
#     for (i in 1:2){
#       lines(cumsum(x$fit[,2+i]),col=cmp[i+1])
#     }
#     # Check if forecasts exist and plot
#     if (exists("xhat",where=x)){
#       fstart <- apply(x$fit,2,cumsum)[length(x$x),2:4]
#       for (i in 1:3){
#         lines((length(x$x)+1):xx[2],cumsum(xhat[,i+1])+fstart[i],col=cmp[i])
#       }
#     }
#     legend("bottomright",c("Adoption","Innovators","Imitators"),col=cmp,lty=1,bty="n")
#     
#     
#   }
#   
# }
# 
# cleanzero <- function(x){
#   # Internal function: remove leadig zeros
#   # x, vector of values
#   
#   idx <- which(x == 0)
#   n <- length(x)
#   l <- length(idx)
#   if (l>0 & idx[1]==1){
#     d.idx <- diff(idx)
#     loc <- which(d.idx > 1)[1]
#     if (is.na(loc)){
#       loc <- l
#     }
#     x <- x[(loc+1):n]
#   } else {
#     loc <- 1
#   }
#   return(list("x"=x,"loc"=loc))
# }
# 
# ## ---- Sequential Bass Functions ----
# 
# seqbass  <- function(x,cleanlead=c(TRUE,FALSE),prepqm=NULL,l=2,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE)){
#   # Sequential bass fit across generations
#   # This is calling bass multiple times and organises the output
#   # x, adoption per period
#   # cleanlead, removes leading zeros
#   # prepqm, the pqm of the previous generation - this is used for sequential fitting
#   # l, the l-norm (1 is absolute errors, 2 is squared errors)
#   # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#   # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#   # sig, significance level used to eliminate parameters
#   # verbose, if TRUE provide console output during estimation
#   
#   verbose <- verbose[1]
#   
#   # Number of curves
#   k <- dim(x)[2]
#   
#   fit <- vector("list",k)
#   names(fit) <- paste0("Gen",1:k)
#   
#   # Fit iteratively across generations
#   for (i in 1:k){
#     
#     if (verbose == TRUE){
#       writeLines(paste0("Generation ",i))
#     }
#     if (i > 1){
#       prepqm <- fit[[i-1]]$pqm
#       elimin <- eliminate
#     } else {
#       elimin <- FALSE
#     }
#     fit[[i]] <- bass(x[,i],pqm=NULL,cleanlead,prepqm,l,pvalreps,elimin,sig,verbose)
#     
#   }
#   
#   allpqm <- do.call(rbind,lapply(fit,function(x){x$pqm}))
#   allmse <- do.call(rbind,lapply(fit,function(x){x$mse}))
#   allpval <- do.call(rbind,lapply(fit,function(x){x$pval}))
#   
#   return(structure(list("method"="Sequential Bass","bass"=fit,"x"=x,"pqm"=allpqm,"mse"=allmse,"pval"=allpval),class="seqbass"))
#   
# }
# 
# print.seqbass <- function(x, ...){
#   # Print console output for bass
#   # x, object estimated using bass
#   
#   writeLines(paste(x$method,"model"))
#   writeLines("")
#   writeLines("Parameters:")
#   temp <- round(cbind(cbind(x$pqm,x$pval)[,c(1,4,2,5,3,6)],sqrt(x$mse)),4)
#   colnames(temp) <- c("p coef.","pval.","q coef.","pval.","M size","pval.","sigma")
#   print(temp)
#   writeLines("")
#   
# }
# 
# plot.seqbass <- function(x,cumulative=c(FALSE,TRUE),...){
#   # Plot sequential bass curves
#   # x, object estimated using bass
#   # cumulative, if TRUE plot cumulative adoption
#   
#   cumulative <- cumulative[1]
#   
#   cmp <- c("#B2182B","#EF8A62","#67A9CF","#2166AC")
#   k <- dim(x$x)[2]
#   cmp <- colorRampPalette(cmp)(k)
#   N <- dim(x$x)[1]
#   
#   if (cumulative == FALSE){
#     X <- x$x
#     ll <- 2
#   } else {
#     X <- apply(x$x,2,cumsum)
#     ll <- 1
#   }
#   
#   yy <- range(X)
#   yy <- yy + c(-1,1)*0.04*diff(yy)
#   yy[1] <- max(0,yy[1])
#   
#   plot(NA,NA,xlim=c(1,N),ylim=yy,xlab="Period",ylab="Adoption",main=x$method)
#   for (i in 1:k){
#     x.temp <- X[,i]
#     x.temp <- cleanzero(x.temp)
#     n <- length(x.temp$x)
#     l <- x.temp$loc
#     xx <- l:(l+n-1)
#     points(xx,x.temp$x,col="black",pch=21,bg=cmp[i],cex=0.7)
#     lines(xx,x$bass[[i]]$fit[,ll],col=cmp[i],lwd=2)
#   }
#   
# }