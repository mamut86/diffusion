# Implemented with v.0.1.0.905
#
# ## ---- Gompertz functions ----
# 
# gompertzCurve <- function(n, w){
#     # Generate Gompertz curve
#     # n, sample size
#     # w, vector of parameters
#     
#     t <- 1:n
#     # Cumulative
#     At <- w[3] * exp(- w[1] * exp(-w[2] * t))
#     # Adoption
#     at <- diff(c(0,At))
#     Y <- cbind(At,at)
#     colnames(Y) <- c("Cumulative Adoption","Adoption")
#     
#     return(Y)
#     
# }
# 
# gompertzInit <- function(x){
#     # Internal function: get initial values
#     # x in adoption per period
#     
#     n <- length(x)
#     x <- cumsum(x)
#     
#     t0 <- c(1, floor((1 + n)/2), n)
#     x0 <- x[t0]
#     m <- (x0[1]) - ((((x0[2]) - (x0[1]))^2)/((x0[3]) - (2 * (x0[2])) + (x0[1])))
#     
#     a <- ((-(log(x0[2]) - log(x0[1]))^2)/(log(x0[3]) - (2 * 
#                                                             log(x0[2])) + log(x0[1]))) * ((log(x0[2]) - log(x0[1]))/(log(x0[3]) - 
#                                                                                                                          log(x0[2])))^(2 * t0[1]/(t0[3] - t0[1]))
#     b <- (-2/(t0[3] - t0[1])) * log((log(x0[3]) - log(x0[2]))/(log(x0[2]) - 
#                                                                    log(x0[1])))
#     w <- c(a,b,m)
#     names(w) <- c("a","b","m")
#     return(w)
#     
# }
# 
# gompertzCost <- function(w,x,l,w.idx=rep(TRUE,3),prew=NULL){
#     # Internal function: cost function for numerical optimisation
#     # w, current parameters
#     # x, adoption per period
#     # l, the l-norm (1 is absolute errors, 2 is squared errors)
#     # w.idx, logical vector with three elements. Use FALSE to not estimate respective parameter
#     # prew, the w of the previous generation - this is used for sequential fitting
#     
#     n <- length(x)
#     
#     # If some elements of w are not optimised, sort out vectors
#     w.all <- rep(0,3)
#     w.all[w.idx] <- w
#     # If sequential construct total parameters
#     if (is.null(prew)){
#         gompw <- w.all    
#     } else {
#         gompw <- w.all + prew
#     }
#     
#     fit <- gompertzCurve(n,gompw)
#     
#     if (l == 1){
#         se <- sum(abs(x-fit[,2]))
#     } else if (l == 2){
#         se <- sum((x-fit[,2])^2)
#     } else {
#         se <- sum(abs(x-fit[,2])^l)
#     }
#     
#     # Ensure positive coefficients
#     if (any(gompw<=0)){
#         se <- 10e200
#     }
#     
#     return(se)
#     
# }
# 
# forecast.gompertz <- function(object,h){
#     # Produce forecasts for Gompertz
#     # object, estimated gompertz model using diffuse
#     # h, forecast horizon
#     
#     n <- length(object$x)
#     xhat <- gompertzCurve(n+h,object$w)[(n+1):(n+h),]
#     
#     # Append forecasts to bass object
#     return(structure(c(object,list("mean"=xhat[,2],"xhat"=xhat)),class="gompertz"))
#     
# }
# 
# print.gompertz <- function(x, ...){
#     # Print console output for gompertz
#     # x, object estimated using diffusion
#     
#     diffusePrint(x, ...)
#     
# }
# 
# plot.gompertz <- function(x, cumulative=c(FALSE,TRUE),...){
#     # Plot bass curves
#     # x, object estimated using bass
#     # cumulative, if TRUE plot cumulative adoption
#     
#     diffusePlot(x, cumulative,...)
#     
# }
# 
# ## ---- Bass functions ----
# 
# bassCurve <- function(n, w){
#     # Generate bass curve
#     # n, sample size
#     # w, vector of parameters
#     
#     # Cumulative adoption
#     t <- 1:n
#     At <- w[3]*(1-exp(-(w[1]+w[2])*t))/(1+(w[2]/w[1])*exp(-(w[1]+w[2])*t))
#     # Adoption
#     at <- diff(c(0,At))
#     # Separate into innovator and imitators
#     innov <- w[1]*(w[3] - At)
#     imit <- at - innov
#     # Merge
#     Y <- cbind(At,at,innov,imit)
#     colnames(Y) <- c("Cumulative Adoption","Adoption","Innovators","Imitators")
#     
#     return(Y)
#     
# }
# 
# bassInit <- function(x){
#     # Internal function: get initial values using linear regression
#     # x in adoption per period
#     
#     # Estimate via linear regression
#     X <- cumsum(x)
#     X2 <- X^2
#     cf <- lm(x ~ X + X2)$coefficients
#     
#     # Solve the quadratic and get all p, q, m
#     m <- polyroot(cf) 
#     m <- Re(m)        
#     m <- max(m)       
#     p <- cf[1]/m
#     q <- cf[2]+p
#     
#     init <- c(p,q,m)
#     names(init) <- c("p","q","m")
#     
#     return(init)
#     
# }
# 
# bassCost <- function(w,x,l,w.idx=rep(TRUE,3),prew=NULL){
#     # Internal function: cost function for numerical optimisation
#     # w, current parameters
#     # x, adoption per period
#     # l, the l-norm (1 is absolute errors, 2 is squared errors)
#     # w.idx, logical vector with three elements. Use FALSE to not estimate respective parameter
#     # prew, the w of the previous generation - this is used for sequential fitting
#     
#     n <- length(x)
#     
#     # If some elements of w are not optimised, sort out vectors
#     w.all <- rep(0,3)
#     w.all[w.idx] <- w
#     # If sequential construct total parameters
#     if (is.null(prew)){
#         bassw <- w.all    
#     } else {
#         bassw <- w.all + prew
#     }
#     
#     fit <- bassCurve(n,bassw)
#     
#     if (l == 1){
#         se <- sum(abs(x-fit[,2]))
#     } else if (l == 2){
#         se <- sum((x-fit[,2])^2)
#     } else {
#         se <- sum(abs(x-fit[,2])^l)
#     }
#     
#     # Ensure positive coefficients
#     if (any(bassw<=0)){
#         se <- 10e200
#     }
#     
#     return(se)
#     
# }
# 
# forecast.bass <- function(object,h){
#     # Produce forecasts for Bass
#     # object, estimated bass model using bass
#     # h, forecast horizon
#     
#     n <- length(object$x)
#     xhat <- bassCurve(n+h,object$w)[(n+1):(n+h),]
#     
#     # Append forecasts to bass object
#     return(structure(c(object,list("mean"=xhat[,2],"xhat"=xhat)),class="bass"))
#     
# }
# 
# print.bass <- function(x, ...){
#     # Print console output for bass
#     # x, object estimated using diffusion
#     
#     diffusePrint(x, ...)
#     
# }
# 
# plot.bass <- function(x, cumulative=c(FALSE,TRUE),...){
#     # Plot bass curves
#     # x, object estimated using bass
#     # cumulative, if TRUE plot cumulative adoption
#     
#     diffusePlot(x, cumulative,...)
#     
# }
# 
# ## ---- Diffusion general functions ----
# 
# diffusion <- function(x,w=NULL,cleanlead=c(TRUE,FALSE),prew=NULL,l=2,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE),
#                       type=c("bass","gompertz")){
#     # Bass model
#     # x, adoption per period
#     # w, vector of model parameters. If provided no estimation is done.
#     # cleanlead, removes leading zeros
#     # prew, the w of the previous generation - this is used for sequential fitting
#     # l, the l-norm (1 is absolute errors, 2 is squared errors)
#     # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#     # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#     # sig, significance level used to eliminate parameters
#     # verbose, if TRUE provide console output during estimation
#     # type, diffusion model to use
#     
#     type <- match.arg(type,c("bass","gompertz"))
#     
#     cleanlead <- cleanlead[1]
#     if (cleanlead == TRUE){
#         x <- cleanzero(x)$x
#     }
#     
#     eliminate <- eliminate[1]
#     verbose <- verbose[1]
#     
#     # Optimise parameters
#     if (is.null(w)){
#         opt <- diffuseEstim(x,l,prew,pvalreps,eliminate,sig,verbose,type=type)
#         w <- opt$w
#         pval <- opt$pval
#     } else {
#         pval <- rep(NA,3)
#     }
#     
#     n <- length(x)
#     if (type=="bass"){
#         fit <- bassCurve(n,w)
#     } else if (type=="gompertz"){
#         fit <- gompertzCurve(n,w)
#     }
#     
#     mse <- mean((x - fit[,2])^2)
#     
#     if (type == "bass"){
#         out <- structure(list("method"="Bass","call"=sys.call(),"w"=w,"x"=x,"fit"=fit,"mse"=mse,"prew"=prew,"pval"=pval),class="bass")
#     } else {
#         out <- structure(list("method"="Gompertz","call"=sys.call(),"w"=w,"x"=x,"fit"=fit,"mse"=mse,"prew"=prew,"pval"=pval),class="gompertz")
#     }
#     
#     return(out)
#     
# }
# 
# diffuseEstim <- function(x,l=2,prew=NULL,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE),
#                          type=c("bass","gompertz")){
#     # Internal function: estimate bass parameters 
#     # x, adoption per period
#     # l, the l-norm (1 is absolute errors, 2 is squared errors)
#     # prew, the w of the previous generation - this is used for sequential fitting
#     # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#     # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#     # sig, significance level used to eliminate parameters
#     # verbose, if TRUE provide console output during estimation
#     # type, diffusion model to use
#     
#     type <- match.arg(type,c("bass","gompertz"))
#     
#     # Defaults for eliminating parameters
#     eliminate <- eliminate[1]
#     verbose <- verbose[1]
#     if (eliminate == TRUE & pvalreps == 0){
#         pvalreps <- 1000
#         warning("To eliminate parameters p-values must be estimated. Setting pvalreps=1000.")
#     }
#     
#     # Initially all parameters are estimated
#     w.idx=rep(TRUE,3)         # Which parameters to estimate 
#     
#     # Check botstrap repetitions (pvalreps)
#     if (pvalreps>0 & pvalreps < 500){
#         warning("Very few bootstraps, unreliable p-values.")
#     }
#     if (pvalreps<0){
#         stop("pvalreps must be positive.")
#     }
#     if (pvalreps==0 & eliminate == TRUE){
#         stop("To eliminate coefficients from the estimation p-values need to be calculated. Use positive pvalreps.")
#     }
#     
#     # Initialise
#     if (is.null(prew)){
#         prew <- rep(0,3)
#     }
#     
#     if (type == "bass"){
#         init <- bassInit(x)
#     } else if (type == "gompertz"){
#         init <- gompertzInit(x)
#     }
#     
#     init <- init - prew
#     init[(init + prew)<=0] <- 0.00001
#     n <- length(x)
#     
#     # Iterate until all p-values are < sig
#     # If eliminate is not requested it will only iterate once
#     elim <- TRUE; it <- 1
#     while (elim == TRUE){
#         
#         # Optimise
#         w <- rep(0,3)
#         if (sum(w.idx)>0){
#             if (sum(w.idx)>1){ # Allow for different optimiser - not needed though!
#                 if (type == "bass"){
#                     w.new <- optim(init[w.idx],bassCost,x=x,l=l,prew=prew,w.idx=w.idx,method="BFGS")$par    
#                 } else if (type == "gompertz"){
#                     w.new <- optim(init[w.idx],gompertzCost,x=x,l=l,prew=prew,w.idx=w.idx,method="BFGS")$par
#                 }
#             } else {
#                 if (type == "bass"){
#                     w.new <- optim(init[w.idx],bassCost,x=x,l=l,prew=prew,w.idx=w.idx,method="BFGS")$par
#                 } else if (type == "gompertz"){
#                     w.new <- optim(init[w.idx],gompertzCost,x=x,l=l,prew=prew,w.idx=w.idx,method="BFGS")$par
#                 }
#             }
#             w[w.idx] <- w.new
#         }
#         
#         # Bootstrap p-values
#         if (pvalreps > 0){
#             # Calculate sigma
#             if (type == "bass"){
#                 yhat <- bassCurve(n,prew+w)[,2]
#             } else if (type == "gompertz"){
#                 yhat <- gompertzCurve(n,prew+w)[,2]
#             }
#             sigma <- sqrt(mean((x - yhat)^2))
#             # Construct bootstraps
#             wboot <- array(NA,c(pvalreps,3))
#             yboot <- matrix(rnorm(n*pvalreps,0,sigma),nrow=n) + matrix(rep(yhat,pvalreps),ncol=pvalreps)
#             # This needs to become multiplicative
#             yboot[yboot<0] <- 0.001
#             # Estimate model
#             for (i in 1:pvalreps){
#                 if (type == "bass"){
#                     wboot[i,] <- diffuseEstim(yboot[,i],l,pvalreps=0,type="bass")$w - prew
#                 } else if (type == "gompertz"){
#                     wboot[i,] <- diffuseEstim(yboot[,i],l,pvalreps=0,type="gompertz")$w - prew
#                 }
#             }
#             pval <- colMeans((abs(wboot - matrix(rep(colMeans(wboot),pvalreps),ncol=3,byrow=TRUE))) > abs(matrix(rep(w,pvalreps),ncol=3,byrow=TRUE)))
#         } else {
#             pval <- rep(NA,3)
#         }
#         # Elimination process
#         if (eliminate == TRUE & any(pval[w.idx]>sig)){
#             # Find most insignificant
#             pval.temp <- pval
#             pval.temp[pval.temp<sig] <- sig
#             pval.temp <- pval.temp - sig
#             pval.temp[!w.idx] <- 0
#             loc <- which(pval.temp == max(pval.temp))[1]
#             w.idx[loc] <- FALSE
#         } else {
#             # Stop elimination iterations
#             elim <- FALSE
#             loc <- NA
#         }
#         # Provide console output
#         if (verbose == TRUE){
#             writeLines(paste0("Estimation iteration: ",it))
#             it <- it + 1
#             if (!is.na(loc)){
#                 locv <- rep("",3)
#                 locv[loc] <- "X"
#             } else {
#                 locv <- NULL
#             }
#             temp <- cbind(round(cbind(w,pval),4),locv)
#             if (type == "bass"){
#                 rownames(temp) <- c("p","q","M")    
#             } else if (type == "gompertz"){
#                 rownames(temp) <- c("a","b","M")    
#             }
#             colnames(temp) <- c("Estimate","p-value","")[1:(2+!is.na(loc))]
#             print(temp,quote=FALSE)
#             if (elim == FALSE){
#                 writeLines("Estimation completed")
#             } 
#             writeLines("")
#         }
#         
#     }
#     
#     w <- w + prew
#     names(w) <- names(init)
#     
#     names(pval) <- names(w)
#     return(list("w"=w,"pval"=pval))
#     
# }
# 
# diffusePlot <- function(x, cumulative=c(FALSE,TRUE),...){
#     # Plot diffusion curves
#     # x, object estimated using diffusion
#     # cumulative, if TRUE plot cumulative adoption
#     
#     type <- class(x)
#     if (type == "bass"){
#         elmt <- 3
#     } else if (type == "gompertz"){
#         elmt <- 1
#     }
#     cumulative <- cumulative[1]
#     
#     # Colorbrewer colours
#     cmp <- c("#E41A1C","#377EB8","#4DAF4A")
#     # Check if forecasts exist and construct xx
#     if (exists("xhat",where=x)){
#         xx <- c(1,(length(x$x)+length(x$mean)))
#     } else {
#         xx <- c(1,(length(x$x)))
#     }
#     
#     if (cumulative == FALSE){
#         
#         # Get yy min-max
#         yy <- range(cbind(x$x,x$fit[,2:(1+elmt)]))
#         yy <- yy + c(-1,1)*0.04*diff(yy)
#         yy[1] <- max(0,yy[1])
#         
#         # Plot fit
#         plot(x$x,type="p",pch=20,ylab="Adoption",xlab="Period",ylim=yy,xlim=xx,main=x$method)
#         for (i in 1:elmt){
#             lines(x$fit[,1+i],col=cmp[i])
#         }
#         # Check if forecasts exist and plot
#         if (exists("xhat",where=x)){
#             for (i in 1:elmt){
#                 lines((length(x$x)+1):xx[2],x$xhat[,i+1],col=cmp[i])
#             }
#         }
#         legend("topleft",c("Adoption","Innovators","Imitators")[1:elmt],col=cmp,lty=1,bty="n")
#         
#     } else {
#         # Cumulative plot
#         
#         # Get yy min-max
#         yy <- range(cbind(cumsum(x$x),x$fit[,1]))
#         yy <- yy + c(-1,1)*0.04*diff(yy)
#         yy[1] <- max(0,yy[1])
#         
#         # Plot fit
#         plot(cumsum(x$x),type="p",pch=20,ylab="Cumulative Adoption",xlab="Period",ylim=yy,xlim=xx,main=x$method)
#         lines(x$fit[,1],col=cmp[1])
#         if (type == "bass"){
#             for (i in 1:2){
#                 lines(cumsum(x$fit[,2+i]),col=cmp[i+1])
#             }
#         }
#         # Check if forecasts exist and plot
#         if (exists("xhat",where=x)){
#             fstart <- apply(x$fit,2,cumsum)[length(x$x),2:(1+elmt)]
#             for (i in 1:elmt){
#                 lines((length(x$x)+1):xx[2],cumsum(x$xhat[,i+1])+fstart[i],col=cmp[i])
#             }
#         }
#         legend("bottomright",c("Adoption","Innovators","Imitators")[1:elmt],col=cmp,lty=1,bty="n")
#         
#     }
# }
# 
# diffusePrint <- function(x, ...){
#     # Print console output for diffusion models
#     # x, object estimated using diffusion
#     
#     type <- tolower(x$method)
#     
#     writeLines(paste(x$method,"model"))
#     writeLines("")
#     writeLines("Parameters:")
#     if (is.null(x$prew)){
#         temp <- round(cbind(x$w,x$pval),4)    
#         colnames(temp) <- c("Estimate","p-value")
#     } else {
#         temp <- round(cbind(x$w,x$w-x$prew,x$pval),4)    
#         colnames(temp) <- c("Estimate","Marginal"," Marginal p-value")
#     }
#     if (type == "bass"){
#         rownames(temp) <- c("p - Coefficient of innovation","q - Coefficient of imitation","M - Market potential")
#     } else if (type == "gompertz"){
#         rownames(temp) <- c("a - displacement","b - growth","M - Market potential")
#     }
#     print(temp)
#     writeLines("")
#     writeLines(paste("sigma:",round(sqrt(x$mse),4)))
#     
# }
# 
# cleanzero <- function(x){
#     # Internal function: remove leadig zeros
#     # x, vector of values
#     
#     idx <- which(x == 0)
#     n <- length(x)
#     l <- length(idx)
#     if (l>0 & idx[1]==1){
#         d.idx <- diff(idx)
#         loc <- which(d.idx > 1)[1]
#         if (is.na(loc)){
#             loc <- l
#         }
#         x <- x[(loc+1):n]
#     } else {
#         loc <- 1
#     }
#     return(list("x"=x,"loc"=loc))
# }
# 
# ## ---- Sequential Bass Functions ----
# 
# seqdiffusion  <- function(x,cleanlead=c(TRUE,FALSE),prew=NULL,l=2,pvalreps=0,eliminate=c(FALSE,TRUE),sig=0.05,verbose=c(FALSE,TRUE),type=c("bass","gompertz")){
#     # Sequential bass fit across generations
#     # This is calling bass multiple times and organises the output
#     # x, adoption per period
#     # cleanlead, removes leading zeros
#     # prew, the w of the previous generation - this is used for sequential fitting
#     # l, the l-norm (1 is absolute errors, 2 is squared errors)
#     # pvalreps, bootstrap repetitions to estimate (marginal) p-values
#     # eliminate, if TRUE eliminates insignificant parameters from estimation. Forces pvalreps=1000 if left to 0.
#     # sig, significance level used to eliminate parameters
#     # verbose, if TRUE provide console output during estimation
#     # type, diffusion model to use
#     
#     type <- match.arg(type,c("bass","gompertz"))
#     verbose <- verbose[1]
#     
#     # Number of curves
#     k <- dim(x)[2]
#     
#     fit <- vector("list",k)
#     names(fit) <- paste0("Gen",1:k)
#     
#     # Fit iteratively across generations
#     for (i in 1:k){
#         
#         if (verbose == TRUE){
#             writeLines(paste0("Generation ",i))
#         }
#         if (i > 1){
#             prew <- fit[[i-1]]$w
#             elimin <- eliminate
#         } else {
#             elimin <- FALSE
#         }
#         
#         fit[[i]] <- diffusion(x[,i],w=NULL,cleanlead,prew,l,pvalreps,elimin,sig,verbose,type=type)
#         
#     }
#     
#     allw <- do.call(rbind,lapply(fit,function(x){x$w}))
#     allmse <- do.call(rbind,lapply(fit,function(x){x$mse}))
#     allpval <- do.call(rbind,lapply(fit,function(x){x$pval}))
#     
#     mth <- paste("Sequential",fit[[1]]$method)
#     return(structure(list("method"=mth,"diffusion"=fit,"x"=x,"w"=allw,"mse"=allmse,"pval"=allpval),class="seqdiffusion"))
#     
# }
# 
# print.seqdiffusion <- function(x, ...){
#     # Print console output for bass
#     # x, object estimated using bass
#     
#     type <- tolower(x$diffusion[[1]]$method)
#     
#     writeLines(paste(x$method,"model"))
#     writeLines("")
#     writeLines("Parameters:")
#     temp <- round(cbind(cbind(x$w,x$pval)[,c(1,4,2,5,3,6)],sqrt(x$mse)),4)
#     if (type == "bass"){
#         colnames(temp) <- c("p coef.","pval.","q coef.","pval.","M size","pval.","sigma")
#     } else if (type == "gompertz"){
#         colnames(temp) <- c("a coef.","pval.","b coef.","pval.","M size","pval.","sigma")
#     }
#     print(temp)
#     writeLines("")
#     
# }
# 
# plot.seqdiffusion <- function(x,cumulative=c(FALSE,TRUE),...){
#     # Plot sequential bass curves
#     # x, object estimated using bass
#     # cumulative, if TRUE plot cumulative adoption
#     
#     cumulative <- cumulative[1]
#     
#     cmp <- c("#B2182B","#EF8A62","#67A9CF","#2166AC")
#     k <- dim(x$x)[2]
#     cmp <- colorRampPalette(cmp)(k)
#     N <- dim(x$x)[1]
#     
#     if (cumulative == FALSE){
#         X <- x$x
#         ll <- 2
#     } else {
#         X <- apply(x$x,2,cumsum)
#         ll <- 1
#     }
#     
#     yy <- range(X)
#     yy <- yy + c(-1,1)*0.04*diff(yy)
#     yy[1] <- max(0,yy[1])
#     
#     plot(NA,NA,xlim=c(1,N),ylim=yy,xlab="Period",ylab="Adoption",main=x$method)
#     for (i in 1:k){
#         x.temp <- X[,i]
#         x.temp <- cleanzero(x.temp)
#         n <- length(x.temp$x)
#         l <- x.temp$loc
#         xx <- l:(l+n-1)
#         points(xx,x.temp$x,col="black",pch=21,bg=cmp[i],cex=0.7)
#         lines(xx,x$diffusion[[i]]$fit[,ll],col=cmp[i],lwd=2)
#     }
#     
# }
# 
# 
# ## ---- Experimental Sequential Bass ----
# # DO NOT USE - IT WILL CAUSE VERY VERY BAD THINGS
# # 
# # seqbass <- function(x,estim=c("mse","shrink"),pvalreps=1000,eliminate=c(FALSE,TRUE),folds=5){
# #     
# #     estim <- match.arg(estim,c("mse","shrink"))
# #     eliminate <- eliminate[1]
# #     
# #     # Generations
# #     k <- dim(x)[2]
# #     
# #     fit <- vector("list",k)
# #     names(fit) <- paste0("Gen",1:k)
# #     # Model first generation
# #     margin <- list("margin.pqm"=rep(NA,3),"margin.pval"=rep(NA,3),"lambda"=0)
# #     fit[[1]] <- structure(c(unclass(bass(x[,1])),margin),class="bass")
# #     
# #     # Model each generation
# #     for (i in 2:k){
# #         
# #         # Get previous generation pqm
# #         initpqm <- fit[[i-1]]$pqm
# #         # Estimate marginal change in pqm
# #         x.temp <- cleanzero(x[,i])$x
# #         marginpqm <- seqbassEstim(x.temp,initpqm,folds=folds,eliminate=eliminate,estim=estim)
# #         # plot(marginpqm$cv.stats[,1],type="l")
# #         # Fit bass model
# #         basspqm <- initpqm + marginpqm$pqm
# #         # Store results
# #         bass.temp <- unclass(bass(x.temp,basspqm))
# #         margin <- list("margin.pqm"=marginpqm$pqm,"margin.pval"=marginpqm$pval,"lambda"=marginpqm$lambda)
# #         fit[[i]] <- structure(c(bass.temp,margin),class="bass")
# #     }
# #     
# #     allpqm <- do.call(rbind,lapply(fit,function(x){x$pqm}))
# #     allmse <- do.call(rbind,lapply(fit,function(x){x$mse}))
# #     allpval <- do.call(rbind,lapply(fit,function(x){x$margin.pval}))
# #     
# #     return(structure(list("method"="Sequantial Bass","bass"=fit,"x"=x,"pqm"=allpqm,"mse"=allmse,"pval"=allpval),class="seqbass"))
# #     
# # }
# # 
# # seqbassEstim <- function(x,initpqm,folds=5,pvalreps=1000,eliminate,estim){
# #     # Estimate parameters for generation i, i>1
# #     # x is per period adoption
# # 
# #     pqm.idx=rep(TRUE,3)         # Which parameters to estimate 
# #     
# #     # Initialise
# #     init <- bassInit(x)
# #     init <- init - initpqm
# #     init[(init + initpqm)<=0] <- 0.00001
# # 
# #     # Create CV subsets
# #     n <- length(x)
# #     idx <- cut(seq(1,n),breaks=folds,labels=FALSE)
# #     idx <- idx[sample(n)]
# # 
# #     if (estim == "mse"){
# #         
# #     }
# #     
# #     # Setup shrinkage
# #     lambda <- seq(0,1,length=150)
# #     # pqm.temp <- rep(1,3); i <- 1
# #     # cv.stats <- array(NA,c(length(lambda),2))
# #     # while (any(pqm.temp != 0) & i <= length(lambda)){
# #     #     # Optimise for given lambda and cross-validate
# #     #     cv.mse <- vector("numeric",folds)
# #     #     cv.pqm <- array(0,c(folds,3))
# #     #     for (j in 1:folds){
# #     #         cv.pqm[j,pqm.idx] <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=(idx!=j),lambda=lambda[i],pqm.idx=pqm.idx)$par    
# #     #         fit.temp <- bassCurve(n,initpqm+cv.pqm[j,])
# #     #         cv.mse[j] <- mean((x[idx==j] - fit.temp[idx==j,2])^2)
# #     #     }
# #     #     pqm.temp <- colSums(abs(cv.pqm))
# #     #     cv.stats[i,] <- c(mean(cv.mse),sd(cv.mse))
# #     #     i <- i + 1
# #     # }
# #     # cv.stats <- cv.stats[1:(i-1),]
# #     # 
# #     # # Estimate on complete sample with selected lambda
# #     # loc <- which(cv.stats[,1]==min(cv.stats[,1]))[1]
# #     # pqm <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=lambda[loc],pqm.idx=pqm.idx)$par    
# #     
# #     
# #     pqm <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.idx)$par    
# #     loc <- 1
# #     cv.stats <- NULL
# #     
# #     # Bootstrap p-values
# #     if (pvalreps > 0){
# #         yhat <- bassCurve(n,initpqm+pqm)[,2]
# #         sigma <- sqrt(mean((x - yhat)^2))
# #         pqmboot <- array(NA,c(pvalreps,3))
# #         yboot <- matrix(rnorm(n*pvalreps,0,sigma),nrow=n) + matrix(rep(yhat,pvalreps),ncol=pvalreps)
# #         for (i in 1:pvalreps){
# #             pqmboot[i,] <- bassEstim(yboot[,i]) - initpqm
# #         }
# #         pval <- colMeans((abs(pqmboot - matrix(rep(colMeans(pqmboot),pvalreps),ncol=3,byrow=TRUE))) > abs(matrix(rep(pqm,pvalreps),ncol=3,byrow=TRUE)))
# #         # If elimination is requested then remove insignificant and re-estimate
# #         if (eliminate == TRUE){
# #             pqm.pval <- pval<=0.05
# #             if (sum(pqm.pval)!=3){
# #                 # If at least one index is not retained
# #                 pqm.new <- rep(0,3)
# #                 if (sum(pqm.pval)>1){
# #                     pqm.new[pqm.pval] <- optim(init[pqm.pval],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.pval)$par    
# #                 } else if (sum(pqm.pval) == 1){
# #                     pqm.new[pqm.pval] <- optim(init[pqm.pval],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.pval,method="BFGS")$par    
# #                 } 
# #                 pqm <- pqm.new
# #             }
# #         }
# #     } else {
# #         pval <- rep(NA,3)
# #     }
# #     names(pval) <- names(pqm)
# # 
# #     return(list("pqm"=pqm,"pval"=pval,"lambda"=lambda[loc],"cv.stats"=cv.stats))
# #     
# # }
# # 
# # seqbassCost <- function(pqm,x,initpqm,idx,lambda,pqm.idx){
# #     # Cost function for numerical optimisation
# #     
# #     n <- length(x)
# #     # If elements of pqm are not send to optimise sort out vectors
# #     pqm.all <- rep(0,3)
# #     pqm.all[pqm.idx] <- pqm
# #     basspqm <- pqm.all + initpqm
# #     fit <- bassCurve(n,basspqm)
# #     
# #     X <- cumsum(x)
# #     # rsse <- (1-lambda)*(sqrt(mean((x[idx] - fit[idx,2])^2))/mean(x[idx]) + sqrt(mean((X[idx] - fit[idx,1])^2))/mean(X[idx]))/2 + lambda*sum(abs(pqm/initpqm))
# #     # rsse <- (1-lambda)*mean((x[idx] - fit[idx,2])^2) + lambda*sum(abs(pqm/initpqm))
# #     # rsse <- (1-lambda)*(mean((x[idx] - fit[idx,2])^2)/2 + mean((X[idx] - fit[idx,1])^2)/2) + lambda*sum(abs(pqm/initpqm))
# #     # rsse <- (1-lambda)*(0.5*mean((x - fit[,2])^2)/mean(x)^2 + 0.5*mean((X - fit[,1])^2)/mean(X)^2) + lambda*sum(abs(pqm/initpqm[pqm.idx]))
# #     
# #     rsse <- (1-lambda)*mean((x - fit[,2])^2)/mean(x)^2 + lambda*sum(abs(pqm/initpqm[pqm.idx]))
# #     
# #     # Ensure positive coefficients
# #     if (any(basspqm<=0)){
# #         rsse <- 10e2000
# #     }
# #     
# #     return(rsse)
# #     
# # }
# # 
