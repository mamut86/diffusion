## ---- Experimental Sequential Bass ----
# DO NOT USE - IT WILL CAUSE VERY VERY BAD THINGS
# 
# seqbass <- function(x,estim=c("mse","shrink"),pvalreps=1000,eliminate=c(FALSE,TRUE),folds=5){
#     
#     estim <- match.arg(estim,c("mse","shrink"))
#     eliminate <- eliminate[1]
#     
#     # Generations
#     k <- dim(x)[2]
#     
#     fit <- vector("list",k)
#     names(fit) <- paste0("Gen",1:k)
#     # Model first generation
#     margin <- list("margin.pqm"=rep(NA,3),"margin.pval"=rep(NA,3),"lambda"=0)
#     fit[[1]] <- structure(c(unclass(bass(x[,1])),margin),class="bass")
#     
#     # Model each generation
#     for (i in 2:k){
#         
#         # Get previous generation pqm
#         initpqm <- fit[[i-1]]$pqm
#         # Estimate marginal change in pqm
#         x.temp <- cleanzero(x[,i])$x
#         marginpqm <- seqbassEstim(x.temp,initpqm,folds=folds,eliminate=eliminate,estim=estim)
#         # plot(marginpqm$cv.stats[,1],type="l")
#         # Fit bass model
#         basspqm <- initpqm + marginpqm$pqm
#         # Store results
#         bass.temp <- unclass(bass(x.temp,basspqm))
#         margin <- list("margin.pqm"=marginpqm$pqm,"margin.pval"=marginpqm$pval,"lambda"=marginpqm$lambda)
#         fit[[i]] <- structure(c(bass.temp,margin),class="bass")
#     }
#     
#     allpqm <- do.call(rbind,lapply(fit,function(x){x$pqm}))
#     allmse <- do.call(rbind,lapply(fit,function(x){x$mse}))
#     allpval <- do.call(rbind,lapply(fit,function(x){x$margin.pval}))
#     
#     return(structure(list("method"="Sequantial Bass","bass"=fit,"x"=x,"pqm"=allpqm,"mse"=allmse,"pval"=allpval),class="seqbass"))
#     
# }
# 
# seqbassEstim <- function(x,initpqm,folds=5,pvalreps=1000,eliminate,estim){
#     # Estimate parameters for generation i, i>1
#     # x is per period adoption
# 
#     pqm.idx=rep(TRUE,3)         # Which parameters to estimate 
#     
#     # Initialise
#     init <- bassInit(x)
#     init <- init - initpqm
#     init[(init + initpqm)<=0] <- 0.00001
# 
#     # Create CV subsets
#     n <- length(x)
#     idx <- cut(seq(1,n),breaks=folds,labels=FALSE)
#     idx <- idx[sample(n)]
# 
#     if (estim == "mse"){
#         
#     }
#     
#     # Setup shrinkage
#     lambda <- seq(0,1,length=150)
#     # pqm.temp <- rep(1,3); i <- 1
#     # cv.stats <- array(NA,c(length(lambda),2))
#     # while (any(pqm.temp != 0) & i <= length(lambda)){
#     #     # Optimise for given lambda and cross-validate
#     #     cv.mse <- vector("numeric",folds)
#     #     cv.pqm <- array(0,c(folds,3))
#     #     for (j in 1:folds){
#     #         cv.pqm[j,pqm.idx] <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=(idx!=j),lambda=lambda[i],pqm.idx=pqm.idx)$par    
#     #         fit.temp <- bassCurve(n,initpqm+cv.pqm[j,])
#     #         cv.mse[j] <- mean((x[idx==j] - fit.temp[idx==j,2])^2)
#     #     }
#     #     pqm.temp <- colSums(abs(cv.pqm))
#     #     cv.stats[i,] <- c(mean(cv.mse),sd(cv.mse))
#     #     i <- i + 1
#     # }
#     # cv.stats <- cv.stats[1:(i-1),]
#     # 
#     # # Estimate on complete sample with selected lambda
#     # loc <- which(cv.stats[,1]==min(cv.stats[,1]))[1]
#     # pqm <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=lambda[loc],pqm.idx=pqm.idx)$par    
#     
#     
#     pqm <- optim(init[pqm.idx],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.idx)$par    
#     loc <- 1
#     cv.stats <- NULL
#     
#     # Bootstrap p-values
#     if (pvalreps > 0){
#         yhat <- bassCurve(n,initpqm+pqm)[,2]
#         sigma <- sqrt(mean((x - yhat)^2))
#         pqmboot <- array(NA,c(pvalreps,3))
#         yboot <- matrix(rnorm(n*pvalreps,0,sigma),nrow=n) + matrix(rep(yhat,pvalreps),ncol=pvalreps)
#         for (i in 1:pvalreps){
#             pqmboot[i,] <- bassEstim(yboot[,i]) - initpqm
#         }
#         pval <- colMeans((abs(pqmboot - matrix(rep(colMeans(pqmboot),pvalreps),ncol=3,byrow=TRUE))) > abs(matrix(rep(pqm,pvalreps),ncol=3,byrow=TRUE)))
#         # If elimination is requested then remove insignificant and re-estimate
#         if (eliminate == TRUE){
#             pqm.pval <- pval<=0.05
#             if (sum(pqm.pval)!=3){
#                 # If at least one index is not retained
#                 pqm.new <- rep(0,3)
#                 if (sum(pqm.pval)>1){
#                     pqm.new[pqm.pval] <- optim(init[pqm.pval],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.pval)$par    
#                 } else if (sum(pqm.pval) == 1){
#                     pqm.new[pqm.pval] <- optim(init[pqm.pval],seqbassCost,x=x,initpqm=initpqm,idx=rep(TRUE,n),lambda=0,pqm.idx=pqm.pval,method="BFGS")$par    
#                 } 
#                 pqm <- pqm.new
#             }
#         }
#     } else {
#         pval <- rep(NA,3)
#     }
#     names(pval) <- names(pqm)
# 
#     return(list("pqm"=pqm,"pval"=pval,"lambda"=lambda[loc],"cv.stats"=cv.stats))
#     
# }
# 
# seqbassCost <- function(pqm,x,initpqm,idx,lambda,pqm.idx){
#     # Cost function for numerical optimisation
#     
#     n <- length(x)
#     # If elements of pqm are not send to optimise sort out vectors
#     pqm.all <- rep(0,3)
#     pqm.all[pqm.idx] <- pqm
#     basspqm <- pqm.all + initpqm
#     fit <- bassCurve(n,basspqm)
#     
#     X <- cumsum(x)
#     # rsse <- (1-lambda)*(sqrt(mean((x[idx] - fit[idx,2])^2))/mean(x[idx]) + sqrt(mean((X[idx] - fit[idx,1])^2))/mean(X[idx]))/2 + lambda*sum(abs(pqm/initpqm))
#     # rsse <- (1-lambda)*mean((x[idx] - fit[idx,2])^2) + lambda*sum(abs(pqm/initpqm))
#     # rsse <- (1-lambda)*(mean((x[idx] - fit[idx,2])^2)/2 + mean((X[idx] - fit[idx,1])^2)/2) + lambda*sum(abs(pqm/initpqm))
#     # rsse <- (1-lambda)*(0.5*mean((x - fit[,2])^2)/mean(x)^2 + 0.5*mean((X - fit[,1])^2)/mean(X)^2) + lambda*sum(abs(pqm/initpqm[pqm.idx]))
#     
#     rsse <- (1-lambda)*mean((x - fit[,2])^2)/mean(x)^2 + lambda*sum(abs(pqm/initpqm[pqm.idx]))
#     
#     # Ensure positive coefficients
#     if (any(basspqm<=0)){
#         rsse <- 10e2000
#     }
#     
#     return(rsse)
#     
# }
# 
