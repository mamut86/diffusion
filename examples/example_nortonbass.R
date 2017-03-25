### Norton Bass

\dontrun{
  data(ibm)
  
  fitNB1 <- Nortonbass(ibm, startval.met = "2ST", estim.met = "OLS",
                       startval = NULL, flexpq = F, gstart = NULL)
  fitNB2 <- Nortonbass(ibm, startval.met = "2ST", estim.met = "OLS",
                       startval = NULL, flexpq = T, gstart = NULL)
  # using BOBYQA algorithm
  fitNB3 <- Nortonbass(ibm, startval.met = "2ST", estim.met = "BOBYQA",
                       startval = NULL, flexpq = F, gstart = NULL)
  
  # Create some plots
  plot(ibm[, 1],type = "l", ylim=c(0,35000))
  lines(ibm[, 2],col ="blue")
  lines(ibm[, 3],col ="green")
  lines(ibm[, 4],col ="pink")
  
  lines(fitNB1$fit$fitted[[1]], col = "black", lty = 2)
  lines(fitNB1$fit$fitted[[2]], col = "blue", lty = 2)
  lines(fitNB1$fit$fitted[[3]], col = "green", lty = 2)
  lines(fitNB1$fit$fitted[[4]], col = "pink", lty = 2)
  
  lines(fitNB2$fit$fitted[[1]], col = "black", lty = 3)
  lines(fitNB2$fit$fitted[[2]], col = "blue", lty = 3)
  lines(fitNB2$fit$fitted[[3]], col = "green", lty = 3)
  lines(fitNB2$fit$fitted[[4]], col = "pink", lty = 3)
  
  lines(fitNB3$fit$fitted[[1]], col = "black", lty = 4)
  lines(fitNB3$fit$fitted[[2]], col = "blue", lty = 4)
  lines(fitNB3$fit$fitted[[3]], col = "green", lty = 4)
  lines(fitNB3$fit$fitted[[4]], col = "pink", lty = 4)
  
  # read out RMSE
  fitNB1$fitted[[1]]$rmse
  fitNB1$fitted[[2]]$rmse
  fitNB1$fitted[[3]]$rmse
  fitNB1$fitted[[4]]$rmse
  
  fitNB2$fitted[[1]]$rmse
  fitNB2$fitted[[2]]$rmse
  fitNB2$fitted[[3]]$rmse
  fitNB2$fitted[[4]]$rmse
  
  fitNB3$fit$RMSE[[1]]
  fitNB3$fit$RMSE[[2]]
  fitNB3$fit$RMSE[[3]]
  fitNB3$fit$RMSE[[4]]
}