### Norton Bass

\dontrun{
  data(tsibm)
  
  fitNB1 <- Nortonbass(tsibm, startval.met = "2ST", estim.met = "OLS",
                       startval = NULL, flexpq = F, gstart = NULL)
  fitNB2 <- Nortonbass(tsibm, startval.met = "2ST", estim.met = "SUR",
                       startval = NULL, flexpq = F, gstart = NULL)
  # using BOBYQA algorithm
  fitNB3 <- Nortonbass(tsibm, startval.met = "2ST", estim.met = "BOBYQA",
                       startval = NULL, flexpq = F, gstart = NULL)
  
  # Create some plots
  plot(tsibm[, 1],type = "l", ylim=c(0,35000))
  lines(tsibm[, 2],col ="blue")
  lines(tsibm[, 3],col ="green")
  lines(tsibm[, 4],col ="pink")
  
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
  fitNB1$fit$RMSE[[1]]
  fitNB1$fit$RMSE[[2]]
  fitNB1$fit$RMSE[[3]]
  fitNB1$fit$RMSE[[4]]
  
  fitNB2$fit$RMSE[[1]]
  fitNB2$fit$RMSE[[2]]
  fitNB2$fit$RMSE[[3]]
  fitNB2$fit$RMSE[[4]]
  
  fitNB3$fit$RMSE[[1]]
  fitNB3$fit$RMSE[[2]]
  fitNB3$fit$RMSE[[3]]
  fitNB3$fit$RMSE[[4]]
}
