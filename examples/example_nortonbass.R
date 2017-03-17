### Norton Bass

\dontrun{
  data(ibm)
  
  fitNB1 <- Nortonbass(ibm, startval.met = "2ST", startval = NULL, flexpq = F, gstart = NULL)
  fitNB2 <- Nortonbass(ibm, startval.met = "2ST", startval = NULL, flexpq = T, gstart = NULL)
  
  # Create some plots
  plot(ibm[, 1],type = "l", ylim=c(0,30000))
  lines(ibm[, 2],col ="blue")
  lines(ibm[, 3],col ="green")
  lines(ibm[, 4],col ="pink")
  
  lines(fitNB1$fitted[[1]]$predicted, col = "black", lty = 2)
  lines(fitNB1$fitted[[2]]$predicted, col = "blue", lty = 2)
  lines(fitNB1$fitted[[3]]$predicted, col = "green", lty = 2)
  lines(fitNB1$fitted[[4]]$predicted, col = "pink", lty = 2)
  
  lines(fitNB2$fitted[[1]]$predicted, col = "black", lty = 3)
  lines(fitNB2$fitted[[2]]$predicted, col = "blue", lty = 3)
  lines(fitNB2$fitted[[3]]$predicted, col = "green", lty = 3)
  lines(fitNB2$fitted[[4]]$predicted, col = "pink", lty = 3)
  
  # read out RMSE
  fitNB1$fitted[[1]]$rmse
  fitNB1$fitted[[2]]$rmse
  fitNB1$fitted[[3]]$rmse
  fitNB1$fitted[[4]]$rmse
  
  fitNB2$fitted[[1]]$rmse
  fitNB2$fitted[[2]]$rmse
  fitNB2$fitted[[3]]$rmse
  fitNB2$fitted[[4]]$rmse
}