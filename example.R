library(systemfit)
source("bass.R")
source("nortonbass.R")
load("dram.RData")

fitNB1 <- nortonBass(data, startval.met = "2ST", startval = NULL, flexpq = F, gstart = NULL)
fitNB2 <- nortonBass(data, startval.met = "2ST", startval = NULL, flexpq = T, gstart = NULL)

# Create some plots
plot(data[,1],type = "l", ylim=c(0,30000))
lines(data[,2],col ="blue")
lines(data[,3],col ="green")
lines(data[,4],col ="pink")

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