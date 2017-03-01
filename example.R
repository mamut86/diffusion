

# Norton Bass example -----------------------------------------------------
library(systemfit)
source("bass.R")
source("nortonbass.R")
load("data/dram.Rdram")

fitNB1 <- nortonBass(dram, startval.met = "2ST", startval = NULL, flexpq = F, gstart = NULL)
fitNB2 <- nortonBass(dram, startval.met = "2ST", startval = NULL, flexpq = T, gstart = NULL)

# Create some plots
plot(dram[,1],type = "l", ylim=c(0,30000))
lines(dram[,2],col ="blue")
lines(dram[,3],col ="green")
lines(dram[,4],col ="pink")

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


# Gompertz model example --------------------------------------------------

source("gompertz.R")
load("data/chicken.RData")

Gompertz(x = chicken$weight)

load("data/cars.RData")
Gompertz(cars$raw)
