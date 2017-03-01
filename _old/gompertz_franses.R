# P.H. Franses approach

frans <- read.csv("Gompertz.csv", sep = ",", header = T)
plot(frans$raw, type = "l")
lines(frans$smoothed, col = "blue")

y <- frans$smoothed
# or
y <- frans$raw

ylog <- log(y)
ylogdiff <- log(diff(ylog))
t0 <- 1:length(ylogdiff)

startval <- list(g0 = 0.0001, b0 = 1.1)

# something is wrong in this formula, don't know what. it produces NANs
fit1 <- nls(ylogdiff ~ -g0*t0 + log(b0*exp(g0)-b0), start = startval)

ghat <- coef(fit1)[1]
bhat <- coef(fit1)[2]
t1 <- 1:length(ylog)

startval1 <- list(ahat = rep(5000, length(ylog)))

# cannot be estimated due to time series on the left-hand side
nls(ahat ~ exp(ylog + bhat*exp(-ghat*t1)))