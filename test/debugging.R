# sequential diffusion ----------------------------------------------------
devtools::load_all()

load("test/adoptionData.RData")

# Issue with L-BFGS-B optimiser
# this gives errors
tt1 <- seqdiffusion(adat[[5]], type = "gsgompertz", mscal = T, eliminate = T, pvalreps = 100, cumulative = F, method = "L-BFGS-B", verbose = T)



# case when the second generation does not fit well at all
tt2 <-  seqdiffusion(adat[[4]], type = "gompertz", mscal = T, eliminate = T, pvalreps = 500)
plot(tt2)

# seems the bobyqa optimisation algorithm is dead. I remember vaguely that it worked at some point.
# The error seems to be with the control variables but we haven't added much there. Maybe just remove bobyqa option? 
tt3 <- diffusion(adat[[1]][,2], w = NULL, optim = "bobyqa")


# Problem produces a flat curve
tt4 <- diffusion(adat[[7]][1:33, 1], type = "gsgompertz", method = "nm", cumulative = T, initpar = "linearize", mscal = T, multisol = F)
plot(tt4)

# need to investigate weibull
plot(diffusion(tsCovid, type = "weibull", method = "nm"))


# testing fixed parameters
wFix <- rbind(c(7139.775, 15384.65, 15632.38 ), rep(NA, 3), rep(NA, 3))
fitinit <- seqdiffusion(adat[[11]], w = NULL,  type = "bass",  mscal = T, eliminate = F,
                        pvalreps = 0, cumulative = F, loss = 2, maxiter = 5000,
                        method = "hj", verbose = T, initpar = "preset")



p <- c(fitinit$diffusion[[1]]$w[2], fitinit$diffusion[[2]]$w[2], fitinit$diffusion[[3]]$w[2])

wFix <- rbind(c(7139.775, 15384.65, 15632.38 ), p, rep(NA, 3))

fitinit <- seqdiffusion(adat[[11]], w = NULL,  type = "bass",  mscal = T, eliminate = F,
                        pvalreps = 100, cumulative = F, loss = 2, maxiter = 5000,
                        method = "hj", verbose = T, initpar = "preset")

plot(fitinit)

diffusion(adat[[11]][,2], w = c(15196.3987, NA, NA), verbose = T)
