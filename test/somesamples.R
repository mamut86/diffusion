# sequential diffusion ----------------------------------------------------
devtools::load_all()
load("test/oil.RData")

# when we use "linearize" this is more or less as it was before the new optimisation was implemented

# this works (using bass linearization without scaling)
bln <- seqdiffusion(oil, type = "bass", optim = "nm", initpar = "linearize",  mscal = F)
plot(bln, cumulative = T)

# the same with scaling does not work. Also does not work with static initalisation parameters
blnscal <- seqdiffusion(oil, type = "bass", optim = "nm", initpar = "linearize",  mscal = T)
plot(blnscal, cumulative = T)


# similar result when using Gompertz (it does not work when mscal is TRUE as with Bass)
gln <- seqdiffusion(oil, type = "gompertz", optim = "nm", initpar = "linearize",  mscal = F)
plot(gln, cumulative = T)

# when using static initalsiation scaling is needed and works with the first generation, but fails at the second 
gstscal <- seqdiffusion(oil, type = "gompertz", optim = "nm", initpar = "static",  mscal = T, optsol = "multi")
plot(gstscal, cumulative = T)

# if both are individually calculated it is all fine
gen1 <- diffusion(oil[,1], type = "bass", optim = "nm", mscal = T, initpar = "static")
plot(gen1, cumulative = T)

gen2 <- diffusion(oil[,2], type = "gompertz", optim = "nm", mscal = T, initpar = "linearize", ooptsol = "multi")
plot(gen2, cumulative = T)


# example with another sample

# with linearisation works well and no scaling
tt <- seqdiffusion(tsIbm, type = "gompertz", optim = "nm", initpar = "linearize",  mscal = F)
plot(tt, cumulative = T)

# scaling destroys it
tt <- seqdiffusion(tsIbm, type = "gompertz", optim = "nm", initpar = "linearize",  mscal = T)
plot(tt, cumulative = T)

# when static first generation good, rest not working
tt <- seqdiffusion(tsIbm, type = "gompertz", optim = "nm", initpar = "static",  mscal = T, optsol = "multi")
plot(tt, cumulative = T)

# individual results works just fine
gen2 <- diffusion(tsIbm[, 2], type = "gompertz", optim = "nm", initpar = "linearize",  mscal = F)
plot(gen2, cumulative = T)

# fails when using not using scaling for static
gen2 <- diffusion(tsIbm[, 2], type = "gompertz", optim = "nm", initpar = "static",  mscal = T)
plot(gen2, cumulative = T)

# when using linearizatoin it works when not scaling and fails otherwise
gen2 <- diffusion(tsIbm[, 2], type = "gompertz", optim = "nm", initpar = "linearize",  mscal = F)
plot(gen2, cumulative = T)


gln <- seqdiffusion(oil, type = "gompertz", optim = "nm", initpar = "linearize",
                    mscal = T, eliminate= TRUE, verbose = F, pvalreps = 1000)


gln <- seqdiffusion(tsIbm, type = "gompertz", optim = "nm", initpar = "linearize",
                    mscal = T, eliminate= TRUE, verbose = T, pvalreps = 100)
