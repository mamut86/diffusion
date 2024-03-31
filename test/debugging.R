# sequential diffusion ----------------------------------------------------
devtools::load_all()

load("test/adoptionData.RData")

# there is a commented in the bootstrap loop which enters browser when NA is produced to investigate NA
tt1 <- seqdiffusion(adat[[11]], type = "gompertz", mscal = T, eliminate = T, pvalreps = 500)

# case when the second generation does not fit well at all
tt2 <-  seqdiffusion(adat[[4]], type = "gompertz", mscal = T, eliminate = T, pvalreps = 500)
plot(tt2)

# seems the bobyqa optimisation algorithm is dead. I remember vaguely that it worked at some point.
# The error seems to be with the control variables but we haven't added much there. Maybe just remove bobyqa option? 
tt3 <- diffusion(adat[[1]][,2], w = NULL, optim = "bobyqa")


