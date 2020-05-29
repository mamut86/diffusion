# sequential diffusion ----------------------------------------------------
devtools::load_all()

load("test/riaa.RData")

# there is a commented in the bootstrap loop which enters browser when NA is produced to investigate NA
tt <- seqdiffusion(riaa, type = "gompertz", mscal = T, eliminate = T, pvalreps = 500)
