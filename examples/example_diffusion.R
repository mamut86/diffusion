# example for diffusion() function
fitbass <- diffusion(tschicken[, 2], type = "bass")
fitgomp <- diffusion(tschicken[, 2], type = "gompertz")
fitsgomp <- diffusion(tschicken[, 2], type = "sgompertz")

# Produce some plots
plot(fitbass)
plot(fitgomp)
plot(fitsgomp)
