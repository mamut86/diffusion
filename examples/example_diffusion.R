# example for diffusion() function
fitbass <- diffusion(tschicken[, 2], type = "bass")
fitgomp <- diffusion(tschicken[, 2], type = "gompertz")
fitgsg <- diffusion(tschicken[, 2], type = "gsgompertz")

# Produce some plots
plot(fitbass)
plot(fitgomp)
plot(fitgsg)
