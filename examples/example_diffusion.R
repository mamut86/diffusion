# example for diffusion() function
data("chicken")
fitbass <- diffusion(chicken[, 2], type = "bass")
fitgomp <- diffusion(chicken[, 2], type = "gompertz")
fitsgomp <- diffusion(chicken[, 2], type = "sgompertz")

# Produce some plots
plot(fitbass)
plot(fitgomp)
plot(fitsgomp)