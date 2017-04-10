# example for seqdiffusion() function
  data("ibm")
  fitsgomp <- seqdiffusion(ibm, type = "sgompertz")
  fitgomp <- seqdiffusion(ibm, type = "gompertz")
  fitbass <- seqdiffusion(ibm, type = "bass")
  
  plot(fitgomp)
  plot(fitsgomp)
  plot(fitbass)
  
