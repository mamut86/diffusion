[![Travis-CI Build Status](https://api.travis-ci.org/mamut86/diffusion.svg?branch=master)](https://travis-ci.org/mamut86/diffusion) [![CRAN](http://www.r-pkg.org/badges/version/diffusion)](https://cran.r-project.org/package=diffusion) [![Downloads](http://cranlogs.r-pkg.org/badges/diffusion?color=brightgreen)](http://www.r-pkg.org/pkg/diffusion)
![GitHub version](https://badge.fury.io/gh/mamut86%2Fdiffusion.svg)

# diffusion
The R package __diffusion__ is for forecasting with diffusion curves.

Currently the following diffusion models are implemented:

1. Bass model
2. Gompertz model
3. Gamma/Shifted Gompertz model
4. Weibull model
5. Norton-Bass model for generational modelling (not working well)


### Installation
Stable version can be installed from CRAN:
```r
install.packages("diffusion")
```

For installation from github use devtools:
```r
if (!require("devtools")){install.packages("devtools")}
devtools::install_github("mamut86/diffusion")
```
