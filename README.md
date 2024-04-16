[![CRAN](https://www.r-pkg.org/badges/version/diffusion)](https://cran.r-project.org/package=diffusion)
[![Downloads](https://cranlogs.r-pkg.org/badges/diffusion?color=brightgreen)](https://www.r-pkg.org/pkg/diffusion)
[![R-CMD-check](https://github.com/mamut86/diffusion/actions/workflows/test.yml/badge.svg)](https://github.com/mamut86/diffusion/actions/workflows/test.yml)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

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

For installation from github use remotes:
```r
if (!require("remotes")){install.packages("remotes")}
remotes::install_github("mamut86/diffusion")
```
