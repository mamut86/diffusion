# Version 0.4.0 [2024-04-01]

## changes
* diffusion is now released under LGPLv2.1. The previous version under GPL(>=2) can be downloaded from here: https://github.com/mamut86/diffusion/releases/tag/v0.3.1


# Version 0.3.1 [2024-03-30]

## changes

* new automated removal of leading and trailing NAs. Error abort if NA within series
* new search optimisation argument (multisol)
* new option to partially fix/optimise parameters w in both diffusion and seqdiffusion
* new bootloss argument to control fo r different loss function of the bootstrapping
* added documentation for tsMetal.RData
* added internal sum squared error function for the optimiser
* added passing optim setting int trough Gompertz and GSGompertz initialisation
* added error handling for optimx function
* renamed argument l to loss (for l-norm selection)
* renamed argument x to y (the response variable)
* renamed argument optim to method (the optimiser selection)
* introduced argument initpar to allow manual initialisation, preset and automatic
* introduced argument mscal to scale market potential for better optimisation results
* introduced argument bootloss to choose from different loss function for bootstraps
* changed non-critical warnings to type message, to allow easy suppression
* is.diffusion() and is.bass() functions
* renamed option variable l to loss (for l-norm selection)
* renamed option variable x to y (the response variable)
* updated documentation for diffusion() - Thx Ivan!
* removed tsBroadband dataset due to data privacy concerns
* new search optimisation option

## bugfixes

* issues with log errors - reverted error measure to v.0.2.7 for the time being
* fixed an initalisation issue on the weibull model
* fixed titles for graphs
* fixed typo in documentation for predict function
* fixed fcst table to contain same number as horizons
* fixed number of parameters that go into G/GS (only relevant if w.idx is active)
* fixed error when plot.seqdiffusion() was used with cleanlead = F
* fixed error in gompertz when cleanlead = F
* fixed the tsmetal issue - the object now has the correct name
* fixed error when plotting ts-object with cumulative = F
* fixed an error when eliminate was TRUE with 0 pval reps to stop.
* fixed the sequential diffusion pval elimination process
* fixed error when linearization fails and reverting now to preset
* fixed error when bootstrapping produced NA now ignoring NA and warning.
* included error check for pvalreps in seqdiffusion
* included error check for y to be vector or ts-object / matrix or mts for seqdiffusion
* included a revert method in case optimization fails and warning messages

# Version 0.3.0 [2018-04-30]

## changes

* added Weibull curve
* Changed the optimisation errors to log

# Version 0.2.7 [2018-01-05]

## changes

* update description file

# Version 0.2.6 [2018-01-05]

## bugfixes

* fixed dataset urls

# Version 0.2.5 [2018-01-04]

## changes

* updated naming for datasets
* merged examples into main code

# Version 0.2.4.001 [2018-01-04]

## changes

* renamed sgompertz function to gsgompertz to reflect the correct name Gamma/Shifted Gompertz
* fixed some missing functions in norton-bass function
* Fixed roxygen2 documentation
* Fixed datasets
* Added difcurve function to produce curves for given parameters
* Reintroduced predict, print and plot functions for diffusion objects
* Created new diffusion class for outputs of diffusion()
* Hidden (until fixed) seqdiffusion and norton-bass

# Version 0.2.3 [2017-05-27]

## changes

* Added method predict() for diffusion models
* Added maxiter and tolerance option control
* New option np in predict() which allows to generate new product forecasts with 0 data

## bugfixes

* Fixed print issue for Gompertz

# Version 0.2.2 [2017-05-16]

## changes

* Updated some of the documentation so it contains information about the new optim = parameter
* introducing optim paramter choice to allow for Nelder-Mead or Hooke-Jeeves method


## bugfixes

* Fixed various RoxyGen2 issues and
* Fixed plotting issue
* Added print and plot to Namespace

# Version 0.2.1 [2017-04-11]

## changes

* Updated some of the documentation so it includes details about parameter order of S-Gompertz
* Replaced if statements with switches where necessary - closing Issue #1
* Updated the optimisation process. This should give better results but still not perfect
* Added better commented for diffusionPlot() function - closing Issue #2
* Open tasks in News depreciated and now tracking them with Issues on github
* All the parameter optimisation are now using Subplex algorithm from the nloptr package


## bugfixes

* In diffusion() When providing w, pval length was not using the right parameter
* Several small variable name errors due to the integration

# Version 0.2.0 [2017-04-05]

## changes

* Introducing shifted-gompertz model to the diffusion() function

## bugfixes

* Some typo in the print out function fixed
* carstock.RData file should now be available

# Version 0.1.0 [2017-03-30]

## changes

* Depreciating function Bass() and Gompertz()
* Introducing function diffusion() and seqdiffusion (kindly donated by N. Kourentzes)
* Added dataset safari
* Added dataset windows
* Added cost function for Norton-Bass model
* Included curve fitting into Nortonbass() function
* Added curve fitting ability and insample error estimation for the Gompertz curve
* Added data "broadband"
* Added curve fitting and insample error estimation for Norton-Bass model