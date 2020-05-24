#### v.0.3.1.910 ####

changes
- new automated removal of leading and trailing NAs. Error abort if NA within series
- new search optimisation option
- added documentation for tsMetal.RData
- added internal sum squared error function for the optimiser
- added passing optim setting int trough Gompertz and GSGompertz initialisation
- added error handling for optimx function
- changed non-critical warnings to type message, to allow easy surpression
- renamed option variable l to loss (for l-norm selection)
- renamed option variable x to y (the response variable)
- introudced initmeth option to chose between set of different initalisation ways
- introduced scal option to scale market potential for better optimisation results
- updated documentation for diffusion() - Thx Ivan!


bugfixes
- issues with log errors - reverted error measure to v.0.2.7 for the time being
- fixed an initalisation issue on the weibull model
- fixed titles for graphs
- fixed typo in documentation for predict function
- fixed fcst table to contain same number as horizons
- fixed number of paramters that go into G/GS (only relevant if w.idx is active)
- fixed error when plot.seqdiffusion() was used with cleanlead = F
- fixed error in gompertz when cleanlead = F
- fixed error when plotting ts-object with cumulative = F
- fixed an error when elimante was TRUE with 0 pval reps to stop.
- fixed the sequential diffusion pval elimination process

#### v.0.3.0 (30/04/18) ####

changes
- added Weibull curve
- Changed the optimisation errors to log

#### v.0.2.7 (05/01/18) ####

changes
- update description file

#### v.0.2.6 (05/01/18) ####

bugfixes
- fixed dataset urls

#### v.0.2.5 (04/01/18) ####

changes
- updated naming for datasets
- merged examples into main code

#### v.0.2.4.001 (04/01/18) ####

changes
- renamed sgompertz function to gsgompertz to reflect the correct name Gamma/Shifted Gompertz
- fixed some missing functions in norton-bass function

#### v.0.2.4.000 (03/01/18) ####

changes
- Fixed roxygen2 documentation
- Fixed datasets
- Added difcurve function to produce curves for given parameters
- Reintroduced predict, print and plot functions for diffusion objects
- Created new diffusion class for outputs of diffusion()
- Hidden (until fixed) seqdiffusion and norton-bass

#### v.0.2.3.911 (27/05/17) ####

changes
- Added method predict() for diffusion models
- New option np in predict() which allows to generate new product forecasts with 0 data

#### v.0.2.3.910 (25/05/17) ####

changes
- added maxiter and tolerance option control

Bugfixes:
- Fixed print issue for Gompertz


#### v.0.2.2.909 (16/05/17) ####

Bugfixes:
- Fixed various RoxyGen2 issues and
- Fixed plotting issue


#### v.0.2.2.908 (16/05/17) ####

Changes:
- Updated some of the documentation so it contains information about the new optim = parameter
- introducing optim paramter choice to allow for Nelder-Mead or Hooke-Jeeves method

Bugfixes:
- Added print and plot to Namespace

#### v.0.2.1.907 (11/04/17) ####

Changes:
- Updated some of the documentation so it includes details about parameter order of S-Gompertz

Bugfixes:
- In diffusion() When providing w, pval length was not using the right parameter

#### v.0.2.1.906 (10/04/17) ####

Changes:
Replaced if statements with switches where necessary - closing Issue #1
Updated the optimisation process. This should give better results but still not perfect
Added better commented for diffusionPlot() function - closing Issue #2
Open tasks in News depreciated and now tracking them with Issues on github
All the parameter optimisation are now using Subplex algorithm from the nloptr package

Bugfixes:
Several small variable name errors due to the integration

#### v.0.2.0.905 (05/04/17) ####
Changes:
Introducing shifted-gompertz model to the diffusion() function

Bugfixes:
Some typo in the print out function fixed
carstock.RData file should now be available

#### v.0.1.0.904 (30/03/17) ####
Changes:
Depreciating funciton Bass() and Gompertz()
Introducing function diffusion() and seqdiffusion (kindly donated by N. Kourentzes)
  - allows for p-value estimation and sequential generation fitting
  - includes plotting
  - allows forecasting
Added dataset safari
Added dataset windows

#### v.0.0.0.903 (25/03/17) ####
Changes:
Added cost function for Norton-Bass model
Included curve fitting into Nortonbass() function

#### v.0.0.0.902 (20/03/17) ####
Changes:
Added curve fitting ability and insample error estimation for the Gompertz curve
Added data "broadband"

#### v.0.0.0.901 (18/03/17) ####
Changes:
Added curve fitting and insample error estimation for Norton-Bass model