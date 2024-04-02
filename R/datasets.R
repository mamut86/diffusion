#' Time series: Assassins Creeds 
#' 
#' A dataset containing the weekly sales of Assassins Creeds game.
#' 
#' @name tsAc
#' @docType data
#' @format A matrix with 380 observations and 8 variables
#' \describe{
#' \item{ac1}{Assassins Creed 1}
#' \item{ac2}{Assassins Creed 2}
#' \item{ac3}{Assassins Creed 3}
#' \item{ac4}{Assassins Creed 4}
#' \item{ac5}{Assassins Creed 5}
#' \item{ac6}{Assassins Creed 6}
#' \item{ac7}{Assassins Creed 7}
#' \item{ac8}{Assassins Creed 8}
#'   }
#'   
#' @references VGChartz
NULL

#' Time series: Stock of cars
#' 
#' A dataset containing the yearly stock of cars in the Netherlands (1965-1989).
#' 
#' @name tsCarstock
#' @docType data
#' @format A data frame with 25 observations and 3 variables
#' \describe{
#' \item{year}{Year}
#' \item{raw}{Raw stock numbers}
#' \item{smoothed}{Smoothed stock numbers as described by Franses (1994)}
#'   }
#'   
#' @references Franses, P.H. 1994. Fitting a Gompertz curve. Journal of
#'   Operational Research Society, 45, 109-113.
NULL

#' Time series: Chicken weight
#' 
#' A dataset containing the average weekly female chicken weight.
#' 
#' @name tsChicken
#' @docType data
#' @format A data frame with 13 observations and 2 variables
#' \describe{
#' \item{time}{Weeks since birth}
#' \item{weight}{Weight of the female chicken in Kg}
#'   }
#'   
#' @references Jukic, D., Kralik, G. and Scitovski, R. 2004. Least-square
#'   fitting Gompertz curve. Journal of Computational and Applied Mathematics,
#'   169, 359-375.
NULL

#' Time series: Sales of IBM Computers
#' 
#' A dataset containing the first four generations of yearly IBM general-purpose
#' computers installations in the USA.
#' 
#' @name tsIbm
#' @docType data
#' @format A data frame with 24 observations and 4 variables \describe{
#' \item{SIU1}{1st generation}
#' \item{SIU2}{2nd generation (starts 6 years after first generation)}
#' \item{SIU3}{3rd generation (starts 11 years after first generation)}
#' \item{SIU4}{4th generation (starts 16 years after first generation)}}
#'   
#' @references Bass, P.I. and Bass, F.M. 2004. IT Waves: Two Completed
#'   Generational Diffusion Models. Working Paper Basseconomics, 1-33.
#'   
#' @source \url{https://goo.gl/VSEkgM}
NULL

#' Time series: Safari Browser market share
#' 
#' A dataset containing the monthly market share of Safari browser generations
#' from Safari 4.0 to Safari 10.
#' 
#' @name tsSafari
#' @docType data
#' @format A data frame with 98 observations and 13 variables \describe{ 
#'   \item{Date}{Log file date}
#'   \item{Safari10.0}{Usage of Windows 10} 
#'   \item{Safari9.1}{Market share of Safari browser v 10.0}
#'   \item{Safari9.0}{Market share of Safari browser v 9.1} 
#'   \item{Safari8.0}{Market share of Safari browser v 9.0}
#'   \item{Safari7.1}{Market share of Safari browser v 8.0} 
#'   \item{Safari7.0}{Market share of Safari browser v 7.1}
#'   \item{Safari6.1}{Market share of Safari browser v 6.1} 
#'   \item{Safari6.0}{Market share of Safari browser v 6.0}
#'   \item{Safari5.1}{Market share of Safari browser v 5.1}
#'   \item{Safari5.0}{Market share of Safari browser v 5.0}
#'   \item{Safari4.1}{Market share of Safari browser v 4.1}
#'   \item{Safari4.0}{Market share of Safari browser v 4.0}}
#'   
#' @source \url{http://gs.statcounter.com/browser-version-market-share}
NULL

#' Time series: U.S. Merchant Marine conversion to metal
#' 
#' A dataset with conversion of U.S. Merchant Marine from wood to metal.
#' 
#' @name tsMetal
#' @docType data
#' @format A data frame with 17 observations and 2 variables
#' \describe{
#' \item{year}{Year}
#' \item{substitution}{Conversion to metal}
#'   }
#'   
#' @references Martino, J.P. 1993. Technological Forecasting for Decision
#'   Making. 3rd edition. New York: McGraw-Hill.
NULL


#' Time series: Windows OS Platform Statistics
#' 
#' A dataset containing the 3WSchools monthly log files of Windows operating 
#' system usage from March 2003 until February 2017.
#' 
#' @name tsWindows
#' @docType data
#' @format A data frame with 168 observations and 9 variables \describe{ 
#'   \item{Date}{Log file date} \item{Win10}{Usage of Windows 10} 
#'   \item{Win8}{Usage of Windows 8} \item{Win7}{Usage of Windows 7} 
#'   \item{Vista}{Usage of Windows Vista} \item{WinXP}{Usage of Windows XP} 
#'   \item{Win2000}{Usage of Windows 2000} \item{Win98}{Usage of Windows 98} 
#'   \item{Win95}{Usage of Windows 95}}
#'   
#' @note From March 2003 until January 2008 log file is only available
#'   bi-monthly. To retain monthly consistency, values have been linearly
#'   interpolated
#'   
#' @source \url{https://www.w3schools.com/browsers/browsers_os.asp}
NULL

#' Time series: COVID-19 confirmed cases US
#' 
#' A dataset containing the number of confirmed COVID-19 cases in the US.
#' 
#' @name tsCovid
#' @docType data
#' @format A ts object with 107 days of observations
#' \describe{
#' \item{tsCovid}{Daily confirmed COVDID-19 cases}
#'   }
#'   
#' @references COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
#' 
#' @source \url{https://github.com/CSSEGISandData/COVID-19}
NULL
