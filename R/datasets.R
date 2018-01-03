#' Time series: Assassins Creeds 
#' 
#' A dataset containing the weekly sales of Assassins Creeds game.
#' 
#' @name tsac
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

#' Time series: Broadcast subscribers
#' 
#' A dataset containing the broadcast subscribers to UK market 4Q 2011 to 2Q 2016.
#' 
#' @name tsbroadband
#' @docType data
#' @format A data frame with 51 observations and 4 variables
#' \describe{
#' \item{time}{Quarters}
#' \item{DSL}{Adoption of DSL subscribers}
#' \item{CableModem}{Adoption of CableModem users}
#' \item{FTTPb}{Adoption of FTTPb sales}
#'   }
#'   
#' @references Telecoms Market Matrix
NULL

#' Time series: Stock of cars
#' 
#' A dataset containing the yearly stock of cars in the Netherlands (1965-1989).
#' 
#' @name tscarstock
#' @docType data
#' @format A data frame with 25 observations and 3 variables
#' \describe{
#' \item{year}{Year}
#' \item{raw}{Raw stock numbers}
#' \item{smoothed}{Smoothed stock numbers as described by Franses (1994)}
#'   }
#'   
#' @references Franses, P.H. (1994). Fitting a Gompertz curve. Journal of
#'   Operational Research Society, 45, 109-113.
NULL

#' Time series: Chicken weight
#' 
#' A dataset containing the average weekly female chicken weight.
#' 
#' @name tschicken
#' @docType data
#' @format A data frame with 13 observations and 2 variables
#' \describe{
#' \item{time}{Weeks since birth}
#' \item{weight}{Weight of the female chicken in Kg}
#'   }
#'   
#' @references Jukic, D., Kralik, G. and Scitovski, R. (2004). Least-square
#'   fitting Gompertz curve. Journal of Computational and Applied Mathematics,
#'   169, 359-375.
NULL

#' Time series: Sales of IBM Computers
#' 
#' A dataset containing the first four generations of yearly IBM general-purpose
#' computers installations in the USA.
#' 
#' @name tsibm
#' @docType data
#' @format A data frame with 24 observations and 4 variables \describe{
#' \item{SIU1}{1st generation}
#' \item{SIu2}{2nd generation (starts 6 years after first generation)}
#' \item{SIu3}{3rd generation (starts 11 years after first generation)}
#' \item{SIu4}{4th generation (starts 16 years after first generation)}}
#'   
#' @references Bass, P.I. and Bass, F.M., 2004. IT Waves: Two Completed
#'   Generational Diffusion Models. Working Paper Basseconomics, 1-33.
#' 
#' @source \url{http://www.bassbasement.org/F/N/BBDL/Bass and Bass 2004 AS.pdf}
NULL

#' Time series: Safari Browser market share
#' 
#' A dataset containing the monthly market share of Safri browser generations
#' from Safari 4.0 to Safari 10.
#' 
#' @name tssafari
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
#'   \item{Safari5.0}{Market share of Safari browser v 5.0}
#'   \item{Safari4.1}{Market share of Safari browser v 4.1}
#'   \item{Safari4.0}{Market share of Safari browser v 4.0}}
#'   
#' @source \url{http://gs.statcounter.com/browser-version-market-share}
NULL

#' Time series: Windows OS Platform Statistics
#' 
#' A dataset containing the 3WSchools monthly log files of Windows operating 
#' system usage from March 2003 until February 2017.
#' 
#' @name tswindows
#' @docType data
#' @format A data frame with 168 observations and 9 variables \describe{ 
#'   \item{Date}{Log file date} \item{Win10}{Usage of Windows 10} 
#'   \item{Win8}{Usage of Windows 8} \item{Win7}{Usage of Windows 7} 
#'   \item{Vista}{Usage of Windows Vista} \item{WinXP}{Usage of Windows XP} 
#'   \item{Win2000}{Usage of Windows 2000} \item{Win98}{Usage of Windows 98} 
#'   \item{Win95}{Usage of Windows 95} }
#'   
#' @note From March 2003 until January 2008 log file is only available
#'   bi-monthly. To retain monthly consistency, values have been linearly
#'   interpolated
#'   
#' @source \url{https://www.w3schools.com/browsers/browsers_os.asp}
NULL