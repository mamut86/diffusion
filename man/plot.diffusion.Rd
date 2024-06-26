% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffusion.R
\name{plot.diffusion}
\alias{plot.diffusion}
\title{Plot a fitted diffusion curve.}
\usage{
\method{plot}{diffusion}(x, cumulative = c(FALSE, TRUE), ...)
}
\arguments{
\item{x}{\code{diffusion} object, produced using \code{\link{diffusion}}.}

\item{cumulative}{If TRUE plot cumulative adoption.}

\item{...}{Unused argument.}
}
\value{
None. Function produces a plot.
}
\description{
Produces a plot of a fitted diffusion curve.
}
\examples{
 fit <- diffusion(tsChicken[, 2])
 plot(fit)

}
\seealso{
\code{\link{diffusion}}.
}
\author{
Oliver Schaer, \email{info@oliverschaer.ch},

Nikolaos Kourentzes, \email{nikolaos@kourentzes.com}
}
