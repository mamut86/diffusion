#' Diffusion class checkers
#'
#' Functions to check if an object is of the specified class
#'
#' The list of functions includes:
#' \itemize{
#' \item \code{is.diffusion()} tests if the object was produced by a diffusion() function.
#' \item \code{is.bass()} checks if the forecast was produced by the bass() function.
#' }
#'
#' @param x The object to check.
#' @return \code{TRUE} if this is the specified class and \code{FALSE} otherwise.
#'
#' @author Ivan Svetunkov, \email{ivan@@svetunkov.ru},
#' @author Oliver Schaer, \email{info@@oliverschaer.ch}
#' @keywords ts univar
#'
#' @rdname isFunctions
#' @export
is.diffusion <- function(x){
  return(inherits(x,"diffusion"))
}

#' @rdname isFunctions
#' @export
is.bass <- function(x){
  return(inherits(x,"bass"))
}