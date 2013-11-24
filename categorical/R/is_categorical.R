
#' Check is object is of type categorical
#'
#' @param x object for which the type needs to be checked
#'
#' @return
#' A logical indicating whether \code{x} has class \code{\link{categorical}}.
#'
#' @export
is.categorical <- function(x) {
  inherits(x, "categorical")
}


