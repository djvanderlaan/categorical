
#' @method as.factor default
#' @export
as.factor.default <- as.factor

#' Convert a variable to factor
#' 
#' @param x the object to convert to factor
#'
#' @export
as.factor <- function(x) {
  UseMethod("as.factor", x)
}

#' @method as.factor categorical
#' @export
as.factor.categorical <- function(x) {
  attr(x, "class") <- "factor"
  attr(x, "categories") <- NULL
  attr(x, "x") <- NULL
  attr(x, "xcategories") <- NULL
  attr(x, "cat") <- NULL
  x
}

