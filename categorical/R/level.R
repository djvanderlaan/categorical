
#' Getting and setting the current level of a categorical vector
#'
#' @param x the object for which the current level needs to obtained or set.
#' @param value the new current level
#'
#' @details
#'
#' @export
level <- function(x) {
  UseMethod("level")
}

level.categorical <- function(x) {
  attr(x, "cat")
}

`level<-` <- function(x, value) {
  UseMethod("level<-")
}

`level<-.categorical` <- function(x, value) {
  if (!is.character(value) && !is.numeric(value)) 
    stop("value needs to be either character or numeric")
  value <- value[1]
  categories <- attr(x, "categories")
  cats  <- names(categories)
  if (is.numeric(value)) {
    ncat <- ncol(attr(x, "categories"))
    if (value > ncat || value < 1)
      stop("value needs to be between 1 and ", ncat, ".")
    value <- cats[value]
  }
  if (!(value %in% cats)) stop("Unknown category.")
  attr(x, "cat") <- value
  labels <- categories[[value]]
  r <- labels[match(attr(x, "x"), attr(x, "xcategories"))]
  attributes(r) <- attributes(x)
  attr(r, "levels") <- levels(labels)
  r
}

