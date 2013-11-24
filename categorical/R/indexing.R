
#' @method `[` categorical
#' @export
`[.categorical` <- function(x, i) {
  r <- NextMethod()
  attr(r, "categories") <- attr(x, "categories")
  attr(r, "x") <- attr(x, "x")
  attr(r, "xcategories") <- attr(x, "xcategories")
  attr(r, "cat") <- attr(x, "cat")
  r
}

#' @method `[<-` categorical
#' @export
`[<-.categorical` <- function(x, i, value) {
  # Check if value occurs in xcategories or first column of categories
  if (is.categorical(value)) {
  } else { 
    value <- as.character(value)
    v <- match(value, attr(x, "xcategories"))
    if (any(is.na(v))) warning("Invalid levels, NAs generated.")
    r <- unclass(x)

    attr(r, "x")[i] <- attr(r, "xcategories")[v]
    r[i] <- v
    class(r) <- class(x)
    level(r) <- level(x)
    r
  }
}

