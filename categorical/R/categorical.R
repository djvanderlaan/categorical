
#' Create a categorical
#'
#' @param x an atomic vector (e.g. a numeric, factor or character vector) that
#'   is to be converted to a categorical.
#' @param categories a data.frame with the categories. See details and examples
#'   for more information. The values in the first column of this data.frame
#'   have to match those in \code{x}.
#' @param cat the default level.
#'
#' @export
categorical <- function(x, categories, cat = 1) {
  if (!is.atomic(x)) 
    stop("x needs to a atomic vector.")
  if (!is.data.frame(categories))
    stop("categories needs to be a data.frame")
  if (ncol(categories) < 2)
    stop("categories needs to have at least two columns")
  levels <- categories[[1]]
  if (any(!(x %in% levels)))
    warning("Not all values of x occur in first column. ",
      "This will introduce missing values")
  for (col in seq_along(categories)) {
    categories[[col]] <- as.factor(categories[[col]])
  }
  labels <- categories[[cat]]
  r <- labels[match(x, levels)]
  attr(r, "class") <- c("categorical", "factor")
  attr(r, "categories") <- categories
  attr(r, "x") <- x
  attr(r, "xcategories") <- levels
  attr(r, "cat") <- names(categories)[cat]
  r
}


