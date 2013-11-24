
#' Add a new level to a categorical
#' 
#' @param x a \code{\link{categorical}}.
#' @param level a data.frame with the new level(s). This data.frame should
#'   contain at most one column that matches with the existing levels in
#'   \code{x}. This level is used to match the new levels to the existing levels
#'   in \code{x}.
#'
#' @export
add_level <- function(x, level) {
  if (!is.data.frame(level)) stop("level should be a data.frame")
  # one column of level should exist in the categories the remaining not
  categories <- attr(x, "categories")
  exist <- names(level) %in% names(categories)
  if (sum(exist) < 1) 
    stop("At least one column in level should exist in the categories of x")
  if (sum(exist) > 1) 
    stop("More than one column of level matches categories of x")
  for (col in seq_along(level)) {
    level[[col]] <- as.factor(level[[col]])
  }
  # link level to categories using the one matching column
  cats <- names(categories)
  categories <- merge(categories, level, by = names(level)[exist], 
    all.x=TRUE, sort=FALSE)
  # reorder categories
  categories <- categories[c(cats, names(level)[!exist])]
  # add new categories to x
  attr(x, "categories") <- categories
  x
}

