
remove_level <- function(x, level) {
  categories <- attr(x, "categories")
  cats  <- names(categories)
  if (length(cats) == 1)
    stop("level can not be removes as there is only one level.")
  if (is.numeric(level)) {
    ncat <- ncol(categories)
    if (level > ncat || level < 1)
      stop("level needs to be between 1 and ", ncat, ".")
    level <- cats[level]
  }
  if (!(level %in% cats)) stop("Unknown category.")
  # when deleting the current level, set the current level to the first 
  # available
  if (level == attr(x, "cat")) {
    level(x) <- cats[cats != level][1]
  }
  # remove categories
  categories <- categories[cats != level]
  attr(x, "categories") <- categories
  x
}

