
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

print.categorical <- function(x, ...) {
  xx <- x
  attr(xx, "class") <- "factor"
  attr(xx, "categories") <- NULL
  attr(xx, "x") <- NULL
  attr(xx, "xcategories") <- NULL
  attr(xx, "cat") <- NULL
  print(xx, ...)
  cats <- names(attr(x, "categories"))
  sel <- cats == attr(x, "cat")
  cats[sel] <- paste0(cats[sel], "*")
  cat("Categories:", paste0(cats, collapse=", "), "\n")
  invisible(x)
}


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

is.categorical <- function(x) {
  inherits(x, "categorical")
}


as.factor.default <- as.factor

as.factor <- function(x) {
  UseMethod("as.factor", x)
}

as.factor.categorical <- function(x) {
  attr(x, "class") <- "factor"
  attr(x, "categories") <- NULL
  attr(x, "x") <- NULL
  attr(x, "xcategories") <- NULL
  attr(x, "cat") <- NULL
  x
}

`[.categorical` <- function(x, i) {
  r <- NextMethod()
  attr(r, "categories") <- attr(x, "categories")
  attr(r, "x") <- attr(x, "x")
  attr(r, "xcategories") <- attr(x, "xcategories")
  attr(r, "cat") <- attr(x, "cat")
  r
}

`[<-.categorical` <- function(x, i, value) {
  # Check if value occurs in xcategories or first column of categories
  if (is.categorical(value)) {
  } else { #if (is.character(value) || is.factor(value)) {
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

