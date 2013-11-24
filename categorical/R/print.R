
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

