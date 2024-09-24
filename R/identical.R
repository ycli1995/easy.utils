
#' Equality testing with some attributes ignored
#' 
#' A wrapper for function \code{\link{identical}}. Some attributes of the two 
#' objects can be ignored when testing.
#' 
#' @param x,y Any R objects.
#' @param ignore.attrs Names of attributes in `x` and `y`. The selected 
#' attributes will be removed before testing. Default is `NULL` (keep all 
#' attributes)
#' @param ... Arguments passed to \code{\link{identical}}.
#' 
#' @return
#' A single logical value (`TRUE` or `FALSE`), same as \code{\link{identical}}.
#' 
#' @name identicalNoAttr
NULL

#' @export
#' @rdname identicalNoAttr 
identicalNoAttr <- function(x, y, ignore.attrs = NULL, ...) {
  if (length(ignore.attrs) > 0) {
    attrs.x <- attributes(x)
    attrs.x <- attrs.x[setdiff(names(attrs.x), ignore.attrs)]
    attributes(x) <- attrs.x

    attrs.y <- attributes(y)
    attrs.y <- attrs.y[setdiff(names(attrs.y), ignore.attrs)]
    attributes(y) <- attrs.y
  }
  identical(x = x, y = y, ...)
}

#' @details
#' `identicalFMatch` is a wrapper for `identicalNoAttr`, where `ignore.attrs` is 
#' set to `".match.hash"`. This function is helpful to test two vectors after 
#' using \code{\link[fastmatch]{fmatch}}, which add external hash tables to the 
#' compared vectors.
#' 
#' @examples
#' x1 <- LETTERS[1:10]
#' x2 <- sample(x1, 5)
#' x3 <- x1[fastmatch::fmatch(x2, x1)]
#' identical(x3, x2)  ## TRUE, but x1 has the '.match.hash' attribute now.
#' 
#' identical(LETTERS[1:10], x1)  ## FALSE
#' identicalFMatch(x3, x2)  ## TRUE
#' 
#' @export
#' @rdname identicalNoAttr
identicalFMatch <- function(x, y, ...) {
  identicalNoAttr(x, y, ignore.attrs = ".match.hash", ...)
}
