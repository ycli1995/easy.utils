
#' A fast version of base::intersect()
#'
#' @param x,y Vectors to be compared.
#' @param keep.duplicated Whether or not to keep duplicated elements in \code{x}
#'
#' @references \url{https://stackoverflow.com/questions/72631297/speed-up-
#' setdiff-intersect-union-operations-on-vectors-in-r}
#' 
#' @return A vector of a common mode.
#' 
#' @seealso \code{\link[base]{intersect}}
#' 
#' @examples
#' x <- sample(LETTERS, 12)
#' y <- sample(LETTERS, 12)
#' fastIntersect(x, y)
#' 
#' @importFrom fastmatch fmatch
#'
#' @export
fastIntersect <- function(x, y, keep.duplicated = FALSE) {
  if (is.null(x = x) || is.null(x = y)) {
    return(NULL)
  }
  if (keep.duplicated) {
    u <- x
  } else {
    u <- unique(x = x)
  }
  ind <- fmatch(x = u, table = y, nomatch = 0L)
  y[ind]
}
