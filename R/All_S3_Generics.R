
#' Check color map for a factor
#'
#' Function to make sure that all levels of a factor map to distinct colors.
#'
#' @param x An R object contains the factor vector to be checked.
#' @param colors A named vector, whose names are factor levels and values are
#' colors. If is \code{NULL}, just generate colors with \code{setColor}.
#' Otherwise, it first generates colors for each level, then replaces those with
#' names mapping to \code{colors}.
#' @param ... Arguments passed to other methods.
#'
#' @return An updated \code{colors} vector, whose names are identical to the
#' levels.
#'
#' @rdname checkColorMap
#' @export checkColorMap
checkColorMap <- function(x, colors = NULL, ...) {
  UseMethod(generic = "checkColorMap", object = x)
}
