
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions ####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Unlist a mapping list into a named vector
#'
#' Function to unlist a one-to-one or one-to-many 'key-value' \code{list} into
#' a named \code{vector}. Useful for batched replacement of vector elements.
#'
#' @param map A named list. Each element must be a vector.
#' @param keep.unique Whether or not to remove elements with duplicated names
#' from the output vector.
#'
#' @return
#' A named vector whose names are original values in \code{map}, and elements
#' are keys of \code{map}
#'
#' @examples
#' map <- list(X = c("a", "b"), Y = c("c", "d"))
#' unlistMap(map)
#'
#' map <- list(X = c("a", "b", "c"), Y = c("c", "d"))
#' unlistMap(map)
#' unlistMap(map, keep.unique = FALSE)
#'
#' @importFrom utils stack
#'
#' @export
unlistMap <- function(map, keep.unique = TRUE) {
  map <- stack(x = map)
  map <- setNames(object = as.character(x = map$ind), nm = map$values)
  if (keep.unique) {
    map <- map[unique(x = names(x = map))]
  }
  return(map)
}

#' Paste two factor vectors
#'
#' Paste two factors and re-assign the levels
#'
#' @param x,y Factor vectors
#' @param collapse A character string to separate the \code{x} and \code{y}.
#'
#' @return A new factor vector
#'
#' @examples
#' x <- factor(c(rep("A", 10), rep("B", 10)), levels = c("A", "B"))
#' y <- factor(c(rep("a", 5), rep("b", 15)), levels = c("a", "b"))
#' pasteFactors(x, y)
#'
#' @export
pasteFactors <- function(x, y, collapse = "_") {
  x <- as.factor(x = x)
  y <- as.factor(x = y)
  str <- paste0(x, collapse, y)
  lv <- as.vector(x = sapply(
    X = levels(x = x),
    FUN = paste0,
    collapse,
    levels(x = y),
    simplify = TRUE
  ))
  str <- factor(x = str, levels = lv)
  return(str)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S4 methods ###################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Replace entries according to a mapping list
#'
#' @param x An R vector
#' @param map A named list representing one-to-one or one-to-many mappings.
#' Normally, each name represents a new value, and each element contain the old
#' value(s) to be replaced.
#' @param ... Arguments passed to other methods.
#'
#' @return A updated \code{x}
#'
#' @examples
#' set.seed(1234)
#' fact <- factor(c("A", "A", "B", "A", "B", "C", "D", "E", "D"))
#' map <- list("a" = c("B", "e")) ## Turn all "B" and "E" into "a"
#' replaceEntries(fact, map)
#'
#' @name replaceEntries
NULL

#' @importFrom dplyr recode_factor
#' @importFrom rlang `!!!`
#' @export
#' @rdname replaceEntries
setMethod(
  f = "replaceEntries",
  signature = c("vector", "list"),
  definition = function(x, map, ...) {
    map <- unlistMap(map = map)
    x2 <- as(
      object = recode_factor(x, !!!map, .ordered = FALSE),
      Class = class(x = x)
    )
    return(x2)
  }
)
