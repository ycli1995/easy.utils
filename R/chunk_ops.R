
#' Generate chunk points
#'
#' Unexported helper function \code{ChunkPoints} from \pkg{Seurat}. This
#' can be quite useful when user needs to chunk some operations.
#'
#' @param dsize How big is the data being chunked
#' @param csize How big should each chunk be
#'
#' @return A 2 x N \code{\link{matrix}} where each column is a chunk. The first
#' row contains start points, and the second row contains end points.
#' 
#' @references \url{https://github.com/satijalab/seurat/blob/763259d05991d40721d
#' ee99c9919ec6d4491d15e/R/utilities.R#L1699}
#' 
#' @examples
#' ### Split an index vector with 15273 elements into chunks, each of which has
#' ### 3000 elements.
#' chunkPoints(15273, 3000)
#'
#' @export
chunkPoints <- function(dsize, csize) {
  return(vapply(
    X = 1L:ceiling(x = dsize / csize),
    FUN = function(i) {
      return(c(
        start = (csize * (i - 1L)) + 1L,
        end = min(csize * i, dsize)
      ))
    },
    FUN.VALUE = numeric(length = 2L)
  ))
}
