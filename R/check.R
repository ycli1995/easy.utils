
#' Check whether the lengths of input objects are equal
#'
#' @param ... R objects to be compared
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @export
checkSameLength <- function(...) {
  ll <- list(...)
  return(all(lengths(x = ll) == length(x = ll[[1]])))
}

#' Check whether some dimensions of two arrays are aligned
#'
#' @param incoming The array-like object to check
#' @param reference The array-like object to be aligned with
#' @param align.dims A integer vector indicating which dimensions of
#' \code{reference} should be used for alignment. The length must be equal to
#' the dimension numbers of \code{incoming}
#' @param in.name The name of \code{incoming}. Only use for verbose.
#' @param ref.name The name of \code{reference}. Only use for verbose.
#' @param withDimnames Logical. Whether to also align the dimension names.
#'
#' @details
#' Some examples for \code{align.dims}:
#' \itemize{
#' \item \code{c(1, 1)}: The dim[1] of \code{incoming} must align with the
#' dim[1] of \code{reference}, and the dim[2] of \code{incoming} must align with
#' the dim[1] of \code{reference}.
#' \item \code{c(2, 1)}: The dim[1] of \code{incoming} must align with the
#' dim[2] of \code{reference}, and the dim[2] of \code{incoming} must align with
#' the dim[1] of \code{reference}.
#' \item \code{c(NA, 1)}: The dim[1] of \code{incoming} doesn't need to align
#' with any dimension of \code{reference}, but the dim[2] of \code{incoming}
#' must align with the dim[1] of \code{reference}.
#' \item \code{c(2, NA)}: The dim[1] of \code{incoming} must align with the
#' dim[2] of \code{reference}, but the dim[2] of \code{incoming} doesn't need to
#' align with any dimension of \code{reference}.
#' }
#'
#' @return If any dimension is not aligned, raise an error.
#'
#' @examples
#' 
#' # Get some expression matrices ----
#' exp1 <- matrix(0, 10, 20)
#' colnames(exp1) <- paste0("cell_", 1:ncol(exp1))
#' rownames(exp1) <- paste0("gene_", 1:nrow(exp1))
#'
#' exp2 <- matrix(0, 10, 15)
#' colnames(exp2) <- paste0("cell_", 1:ncol(exp2))
#' rownames(exp2) <- paste0("gene_", 1:nrow(exp2))
#'
#' exp3 <- matrix(0, 10, 20)
#' colnames(exp3) <- paste0("c_", 1:ncol(exp3))
#' rownames(exp3) <- paste0("g_", 1:nrow(exp3))
#'
#' # Get some PCA embbeding matrices ----
#' pca1 <- matrix(0, 10, 5)
#' rownames(pca1) <- paste0("cell_", 1:nrow(pca1))
#' colnames(pca1) <- paste0("PC_", 1:ncol(pca1))
#'
#' pca2 <- matrix(0, 20, 5)
#' rownames(pca2) <- paste0("cell_", 1:nrow(pca2))
#' colnames(pca2) <- paste0("PC_", 1:ncol(pca2))
#'
#' pca3 <- matrix(0, 20, 5)
#' rownames(pca3) <- paste0("c_", 1:nrow(pca3))
#' colnames(pca3) <- paste0("PC_", 1:ncol(pca3))
#'
#' # Error: The Dim 2 of exp1 is not aligned with the Dim 2 of exp2!
#' try(checkAlignedDims(exp2, exp1, c(1, 2)))
#' 
#' checkAlignedDims(exp3, exp1, c(1, 2))
#' 
#' # Error: The Dim 1 of exp3 is not aligned with the Dim 1 of exp1!
#' try(checkAlignedDims(exp3, exp1, c(1, 2), withDimnames = TRUE))
#'
#' checkAlignedDims(exp3, exp1, c(NA, 2)) # Don't check the rows of exp3
#' 
#' # Error: The Dim 2 of exp3 is not aligned with the Dim 2 of exp1!
#' try(checkAlignedDims(exp3, exp1, c(NA, 2), withDimnames = TRUE))
#'
#' # Error: The Dim 1 of pca1 is not aligned with the Dim 2 of exp1!
#' # Don't check the columns of pca1
#' try(checkAlignedDims(pca1, exp1, c(2, NA)))
#' 
#' checkAlignedDims(pca2, exp1, c(2, NA))
#' checkAlignedDims(pca2, exp1, c(2, NA), withDimnames = TRUE)
#' checkAlignedDims(pca3, exp1, c(2, NA))
#' 
#' # Error: The Dim 1 of pca3 is not aligned with the Dim 2 of exp1!
#' try(checkAlignedDims(pca3, exp1, c(2, NA), withDimnames = TRUE))
#' 
#' 
#' @export
checkAlignedDims <- function(
    incoming,
    reference,
    align.dims,
    in.name = NULL,
    ref.name = NULL,
    withDimnames = FALSE
) {
  if (!is.null(x = in.name)) {
    in.name <- paste0(" '", in.name, "'")
  }
  if (!is.null(x = ref.name)) {
    ref.name <- paste0(" '", ref.name, "'")
  }
  in.dims <- dim(x = incoming)
  ref.dims <- dim(x = reference)
  if (withDimnames) {
    in.dimns <- dimnames(x = incoming)
    ref.dimns <- dimnames(x = reference)
  }
  if (!checkSameLength(align.dims, in.dims)) {
    stop(
      "checkAlignedDims failed: \n  ",
      "The length of 'align.dims' must be equal to ",
      "the dimension number of input", in.name, "!"
    )
  }
  for (i in seq_along(along.with = align.dims)) {
    d <- align.dims[i]
    if (is.na(x = d)) {
      next
    }
    chk <- ifelse(
      test = withDimnames,
      yes = identical(x = in.dimns[[i]], y = ref.dimns[[d]]),
      no = identical(x = in.dims[i], y = ref.dims[d])
    )
    if (chk) {
      next
    }
    stop(
      "checkAlignedDims failed: \n  ",
      "The Dim ", i, " of input", in.name, " is not aligned with the Dim ",
      d, " of reference", ref.name, "!"
    )
  }
  return(invisible(x = NULL))
}

#' Check valid characters
#'
#' Check if input characters are valid (neither \code{NA} nor \code{""})
#'
#' @param x A vector, matrix or list
#'
#' @return A logical vector
#'
#' @examples
#' isValidCharacters(c("a", "", "b"))
#' isValidCharacters(c("a", NA, "b"))
#'
#' @export
isValidCharacters <- function(x) {
  if (any(!is.character(x = x))) {
    return(logical(length = length(x = x)))
  }
  return(
    nzchar(x = x, keepNA = FALSE) &
      !is.na(x = x) &
      is.atomic(x = x)
  )
}

#' Fetch column names exists in the data object
#'
#' @param object Any object that has implemented \code{colnames(object)}.
#' @param query Column names to check.
#' 
#' @return An update \code{query} where only entries existing in 
#' \code{colnames(object)} are kept. If no any \code{query} was found, raise an 
#' error.
#'
#' @export
fetchColnames <- function(object, query) {
  col.names <- colnames(x = object)
  query.notfound <- setdiff(x = query, y = col.names)
  if (length(x = query.notfound) > 0) {
    warning(
      length(x = query.notfound), " query column names were not found:\n",
      paste(head(x = query.notfound), collapse = ", "), "...",
      immediate. = TRUE
    )
    query <- fastIntersect(x = query, y = col.names)
  }
  if (length(x = query) == 0) {
    stop("No query column name was found")
  }
  return(query)
}
