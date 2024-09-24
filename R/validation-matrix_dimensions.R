
#' Validation functions for the dimensions of matrix-like objects
#'
#' Functions to check whether a matrix-like object has expected dimension
#' numbers or names.
#'
#' @param mat A matrix-like object
#'
#' @returns
#' If all the validations are passed, return invisible `NULL`.
#'
#' @name validation-matrix_dimensions
NULL

#' @param nrow Expect how many rows in `mat`.
#' @param ncol Expect how many columns in `mat`.
#' 
#' @examples
#' mat1 <- matrix(0, 3, 5)
#' validMatDims(mat1, 3, 5)
#'
#' @export
#' @rdname validation-matrix_dimensions
validMatDims <- function(mat, nrow = NULL, ncol = NULL) {
  if (length(dim(mat)) != 2) {
    stop("Invalid matrix-like object. The dimension number must be 2.")
  }
  nrow <- nrow[1]
  ncol <- ncol[1]
  if (length(nrow) > 0) {
    if (nrow(mat) != nrow) {
      stop("Row number (", nrow(mat), ") is not equal to ", nrow)
    }
  }
  if (length(ncol) > 0) {
    if (ncol(mat) != ncol) {
      stop("Column number (", ncol(mat), ") is not equal to ", ncol)
    }
  }
  return(invisible(NULL))
}

#' @param row.names Expected row names for `mat`.
#' @param col.names Expected column names for `mat`.
#' @param dup.rownames,dup.colnames Whether or not to allow duplicated dimension
#' names in `mat`.
#'
#' @examples
#' ## Check dimnames
#' mat1 <- matrix(0, 3, 5)
#' rownames(mat1) <- letters[1:3]
#' colnames(mat1) <- LETTERS[1:5]
#' try(validMatDimnames(mat1, row.names = letters[2:4]))  ## Error
#' rownames(mat1) <- c("A", "B", "A")
#' try(validMatDimnames(mat1, row.names = letters[2:4]))  ## Error
#'
#' @export
#' @rdname validation-matrix_dimensions
validMatDimnames <- function(
    mat,
    row.names = NULL,
    col.names = NULL,
    dup.rownames = FALSE,
    dup.colnames = FALSE
) {
  validMatDims(mat)
  .valid_matrix_one_dimnames(
    mat,
    dname.func = rownames,
    dup = dup.rownames,
    ref.dnames = row.names
  )
  .valid_matrix_one_dimnames(
    mat,
    dname.func = colnames,
    dup = dup.colnames,
    ref.dnames = col.names
  )
  return(invisible(NULL))
}

.valid_matrix_one_dimnames <- function(
    mat,
    dname.func,
    dup = FALSE,
    ref.dnames = NULL
) {
  dnames <- dname.func(mat)
  func.name <- as.character(substitute(dname.func))
  if (length(dnames) > 0) {
    if (!dup && anyDuplicated(dnames)) {
      stop("Duplicated ", func.name, " are not allowed.")
    }
  }
  if (length(ref.dnames) > 0) {
    check.dnames <- identicalFMatch(dnames, ref.dnames)
    if (!check.dnames) {
      stop("Not matched ", func.name, ".")
    }
  }
  invisible(NULL)
}
