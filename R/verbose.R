
#' Simple verbose message wrapper
#'
#' @param ... Pass to \code{\link{message}}
#' @param verbose Whether or not to show the message
#'
#' @return Print the progress to console when \code{verbose} is \code{TRUE}.
#'
#' @importFrom rlang is_true
#' @export
verboseMsg <- function(..., verbose = TRUE) {
  if (is_true(x = verbose)) {
    message(...)
  }
  return(invisible())
}
