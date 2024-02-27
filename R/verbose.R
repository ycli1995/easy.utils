
#' Simple verbose message wrapper
#'
#' @param ... Pass to \code{\link{message}}
#' @param verbose Whether or not to show the message. If is \code{NULL}, will 
#' search \code{verbose} variable in \code{\link{parent.frame}}.
#'
#' @return Print the progress to console when \code{verbose} is \code{TRUE}.
#'
#' @importFrom rlang is_true
#' @export
verboseMsg <- function(..., verbose = NULL) {
  verbose <- verbose %||% parent.frame()$verbose %||% TRUE
  if (is_true(x = verbose)) {
    message(...)
  }
  return(invisible())
}
