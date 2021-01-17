#' @title Backports of newer R functions
#' @rdname str2lang
#'
#' @description Backports of newer R functions.
#'
#' @keywords internal
#' @rawNamespace if (getRversion() < "3.6.0") export(str2lang)
str2lang <- function(s) {
  stopifnot(length(s) == 1L)
  ex <- parse(text = s, keep.source = FALSE)
  stopifnot(length(ex) == 1L)
  ex[[1L]]
}


#' @rdname str2lang
#' @keywords internal
#' @rawNamespace if (getRversion() < "3.5.0") export(isTRUE)
isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


#' @rdname str2lang
#' @keywords internal
#' @rawNamespace if (getRversion() < "3.5.0") export(isFALSE)
isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}



#' @rdname str2lang
#' @keywords internal
#' @rawNamespace if (getRversion() < "4.0.0") export(deparse1)
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  if (is.null(expr)) {
    return(NULL)
  }
  paste0(sapply(deparse(expr, width.cutoff = width.cutoff), trimws, simplify = TRUE), collapse = collapse)
}
