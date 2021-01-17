.str2lang <- function(s) {
  stopifnot(length(s) == 1L)
  ex <- parse(text = s, keep.source = FALSE)
  stopifnot(length(ex) == 1L)
  ex[[1L]]
}


isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
