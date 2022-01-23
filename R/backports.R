isTRUE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


isFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
