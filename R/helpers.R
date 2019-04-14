#' is string empty?
#' @keywords internal
.is_empty_object <- function(x) {
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}

#' has object an element with given name?
#' @keywords internal
.obj_has_name <- function(x, name) {
  name %in% names(x)
}


#' @importFrom stats na.omit sd
#' @keywords internal
.std <- function(x) {
  if (!is.numeric(x)) return(x)

  # remove missings
  tmp <- stats::na.omit(x)

  # standardize
  tmp <- (tmp - mean(tmp)) / stats::sd(tmp)

  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  x
}


#' @keywords internal
recode_to_zero <- function(x) {
  # check if factor
  if (is.factor(x)) {
    # try to convert to numeric
    x <- as.numeric(as.character(x))
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}
