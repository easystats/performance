# is string empty?
is_empty_object <- function(x) {
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}

# has object an element with given name?
obj_has_name <- function(x, name) {
  name %in% names(x)
}