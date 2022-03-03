# trim leading / trailing whitespaces
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)


.get_BIC <- function(x) {
  if (inherits(x, c("vgam", "vglm"))) {
    insight::check_if_installed("VGAM")
    out <- VGAM::BIC(x)
  } else if (inherits(x, "bayesx")) {
    out <- stats::BIC(x)[["BIC"]]
  } else {
    out <- tryCatch(stats::BIC(x), error = function(e) NULL)
  }
  .adjust_ic_jacobian(x, out)
}




# safe deparse, works for very long strings
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = "")
}


.std <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }

  # remove missings
  tmp <- stats::na.omit(x)

  # standardize
  tmp <- (tmp - mean(tmp)) / stats::sd(tmp)

  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  x
}




# recode numeric vector, so lowest value stats with 0
# factors are coerced to numeric
.recode_to_zero <- function(x) {
  # check if factor
  if (is.factor(x) || is.character(x)) {
    # try to convert to numeric
    x <- .factor_to_numeric(x)
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}





# safe conversion from factor to numeric
.factor_to_numeric <- function(x, lowest = NULL) {
  if (is.data.frame(x)) {
    as.data.frame(lapply(x, .factor_to_numeric_helper, lowest = lowest))
  } else {
    .factor_to_numeric_helper(x, lowest = lowest)
  }
}

.factor_to_numeric_helper <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }

  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }

  out <- as.numeric(as.character(x))

  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }

  out
}

.get_sigma <- function(model, verbose = TRUE) {
  s <- insight::get_sigma(model, ci = NULL, verbose = verbose)
  if (!is.null(s)) {
    as.numeric(s)
  } else {
    NULL
  }
}
