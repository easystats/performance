.get_BIC <- function(x, estimator = "ML") {
  # check ML estimator
  if (missing(estimator) && inherits(x, "lmerMod")) {
    estimator <- "REML"
  }
  REML <- identical(estimator, "REML")

  if (inherits(x, c("vgam", "vglm"))) {
    insight::check_if_installed("VGAM")
    out <- VGAM::BIC(x)

  } else if (inherits(x, "bayesx")) {
    out <- stats::BIC(x)[["BIC"]]

  } else if (inherits(x, "fixest")) { # hangs on "else" call
    out <- stats::BIC(x)[["BIC"]]

  } else {
    out <- tryCatch(
      stats::BIC(insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = FALSE)),
      error = function(e) NULL
    )
  }
  .adjust_ic_jacobian(x, out)
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
    x <- datawizard::data_to_numeric(x, dummy_factors = FALSE, preserve_levels = TRUE)
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}


.get_sigma <- function(model, verbose = TRUE) {
  s <- insight::get_sigma(model, ci = NULL, verbose = verbose)
  if (!is.null(s)) {
    as.numeric(s)
  } else {
    NULL
  }
}
