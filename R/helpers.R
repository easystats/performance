# small wrapper around this commonly used try-catch
.safe <- function(code, on_error = NULL) {
  if (isTRUE(getOption("easystats_erros", FALSE) && is.null(on_error))) {
    code
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


.get_BIC <- function(x, estimator = "ML") {
  # check ML estimator
  if (missing(estimator) && inherits(x, "lmerMod")) {
    estimator <- "REML"
  }
  REML <- identical(estimator, "REML")

  if (inherits(x, c("vgam", "vglm"))) {
    insight::check_if_installed("VGAM")
    out <- .adjust_ic_jacobian(x, VGAM::BIC(x))
  } else if (inherits(x, "bayesx")) {
    out <- .adjust_ic_jacobian(x, stats::BIC(x)[["BIC"]])
  } else {
    out <- .safe(
      stats::BIC(insight::get_loglikelihood(x, check_response = TRUE, REML = REML, verbose = FALSE))
    )
    # when `get_loglikelihood()` does not work, `stats::BIC` sometimes still works (e.g., `fixest`)
    if (is.null(out)) {
      out <- .safe(stats::BIC(x))
    }
  }

  out
}


.std <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  # remove missings
  tmp <- x[!is.na(x)]
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
    x <- datawizard::to_numeric(x, dummy_factors = FALSE, preserve_levels = TRUE)
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}


.get_sigma <- function(model, verbose = TRUE) {
  s <- insight::get_sigma(model, ci = NULL, verbose = verbose)
  if (is.null(s)) {
    NULL
  } else {
    as.numeric(s)
  }
}


# functions to check if necessary default argument was provided ------------

.is_model_valid <- function(model) {
  if (missing(model) || is.null(model)) {
    insight::format_error(
      "You must provide a model-object. Argument `model` cannot be missing or `NULL`."
    )
  }
}
