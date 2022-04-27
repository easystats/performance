#' @exportS3Method residuals iv_robust
residuals.iv_robust <- function(object, ...) {
  datawizard::data_to_numeric(insight::get_response(object, verbose = FALSE), dummy_factors = FALSE, preserve_levels = TRUE) - object$fitted.values
}
