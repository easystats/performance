#' @exportS3Method residuals iv_robust
residuals.iv_robust <- function(object, ...) {
  datawizard::convert_data_to_numeric(insight::get_response(object, verbose = FALSE)) - object$fitted.values
}
