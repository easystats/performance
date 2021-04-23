#' @export
residuals.iv_robust <- function(object, ...) {
  .factor_to_numeric(insight::get_response(object, verbose = FALSE)) - object$fitted.values
}
