#' @importFrom insight get_response
#' @export
residuals.iv_robust <- function(object, ...) {
  .factor_to_numeric(insight::get_response(object)) - object$fitted.values
}
