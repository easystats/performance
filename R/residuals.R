#' @importFrom insight get_response
#' @export
residuals.iv_robust <- function(object, ...) {
  insight::get_response(object) - object$fitted.values
}
