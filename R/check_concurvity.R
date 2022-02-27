#' @rdname check_collinearity
#' @export
check_concurvity <- function(x, ...) {
  UseMethod("check_concurvity")
}


#' @export
check_concurvity.gam <- function(x, ...) {
  insight::check_if_installed("mgcv")
  out <- as.data.frame(mgcv::concurvity(x))

  # only smooth terms
  smooth_terms <- colnames(out)[grepl("s\\((.*)\\)", colnames(out))]
  out <- out[smooth_terms]

  data.frame(
    Term = smooth_terms,
    VIF = as.vector(1 / (1 - as.numeric(out[1, ]))),
    VIF_proportion = as.vector(1 / (1 - as.numeric(out[3, ]))),
    stringsAsFactors = FALSE
  )
}
