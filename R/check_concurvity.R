#' @rdname check_collinearity
#' @export
check_concurvity <- function(x, ...) {
  UseMethod("check_concurvity")
}


#' @export
check_concurvity.gam <- function(x, ...) {
  insight::check_if_installed("mgcv")
  conc <- as.data.frame(mgcv::concurvity(x))

  # only smooth terms
  smooth_terms <- colnames(conc)[grepl("s\\((.*)\\)", colnames(conc))]
  conc <- conc[smooth_terms]

  out <- data.frame(
    Term = smooth_terms,
    VIF = as.vector(1 / (1 - as.numeric(conc[1, ])))^2,
    VIF_proportion = as.vector(as.numeric(conc[3, ])),
    stringsAsFactors = FALSE
  )

  class(out) <- c("check_concurvity", "see_check_concurvity", class(out))
  out
}
