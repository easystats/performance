#' @title Somers' Dxy rank correlation for binary outcomes
#' @name r2_somers
#'
#' @description Calculates the Somers' Dxy rank correlation for logistic regression models.
#'
#' @param model A logistic regression model.
#'
#' @return A named vector with the R2 value.
#'
#' @examples
#' \dontrun{
#' if (require("correlation") && require("Hmisc")) {
#'   model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#'   r2_somers(model)
#' }
#' }
#'
#' @references Somers, R. H. (1962). A new asymmetric measure of association for
#'   ordinal variables. American Sociological Review. 27 (6).
#' @export
r2_somers <- function(model) {
  insight::check_if_installed("correlation")

  info <- insight::model_info(model, verbose = FALSE)
  if (!info$is_binomial) {
    stop("'r2_somers()' only accepts logistic regression models.", call. = FALSE)
  }

  input <- data.frame(
    y = .recode_to_zero(insight::get_response(model, verbose = FALSE)),
    pred = stats::predict(model, type = "response"),
    stringsAsFactors = FALSE
  )

  out <- correlation::cor_test(input, "y", "pred", method = "somers")

  r2somers <- out$Dxy
  names(r2somers) <- "Somers' Dxy"
  r2somers
}
