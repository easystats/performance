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
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_somers(model)
#' @references Somers, R. H. (1962). A new asymmetric measure of association for ordinal variables. American Sociological Review. 27 (6).
#'
#' @export
r2_somers <- function(model) {
  info <- insight::model_info(model)
  if (!info$is_binomial) {
    stop("'r2_somers()' only accepts logistic regression models.", call. = FALSE)
  }
  if (!requireNamespace("correlation", quietly = TRUE)) {
    stop("Package 'correlation' needed for this function to work. Please install it.", call. = FALSE)
  }

  input <- data.frame(
    y = .recode_to_zero(insight::get_response(model)),
    pred = stats::predict(model, type = "response"),
    stringsAsFactors = FALSE
  )

  out <- correlation::cor_test(input, "y", "pred", method = "somers")

  r2somers <- out$Dxy
  names(r2somers) <- "Somers' Dxy"
  r2somers
}
