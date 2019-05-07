#' @title R2 for models with zero-inflation
#' @name r2_zeroinflated
#'
#' @description Calculates R2 for models with zero-inflation component, including mixed effects models.
#'
#' @param model A model.
#' @param method Indicates the method to calculate R2. See 'Details'. May be abbreviated.
#'
#' @return The R2 value.
#'
#' @details The default-method calculates an R2 value based on the residual
#'   sums of squares (using Pearson residuals), divided by the total sum of
#'   squares. For \code{method = "correlation"}, R2 is a correlation-based measure,
#'   which is rather crude. It simply computes the squared correlation between the
#'   model's actual and predicted reponse.
#'
#' @examples
#' library(pscl)
#' data(bioChemists)
#' model <- zeroinfl(
#'   art ~ fem + mar + kid5 + ment | kid5 + phd,
#'   data = bioChemists
#' )
#'
#' r2_zeroinflated(model)
#'
#' @importFrom stats cor predict residuals fitted
#' @importFrom insight model_info get_response
#' @export
r2_zeroinflated <- function(model, method = c("default", "correlation")) {
  method <- match.arg(method)

  mi <- insight::model_info(model)
  if (!mi$is_zero_inflated) {
    warning("Model has no zero-inflation component.")
  }

  if (method == "default")
    .r2_zi_default(model)
  else
    .r2_zi_correlation(model)
}


.r2_zi_correlation <- function(model) {
  r2_zi <- stats::cor(insight::get_response(model), stats::predict(model, type = "response"))^2
  names(r2_zi) <- "R2 for ZI-models"
  r2_zi
}


.r2_zi_default <- function(model) {
  r2_zi <- 1 - (sum(stats::fitted(model)^2) /
                  (sum(stats::fitted(model)^2) + sum(stats::residuals(model, type = "pearson")^2)))
  names(r2_zi) <- "R2 for ZI-models"
  r2_zi
}
