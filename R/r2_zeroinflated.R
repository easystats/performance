#' @title R2 for models with zero-inflation
#' @name r2_zeroinflated
#'
#' @description Calculates R2 for models with zero-inflation component, including mixed effects models.
#'
#' @param model A model.
#'
#' @return The R2 value.
#'
#' @details \code{r2_zeroinflated()} is a correlation-based R2 measure, which
#'   is rather crude. It simply computes the squared correlation between the
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
#' @importFrom stats cor predict
#' @importFrom insight model_info get_response
#' @export
r2_zeroinflated <- function(model) {
  mi <- insight::model_info(model)
  if (!mi$is_zero_inflated) {
    warning("Model has no zero-inflation component.")
  }

  r2_zi <- stats::cor(insight::get_response(model), stats::predict(model, type = "response"))^2
  names(r2_zi) <- "R2 for ZI-models"
  r2_zi
}
