#' Null-model
#'
#' Generate a null-model (intercept and random effects only, no fixed effects).
#'
#' @param model A Statistical model.
#'
#' @importFrom stats formula reformulate update
#' @export
null_model <- function(model) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needs to be installed to compute r-squared for mixed models.", call. = FALSE)
  }

  # yet another brms fix
  f <- stats::formula(model)

  if (is.list(f) && .obj_has_name(f, "formula")) f <- f$formula

  ## https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q4/023013.html
  rterms <- paste0("(", sapply(lme4::findbars(f), deparse, width.cutoff = 500), ")")
  nullform <- stats::reformulate(rterms, response = ".")
  null.model <- stats::update(model, nullform)

  ## Get the fixed effects of the null model
  unname(.collapse_cond(lme4::fixef(null.model)))
}
