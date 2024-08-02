#' @title Check correct model adjustment for identifying causal effects
#' @name check_dag
#'
#' @description Check correct model adjustment for identifying causal effects.
#'
#' @export
check_dag <- function(...,
                      outcome = NULL,
                      exposure = NULL,
                      adjusted = NULL,
                      effect = c("all", "total", "direct")) {
  insight::check_if_installed(
    c("ggdag", "dagitty"),
    reason = "to check correct adjustments for identifying causal effects."
  )

  # retrieve formulas
  formulas <- list(...)

  # check if first object is a model object, and convert to formula
  if (insight::is_regression_model(formulas[[1]])) {
    vars <- insight::find_variables(
      formulas[[1]],
      effects = "fixed",
      component = "conditional",
      flatten = FALSE
    )
    formulas[[1]] <- stats::as.formula(
      paste(vars$response, "~", paste(vars$conditional, collapse = "+"))
    )
  }

  # if outcome is not set, use first dependent variable
  if (is.null(outcome)) {
    outcome <- insight::find_response(formulas[[1]])
  }

  # if exposure is not set, use first independent variable
  if (is.null(exposure)) {
    exposure <- insight::find_variables(
      formulas[[1]],
      effects = "fixed",
      component = "conditional",
      flatten = FALSE
    )$conditional[1]
  }

  # convert to dag
  dag <- ggdag::dagify(formulas, exposure = exposure, outcome = outcome)

  # add adjustments
  if (!is.null(adjusted)) {
    for (i in adjusted) {
      dag <- gsub(paste0("\n", i, "\n"), paste0("\n", i, " [adjusted]\n"), dag)
    }
  }

  attr(dag, "effect") <- effect
  class(dag) <- c("check_dag", class(dag))

  dag
}


#' @export
print.check_dag <- function(x, effect = NULL, ...) {
  if (is.null(effect)) {
    effect <- attributes(x)$effect
  }

  if (effect)
  dagitty::adjustmentSets(dag)
  ggdag_adjustment_set(dag)
  # wofür müssen wir adjustieren, wenn wir am direkten Effekt interessiert sind?
  dagitty::adjustmentSets(dag, effect = "direct")
  ggdag_adjustment_set(dag, effect = "direct")
  # incorrect adjustement - adjustmentSets() liefert leeres Element und
  # adjustedNodes() hat mehr als 1 Element
  r <- dagitty::adjustmentSets(dag)
  incorrectly_adjusted <- (!length(r) && length(dagitty::adjustedNodes(dag)) > 0)
  incorrectly_adjusted
  r <- dagitty::adjustmentSets(dag, effect = "direct")
  incorrectly_adjusted <- (!length(r) && length(dagitty::adjustedNodes(dag)) > 0)
  incorrectly_adjusted


}
