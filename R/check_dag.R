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

  effect <- match.arg(effect)

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
  dag_args <- c(formulas, list(exposure = exposure, outcome = outcome))
  dag <- do.call(ggdag::dagify, dag_args)

  # add adjustments
  if (!is.null(adjusted)) {
    dag <- .adjust_dag(dag, adjusted)
  }

  # data for checking effects
  checks <- lapply(c("direct", "total"), function(x) {
    adjustment_set <- unlist(dagitty::adjustmentSets(dag, effect = x), use.names = FALSE)
    adjustment_nodes <- unlist(dagitty::adjustedNodes(dag), use.names = FALSE)
    list(
      adjustment_not_needed = is.null(adjustment_set) && is.null(adjustment_nodes),
      incorrectly_adjusted = is.null(adjustment_set) && !is.null(adjustment_nodes),
      current_adjustments = adjustment_nodes,
      minimal_adjustments = adjustment_set
    )
  })

  attr(dag, "effect") <- effect
  attr(dag, "outcome") <- outcome
  attr(dag, "exposure") <- exposure
  attr(dag, "check_direct") <- insight::compact_list(checks[[1]])
  attr(dag, "check_total") <- insight::compact_list(checks[[2]])

  class(dag) <- c("check_dag", class(dag))

  dag
}


# helper ----------------------------------------------------------------------

.adjust_dag <- function(dag, adjusted) {
  for (i in adjusted) {
    dag <- gsub(paste0("\n", i, "\n"), paste0("\n", i, " [adjusted]\n"), dag)
  }
  dag
}


# methods --------------------------------------------------------------------

#' @export
print.check_dag <- function(x, ...) {
  effect <- attributes(x)$effect

  for (i in c("direct", "total")) {
    if (i == "direct") {
      out <- attributes(x)$check_direct
    } else {
      out <- attributes(x)$check_total
    }

    # build message with check results for effects -----------------------

    if (isTRUE(out$adjustment_not_needed)) {
      # Scenario 1: no adjustment needed
      msg <- paste0(
        "Model is correctly specified. No adjustment is necessary for estimating the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        "."
      )
    } else if (isTRUE(out$incorrectly_adjusted)) {
      # Scenario 2: incorrectly adjusted, adjustments where none is allowed
      msg <- paste0(
        "Incorrectly adjusted! To estimate the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        ", do *not* adjust for following variables: ",
        datawizard::text_concatenate(out$current_adjustments),
        "."
      )
    } else if (length(out$current_adjustments) != length(out$minimal_adjustment)) {
      # Scenario 3: missing adjustments
      msg <- paste0(
        "Incorrectly adjusted! The minimal sufficient adjustments for estimating the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        " are following variables: ",
        datawizard::text_concatenate(out$minimal_adjustments),
        ". "
      )
      if (is.null(out$current_adjustments)) {
        msg <- paste0(msg, "However, the model does not adjusts for any variables.")
      } else {
        msg <- paste0(
          msg, "However, the model currently only adjusts for ",
          datawizard::text_concatenate(out$current_adjustments), "."
        )
      }
    } else {
      # Scenario 4: correct adjustment
      msg <- paste0(
        "Model is correctly specified. All minimal sufficient adjustments for estimating the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        " were done."
      )
    }

    if (effect %in% c("all", i)) {
      cat(insight::print_color(insight::format_message(paste0("# Correct adjustments for identifying {.i ", i, "} effects\n\n")), "blue"))
      cat(insight::format_message(msg))
      cat("\n\n")
    }
  }
}
