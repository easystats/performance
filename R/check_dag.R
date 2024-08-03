#' @title Check correct model adjustment for identifying causal effects
#' @name check_dag
#'
#' @description `check_dag()` checks if a model is correctly adjusted for
#' identifying causal effects. It returns a **dagitty** object that can be
#' visualized with `plot()`. `check_dag()` is a convenient wrapper around
#' `ggdag::dagify()`, which used `dagitty::adjustmentSets()` and
#' `dagitty::adjustedNodes()` to check if the model is correctly adjusted for
#' identifying causal (direct and total) effects of a given exposure on the
#' outcome. `as.dag()` is a small convenient function to return the dagitty-string,
#' which can be used for the online-tool from the dagitty-website.
#'
#' @param ... One or more formulas, which are converted into **dagitty** syntax.
#' First element may also be model object. If a model objects is provided, its
#' formula is used as first formula. See 'Details'.
#' @param outcome Name of the dependent variable (outcome), as character string.
#' Must be a valid name from the formulas. If not set, the first dependent
#' variable from the formulas is used.
#' @param exposure Name of the exposure variable (as character string), for
#' which the direct and total causal effect on the `outcome` should be checked.
#' Must be a valid name from the formulas. If not set, the first independent
#' variable from the formulas is used.
#' @param adjusted A character vector with names of variables that are adjusted
#' for in the model.
#' @param latent A character vector with names of latent variables in the model.
#' @param effect Character string, indicating which effect to check. Can be
#' `"all"` (default), `"total"`, or `"direct"`.
#' @param x An object of class `check_dag`, as returned by `check_dag()`.
#'
#' @details
#' The formulas have following syntax:
#'
#' - One-directed paths: On the *left-hand-side* is the name of the variables
#'   where causal effects point to (direction of the arrows, in dagitty-language).
#'   On the *right-hand-side* are all variables where causal effects are assumed
#'   to come from. For example, the formula `Y ~ X1 + X2`, paths directed from
#'   both `X1` and `X2` to `Y` are assumed.
#'
#' - Bi-directed paths: Use `~~` to indicate bi-directed paths. For example,
#'   `Y ~~ X` indicates that the path between `Y` and `X` is bi-directed, and
#'   the arrow points in both directions.
#'
#' @return An object of class `check_dag`, which can be visualized with `plot()`.
#' The returned object also inherits from class `dagitty` and thus can be used
#' with all functions from the **ggdag** and **dagitty** packages.
#'
#' @examplesIf require("ggdag", quietly = TRUE) && require("dagitty", quietly = TRUE) && require("see", quietly = TRUE)
#' # no adjustment needed
#' check_dag(
#'   y ~ x + b,
#'   outcome = "y",
#'   exposure = "x"
#' )
#'
#' # incorrect adjustment
#' dag <- check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   outcome = "y",
#'   exposure = "x"
#' )
#' dag
#' plot(dag)
#'
#' # After adjusting for `b`, the model is correctly specified
#' dag <- check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   outcome = "y",
#'   exposure = "x",
#'   adjusted = "b"
#' )
#' dag
#'
#' # Objects returned by `check_dag()` can be used with "ggdag" or "dagitty"
#' ggdag::ggdag_status(dag)
#' @export
check_dag <- function(...,
                      outcome = NULL,
                      exposure = NULL,
                      adjusted = NULL,
                      latent = NULL,
                      effect = c("all", "total", "direct")) {
  UseMethod("check_dag")
}


#' @export
check_dag.dagitty <- function(...,
                              outcome = NULL,
                              exposure = NULL,
                              adjusted = NULL,
                              latent = NULL,
                              effect = c("all", "total", "direct")) {
  insight::format_error("This function is not yet implemented.")
}


#' @export
check_dag.default <- function(...,
                              outcome = NULL,
                              exposure = NULL,
                              adjusted = NULL,
                              latent = NULL,
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
  dag_args <- c(formulas, list(exposure = exposure, outcome = outcome, latent = latent))
  dag <- do.call(ggdag::dagify, dag_args)

  # add adjustments
  if (!is.null(adjusted)) {
    dag <- .adjust_dag(dag, adjusted)
  }

  .finalize_dag(dag, effect, outcome, exposure)
}


# helper ----------------------------------------------------------------------

.finalize_dag <- function(dag, effect, outcome, exposure) {
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

  class(dag) <- c(c("check_dag",  "see_check_dag"), class(dag))

  dag
}


.adjust_dag <- function(dag, adjusted) {
  for (i in adjusted) {
    dag <- gsub(paste0("\n", i, "\n"), paste0("\n", i, " [adjusted]\n"), dag)
  }
  dag
}


# methods --------------------------------------------------------------------

#' @rdname check_dag
#' @export
as.dag.check_dag <- function(x, ...) {
  cat(as.character(x))
}


#' @export
print.check_dag <- function(x, ...) {
  effect <- attributes(x)$effect

  for (i in c("direct", "total")) {
    if (i == "direct") {
      out <- attributes(x)$check_direct
    } else {
      out <- attributes(x)$check_total
    }

    exposure_outcome_text <- paste0(
      "\n- Outcome: ", attributes(x)$outcome,
      "\n- Exposure", ifelse(length(attributes(x)$exposure) > 1, "s", ""),
      ": ", datawizard::text_concatenate(attributes(x)$exposure)
    )

    # build message with check results for effects -----------------------

    if (isTRUE(out$adjustment_not_needed)) {
      # Scenario 1: no adjustment needed
      msg <- paste0(
        insight::color_text("Model is correctly specified.", "green"),
        exposure_outcome_text,
        "\n\nNo adjustment needed to estimate the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        "."
      )
    } else if (isTRUE(out$incorrectly_adjusted)) {
      # Scenario 2: incorrectly adjusted, adjustments where none is allowed
      msg <- paste0(
        insight::color_text("Incorrectly adjusted!", "red"),
        exposure_outcome_text,
        "\n\nTo estimate the ", i, " effect, do ",
        insight::color_text("not", "italic"),
        " adjust for: ",
        datawizard::text_concatenate(out$current_adjustments),
        "."
      )
    } else if (length(out$current_adjustments) != length(out$minimal_adjustment)) {
      # Scenario 3: missing adjustments
      msg <- paste0(
        insight::color_text("Incorrectly adjusted!", "red"),
        exposure_outcome_text,
        "\n\nTo estimate the ", i, " effect, ",
        insight::color_text("also", "italic"),
        " adjust for: ",
        insight::color_text(datawizard::text_concatenate(out$minimal_adjustments), "yellow"),
        "."
      )
      if (is.null(out$current_adjustments)) {
        msg <- paste0(msg, "\nCurrently, the model does not adjust for any variables.")
      } else {
        msg <- paste0(
          msg, "\nCurrently, the model currently only adjusts for ",
          insight::color_text(datawizard::text_concatenate(out$current_adjustments), "yellow"), "."
        )
      }
    } else {
      # Scenario 4: correct adjustment
      msg <- paste0(
        insight::color_text("Model is correctly specified.", "green"),
        exposure_outcome_text,
        "\n\nAll minimal sufficient adjustments to estimate the ", i, " effect were done."
      )
    }

    if (effect %in% c("all", i)) {
      cat(insight::print_color(insight::format_message(
        paste0("# Correct adjustments for identifying {.i ", i, "} effects\n\n")
      ), "blue"))
      cat(msg)
      cat("\n\n")
    }
  }
}

#' @export
plot.check_dag <- function(x, ...) {
  insight::check_if_installed("see", "to plot DAG")
  NextMethod()
}
