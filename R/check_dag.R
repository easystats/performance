#' @title Check correct model adjustment for identifying causal effects
#' @name check_dag
#'
#' @description The purpose of `check_dag()` is to build, check and visualize
#' your model based on directed acyclic graphs (DAG). The function checks if a
#' model is correctly adjusted for identifying specific relationships of
#' variables, especially directed (maybe also "causal") effects for given
#' exposures on an outcome. In case of incorrect adjustments, the function
#' suggests the minimal required variables that should be adjusted for (sometimes
#' also called "controlled for"), i.e. variables that *at least* need to be
#' included in the model. Depending on the goal of the analysis, it is still
#' possible to add more variables to the model than just the minimally required
#' adjustment sets.
#'
#' `check_dag()` is a convenient wrapper around `ggdag::dagify()`,
#' `dagitty::adjustmentSets()` and `dagitty::adjustedNodes()` to check correct
#' adjustment sets. It returns a **dagitty** object that can be visualized with
#' `plot()`. `as.dag()` is a small convenient function to return the
#' dagitty-string, which can be used for the online-tool from the
#' dagitty-website.
#'
#' @param ... One or more formulas, which are converted into **dagitty** syntax.
#' First element may also be model object. If a model objects is provided, its
#' formula is used as first formula, and all independent variables will be used
#' for the `adjusted` argument. See 'Details' and 'Examples'.
#' @param outcome Name of the dependent variable (outcome), as character string
#' or as formula. Must be a valid name from the formulas provided in `...`. If
#' not set, the first dependent variable from the formulas is used.
#' @param exposure Name of the exposure variable (as character string or
#' formula), for which the direct and total causal effect on the `outcome`
#' should be checked. Must be a valid name from the formulas provided in `...`.
#' If not set, the first independent variable from the formulas is used.
#' @param adjusted A character vector or formula with names of variables that
#' are adjusted for in the model, e.g. `adjusted = c("x1", "x2")` or
#' `adjusted = ~ x1 + x2`. If a model object is provided in `...`, any values in
#' `adjusted` will be overwritten by the model's independent variables.
#' @param latent A character vector with names of latent variables in the model.
#' @param effect Character string, indicating which effect to check. Can be
#' `"all"` (default), `"total"`, or `"direct"`.
#' @param coords Coordinates of the variables when plotting the DAG. The
#' coordinates can be provided in three different ways:
#'
#' - a list with two elements, `x` and `y`, which both are named vectors of
#'   numerics. The names correspond to the variable names in the DAG, and the
#'   values for `x` and `y` indicate the x/y coordinates in the plot.
#' - a list with elements that correspond to the variables in the DAG. Each
#'   element is a numeric vector of length two with x- and y-coordinate.
#' - a data frame with three columns: `x`, `y` and `name` (which contains the
#'   variable names).
#'
#' See 'Examples'.
#' @param x An object of class `check_dag`, as returned by `check_dag()`.
#'
#' @section Specifying the DAG formulas:
#'
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
#'   the arrow points in both directions. Bi-directed paths often indicate
#'   unmeasured cause, or unmeasured confounding, of the two involved variables.
#'
#' @section Minimally required adjustments:
#'
#' The function checks if the model is correctly adjusted for identifying the
#' direct and total effects of the exposure on the outcome. If the model is
#' correctly specified, no adjustment is needed to estimate the direct effect.
#' If the model is not correctly specified, the function suggests the minimally
#' required variables that should be adjusted for. The function distinguishes
#' between direct and total effects, and checks if the model is correctly
#' adjusted for both. If the model is cyclic, the function stops and suggests
#' to remove cycles from the model.
#'
#' Note that it sometimes could be necessary to try out different combinations
#' of suggested adjustments, because `check_dag()` can not always detect whether
#' _at least_ one of several variables is required, or whether adjustments should
#' be done for _all_ listed variables. It can be useful to copy the dagitty-code
#' (using `as.dag()`, which prints the dagitty-string into the console) into
#' the dagitty-website and play around with different adjustments.
#'
#' @section Direct and total effects:
#'
#' The direct effect of an exposure on an outcome is the effect that is not
#' mediated by any other variable in the model. The total effect is the sum of
#' the direct and indirect effects. The function checks if the model is correctly
#' adjusted for identifying the direct and total effects of the exposure on the
#' outcome.
#'
#' @section Why are DAGs important - the Table 2 fallacy:
#'
#' Correctly thinking about and identifying the relationships between variables
#' is important when it comes to reporting coefficients from regression models
#' that mutually adjust for "confounders" or include covariates. Different
#' coefficients might have different interpretations, depending on their
#' relationship to other variables in the model. Sometimes, a regression
#' coefficient represents the direct effect of an exposure on an outcome, but
#' sometimes it must be interpreted as total effect, due to the involvement
#' of mediating effects. This problem is also called "Table 2 fallacy"
#' (_Westreich and Greenland 2013_). DAG helps visualizing and thereby focusing
#' the relationships of variables in a regression model to detect missing
#' adjustments or over-adjustment.
#'
#' @return An object of class `check_dag`, which can be visualized with `plot()`.
#' The returned object also inherits from class `dagitty` and thus can be used
#' with all functions from the **ggdag** and **dagitty** packages.
#'
#' @references
#' - Rohrer, J. M. (2018). Thinking clearly about correlations and causation:
#'   Graphical causal models for observational data. Advances in Methods and
#'   Practices in Psychological Science, 1(1), 27–42. \doi{10.1177/2515245917745629}
#'
#' - Westreich, D., & Greenland, S. (2013). The Table 2 Fallacy: Presenting and
#'   Interpreting Confounder and Modifier Coefficients. American Journal of
#'   Epidemiology, 177(4), 292–298. \doi{10.1093/aje/kws412}
#'
#' @examplesIf all(insight::check_if_installed(c("ggdag", "dagitty", "see"), quietly = TRUE))
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
#' # using formula interface for arguments "outcome", "exposure" and "adjusted"
#' check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   outcome = ~y,
#'   exposure = ~x,
#'   adjusted = ~ b + c
#' )
#'
#' # if not provided, "outcome" is taken from first formula, same for "exposure"
#' # thus, we can simplify the above expression to
#' check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   adjusted = ~ b + c
#' )
#'
#' # use specific layout for the DAG
#' dag <- check_dag(
#'   score ~ exp + b + c,
#'   exp ~ b,
#'   outcome = "score",
#'   exposure = "exp",
#'   coords = list(
#'     # x-coordinates for all nodes
#'     x = c(score = 5, exp = 4, b = 3, c = 3),
#'     # y-coordinates for all nodes
#'     y = c(score = 3, exp = 3, b = 2, c = 4)
#'   )
#' )
#' plot(dag)
#'
#' # alternative way of providing the coordinates
#' dag <- check_dag(
#'   score ~ exp + b + c,
#'   exp ~ b,
#'   outcome = "score",
#'   exposure = "exp",
#'   coords = list(
#'     # x/y coordinates for each node
#'     score = c(5, 3),
#'     exp = c(4, 3),
#'     b = c(3, 2),
#'     c = c(3, 4)
#'   )
#' )
#' plot(dag)
#'
#' # Objects returned by `check_dag()` can be used with "ggdag" or "dagitty"
#' ggdag::ggdag_status(dag)
#'
#' # Using a model object to extract information about outcome,
#' # exposure and adjusted variables
#' data(mtcars)
#' m <- lm(mpg ~ wt + gear + disp + cyl, data = mtcars)
#' dag <- check_dag(
#'   m,
#'   wt ~ disp + cyl,
#'   wt ~ am
#' )
#' dag
#' plot(dag)
#' @export
check_dag <- function(...,
                      outcome = NULL,
                      exposure = NULL,
                      adjusted = NULL,
                      latent = NULL,
                      effect = c("all", "total", "direct"),
                      coords = NULL) {
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
    # if we have a model, we *always* overwrite adjusted
    if (!is.null(adjusted)) {
      insight::format_alert("The `adjusted` argument will be overwritten by all independent variables from the model.") # nolint
    }
    adjusted <- vars$conditional
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

  # handle formula interface - if "outcome", "exposure" or "adjusted" are
  # provided as formulas, convert to character here
  if (inherits(outcome, "formula")) {
    outcome <- all.vars(outcome)
  }
  if (inherits(exposure, "formula")) {
    exposure <- all.vars(exposure)
  }
  if (inherits(adjusted, "formula")) {
    adjusted <- all.vars(adjusted)
  }

  # process coords-argument
  coords <- .process_coords(coords)

  # convert to dag
  dag_args <- c(formulas, list(
    exposure = exposure,
    outcome = outcome,
    latent = latent,
    coords = coords
  ))
  dag <- do.call(ggdag::dagify, dag_args)

  # add adjustments
  if (!is.null(adjusted)) {
    dag <- .adjust_dag(dag, adjusted)
  }

  .finalize_dag(dag, effect, outcome, exposure, adjusted)
}


# helper ----------------------------------------------------------------------

.finalize_dag <- function(dag, effect, outcome, exposure, adjusted) {
  # check for cyclic DAG
  cycles <- unlist(dagitty::findCycle(dag))

  # stop if cyclic
  if (!is.null(cycles)) {
    insight::format_error(paste0(
      "Model is cyclic. Causal effects can't be determined for cyclic models. Please remove cycles from the model. To do so, check following variables: ", # nolint
      datawizard::text_concatenate(unique(cycles))
    ))
  }

  # data for checking effects
  checks <- lapply(c("direct", "total"), function(x) {
    adjustment_set <- unlist(dagitty::adjustmentSets(dag, effect = x), use.names = FALSE)
    adjustment_nodes <- unlist(dagitty::adjustedNodes(dag), use.names = FALSE)
    minimal_adjustments <- as.list(dagitty::adjustmentSets(dag, effect = x))
    collider <- adjustment_nodes[vapply(adjustment_nodes, ggdag::is_collider, logical(1), .dag = dag, downstream = FALSE)] # nolint
    if (length(collider)) {
      # if we *have* colliders, remove them from minimal adjustments
      minimal_adjustments <- lapply(minimal_adjustments, setdiff, y = collider)
    } else {
      # if we don't have colliders, set to NULL
      collider <- NULL
    }
    list(
      # no adjustment needed when
      # - required and current adjustment sets are NULL
      # - AND we have no collider in current adjustments
      adjustment_not_needed = is.null(adjustment_set) && is.null(adjustment_nodes) && is.null(collider),
      # incorrect adjustment when
      # - required is NULL and current adjustment not NULL
      # - OR we have a collider in current adjustments
      incorrectly_adjusted = (is.null(adjustment_set) && !is.null(adjustment_nodes)) || (!is.null(collider) && collider %in% adjustment_nodes), # nolint
      current_adjustments = adjustment_nodes,
      minimal_adjustments = minimal_adjustments,
      collider = collider
    )
  })

  attr(dag, "effect") <- effect
  attr(dag, "outcome") <- outcome
  attr(dag, "exposure") <- exposure
  attr(dag, "adjusted") <- adjusted
  attr(dag, "adjustment_sets") <- checks[[1]]$current_adjustments
  attr(dag, "collider") <- checks[[1]]$collider
  # remove collider from sub-attributes
  checks[[1]]$collider <- NULL
  checks[[2]]$collider <- NULL
  attr(dag, "check_direct") <- insight::compact_list(checks[[1]])
  attr(dag, "check_total") <- insight::compact_list(checks[[2]])

  class(dag) <- c(c("check_dag", "see_check_dag"), class(dag))

  dag
}


.adjust_dag <- function(dag, adjusted) {
  for (i in adjusted) {
    # first option, we just have the variable name
    dag <- gsub(paste0("\n", i, "\n"), paste0("\n", i, " [adjusted]\n"), dag, fixed = TRUE)
    # second option, we have the variable name with a [pos] tag when the user
    # provided coords
    dag <- gsub(paste0("\n", i, " [pos="), paste0("\n", i, " [adjusted,pos="), dag, fixed = TRUE)
  }
  dag
}


.process_coords <- function(coords) {
  # check if the coords are not provided as list with x/y elements, but instead
  # as list x/y coordinates for each element. This means, "coords" is provided as
  #
  # coords <- list(
  #   score = c(5, 3),
  #   exp = c(4, 3),
  #   b = c(3, 2),
  #   c = c(3, 4)
  # )
  #
  # but we want
  #
  # coords = list(
  #   x = c(score = 5, exp = 4, b = 3, c = 3),
  #   y = c(score = 3, exp = 3, b = 2, c = 4)
  # )
  #
  # we have to check that it's not a data frame and that it is a list -
  # values like `ggdag::time_ordered_coords()` returns a function, not a list
  if (!is.null(coords) && !is.data.frame(coords) && is.list(coords) && (length(coords) != 2 || !identical(names(coords), c("x", "y")))) { # nolint
    # transform list into data frame, split x and y coordinates into columns
    coords <- datawizard::rownames_as_column(
      stats::setNames(as.data.frame(do.call(rbind, coords)), c("x", "y")),
      "name"
    )
    # reorder
    coords <- coords[c("x", "y", "name")]
  }
  coords
}


# methods --------------------------------------------------------------------

#' @rdname check_dag
#' @export
as.dag <- function(x, ...) {
  if (!inherits(x, "check_dag")) {
    insight::format_error("Input is not of class `check_dag.")
  }
  cat(as.character(x))
}


#' @export
print.check_dag <- function(x, ...) {
  effect <- attributes(x)$effect
  collider <- attributes(x)$collider

  # header
  cat(insight::print_color("# Check for correct adjustment sets", "blue"))

  # model specification
  exposure_outcome_text <- paste0(
    "\n- Outcome: ", attributes(x)$outcome,
    "\n- Exposure", ifelse(length(attributes(x)$exposure) > 1, "s", ""),
    ": ", datawizard::text_concatenate(attributes(x)$exposure)
  )

  # add information on adjustments
  if (!is.null(attributes(x)$adjustment_sets)) {
    exposure_outcome_text <- paste0(
      exposure_outcome_text,
      "\n- Adjustment",
      ifelse(length(attributes(x)$adjustment_sets) > 1, "s", ""),
      ": ", datawizard::text_concatenate(attributes(x)$adjustment_sets)
    )
  }

  # add information on colliders
  if (!is.null(collider)) {
    exposure_outcome_text <- paste0(
      exposure_outcome_text,
      "\n- Collider",
      ifelse(length(collider) > 1, "s", ""),
      ": ", insight::color_text(datawizard::text_concatenate(collider), "cyan")
    )
  }

  cat(exposure_outcome_text)
  cat("\n\n")

  # minimal adjustment sets for direct and total effect identical?
  # Then print only once
  if (identical(attributes(x)$check_direct$minimal_adjustments, attributes(x)$check_total$minimal_adjustments)) {
    .print_dag_results(attributes(x)$check_direct, x, "direct and total", "all", collider)
  } else {
    for (i in c("direct", "total")) {
      if (i == "direct") {
        out <- attributes(x)$check_direct
      } else {
        out <- attributes(x)$check_total
      }
      .print_dag_results(out, x, i, effect, collider)
    }
  }
}

.print_dag_results <- function(out, x, i, effect, collider = NULL) {
  # missing adjustements - minimal_adjustment can be a list of different
  # options for minimal adjustements, so we check here if any of the minimal
  # adjustments are currently sufficient
  sufficient_adjustments <- vapply(out$minimal_adjustments, function(min_adj) {
    !is.null(out$current_adjustments) && all(min_adj %in% out$current_adjustments)
  }, logical(1))

  # build message with check results for effects -----------------------

  if (isTRUE(out$adjustment_not_needed)) {
    # Scenario 1: no adjustment needed
    msg <- paste0(
      insight::color_text("Model is correctly specified.", "green"),
      "\nNo adjustment needed to estimate the ", i, " effect of ",
      datawizard::text_concatenate(attributes(x)$exposure, enclose = "`"),
      " on `",
      attributes(x)$outcome,
      "`."
    )
  } else if (!is.null(collider)) {
    # Scenario 2: adjusted for collider
    msg <- paste0(
      insight::color_text("Incorrectly adjusted!", "red"),
      "\nYour model adjusts for a potential collider. To estimate the ", i, " effect, do ",
      insight::color_text("not", "italic"),
      " adjust for ",
      insight::color_text(datawizard::text_concatenate(collider, enclose = "`"), "cyan"),
      " to avoid collider-bias. It is recommended to double-check for the collider-bias on the dagitty-website."
    )
  } else if (isTRUE(out$incorrectly_adjusted)) {
    # Scenario 3: incorrectly adjusted, adjustments where none is allowed
    msg <- paste0(
      insight::color_text("Incorrectly adjusted!", "red"),
      "\nTo estimate the ", i, " effect, do ",
      insight::color_text("not", "italic"),
      " adjust for ",
      ifelse(length(out$current_adjustments) > 1, "some or all of ", ""),
      insight::color_text(datawizard::text_concatenate(out$current_adjustments, enclose = "`"), "red"),
      "."
    )
  } else if (any(sufficient_adjustments)) {
    # Scenario 4: correct adjustment
    msg <- paste0(
      insight::color_text("Model is correctly specified.", "green"),
      "\nAll minimal sufficient adjustments to estimate the ", i, " effect were done."
    )
  } else {
    # Scenario 5: missing adjustments
    msg <- paste0(
      insight::color_text("Incorrectly adjusted!", "red"),
      "\nTo estimate the ", i, " effect, ",
      insight::color_text("at least", "italic"),
      " adjust for "
    )
    # we may have multiple valid adjustment sets - handle this here
    if (length(out$minimal_adjustments) > 1) {
      msg <- paste0(
        msg,
        "one of the following sets:\n",
        insight::color_text(
          paste(
            "-",
            unlist(lapply(out$minimal_adjustments, paste, collapse = ", "), use.names = FALSE),
            collapse = "\n"
          ),
          "yellow"
        ),
        "."
      )
      current_str <- "\nCurrently"
    } else {
      msg <- paste0(
        msg,
        insight::color_text(datawizard::text_concatenate(
          unlist(out$minimal_adjustments, use.names = FALSE),
          enclose = "`"
        ), "yellow"),
        "."
      )
      current_str <- " Currently"
    }
    if (is.null(out$current_adjustments)) {
      msg <- paste0(msg, current_str, ", the model does not adjust for any variables.")
    } else {
      msg <- paste0(
        msg, current_str, ", the model only adjusts for ",
        datawizard::text_concatenate(out$current_adjustments, enclose = "`"),
        "."
      )
      # check if we could identify missing variables, and if so, add them to the message
      missing_vars <- setdiff(unlist(out$minimal_adjustments), out$current_adjustments)
      if (length(missing_vars) > 0) {
        msg <- paste0(
          msg, " You possibly also need to adjust for ",
          insight::color_text(datawizard::text_concatenate(missing_vars, enclose = "`"), "green"),
          " to block biasing paths."
        )
      }
    }
  }

  if (effect %in% c("all", i)) {
    cat(insight::print_color(insight::format_message(
      paste0("Identification of ", i, " effects\n\n")
    ), "blue"))
    cat(msg)
    cat("\n\n")
  }
}


#' @export
plot.check_dag <- function(x, ...) {
  insight::check_if_installed("see", "to plot DAG")
  NextMethod()
}
