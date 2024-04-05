#' @title Test if models are different
#'
#' @description
#' Testing whether models are "different" in terms of accuracy or explanatory
#' power is a delicate and often complex procedure, with many limitations and
#' prerequisites. Moreover, many tests exist, each coming with its own
#' interpretation, and set of strengths and weaknesses.
#'
#' The `test_performance()` function runs the most relevant and appropriate
#' tests based on the type of input (for instance, whether the models are
#' *nested* or not). However, it still requires the user to understand what the
#' tests are and what they do in order to prevent their misinterpretation. See
#' the *Details* section for more information regarding the different tests
#' and their interpretation.
#'
#' @param ... Multiple model objects.
#' @param reference This only applies when models are non-nested, and determines
#'   which model should be taken as a reference, against which all the other
#'   models are tested.
#' @param text_length Numeric, length (number of chars) of output lines.
#'   `test_bf()` describes models by their formulas, which can lead to
#'   overly long lines in the output. `text_length` fixes the length of
#'   lines to a specified limit.
#' @param verbose Toggle warning and messages.
#'
#' @return A data frame containing the relevant indices.
#'
#' @seealso [`compare_performance()`] to compare the performance indices of
#' many different models.
#'
#' @details
#' ## Nested vs. Non-nested Models
#' Model's "nesting" is an important concept of models comparison. Indeed, many
#' tests only make sense when the models are *"nested",* i.e., when their
#' predictors are nested. This means that all the *fixed effects* predictors of
#' a model are contained within the *fixed effects* predictors of a larger model
#' (sometimes referred to as the encompassing model). For instance,
#' `model1 (y ~ x1 + x2)` is "nested" within `model2 (y ~ x1 + x2 + x3)`. Usually,
#' people have a list of nested models, for instance `m1 (y ~ 1)`, `m2 (y ~ x1)`,
#' `m3 (y ~ x1 + x2)`, `m4 (y ~ x1 + x2 + x3)`, and it is conventional
#' that they are "ordered" from the smallest to largest, but it is up to the
#' user to reverse the order from largest to smallest. The test then shows
#' whether a more parsimonious model, or whether adding a predictor, results in
#' a significant difference in the model's performance. In this case, models are
#' usually compared *sequentially*: m2 is tested against m1, m3 against m2,
#' m4 against m3, etc.
#'
#' Two models are considered as *"non-nested"* if their predictors are
#' different. For instance, `model1 (y ~ x1 + x2)` and `model2 (y ~ x3 + x4)`.
#' In the case of non-nested models, all models are usually compared
#' against the same *reference* model (by default, the first of the list).
#'
#' Nesting is detected via the `insight::is_nested_models()` function.
#' Note that, apart from the nesting, in order for the tests to be valid,
#' other requirements have often to be the fulfilled. For instance, outcome
#' variables (the response) must be the same. You cannot meaningfully test
#' whether apples are significantly different from oranges!
#'
#' ## Estimator of the standard deviation
#' The estimator is relevant when comparing regression models using
#' `test_likelihoodratio()`. If `estimator = "OLS"`, then it uses the same
#' method as `anova(..., test = "LRT")` implemented in base R, i.e., scaling
#' by n-k (the unbiased OLS estimator) and using this estimator under the
#' alternative hypothesis. If `estimator = "ML"`, which is for instance used
#' by `lrtest(...)` in package **lmtest**, the scaling is done by n (the
#' biased ML estimator) and the estimator under the null hypothesis. In
#' moderately large samples, the differences should be negligible, but it
#' is possible that OLS would perform slightly better in small samples with
#' Gaussian errors. For `estimator = "REML"`, the LRT is based on the REML-fit
#' log-likelihoods of the models. Note that not all types of estimators are
#' available for all model classes.
#'
#' ## REML versus ML estimator
#' When `estimator = "ML"`, which is the default for linear mixed models (unless
#' they share the same fixed effects), values from information criteria (AIC,
#' AICc) are based on the ML-estimator, while the default behaviour of `AIC()`
#' may be different (in particular for linear mixed models from **lme4**, which
#' sets `REML = TRUE`). This default in `test_likelihoodratio()` intentional,
#' because comparing information criteria based on REML fits requires the same
#' fixed effects for all models, which is often not the case. Thus, while
#' `anova.merMod()` automatically refits all models to REML when performing a
#' LRT, `test_likelihoodratio()` checks if a comparison based on REML fits is
#' indeed valid, and if so, uses REML as default (else, ML is the default).
#' Set the `estimator` argument explicitely to override the default behaviour.
#'
#' ## Tests Description
#'
#' - **Bayes factor for Model Comparison** - `test_bf()`: If all
#'   models were fit from the same data, the returned `BF` shows the Bayes
#'   Factor (see `bayestestR::bayesfactor_models()`) for each model against
#'   the reference model (which depends on whether the models are nested or
#'   not). Check out
#'   [this vignette](https://easystats.github.io/bayestestR/articles/bayes_factors.html#bayesfactor_models)
#'   for more details.
#'
#' - **Wald's F-Test** - `test_wald()`: The Wald test is a rough
#'   approximation of the Likelihood Ratio Test. However, it is more applicable
#'   than the LRT: you can often run a Wald test in situations where no other
#'   test can be run. Importantly, this test only makes statistical sense if the
#'   models are nested.
#'
#'   Note: this test is also available in base R
#'   through the [`anova()`][anova] function. It returns an `F-value` column
#'   as a statistic and its associated p-value.
#'
#' - **Likelihood Ratio Test (LRT)** - `test_likelihoodratio()`:
#'   The LRT tests which model is a better (more likely) explanation of the
#'   data. Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if
#'   not equivalent) to the Wald test and, similarly, only makes sense for
#'   nested models. However, maximum likelihood tests make stronger assumptions
#'   than method of moments tests like the F-test, and in turn are more
#'   efficient. Agresti (1990) suggests that you should use the LRT instead of
#'   the Wald test for small sample sizes (under or about 30) or if the
#'   parameters are large.
#'
#'   Note: for regression models, this is similar to
#'   `anova(..., test="LRT")` (on models) or `lmtest::lrtest(...)`, depending
#'   on the `estimator` argument. For **lavaan** models (SEM, CFA), the function
#'   calls `lavaan::lavTestLRT()`.
#'
#'   For models with transformed response variables (like `log(x)` or `sqrt(x)`),
#'   `logLik()` returns a wrong log-likelihood. However, `test_likelihoodratio()`
#'   calls `insight::get_loglikelihood()` with `check_response=TRUE`, which
#'   returns a corrected log-likelihood value for models with transformed
#'   response variables. Furthermore, since the LRT only accepts nested
#'   models (i.e. models that differ in their fixed effects), the computed
#'   log-likelihood is always based on the ML estimator, not on the REML fits.
#'
#' - **Vuong's Test** - `test_vuong()`: Vuong's (1989) test can
#'   be used both for nested and non-nested models, and actually consists of two
#'   tests.
#'
#'   - The **Test of Distinguishability** (the `Omega2` column and
#'     its associated p-value) indicates whether or not the models can possibly be
#'     distinguished on the basis of the observed data. If its p-value is
#'     significant, it means the models are distinguishable.
#'
#'   - The **Robust Likelihood Test** (the `LR` column and its
#'     associated p-value) indicates whether each model fits better than the
#'     reference model. If the models are nested, then the test works as a robust
#'     LRT. The code for this function is adapted from the **nonnest2**
#'     package, and all credit go to their authors.
#'
#' @examples
#' # Nested Models
#' # -------------
#' m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#' m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
#'
#' test_performance(m1, m2, m3)
#'
#' test_bf(m1, m2, m3)
#' test_wald(m1, m2, m3) # Equivalent to anova(m1, m2, m3)
#'
#' # Equivalent to lmtest::lrtest(m1, m2, m3)
#' test_likelihoodratio(m1, m2, m3, estimator = "ML")
#'
#' # Equivalent to anova(m1, m2, m3, test='LRT')
#' test_likelihoodratio(m1, m2, m3, estimator = "OLS")
#'
#' if (require("CompQuadForm")) {
#'   test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2, nested=TRUE)
#'
#'   # Non-nested Models
#'   # -----------------
#'   m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#'   m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
#'   m3 <- lm(Sepal.Length ~ Species, data = iris)
#'
#'   test_performance(m1, m2, m3)
#'   test_bf(m1, m2, m3)
#'   test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2)
#' }
#'
#' # Tweak the output
#' # ----------------
#' test_performance(m1, m2, m3, include_formula = TRUE)
#'
#'
#' # SEM / CFA (lavaan objects)
#' # --------------------------
#' # Lavaan Models
#' if (require("lavaan")) {
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ textual + speed "
#'   m1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ 0 * textual + speed "
#'   m2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ 0 * textual + 0 * speed "
#'   m3 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   test_likelihoodratio(m1, m2, m3)
#'
#'   # Different Model Types
#'   # ---------------------
#'   if (require("lme4") && require("mgcv")) {
#'     m1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#'     m2 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'     m3 <- gam(Sepal.Length ~ s(Petal.Length, by = Species) + Species, data = iris)
#'
#'     test_performance(m1, m2, m3)
#'   }
#' }
#'
#' @references
#'
#'  - Vuong, Q. H. (1989). Likelihood ratio tests for model selection and
#'    non-nested hypotheses. Econometrica, 57, 307-333.
#'
#'  - Merkle, E. C., You, D., & Preacher, K. (2016). Testing non-nested
#'    structural equation models. Psychological Methods, 21, 151-163.
#'
#' @export
test_performance <- function(..., reference = 1, verbose = TRUE) {
  UseMethod("test_performance")
}



# default --------------------------------

#' @export
test_performance.default <- function(..., reference = 1, include_formula = FALSE, verbose = TRUE) {
  # Attribute class to list and get names from the global environment
  my_objects <- insight::ellipsis_info(..., only_models = TRUE)

  # validation checks (will throw error if non-valid objects)
  my_objects <- .test_performance_checks(my_objects, verbose = verbose)

  # ensure proper object names
  my_objects <- .check_objectnames(my_objects, sapply(match.call(expand.dots = FALSE)[["..."]], as.character))

  # If a suitable class is found, run the more specific method on it
  if (inherits(my_objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_performance(my_objects, reference = reference, include_formula = include_formula)
  } else {
    insight::format_error("The models cannot be compared for some reason :/")
  }
}



# methods ------------------------------

#' @export
plot.test_performance <- function(x, ...) {
  insight::format_alert(
    "There is currently no `plot()` method for test-functions.",
    "Please use `plot(compare_perfomance())` for some visual representations of your model comparisons."
  )
}


#' @export
format.test_performance <- function(x, digits = 2, ...) {
  # Format cols and names
  out <- insight::format_table(x, digits = digits, exact = FALSE, ...)

  if (isTRUE(attributes(x)$is_nested)) {
    footer <- paste0(
      "Models were detected as nested (in terms of fixed parameters) and are compared in sequential order.\n"
    )
  } else {
    footer <- paste0(
      "Each model is compared to ",
      x$Name[attributes(x)$reference],
      ".\n"
    )
  }
  attr(out, "table_footer") <- footer
  out
}


#' @export
print.test_performance <- function(x, digits = 2, ...) {
  out <- insight::export_table(format(x, digits = digits, ...), ...)
  cat(out)
}


#' @export
print_md.test_performance <- function(x, digits = 2, ...) {
  insight::export_table(format(x, digits = digits, ...), format = "markdown", ...)
}


#' @export
print_html.test_performance <- function(x, digits = 2, ...) {
  insight::export_table(format(x, digits = digits, ...), format = "html", ...)
}


#' @export
display.test_performance <- function(object, format = "markdown", digits = 2, ...) {
  if (format == "markdown") {
    print_md(x = object, digits = digits, ...)
  } else {
    print_html(x = object, digits = digits, ...)
  }
}



# other classes -----------------------------------

#' @export
test_performance.ListNestedRegressions <- function(objects,
                                                   reference = 1,
                                                   include_formula = FALSE,
                                                   ...) {
  out <- .test_performance_init(objects, include_formula = include_formula, ...)

  # BF test
  tryCatch(
    {
      rez <- test_bf(objects, reference = "sequential")
      if (!is.null(rez)) {
        rez$Model <- NULL
        out <- cbind(out, rez)
      }
    },
    error = function(e) {
      # Do nothing
    }
  )

  # Vuong, or LRT
  tryCatch(
    {
      if (isTRUE(insight::check_if_installed("CompQuadForm", quietly = TRUE))) {
        rez <- test_vuong(objects)
      } else {
        rez <- test_lrt(objects)
      }
      rez$Model <- NULL
      out <- merge(out, rez, sort = FALSE)
    },
    error = function(e) {
      # Do nothing
    }
  )

  attr(out, "is_nested") <- attributes(objects)$is_nested
  attr(out, "reference") <- if (attributes(objects)$is_nested_increasing) "increasing" else "decreasing"
  class(out) <- c("test_performance", class(out))
  out
}


#' @export
test_performance.ListNonNestedRegressions <- function(objects,
                                                      reference = 1,
                                                      include_formula = FALSE,
                                                      ...) {
  out <- .test_performance_init(objects, include_formula = include_formula, ...)

  # BF test
  tryCatch(
    {
      rez <- test_bf(objects, reference = reference)
      if (!is.null(rez)) {
        rez$Model <- NULL
        out <- cbind(out, rez)
      }
    },
    error = function(e) {
      # Do nothing
    }
  )

  # Vuong, or Wald - we have non-nested models, so no LRT here
  tryCatch(
    {
      if (isTRUE(insight::check_if_installed("CompQuadForm", quietly = TRUE))) {
        rez <- test_vuong(objects, reference = reference)
      } else {
        rez <- test_wald(objects)
      }
      rez$Model <- NULL
      out <- merge(out, rez, sort = FALSE)
    },
    error = function(e) {
      # Do nothing
    }
  )

  attr(out, "is_nested") <- attributes(objects)$is_nested
  attr(out, "reference") <- reference
  class(out) <- c("test_performance", class(out))
  out
}



# TESTS IMPLEMENTED IN OTHER PACKAGES

# # Non-nested
# lmtest::coxtest(m2, m3)
# lmtest::jtest(m2, m3)
# lmtest::encomptest(m2, m3)
# nonnest2::icci(m2, m3)



# Helpers -----------------------------------------------------------------


.test_performance_init <- function(objects, include_formula = FALSE) {
  model_names <- insight::model_name(objects, include_formula = include_formula)
  out <- data.frame(
    Name = names(objects),
    Model = model_names,
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out
}



.test_performance_checks <- function(objects, multiple = TRUE, same_response = TRUE, verbose = TRUE) {
  # TODO: we could actually generate a baseline model 'y ~ 1' whenever a single model is passed
  if (multiple && insight::is_model(objects)) {
    null_model <- .safe(insight::null_model(objects, verbose = FALSE))
    if (!is.null(null_model) && insight::is_model(null_model)) {
      objects <- insight::ellipsis_info(list(null_model, objects))
      names(objects) <- c("Null model", "Full model")
      if (verbose) {
        insight::format_alert(
          "Only one model was provided, however, at least two are required for comparison.",
          "Fitting a null-model as reference now."
        )
      }
    } else {
      insight::format_error("At least two models are required to test them.")
    }
  }

  if (same_response && !inherits(objects, "ListLavaan") && isFALSE(attributes(objects)$same_response)) {
    insight::format_error(
      "The models' dependent variables don't have the same data, which is a prerequisite to compare them. Probably the proportion of missing data differs between models." # nolint
    )
  }

  # check formula of all models, but warn only once
  already_warned <- FALSE
  for (i in objects) {
    if (!already_warned) {
      check_formula <- insight::formula_ok(i)
    }
    if (check_formula) {
      already_warned <- TRUE
    }
  }

  objects
}



.check_objectnames <- function(objects, dot_names) {
  # Replace with names from the global environment, if these are not yet properly set
  object_names <- insight::compact_character(names(objects))
  # check if we have any names at all
  if ((is.null(object_names) ||
    # or if length of names doesn't match number of models
    length(object_names) != length(objects) ||
    # or if names are "..1", "..2" pattern
    all(grepl("\\.\\.\\d", object_names))) &&
    # and length of dot-names must match length of objects
    length(objects) == length(dot_names)) {
    names(objects) <- dot_names
  }
  objects
}
