#' @title Test if Models are Different
#'
#' @description Testing whether models are different is a delicate and often complex procedure, with many limitations and requisites. Moreover, several tests are available, each coming with its own interpretation, and set of strengths and weaknesses.\cr \cr The \code{test_performance()} function is available to run the most relevant and appropriate tests based on the input (for instance, whether the models are \emph{nested} or not). That said, it still requires the user to understand what the tests are and what they do in order to prevent their misinterpretation. See the \strong{details} section for more information regarding the different tests and their interpretation.
#'
#' @param ... Multiple model objects.
#' @param reference This only applies when models are non-nested, and determines which model should be taken as a reference, against which all the other models are tested.
#'
#' @return A data frame containing the relevant indices.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare the performance indices of many different models.
#'
#' @details
#' \subsection{Nested vs. Non-nested Models}{
#' Model's "nesting" is an important concept of models comparison. Indeed, many tests only make sense when the models are \emph{"nested",} i.e., when their predictors are nested. This means that all the predictors of a model are contained within the predictors of a larger model (sometimes referred to as the encompassing model). For instance, \code{model1 (y ~ x1 + x2)} is "nested" within \code{model2 (y ~ x1 + x2 + x3)}. Usually, people have a list of nested models, for instance \code{m1 (y ~ x1 + x2 + x3)}, \code{m2 (y ~ x1 + x2)}, \code{m3 (y ~ x1)}, \code{m4 (y ~ 1)}, and they are "ordered" from the largest to smallest on vice versa, to test whether a more parcimonious model, or whether adding a predictor, results in a significant difference in the model's performance. In this case, models are usually compared \emph{sequentially}: m2 is tested against m1, m3 against m2, m4 against m3, etc.
#' \cr\cr
#' Two models are considered as \emph{"non-nested"} if their predictors are different. For instance, \code{model1 (y ~ x1 + x2)} and \code{model2 (y ~ x3 + x4)}. In the case of non-nested models, all models are usually compared against the same \emph{reference} model (by default, the first of the list).
#' \cr\cr
#' Nesting is detected via the \code{insight::is_nested_models()} function. Aside of the nesting, note also that in order for the tests to be valid, other requirements have often to be the fulfilled. For instance, outcome variable (the response) must be the same. You cannot meaningfuly test whether apples are significantly different from oranges!
#' } \subsection{Tests Description}{
#'
#' \itemize{
#'   \item \strong{Bayes factor for Model Comparison} - \code{bayestestR::bayesfactor_models()}: If all models were fit from the same data, the returned \code{BF} shows the Bayes factor (see \code{bayestestR::bayesfactor_models()}) for each model against the reference model (which depends on whether the models are nested or not).
#'   \item \strong{Wald's F-Test} - \code{test_wald()}: The Wald test is a rough approximation of the Likelihood Ratio Test. However, it is more applicable than the LRT: you can often run a Wald test in situations where no other test can be run.Importantly, this test only makes statistical sense if the models are nested.\cr \cr This test is also available in base R through the \code{\link[=anova]{anova()}} function. It returns an \code{F-value} column as a statistic and its associated \code{p-value}.
#'   \item \strong{Likelihood Ratio Test (LRT)} - \code{test_likelihoodratio()}: The LRT tests which model is a better (more likely) explanation of the data. Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if not equivalent) to the Wald test and, similarly, only makes sense for nested models. However, Maximum likelihood tests make stronger assumptions than method of moments tests like the F-test, and in turn are more efficient. Agresti (1990) suggests that you should use the LRT instead of the Wald test for small sample sizes (under or about 30) or if the parameters are large.\cr \cr For regression models, this is similar to \code{anova(..., test="LRT")} or \code{lmtest::lrtest(...)}, depending on the \code{estimator} argument. For \code{lavaan} models (SEM, CFA), the function calls \code{lavaan::lavTestLRT()}.
#'   \item \strong{Vuong's Test} - \code{test_vuong()}: Vuong's (1989) test can be used both for nested and non-nested models, and actually consists of two tests.\itemize{
#'   \item The \strong{Test of Distinguishability} (the \code{Omega2} column and its associated p-value) indicates whether or not the models can possibly be distinguished on the basis of the observed data. If its p-value is significant, it means the models are distinguishable.
#'   \item The \strong{Robust Likelihood Test} (the \code{LR} column and its associated p-value) indicates whether each model fits better than the reference model. If the models are nested, then the test works as a robust LRT. The code for this function is adapted from the \code{nonnest2} package, and all credit go to their authors.}
#' }
#' }
#'
#' @examples
#' # Nested Models
#' # -------------
#' m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#'
#' test_performance(m1, m2, m3)
#' test_wald(m1, m2, m3)  # equivalent to anova(m1, m2, m3)
#' test_likelihoodratio(m1, m2, m3, estimator = "ML") # Equivalent to lmtest::lrtest(m1, m2, m3)
#' test_likelihoodratio(m1, m2, m3, estimator = "OLS") # Equivalent to anova(m1, m2, m3, test='LRT')
#' test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2, nested=TRUE)
#'
#' # Non-nested Models
#' # -----------------
#' m1 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#' m2 <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' m3 <- lm(Sepal.Length ~ Species, data = iris)
#'
#' test_performance(m1, m2, m3)
#' test_vuong(m1, m2, m3) # nonnest2::vuongtest(m1, m2)
#'
#' # Tweak the output
#' # ----------------
#' test_performance(m1, m2, m3, include_formula=TRUE)
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
#' # Different Model Types
#' # ---------------------
#' if (require("lme4") & require("mgcv")) {
#'   m1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#'   m2 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   m3 <- gam(Sepal.Length ~ s(Petal.Length, by = Species) + Species, data = iris)
#'
#'   test_performance(m1, m2, m3)
#' }
#' }
#'
#' @references
#' \itemize{
#'   \item Vuong, Q. H. (1989). Likelihood ratio tests for model selection and non-nested hypotheses. Econometrica, 57, 307-333.
#'   \item Merkle, E. C., You, D., & Preacher, K. (2016). Testing non-nested structural equation models. Psychological Methods, 21, 151-163.
#' }
#' @export
test_performance <- function(..., reference = 1) {
  UseMethod("test_performance")
}

#' @importFrom insight ellipsis_info
#' @export
test_performance.default <- function(..., reference = 1, include_formula = FALSE) {

  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)
  names(objects) <- match.call(expand.dots = FALSE)$`...`

  # Sanity checks
  if (attributes(objects)$same_response == FALSE) {
    stop("The models don't have the same response variable, which is a prerequisite to compare them.")
  }

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_performance(objects, reference = reference, include_formula = include_formula)
  } else{
    stop("The models cannot be compared for some reason :/")
  }
}




#' @export
test_performance.ListNestedRegressions <- function(objects, reference = 1, include_formula = FALSE, ...) {
  out <- .test_performance_init(objects, include_formula = include_formula, ...)

  # BF test
  out <- .test_BF(objects, out, reference = "sequential")

  # Vuong
  out <- tryCatch(
    {
      rez <- test_vuong(objects)
      rez$Model <- NULL
      out <- merge(out, rez)
    },
    error = function(e) {
      out
    }
  )

  attr(out, "is_nested") <- attributes(objects)$is_nested
  attr(out, "reference") <- if (attributes(objects)$is_nested_increasing) "increasing" else "decreasing"
  class(out) <- c("test_performance", class(out))
  out
}


#' @export
test_performance.ListNonNestedRegressions <- function(objects, reference = 1, include_formula = FALSE, ...) {
  out <- .test_performance_init(objects, include_formula = include_formula, ...)

  # BF test
  out <- .test_BF(objects, out, reference = reference)

  # Vuong
  out <- tryCatch(
    {
      rez <- test_vuong(objects, reference = reference)
      rez$Model <- NULL
      out <- merge(out, rez)
    },
    error = function(e) {
      out
    }
  )

  attr(out, "is_nested") <- attributes(objects)$is_nested
  attr(out, "reference") <- reference
  class(out) <- c("test_performance", class(out))
  out
}



# TESTS IMPLEMENTED IN OTHER PACKAGES

# # Nested
# anova(m2, m2)
# lmtest::waldtest(m2, m3)
# anova(m1, m2, test="LRT")
# lmtest::lrtest(m1, m2)
# nonnest2::vuongtest(m1, m2, nested=TRUE)
#
# # Non-nested
# lmtest::coxtest(m2, m3)
# lmtest::jtest(m2, m3)
# lmtest::encomptest(m2, m3)
# nonnest2::vuongtest(m2, m3)
# nonnest2::icci(m2, m3)



# Helpers -----------------------------------------------------------------

#' @importFrom insight format_table format_p
#' @export
format.test_performance <- function(x, ...){

  # Format cols
  if ("p_Omega2" %in% names(x)) x$p_Omega2 <- insight::format_p(x$p_Omega2, name = NULL)
  if ("p_LR" %in% names(x)) x$p_LR <- insight::format_p(x$p_LR, name = NULL)

  # Format names
  n <- names(x)
  names(x)[n == "p_Omega2"] <- "p (Omega2)"
  names(x)[n == "p_LR"] <- "p (LR)"

  out <- insight::format_table(x)

  if (attributes(x)$is_nested) {
    footer <- paste0("Models were detected as nested. Each model is compared to ",
                     "the one below.")
  } else{
    footer <- paste0("Each model is compared to ",
                     x$Name[attributes(x)$reference],
                     ".")
  }
  attr(out, "table_footer") <- footer
  out
}



#' @importFrom insight export_table
#' @export
print.test_performance <- function(x, ...) {
  out <- insight::export_table(format(x), ...)
  cat(out)
}
#' @export
print_md.test_performance <- function(x, ...) {
  insight::export_table(format(x), format="markdown", ...)
}



#' @importFrom insight model_name
.test_performance_init <- function(objects, include_formula = FALSE) {
  names <- insight::model_name(objects, include_formula = include_formula)
  out <- data.frame(Name = names(objects),
                    Model = names,
                    stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}



#' @importFrom bayestestR bayesfactor_models
.test_BF <- function(objects, out, reference = 1) {
  if (.test_performance_areBayesian(objects) %in% c("yes", "no")) {
    if (reference == "sequential") ref <- 1 else ref <- reference

    rez <- bayestestR::bayesfactor_models(objects, denominator = ref)

    # Adjust BFs for sequential testing
    if (reference == "sequential") {
      # TODO: Double check that formula and whether it works for increasing and
      # decreasing order.
      rez$BF <- rez$BF / c(1, rez$BF[1:nrow(rez) - 1])
    }

    method <- attributes(rez)$BF_method
    rez <- as.data.frame(rez, stringsAsFactors = FALSE)
    rez$Model <- NULL  # Remove Model col - there's already one in 'out'
    rez$BF[ref] <- NA
    row.names(rez) <- NULL

    out <- cbind(out, rez)
  }
  out
}


#' @importFrom insight model_info
.test_performance_areBayesian <- function(objects) {
  bayesian_models <- sapply(objects, function(i) isTRUE(insight::model_info(i)$is_bayesian))
  if (all(bayesian_models == TRUE)) {
    "yes"
  } else if (all(bayesian_models == FALSE)) {
    "no"
  } else {
    "mixed"
  }
}

