#' @title Test if Models are Different
#'
#' @description Test if models are different using different tests.
#'
#' @param ... Multiple model objects.
#' @param reference Which model should be taken as a reference, against which all the other models are tested.
#'
#' @return A data frame.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' # Nested Models
#' m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#'
#' test_performance(m1, m2, m3)
#' test_performance(m1, m2, m3, include_formula=TRUE)
#'
#' # Non-nested
#' if (require("lme4") & require("mgcv")) {
#'   m1 <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#'   m2 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   m3 <- gam(Sepal.Length ~ s(Petal.Length, by = Species) + Species, data = iris)
#'
#'   test_performance(m1, m2, m3)
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
  attr(out, "reference") <- if(attributes(objects)$is_nested_increasing) "increasing" else "decreasing"
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

#' @importFrom insight format_table
#' @export
format.test_performance <- function(x, ...){

  # Format cols
  if("p_Omega2" %in% names(x)) x$p_Omega2 <- insight::format_p(x$p_Omega2, name = NULL)
  if("p_LR" %in% names(x)) x$p_LR <- insight::format_p(x$p_LR, name = NULL)

  # Format names
  n <- names(x)
  names(x)[n == "p_Omega2"] <- "p (Omega2)"
  names(x)[n == "p_LR"] <- "p (LR)"

  out <- insight::format_table(x)

  if(attributes(x)$is_nested){
    footer <- paste0("Models were detected as nested. Each model is compared to ",
                     ifelse(attributes(x)$reference == "increasing", "the one below", "the one above"),
                     ".")
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
print.test_performance <- function(x, ...){
  out <- insight::export_table(format(x), ...)
  cat(out)
}




#' @importFrom insight model_name
.test_performance_init <- function(objects, include_formula = FALSE){
  names <- insight::model_name(objects, include_formula = include_formula)
  out <- data.frame(Name = names(objects),
                    Model = names)
  row.names(out) <- NULL
  out
}



#' @importFrom bayestestR bayesfactor_models
.test_BF <- function(objects, out, reference = 1) {
  if (.test_performance_areBayesian(objects) %in% c("yes", "no")) {
    if(reference == "sequential") ref <- 1 else ref <- reference

    rez <- bayestestR::bayesfactor_models(objects, denominator = ref)

    # Adjust BFs for sequential testing
    # if(reference == "sequential"){
    #   if(attributes(objects)$is_nested_increasing){
    #     rez$BFseq <- rez$BF / c(1, rez$BF[1:nrow(rez)-1])
    #   } else{
    #     rez$BFseq <- rez$BF # TODO: correct the formula for other way round
    #   }
    # }

    method <- attributes(rez)$BF_method
    rez <- as.data.frame(rez)
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

