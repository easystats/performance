#' @title Test if Models are Different
#'
#' @description Test if models are different using different tests.
#'
#' @param ... Multiple model objects.
#'
#' @return A data frame.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' # Nested Models
#' m1 <- lm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' m2 <- lm(Sepal.Length ~ Petal.Width + Species, data=iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data=iris)
#'
#' test_performance(m1, m2, m3)
#'
#' # Non-nested
#' if(require("lme4") & require("mgcv")){
#' m1 <- lm(Sepal.Length ~ Petal.Length + Species, data=iris)
#' m2 <- lme4::lmer(Sepal.Length ~ Petal.Length + (1|Species), data=iris)
#' m3 <- mgcv::gam(Sepal.Length ~ s(Petal.Length, by=Species) + Species, data=iris)
#'
#' test_performance(m1, m2, m3)
#' }
#'
#'
#' @export
test_performance <- function(...) {
  UseMethod("test_performance")
}

#' @importFrom insight ellipsis_info
#' @export
test_performance.default <- function(...) {

  # Attribute class to list
  objects <- insight::ellipsis_info(...)

  # Replace with names from the global environment
  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # If a suitable class is found, run the more specific method on it
  if(any(c("ListNestedRegressions", "ListNonNestedRegressions") %in% class(objects))){
    test_performance(objects)
  } else{
    stop("The models cannot be compared for some reason :/")
  }
}




#' @export
test_performance.ListNestedRegressions <- function(objects, reference=1, ...) {
  # BF test
  out <- .test_performance_testBF(objects, reference=reference)
  out
}


#' @export
test_performance.ListNonNestedRegressions <- function(objects, reference=1, ...) {
  # BF test
  out <- .test_performance_testBF(objects, reference=reference)
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
#
#
# m2 <- lm(Sepal.Length ~ Species, data=iris)
# m3 <- lm(Sepal.Length ~ Petal.Width, data=iris)
#
# nonnest2::vuongtest(m2, m3)
# nonnest2::icci(m2, m3)



# Helpers -----------------------------------------------------------------

.test_performance_testBF <- function(objects, reference=1){
  if(.test_performance_areBayesian(objects) %in% c("yes", "no")){
    rez <- bayestestR::bayesfactor_models(objects, denominator=reference)
    method <- attributes(rez)$BF_method
    reference <- attributes(rez)$denominator
    out <- as.data.frame(rez)
    out$BF[reference] <- NA
    row.names(out) <- NULL
  } else{
    out <- data.frame()
  }
  out
}


.test_performance_areBayesian <- function(objects){
  info <- sapply(objects, insight::model_info)
  if(all(unlist(info["is_bayesian", ]))){
    "yes"
  } else if(all(unlist(info["is_bayesian", ]) == FALSE)){
    "no"
  } else {
    "mixed"
  }
}


