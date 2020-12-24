#' @title Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test (LRT) for model comparison, which tests which model is a better (more likely) explanation of the data. Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if not equivalent) to the commonly used F-test (obtained by default with \code{anova(...)}). Maximum likelihood tests make stronger assumptions than method of moments tests like the F-test, and in turn are more efficient.
#' \itemize{
#' \item For regression models, this is similar to \code{anova(..., test="LRT")} or \code{lmtest::lrtest(...)}, depending on the estimator.
#' \item For \code{lavaan} models (SEM, CFA), the function calls \code{lavaan::lavTestLRT()}.
#' }
#'
#' @param ... Multiple model objects.
#'
#' @return A data frame, based on the results from \code{anova()}.
#'
#' @details This only makes statistical sense if the models are nested. It is conventional to list the models from smallest to largest, but this is up to the user. The output shows the tests of the models against one another in the order specified.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' # Regression Models
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' performance_lrt(m1, m2, m3)
#'
#' # Lavaan Models
#' if (require("lavaan")) {
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ textual + speed"
#'   m1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ 0*textual + speed"
#'   m2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9
#'
#'                   visual ~~ 0*textual + 0*speed"
#'   m3 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#' }
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}



#' @rdname performance_lrt
#' @param estimator Applied when comparing regression models. Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="OLS"} (default), then it uses the same method as \code{anova(..., test="LRT")} implemented in base R, i.e., scaling by n−k (the unbiased OLS estimator) and using this estimator under the alternative hypothesis. If \code{estimator="ML"}, which is for instance used by \code{lrtest(...)} in the \code{lmtest}, the scaling is done by n (the biased ML estimator) and the estimator under the null hypothesis. In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors.
#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.List_of_Regressions <- function(..., estimator="OLS") {
  objects <- list(...)

  if (!all(sapply(objects, insight::is_model))) {
    stop("All objects must be valid regression model objects.")
  }

  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # LRT for model comparison
  if (length(objects) > 1) {
    out <- .performance_lrt_lm(..., objects = objects, estimator=estimator)
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}

#' @export
performance_lrt.default <- performance_lrt.List_of_Regressions





#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.lavaan <- function(...) {
  objects <- list(...)

  if (!all(sapply(objects, insight::is_model))) {
    stop("All objects must be valid lavaan models.")
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required. Please install it.")
  }

  if (length(objects) <= 1) {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }

  # Get names
  object_names <- match.call(expand.dots = FALSE)$`...`

  # Create data frame with info about model name and class
  names_types <- do.call(rbind, mapply(function(.x, .y) {
    data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE)
  }, objects, object_names, SIMPLIFY = FALSE))

  out <- as.data.frame(lavaan::lavTestLRT(..., test = "LRT", model.names=object_names))

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[grepl("^Pr\\(>", names(out))] <- "p"
  out$Model <- row.names(out)

  # Bind all data
  out <- merge(names_types, out[c("Model", "df", "Chi2", "p")], by="Model")

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}











# Linear Models --------------------------




# m1 <- lm(mpg ~ wt + cyl, data = mtcars)
# m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
# m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
# objects <- list(m1 = m1, m2 = m2, m3 = m3)
#
# So 'OLS' is equivalent to the anova() implementation
# .performance_lrt_lm(objects, estimator="OLS")
# anova(m1, m2, test="LRT")
# anova(m1, m3, test="LRT")
#
# And 'ML' to the lmtest one
# .performance_lrt_lm(objects, estimator="ML")
# lmtest::lrtest(m1, m2)
# lmtest::lrtest(m1, m3)
.performance_lrt_lm <- function(..., objects, estimator = "OLS") {

  if(tolower(estimator) == "ols"){
    out <- .performance_lrt_lm_OLS(...)
  } else{
    out <- .performance_lrt_lm_ML(objects[[1]], objects[[1]], estimator = estimator)
    out$p <- out$Chi2 <- NA
    for(i in 2:length(objects)){
      out <- rbind(out, .performance_lrt_lm_ML(objects[[i]], objects[[i-1]], estimator = estimator))
    }
  }

  # Get model Type
  names_types <- do.call(rbind, mapply(function(.x, .y) {
    data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE)
  }, objects, names(objects), SIMPLIFY = FALSE))

  # Add Names
  out <- cbind(names_types, out)
  row.names(out) <- NULL

  out
}




#' @importFrom stats df.residual dnorm fitted model.frame model.response dnorm residuals pchisq
.performance_lrt_lm_OLS <- function(...) {
  out <- anova(..., test="LRT")
  data.frame(
    "df" = out$Res.Df,
    "Chi2" = qchisq(out$`Pr(>Chi)`, out$Df, lower.tail = TRUE),
    "p" = out$`Pr(>Chi)`,
    stringsAsFactors = FALSE
  )
}


#' @importFrom stats df.residual dnorm fitted model.frame model.response dnorm residuals pchisq
.performance_lrt_lm_ML <- function(model, model2, estimator="ML") {
  # See https://stats.stackexchange.com/questions/155474/why-does-lrtest-not-match-anovatest-lrt

  # Get DF
  # TODO: Would be good to use parameters::dof if available in insight
  df_model <- stats::df.residual(model)
  df_model2 <- stats::df.residual(model2)
  df_diff <- df_model - df_model2

  # LogLik Function
  loglik_function <- function(object, estimator) {
    sum(stats::dnorm(stats::model.response(stats::model.frame(object)), mean = stats::fitted(object), sd = estimator, log = TRUE))
  }

  # Get Statistic
  # TODO: Could use a possible insight::get_residuals()
  if (tolower(estimator) == "ols") {
    estim <- function(object) sqrt(sum(stats::residuals(object)^2) / stats::df.residual(object))
    loglik <- loglik_function(model2, estim(model))
    stat <- -2 * (loglik - loglik_function(model, estim(model)))
  } else{
    estim <- function(object) sqrt(mean(stats::residuals(object)^2))
    stat <- -2 * (loglik_function(model2, estim(model2)) - loglik_function(model, estim(model)))
    loglik <- as.numeric(stats::logLik(model)) # This is the loglik returned
  }
  # Get p-value
  p <- stats::pchisq(abs(stat), abs(df_diff), lower.tail = FALSE)

  # Out
  data.frame(
    "df" = df_model,
    "LogLik" = loglik,
    "Chi2" = stat,
    "p" = p,
    stringsAsFactors = FALSE
  )
}
