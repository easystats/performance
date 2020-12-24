#' @title Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test (LRT) for model comparison, which tests which model is a better (more likely) explanation of the data. Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if not equivalent) to the commonly used F-test (obtained by default with \code{anova(...)}). Maximum likelihood tests make stronger assumptions than method of moments tests like the F-test, and in turn are more efficient.
#' \itemize{
#' \item For regression models, this is similar to \code{anova(..., test="LRT")} or \code{lmtest::lrtest(...)}, with one major difference: all models are tested against the same reference model (the first one), instead of testing each model to its previous neighbour.
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
#'                  speed   =~ x7 + x8 + x9 "
#'   m1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x2 + x3
#'                  textual =~ x4 + x6
#'                  speed   =~ x8 + x9 "
#'   m2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   performance_lrt(m1, m2)
#' }
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}



#' @param estimator Applied when comparing regression models. Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="OLS"} (default), then it uses the same method as \code{anova(..., test="LRT")} implemented in base R, i.e., scaling by n−k (the unbiased OLS estimator) and using this estimator under the alternative hypothesis. If \code{estimator="ML"}, which is for instance used by \code{lrtest(...)} in the \code{lmtest}, the scaling is done by n (the biased ML estimator) and the estimator under the null hypothesis. In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors.
#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.default <- function(..., estimator="OLS") {
  objects <- list(...)

  if (!all(sapply(objects, insight::is_model))) {
    stop("All objects must be valid regression model objects.")
  }

  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # LRT for model comparison
  if (length(objects) > 1) {
    out <- .performance_lrt_lm(objects, estimator=estimator)
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}



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

  object_names <- match.call(expand.dots = FALSE)$`...`

  # LRT for model comparison
  if (length(objects) > 1) {
    lrt <- lavaan::anova(..., test = "LRT")
    .performance_lrt(objects, object_names, lrt)
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }
}






# helper --------------------------


.performance_lrt <- function(objects, object_names, lrt) {
  # create data frame with info about model name and class
  names_types <- do.call(rbind, mapply(function(.x, .y) {
    data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE)
  }, objects, object_names, SIMPLIFY = FALSE))

  # bind all data
  out <- cbind(names_types, lrt)

  # Drop cols
  out <- out[!names(out) %in% c("AIC", "BIC", "Df diff", "Chisq diff")]  # For regressions

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Res.Df"] <- "df_error"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[names(out) == "Sum of Sq"] <- "Sum_Squares"
  colnames(out)[grepl("^Pr\\(>", names(out))] <- "p"
  rownames(out) <- NULL

  # Reorder columns
  cols <- colnames(out)
  if (all(c("df", "Chi2") %in% cols)) {
    cols[cols %in% c("df", "Chi2")] <- c("Chi2", "df")  # Move 'df' column after Chi2
  }
  if (all(c("df_error", "RSS", "df", "Sum_Squares") %in% cols)) {
    cols[cols %in% c("df_error", "RSS", "df", "Sum_Squares")] <- c("RSS", "Sum_Squares", "df", "df_error")  # 'df_error' after 'df'
  }
  out <- out[cols]

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
.performance_lrt_lm <- function(objects, estimator = "OLS", ref_model = 1) {

  # Get reference model
  ref <- objects[[ref_model]]

  out <- .performance_lrt_lm_pairwise(ref, ref, estimator = estimator)
  out$p <- out$Chi2 <- NA

  for(model in objects[-ref_model]){
    out <- rbind(out, .performance_lrt_lm_pairwise(model, ref = ref, estimator = estimator))
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
.performance_lrt_lm_pairwise <- function(model, ref, estimator="OLS") {
  # See https://stats.stackexchange.com/questions/155474/why-does-lrtest-not-match-anovatest-lrt

  # Get DF
  # TODO: Would be good to use parameters::dof if available in insight
  df_ref <- stats::df.residual(ref)
  df_model <- stats::df.residual(model)
  df_diff <- df_model - df_ref

  # LogLik Function
  loglik_function <- function(object, estimator) {
    sum(stats::dnorm(stats::model.response(stats::model.frame(object)), mean = stats::fitted(object), sd = estimator, log = TRUE))
  }

  # Get Statistic
  # TODO: Could use a possible insight::get_residuals()
  if (tolower(estimator) == "ols") {
    estim <- function(object) sqrt(sum(stats::residuals(object)^2) / stats::df.residual(object))
    loglik <- loglik_function(ref, estim(model))
    stat <- -2 * (loglik - loglik_function(model, estim(model)))
  } else{
    estim <- function(object) sqrt(mean(stats::residuals(object)^2))
    stat <- -2 * (loglik_function(ref, estim(ref)) - loglik_function(model, estim(model)))
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
