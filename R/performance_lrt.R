#' @title Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test (LRT) for model comparison, which tests which model is a better (more likely) explanation of the data. Likelihood-Ratio-Test (LRT) gives usually somewhat close results (if not equivalent) to the commonly used F-test (obtained by default with \code{anova(...)}). Maximum likelihood tests make stronger assumptions than method of moments tests like the F-test, and in turn are more efficient.
#' \itemize{
#' \item For regression models, this is similar to \code{anova(..., test="LRT")} or \code{lmtest::lrtest(...)}, depending on the estimator.
#' \item For \code{lavaan} models (SEM, CFA), the function calls \code{lavaan::lavTestLRT()}.
#' }
#'
#' @param estimator Applied when comparing regression models. Corresponds to the different estimators for the standard deviation of the errors. If \code{estimator="OLS"} (default), then it uses the same method as \code{anova(..., test="LRT")} implemented in base R, i.e., scaling by n-k (the unbiased OLS estimator) and using this estimator under the alternative hypothesis. If \code{estimator="ML"}, which is for instance used by \code{lrtest(...)} in package \pkg{lmtest}, the scaling is done by n (the biased ML estimator) and the estimator under the null hypothesis. In moderately large samples, the differences should be negligible, but it is possible that OLS would perform slightly better in small samples with Gaussian errors.
#' @param ... Multiple model objects.
#'
#' @return A data frame, based on the results from \code{anova()}.
#'
#' @details This only makes statistical sense if the models are nested, in which case the models must be ordered from largest (the encompassing model) to smallest.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' # Regression Models
#' m1 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' m3 <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' performance_lrt(m1, m2, m3, estimator = "ML") # Equivalent to lmtest::lrtest(m1, m2, m3)
#' performance_lrt(m1, m2, m3, estimator = "OLS") # Equivalent to anova(m1, m2, m3, test='LRT')
#'
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
#'   performance_lrt(m1, m2, m3)
#' }
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}



#' @rdname performance_lrt
#' @importFrom insight ellipsis_info
#' @export
performance_lrt.default <- function(..., estimator = "ML") {

  # Attribute class to list
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  if (length(objects) == 1) {
    stop("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }

  # Replace with names from the global environment
  object_names <- match.call(expand.dots = FALSE)$`...`
  names(objects) <- object_names

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, "ListNestedRegressions")) {
    performance_lrt(objects, estimator = estimator)
  } else if (inherits(objects, "ListLavaan")) {
    performance_lrt_ListLavaan(..., objects = objects) # Because lavaanLRT requires the ellipsis
  } else {
    stop("The models are not nested models, which is a prerequisite for `performance_lrt()`. See the details section.")
  }
}







#' @importFrom insight is_model get_df get_loglikelihood
#' @importFrom stats pchisq
#' @export
performance_lrt.ListNestedRegressions <- function(objects, estimator = "ML", ...) {
  dfs <- sapply(objects, insight::get_df, type = "model")
  dfs_diff <- c(NA, diff(sapply(objects, insight::get_df, type = "model")))

  # lmtest::lrtest()
  if (tolower(estimator) %in% c("ml", "mle")) {
    lls <- sapply(objects, insight::get_loglikelihood)
    chi2 <- abs(c(NA, -2 * diff(lls)))
    p <- stats::pchisq(chi2, abs(dfs_diff), lower.tail = FALSE)

    out <- data.frame(
      Model = names(objects),
      df = dfs,
      df_diff = dfs_diff,
      Chi2 = chi2,
      p = p
    )

    # anova(..., test="LRT")
  } else {
    out <- .test_model_wald(objects, test = "LRT")
    out$df <- dfs # Replace residual df with model's df
  }

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}




#' @importFrom insight is_model
performance_lrt_ListLavaan <- function(..., objects = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required. Please install it.")
  }

  # Create data frame with info about model name and class
  names_types <- data.frame(Model = names(objects), Type = sapply(objects, function(x) class(x)[1]))

  out <- as.data.frame(lavaan::lavTestLRT(..., test = "LRT", model.names = names(objects)))

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Df diff"] <- "df_diff"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[grepl("^Pr\\(>", names(out))] <- "p"
  out$Model <- row.names(out)

  # Bind all data
  out <- merge(names_types, out[c("Model", "df", "df_diff", "Chi2", "p")], by = "Model")

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}









# Helpers --------------------------




#' @importFrom insight get_df
#' @importFrom stats pchisq pf deviance
.test_model_wald <- function(objects, test = "F") {

  # Compute stuff
  dfs <- sapply(objects, insight::get_df, type = "residual")
  dfs_diff <- c(NA, diff(sapply(objects, insight::get_df, type = "model")))
  dev <- as.numeric(lapply(objects, stats::deviance))
  dev_diff <- c(NA, -diff(dev))

  out <- data.frame(
    Model = names(objects),
    df = dfs,
    df_diff = dfs_diff
  )


  # Find reference-model related stuff
  refmodel <- order(dfs)[1]
  scale <- dev[refmodel] / dfs[refmodel]

  # test = "F"
  if (test == "F") {
    f_value <- (dev_diff / dfs_diff) / scale
    f_value[!is.na(f_value) & f_value < 0] <- NA # rather than p = 0
    out$`F` <- f_value
    p <- stats::pf(f_value, abs(dfs_diff), dfs[refmodel], lower.tail = FALSE)

    # test = "LRT"
  } else {
    chi2 <- dev_diff / scale * sign(dfs_diff)
    chi2[!is.na(chi2) & chi2 < 0] <- NA # rather than p = 0
    out$Chi2 <- chi2
    p <- stats::pchisq(chi2, abs(dfs_diff), lower.tail = FALSE)
  }
  out$p <- p

  row.names(out) <- NULL
  out
}
