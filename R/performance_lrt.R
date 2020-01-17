#' @title Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test for model comparison.
#'
#' @param ... Multiple model objects, which should respond to \code{anova()}.
#'
#' @return A data frame, based on the results from \code{anova()}.
#'
#' @details This only makes statistical sense if the models are nested. It is conventional to list the models from smallest to largest, but this is up to the user. The ouput shows the tests of the models against one another in the order specified.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' performance_lrt(m1, m2, m3)
#'
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}


#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.default <- function(...) {
  if (!all(sapply(list(...), insight::is_model))) {
    stop("All objects must be valid regression model objects!")
  }

  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  # LRT for model comparison
  if (length(list(...)) > 1) {
    # sd_mle <- function(model) sqrt(mean(residuals(model)^2))
    # ll <- function(model, sd) {
    #   sum(dnorm(insight::get_response(model), mean = fitted(model), sd = sd, log = TRUE))
    # }
    # -2 * (ll(m2, sd_mle(m2)) - ll(m3, sd_mle(m3)))
    lrt <- stats::anova(..., test = "LRT")

    # create data frame with info about model name and class
    m <- mapply(function(.x, .y) {
      data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE)
    }, objects, object_names, SIMPLIFY = FALSE)

    # bind all data
    out <- cbind(do.call(rbind, m), lrt)

    # preserve only some columns
    out <- out[, intersect(colnames(out), c("Model", "Type", "Df", "AIC", "BIC", "Chisq", "Pr(>Chisq)"))]
    colnames(out)[names(out) == "Chisq"] <- "Statistic"
    colnames(out)[names(out) == "Pr(>Chisq)"] <- "p"
    rownames(out) <- NULL

    class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
    out
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }
}
