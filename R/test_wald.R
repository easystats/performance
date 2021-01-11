#' @title Test if Models are Different using Wald Test
#'
#' @description The Wald test (also called the Wald Chi-Squared Test) is a rough approximation of the Likelihood Ratio Test (see \code{test_lrt()}). That said, it is more applicable than the LRT: you can often run a Wald test in situations where no other test can be run. Agresti (1990) suggests that you should use the LRT instead of the Wald test for small sample sizes or if the parameters are large. A "small" sample size is under about 30.
#'
#' @inheritParams test_performance
#'
#' @return A data frame.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#'
#' @examples
#' # Nested Models
#' m1 <- lm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' m2 <- lm(Sepal.Length ~ Petal.Width + Species, data = iris)
#' m3 <- lm(Sepal.Length ~ Petal.Width, data = iris)
#'
#' test_wald(m1, m2, m3)  # equivalent to anova(m1, m2, m3)
#' @export
test_wald <- function(...) {
  UseMethod("test_wald")
}


#' @importFrom insight ellipsis_info
#' @export
test_wald.default <- function(..., reference = 1) {

  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)
  names(objects) <- match.call(expand.dots = FALSE)$`...`

  # Sanity checks
  if (attributes(objects)$same_response == FALSE) {
    stop("The models don't have the same response variable, which is a prerequisite to compare them.")
  }

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_wald(objects, reference = reference)
  } else {
    stop("The models cannot be compared for some reason :/")
  }
}



#' @export
test_wald.ListNestedRegressions <- function(objects, ...) {
  out <- .test_wald(objects, test = "F")
  out$Model <- NULL
  out <- cbind(.test_performance_init(objects), out)
  out
}


#' @export
test_wald.ListNonNestedRegressions <- function(objects, ...) {
  stop("Wald tests cannot be run on non-nested models. Try `test_vuong()`.")
}

# Helpers --------------------------




#' @importFrom insight get_df
#' @importFrom stats pchisq pf deviance
.test_wald <- function(objects, test = "F") {

  # Compute stuff
  dfs <- sapply(objects, insight::get_df, type = "residual")
  dfs_diff <- c(NA, diff(sapply(objects, insight::get_df, type = "model")))
  dev <- as.numeric(lapply(objects, insight::get_deviance))
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
