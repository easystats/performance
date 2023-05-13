#' @title Check model for (non-)constant error variance
#' @name check_heteroscedasticity
#'
#' @description Significance testing for linear regression models assumes that
#'   the model errors (or residuals) have constant variance. If this assumption
#'   is violated the p-values from the model are no longer reliable.
#'
#' @param x A model object.
#' @param ... Currently not used.
#'
#' @return The p-value of the test statistics. A p-value < 0.05 indicates a
#'   non-constant variance (heteroskedasticity).
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/performance.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details This test of the hypothesis of (non-)constant error is also called
#'   *Breusch-Pagan test* (\cite{1979}).
#'
#' @references Breusch, T. S., and Pagan, A. R. (1979) A simple test for heteroscedasticity and random coefficient variation. Econometrica 47, 1287-1294.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examples
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_heteroscedasticity(m)
#'
#' # plot results
#' if (require("see")) {
#'   x <- check_heteroscedasticity(m)
#'   plot(x)
#' }
#' @export
check_heteroscedasticity <- function(x, ...) {
  UseMethod("check_heteroscedasticity")
}

#' @name check_heteroscedasticity
#' @aliases check_heteroscedasticity
#' @export
check_heteroskedasticity <- check_heteroscedasticity


# default ---------------------

#' @export
check_heteroscedasticity.default <- function(x, ...) {
  # check for valid input
  .is_model_valid(x)

  # only for linear models
  info <- insight::model_info(x)
  if (!info$is_linear) {
    msg <- "This Breusch-Pagan Test currently only works Gaussian models."
    if (info$is_count) {
      paste0(msg, " You may check your model for overdispersion or zero-inflation instead (see 'check_overdispersion()' and 'check_zeroinflation()').")
    }
    insight::format_alert(msg)
    return(NULL)
  }

  r <- insight::get_residuals(x, type = "pearson")
  S.sq <- insight::get_df(x, type = "residual") * .sigma(x)^2 / sum(!is.na(r))

  .U <- (r^2) / S.sq
  mod <- stats::lm(.U ~ stats::fitted(x))

  SS <- stats::anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2

  p.val <- stats::pchisq(Chisq, df = 1, lower.tail = FALSE)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(p.val) <- unique(c("check_heteroscedasticity", "see_check_heteroscedasticity", class(p.val)))

  p.val
}



# methods -----------------------

#' @export
print.check_heteroscedasticity <- function(x, ...) {
  if (x < 0.05) {
    insight::print_color(sprintf("Warning: Heteroscedasticity (non-constant error variance) detected (%s).\n", insight::format_p(x)), "red")
  } else {
    insight::print_color(sprintf("OK: Error variance appears to be homoscedastic (%s).\n", insight::format_p(x)), "green")
  }
  invisible(x)
}


#' @export
plot.check_heteroscedasticity <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}



# utilities -----------------------

.sigma <- function(x) {
  UseMethod(".sigma")
}

.sigma.default <- function(x) {
  s <- tryCatch(
    {
      estimates <- insight::get_parameters(x)$Estimate
      sqrt(insight::get_deviance(x) / (insight::n_obs(x) - sum(!is.na(estimates))))
    },
    error = function(e) {
      NULL
    }
  )

  if (insight::is_empty_object(s)) {
    s <- insight::get_variance_residual(x, verbose = FALSE)
  }

  s
}

.sigma.BFBayesFactor <- function(x) {
  mean(.get_bfbf_predictions(x)[["sigma"]])
}
