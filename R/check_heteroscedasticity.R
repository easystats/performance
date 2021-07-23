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
#' @return Invisibly returns the p-value of the test statistics. A p-value <
#'   0.05 indicates a non-constant variance (heteroskedasticity).
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/performance.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details This test of the hypothesis of (non-)constant error is also called
#'   *Breusch-Pagan test* (\cite{1979}).
#'
#' @references Breusch, T. S., and Pagan, A. R. (1979) A simple test for heteroscedasticity and random coefficient variation. Econometrica 47, 1287–1294.
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


#' @export
check_heteroscedasticity.default <- function(x, ...) {
  # only for linear models
  info <- insight::model_info(x)
  if (!info$is_linear) {
    msg <- "This Breusch-Pagan Test currently only works Gaussian models."
    if (info$is_count) {
      paste0(msg, " You may check your model for overdispersion or zero-inflation instead (see 'check_overdispersion()' and 'check_zeroinflation()').")
    }
    message(insight::format_message(msg))
    return(NULL)
  }

  r <- .pearson_residuals(x)
  S.sq <- insight::get_df(x, type = "residual") * .sigma(x)^2 / sum(!is.na(r))

  .U <- (r^2) / S.sq
  mod <- stats::lm(.U ~ stats::fitted(x))

  SS <- stats::anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2

  p.val <- stats::pchisq(Chisq, df = 1, lower.tail = FALSE)

  if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Heteroscedasticity (non-constant error variance) detected (%s).\n", insight::format_p(p.val)), "red")
  } else {
    insight::print_color(sprintf("OK: Error variance appears to be homoscedastic (%s).\n", insight::format_p(p.val)), "green")
  }

  attr(p.val, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  class(p.val) <- unique(c("check_heteroscedasticity", "see_check_heteroscedasticity", class(p.val)))

  invisible(p.val)
}


.sigma <- function(x) {
  s <- tryCatch(
    {
      estimates <- insight::get_parameters(x)$Estimate
      sqrt(insight::get_deviance(x) / (insight::n_obs(x) - sum(!is.na(estimates))))
    },
    error = function(e) {
      NULL
    }
  )

  if (.is_empty_object(s)) {
    s <- insight::get_variance_residual(x, verbose = FALSE)
  }

  s
}



.pearson_residuals <- function(x) {
  pr <- tryCatch(
    {
      stats::residuals(x, type = "pearson")
    },
    error = function(e) {
      NULL
    }
  )

  if (.is_empty_object(pr) && inherits(x, c("glmmTMB", "MixMod"))) {
    faminfo <- insight::model_info(x)
    if (faminfo$is_zero_inflated) {
      if (faminfo$is_negbin) {
        pr <- .resid_zinb(x, faminfo)
      } else {
        pr <- .resid_zip(x, faminfo)
      }
    }
  }

  pr
}




.resid_zinb <- function(model, faminfo) {
  if (inherits(model, "glmmTMB")) {
    v <- stats::family(model)$variance
    # zi probability
    p <- stats::predict(model, type = "zprob")
    # mean of conditional distribution
    mu <- stats::predict(model, type = "conditional")
    # sigma
    betad <- model$fit$par["betad"]
    k <- switch(faminfo$family,
      gaussian = exp(0.5 * betad),
      Gamma = exp(-0.5 * betad),
      exp(betad)
    )
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
    pred <- stats::predict(model, type = "response") ## (1 - p) * mu
  } else if (inherits(model, "MixMod")) {
    sig <- insight::get_variance_distribution(model, verbose = FALSE)
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- stats::predict(model, type_pred = "link", type = "mean_subject")
    v <- mu * (1 + sig)
    k <- sig
    pvar <- (1 - p) * v(mu, k) + mu^2 * (p^2 + p)
    pred <- stats::predict(model, type_pred = "response", type = "mean_subject")
  } else {
    sig <- insight::get_variance_distribution(model, verbose = FALSE)
    pvar <- mu * (1 + sig)
    pred <- stats::predict(model, type = "response")
  }

  # pearson residuals
  (insight::get_response(model, verbose = FALSE) - pred) / sqrt(pvar)
}




.resid_zip <- function(model, faminfo) {
  if (inherits(model, "glmmTMB")) {
    p <- stats::predict(model, type = "zprob")
    mu <- stats::predict(model, type = "conditional")
    pvar <- (1 - p) * (mu + p * mu^2)
    pred <- stats::predict(model, type = "response") ## (1 - p) * mu
  } else if (inherits(model, "MixMod")) {
    p <- stats::plogis(stats::predict(model, type_pred = "link", type = "zero_part"))
    mu <- stats::predict(model, type = "mean_subject")
    pvar <- (1 - p) * (mu + p * mu^2)
    pred <- stats::predict(model, type_pred = "response", type = "mean_subject")
  } else {
    sig <- insight::get_variance_distribution(model, verbose = FALSE)
    pvar <- mu * (1 + sig)
    pred <- stats::predict(model, type = "response")
  }

  # pearson residuals
  (insight::get_response(model) - pred) / sqrt(pvar)
}
