#' @title LOO-adjusted R2
#' @name r2_loo
#'
#' @description Compute LOO-adjusted R2.
#'
#' @param model A Bayesian regression model (from **brms**,
#'   **rstanarm**, **BayesFactor**, etc).
#' @param robust Logical, if `TRUE`, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#' @param ci Value or vector of probability of the CI (between 0 and 1) to be
#'   estimated.
#' @param ... Arguments passed to `r2_posterior()`.
#' @inheritParams model_performance.lm
#'
#' @return A list with the Bayesian R2 value. For mixed models, a list with the
#'   Bayesian R2 value and the marginal Bayesian R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @details `r2_loo()` returns an "adjusted" R2 value computed using a
#'   leave-one-out-adjusted posterior distribution. This is conceptually similar
#'   to an adjusted/unbiased R2 estimate in classical regression modeling. See
#'   [r2_bayes()] for an "unadjusted" R2.
#'
#'   Mixed models are not currently fully supported.
#'
#'   `r2_loo_posterior()` is the actual workhorse for `r2_loo()` and
#'   returns a posterior sample of LOO-adjusted Bayesian R2 values.
#'
#' @return A list with the LOO-adjusted R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @examplesIf require("rstanarm") && require("rstantools")
#' model <- suppressWarnings(rstanarm::stan_glm(
#'   mpg ~ wt + cyl,
#'   data = mtcars,
#'   chains = 1,
#'   iter = 500,
#'   refresh = 0,
#'   show_messages = FALSE
#' ))
#' r2_loo(model)
#' @export
r2_loo <- function(model, robust = TRUE, ci = 0.95, verbose = TRUE, ...) {
  loo_r2 <- r2_loo_posterior(model, verbose = verbose, ...)

  if (is.null(loo_r2)) {
    return(NULL)
  }

  loo_r2 <- structure(
    class = "r2_loo",
    lapply(loo_r2, ifelse(robust, stats::median, mean)),
    "SE" = lapply(loo_r2, ifelse(robust, stats::mad, stats::sd)),
    # "Estimates" = lapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
    "CI" = lapply(loo_r2, bayestestR::hdi, ci = ci),
    "robust" = robust
  )
  return(loo_r2)
}


#' @export
#' @rdname r2_loo
r2_loo_posterior <- function(model, ...) {
  UseMethod("r2_loo_posterior")
}

#' @export
#' @rdname r2_loo
r2_loo_posterior.brmsfit <- function(model, verbose = TRUE, ...) {
  insight::check_if_installed("rstantools")

  algorithm <- insight::find_algorithm(model)
  if (algorithm$algorithm != "sampling") {
    insight::format_warning("`r2()` only available for models fit using the \"sampling\" algorithm.")
    return(NA)
  }

  tryCatch(
    {
      mi <- insight::model_info(model)

      if (insight::is_multivariate(model)) {
        res <- insight::find_response(model)
        if (mi[[1]]$is_mixed) {
          br2_mv <- list(
            "R2_loo" = rstantools::loo_R2(
              model,
              re.form = NULL,
              re_formula = NULL,
              summary = FALSE
            ),
            "R2_loo_marginal" = rstantools::loo_R2(
              model,
              re.form = NA,
              re_formula = NA,
              summary = FALSE
            )
          )
          br2 <- lapply(seq_along(res), function(x) {
            list(
              "R2_loo" = unname(as.vector(br2_mv$R2_loo[, x])),
              "R2_loo_marginal" = unname(as.vector(br2_mv$R2_loo_marginal[, x]))
            )
          })
          names(br2) <- res
        } else {
          br2_mv <- list("R2_loo" = rstantools::loo_R2(model, summary = FALSE))
          br2 <- lapply(seq_along(res), function(x) {
            list("R2_loo" = unname(as.vector(br2_mv$R2_loo[, x])))
          })
          names(br2) <- res
        }
      } else {
        if (mi$is_mixed) {
          br2 <- list(
            "R2_loo" = as.vector(rstantools::loo_R2(
              model,
              re.form = NULL,
              re_formula = NULL,
              summary = FALSE
            )),
            "R2_loo_marginal" = as.vector(rstantools::loo_R2(
              model,
              re.form = NA,
              re_formula = NA,
              summary = FALSE
            ))
          )
          names(br2$R2_loo) <- rep("Conditional R2_adjusted", length(br2$R2_loo))
          names(br2$R2_loo_marginal) <- rep("Marginal R2_adjusted", length(br2$R2_loo))
        } else {
          br2 <- list("R2_loo" = as.vector(rstantools::loo_R2(model, summary = FALSE)))
          names(br2$R2_loo) <- rep("R2_adjusted", length(br2$R2_loo))
        }
      }

      br2
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )
}

#' @export
#' @rdname r2_loo
r2_loo_posterior.stanreg <- r2_loo_posterior.brmsfit

#' @export
r2_loo_posterior.stanmvreg <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_warning("Models of class `stanmvreg` not yet supported.")
  }
  NULL
}

#' @export
r2_loo_posterior.BFBayesFactor <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_warning("Models of class `BFBayesFactor` not yet supported.")
  }
  NULL
}



#' @export
as.data.frame.r2_loo <- function(x, ...) {
  out <- data.frame(
    R2 = x$R2_loo,
    SD = attributes(x)$SE$R2_loo,
    CI = attributes(x)$CI$R2_loo$CI,
    CI_low = attributes(x)$CI$R2_loo$CI_low,
    CI_high = attributes(x)$CI$R2_loo$CI_high,
    stringsAsFactors = FALSE
  )

  if (!is.null(x$R2_loo_marginal)) {
    out_marginal <- data.frame(
      R2 = x$R2_loo_marginal,
      SD = attributes(x)$SE$R2_loo_marginal,
      CI = attributes(x)$CI$R2_loo_marginal$CI,
      CI_low = attributes(x)$CI$R2_loo_marginal$CI_low,
      CI_high = attributes(x)$CI$R2_loo_marginal$CI_high,
      stringsAsFactors = FALSE
    )

    out$Component <- "conditional"
    out_marginal$Component <- "marginal"
    out <- rbind(out, out_marginal)
  }

  out
}
