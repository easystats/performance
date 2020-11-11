#' @title Bayesian R2
#' @name r2_bayes
#'
#' @description Compute R2 for Bayesian models. For mixed models (including a random part),
#' it additionally computes the R2 related to the fixed effects only (marginal R2).
#' While \code{r2_bayes()} returns a single R2 value, \code{r2_posterior()} returns
#' a posterior sample of Bayesian R2 values.
#'
#' @param model A Bayesian regression model (from \code{brms}, \code{rstanarm},
#'   \code{BayesFactor}, etc).
#' @param robust Logical, if \code{TRUE}, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#' @param ci Value or vector of probability of the CI (between 0 and 1) to be estimated.
#' @param ... Arguments passed to \code{r2_posterior()}.
#' @inheritParams model_performance.lm
#'
#' @return A list with the Bayesian R2 value. For mixed models, a list with the
#'   Bayesian R2 value and the marginal Bayesian R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @details \code{r2_bayes()} returns an "unadjusted" R2 value. See \code{\link{r2_loo}}
#'   to calculate a LOO-adjusted R2, which comes conceptionally closer to an
#'   adjusted R2 measure.
#'   \cr \cr
#'   For mixed models, the conditional and marginal R2 are returned. The marginal
#'   R2 considers only the variance of the fixed effects, while the conditional
#'   R2 takes both the fixed and random effects into account.
#'   \cr \cr
#'   \code{r2_posterior()} is the actual workhorse for \code{r2_bayes()} and
#'   returns a posterior sample of Bayesian R2 values.
#'
#' @examples
#' library(performance)
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500, refresh = 0)
#'   r2_bayes(model)
#'
#'   model <- stan_lmer(
#'     Petal.Length ~ Petal.Width + (1 | Species),
#'     data = iris,
#'     chains = 1,
#'     iter = 500,
#'     refresh = 0
#'   )
#'   r2_bayes(model)
#' }
#'
#'
#' \dontrun{
#' if (require("BayesFactor")) {
#'   data(mtcars)
#'
#'   BFM <- generalTestBF(mpg ~ qsec + gear, data = mtcars, progress = FALSE)
#'   FM <- lm(mpg ~ qsec + gear, data = mtcars)
#'
#'   r2_bayes(FM)
#'   r2_bayes(BFM[3])
#'   r2_bayes(BFM, average = TRUE) # across all models
#'
#'
#'   # with random effects:
#'   mtcars$gear <- factor(mtcars$gear)
#'   model <- lmBF(
#'     mpg ~ hp + cyl + gear + gear:wt,
#'     mtcars,
#'     progress = FALSE,
#'     whichRandom = c("gear", "gear:wt")
#'   )
#'   r2_bayes(model)
#' }
#'
#' if (require("brms")) {
#'   model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#'   r2_bayes(model)
#'
#'   model <- brms::brm(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
#'   r2_bayes(model)
#' }
#' }
#' @references Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2018). R-squared for Bayesian regression models. The American Statistician, 1–6. \doi{10.1080/00031305.2018.1549100}
#'
#' @importFrom insight find_algorithm is_multivariate find_response model_info get_response find_predictors
#' @importFrom stats median mad sd
#' @importFrom bayestestR ci hdi point_estimate
#' @export
r2_bayes <- function(model, robust = TRUE, ci = .89, verbose = TRUE, ...) {
  r2_bayesian <- r2_posterior(model, verbose = verbose, ...)

  if (is.null(r2_bayesian)) {
    return(NULL)
  }

  if (insight::is_multivariate(model)) {
    structure(
      class = "r2_bayes_mv",
      rapply(r2_bayesian, ifelse(robust, stats::median, mean)),
      "SE" = rapply(r2_bayesian, ifelse(robust, stats::mad, stats::sd)),
      # "Estimates" = rapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      "CI" = rapply(r2_bayesian, bayestestR::hdi, ci = ci),
      "robust" = robust
    )
  } else {
    structure(
      class = "r2_bayes",
      lapply(r2_bayesian, ifelse(robust, stats::median, mean)),
      "SE" = lapply(r2_bayesian, ifelse(robust, stats::mad, stats::sd)),
      # "Estimates" = lapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      "CI" = lapply(r2_bayesian, bayestestR::hdi, ci = ci),
      "robust" = robust
    )
  }
}

#' @export
#' @rdname r2_bayes
r2_posterior <- function(model, ...){
  UseMethod("r2_posterior")
}

#' @export
#' @rdname r2_bayes
r2_posterior.brmsfit <- function(model, verbose = TRUE, ...) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("Package `rstantools` needed for this function to work. Please install it.")
  }

  algorithm <- insight::find_algorithm(model)
  if (algorithm$algorithm != "sampling") {
    warning("`r2()` only available for models fit using the 'sampling' algorithm.", call. = FALSE)
    return(NA)
  }

  tryCatch(
    {
      mi <- insight::model_info(model)

      if (insight::is_multivariate(model)) {
        res <- insight::find_response(model)
        if (mi[[1]]$is_mixed) {
          br2_mv <- list(
            "R2_Bayes" = rstantools::bayes_R2(model, re.form = NULL, re_formula = NULL, summary = FALSE),
            "R2_Bayes_marginal" = rstantools::bayes_R2(model, re.form = NA, re_formula = NA, summary = FALSE)
          )
          br2 <- lapply(1:length(res), function(x) {
            list(
              "R2_Bayes" = unname(as.vector(br2_mv$R2_Bayes[, x])),
              "R2_Bayes_marginal" = unname(as.vector(br2_mv$R2_Bayes_marginal[, x]))
            )
          })
          names(br2) <- res
        } else {
          br2_mv <- list("R2_Bayes" = rstantools::bayes_R2(model, summary = FALSE))
          br2 <- lapply(1:length(res), function(x) {
            list("R2_Bayes" = unname(as.vector(br2_mv$R2_Bayes[, x])))
          })
          names(br2) <- res
        }
      } else {
        if (mi$is_mixed) {
          br2 <- list(
            "R2_Bayes" = as.vector(rstantools::bayes_R2(model, re.form = NULL, re_formula = NULL, summary = FALSE)),
            "R2_Bayes_marginal" = as.vector(rstantools::bayes_R2(model, re.form = NA, re_formula = NA, summary = FALSE))
          )
          names(br2$R2_Bayes) <- "Conditional R2"
          names(br2$R2_Bayes_marginal) <- "Marginal R2"
        } else {
          br2 <- list("R2_Bayes" = as.vector(rstantools::bayes_R2(model, summary = FALSE)))
          names(br2$R2_Bayes) <- "R2"
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
#' @rdname r2_bayes
r2_posterior.stanreg <- r2_posterior.brmsfit

#' @export
r2_posterior.stanmvreg <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    warning("Models of class 'stanmvreg' not yet supported.", call. = FALSE)
  }
  NULL
}


#' @param average Compute model-averaged index? See
#'   \code{\link[bayestestR:weighted_posteriors]{bayestestR::weighted_posteriors()}}.
#' @inheritParams bayestestR::weighted_posteriors
#' @importFrom insight get_parameters get_response find_predictors
#' @importFrom stats median mad sd
#' @importFrom bayestestR point_estimate hdi
#' @importFrom utils packageVersion
#' @export
#' @rdname r2_bayes
r2_posterior.BFBayesFactor <- function(model, average = FALSE, prior_odds = NULL, ...){
  mi <- insight::model_info(model)
  if (!mi$is_linear || mi$is_correlation || mi$is_ttest || mi$is_binomial || mi$is_meta) {
    warning("Can produce R2 only for linear models.", call. = FALSE)
    return(NULL)
  }

  if (average) {
    return(.r2_posterior_model_average(model, prior_odds = prior_odds))
  }

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("Package `rstantools` needed for this function to work. Please install it.")
  }

  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("Package `BayesFactor` needed for this function to work. Please install it.")
  }

  # Estimates
  params <- insight::get_parameters(model, unreduce = FALSE)
  # remove sig and g cols
  params <- params[, !grepl(pattern = "^sig2$|^g_|^g$", colnames(params))]

  # Model Matrix
  mm <- BayesFactor::model.matrix(model[1])
  colnames(mm)[1] <- "mu"

  # match?
  if ((length(colnames(params)) != length(colnames(mm))) ||
      !all(colnames(params) == colnames(mm))) {
    if (utils::packageVersion("BayesFactor") < package_version("0.9.12.4.3")) {
      stop("R2 for BayesFactor models with random effects requires BayesFactor v0.9.12.4.3 or higher.", call. = FALSE)
    }
    stop("Woops, you seem to have stumbled on some weird edge case. Please file an issue at https://github.com/easystats/performance/issues", call. = FALSE)
  }

  # Compute R2!
  y <- insight::get_response(model)
  yy <- as.matrix(params) %*% t(mm)
  r2s <- rstantools::bayes_R2(yy, y = y)
  r2_bayesian <- data.frame(R2_Bayes = r2s)

  rand <- insight::find_predictors(model[1], effects = "random", flatten = TRUE)
  if (!is.null(rand)) {
    idx <- sapply(paste0("\\b", rand, "\\b"), grepl, x = colnames(params))
    idx <- apply(idx, 1, any)
    params[idx] <- 0

    yy <- as.matrix(params) %*% t(mm)
    r2s_marginal <- rstantools::bayes_R2(yy, y = y)
    r2_bayesian$R2_Bayes_marginal <- r2s_marginal
  }

  r2_bayesian
}



#' @importFrom bayestestR weighted_posteriors
#' @keywords internal
.r2_posterior_model_average <- function(model, prior_odds = NULL) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("Package `BayesFactor` needed for this function to work. Please install it.")
  }

  BFMods <- bayestestR::bayesfactor_models(model, verbose = FALSE)
  has_random <- !is.null(insight::find_predictors(model, effects = "random", flatten = TRUE))

  # extract parameters
  intercept_only <- which(BFMods$Model == "1")
  params <- vector(mode = "list", length = nrow(BFMods))
  for (m in seq_along(params)) {
    if (length(intercept_only) && m == intercept_only) {
      params[[m]] <- data.frame(R2_Bayes = rep(0, 4000))
    } else if (m == 1) {
      # If the model is the "den" model
      params[[m]] <- suppressMessages(r2_posterior(1 / model[1]))
    } else {
      params[[m]] <- suppressMessages(r2_posterior(model[m - 1]))
    }

    # when there is no random effect, marginal = conditional
    if (has_random && is.null(params[[m]]$R2_Bayes_marginal)) {
      params[[m]]$R2_Bayes_marginal <- params[[m]]$R2_Bayes
    }
  }


  # Compute posterior model probabilities
  if (!is.null(prior_odds)) {
    prior_odds <- c(1, prior_odds)
  } else {
    prior_odds <- rep(1, nrow(BFMods))
  }
  posterior_odds <- prior_odds * BFMods$BF
  posterior_odds <- posterior_odds[-1] / posterior_odds[1]

  do.call(bayestestR::weighted_posteriors,
          c(params, list(missing = 0, prior_odds = posterior_odds)))
}



#' @export
as.data.frame.r2_bayes <- function(x, ...) {
  out <- data.frame(
    R2 = x$R2_Bayes,
    SD = attributes(x)$SE$R2_Bayes,
    CI = attributes(x)$CI$R2_Bayes$CI,
    CI_low = attributes(x)$CI$R2_Bayes$CI_low,
    CI_high = attributes(x)$CI$R2_Bayes$CI_high,
    stringsAsFactors = FALSE
  )
  if (!is.null(x$R2_Bayes_marginal)) {
    out_marginal <- data.frame(
      R2 = x$R2_Bayes_marginal,
      SD = attributes(x)$SE$R2_Bayes_marginal,
      CI = attributes(x)$CI$R2_Bayes_marginal$CI,
      CI_low = attributes(x)$CI$R2_Bayes_marginal$CI_low,
      CI_high = attributes(x)$CI$R2_Bayes_marginal$CI_high,
      stringsAsFactors = FALSE
    )
    out$Component <- "conditional"
    out_marginal$Component <- "marginal"
    out <- rbind(out, out_marginal)
  }
  out
}
