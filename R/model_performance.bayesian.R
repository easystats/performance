#' Performance of Bayesian Models
#'
#' Compute indices of model performance for (general) linear models.
#'
#' @param model Object of class `stanreg` or `brmsfit`.
#' @param metrics Can be `"all"`, `"common"` or a character vector of
#'   metrics to be computed (some of `c("LOOIC", "WAIC", "R2", "R2_adj",
#'   "RMSE", "SIGMA", "LOGLOSS", "SCORE")`). `"common"` will compute LOOIC,
#'   WAIC, R2 and RMSE.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_performance.lm
#'
#' @return A data frame (with one row) and one column per "index" (see
#'   `metrics`).
#'
#' @details Depending on `model`, the following indices are computed:
#'
#'   - **ELPD**: expected log predictive density. Larger ELPD values
#'   mean better fit. See [looic()].
#'
#'   - **LOOIC**: leave-one-out cross-validation (LOO) information
#'   criterion. Lower LOOIC values mean better fit. See [looic()].
#'
#'   - **WAIC**: widely applicable information criterion. Lower WAIC
#'   values mean better fit. See `?loo::waic`.
#'
#'   - **R2**: r-squared value, see [r2_bayes()].
#'
#'   - **R2_adjusted**: LOO-adjusted r-squared, see [r2_loo()].
#'
#'   - **RMSE**: root mean squared error, see [performance_rmse()].
#'
#'   - **SIGMA**: residual standard deviation, see [insight::get_sigma()].
#'
#'   - **LOGLOSS**: Log-loss, see [performance_logloss()].
#'
#'   - **SCORE_LOG**: score of logarithmic proper scoring rule, see [performance_score()].
#'
#'   - **SCORE_SPHERICAL**: score of spherical proper scoring rule, see [performance_score()].
#'
#'   - **PCP**: percentage of correct predictions, see [performance_pcp()].
#'
#' @examples
#' \dontrun{
#' if (require("rstanarm") && require("rstantools")) {
#'   model <- stan_glm(mpg ~ wt + cyl, data = mtcars, chains = 1, iter = 500, refresh = 0)
#'   model_performance(model)
#'
#'   model <- stan_glmer(
#'     mpg ~ wt + cyl + (1 | gear),
#'     data = mtcars,
#'     chains = 1,
#'     iter = 500,
#'     refresh = 0
#'   )
#'   model_performance(model)
#' }
#'
#' if (require("BayesFactor") && require("rstantools")) {
#'   model <- generalTestBF(carb ~ am + mpg, mtcars)
#'
#'   model_performance(model)
#'   model_performance(model[3])
#'
#'   model_performance(model, average = TRUE)
#' }
#' }
#' @seealso [r2_bayes]
#' @references Gelman, A., Goodrich, B., Gabry, J., and Vehtari, A. (2018).
#'   R-squared for Bayesian regression models. The American Statistician, The
#'   American Statistician, 1-6.
#'
#' @export
model_performance.stanreg <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  all_metrics <- c("LOOIC", "WAIC", "R2", "R2_adjusted", "ICC", "RMSE", "SIGMA", "LOGLOSS", "SCORE")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("LOOIC", "WAIC", "R2", "RMSE")
  }

  # check for valid input
  metrics <- toupper(.check_bad_metrics(metrics, all_metrics, verbose))

  algorithm <- insight::find_algorithm(model)

  if (algorithm$algorithm != "sampling") {
    if (verbose) {
      insight::format_warning(
        "`model_performance()` only possible for models fit using the \"sampling\" algorithm."
      )
    }
    return(NULL)
  }

  insight::check_if_installed("loo")

  mi <- insight::model_info(model)

  out <- list()
  attri <- list()

  if (insight::is_multivariate(model)) {
    out$Response <- insight::find_response(model, combine = FALSE)
    mi <- mi[[1]]
  }

  # LOOIC ------------------
  if ("LOOIC" %in% metrics) {
    loo_res <- suppressWarnings(looic(model, verbose = verbose))
    out <- append(out, loo_res)
    attri$loo <- attributes(loo_res)$loo # save attributes
  }

  # WAIC ------------------
  if ("WAIC" %in% metrics) {
    out$WAIC <- suppressWarnings(loo::waic(model)$estimates["waic", "Estimate"])
  }

  # R2 ------------------
  attri_r2 <- list()
  if ("R2" %in% metrics) {
    r2 <- r2_bayes(model, verbose = verbose)
    if (!is.null(r2)) {
      # save attributes
      attri_r2$SE$R2_Bayes <- attributes(r2)$SE$R2_Bayes
      attri_r2$CI$R2_Bayes <- attributes(r2)$CI$R2_Bayes
      attri_r2$CI$R2_Bayes_marginal <- attributes(r2)$CI$R2_Bayes_marginal
      attri_r2$robust$R2_Bayes <- attributes(r2)$robust

      # Format to df then to list
      r2_df <- as.data.frame(t(as.numeric(r2)))
      names(r2_df) <- gsub("_Bayes", "", names(r2), fixed = TRUE)
      out <- append(out, as.list(r2_df))
    }
  }

  # LOO-R2 ------------------
  if (("R2_ADJUSTED" %in% metrics || "R2_LOO" %in% metrics) && mi$is_linear) {
    r2_adj <- .safe(suppressWarnings(r2_loo(model, verbose = verbose)))
    if (!is.null(r2_adj)) {
      # save attributes
      attri_r2$SE$R2_loo <- attributes(r2_adj)$SE$R2_loo
      attri_r2$CI$R2_loo <- attributes(r2_adj)$CI$R2_loo
      attri_r2$CI$R2_loo_marginal <- attributes(r2)$CI$R2_loo_marginal
      attri_r2$robust$R2_loo <- attributes(r2_adj)$robust

      # Format to df then to list
      r2_adj_df <- as.data.frame(t(as.numeric(r2_adj)))
      names(r2_adj_df) <- gsub("_loo", "_adjusted", names(r2_adj), fixed = TRUE)
      out <- append(out, as.list(r2_adj_df))
    }
  }

  if (length(attri_r2) > 0L) {
    attri$r2 <- attri_r2
    attri$r2_bayes <- attri_r2
  }

  # ICC ------------------
  if ("ICC" %in% metrics) {
    out$ICC <- .safe(suppressWarnings(icc(model, verbose = verbose)$ICC_adjusted))
  }

  # RMSE ------------------
  if ("RMSE" %in% metrics && !mi$is_ordinal && !mi$is_multinomial && !mi$is_categorical) {
    out$RMSE <- performance_rmse(model, verbose = verbose)
  }

  # SIGMA ------------------
  if ("SIGMA" %in% metrics) {
    out$Sigma <- .safe({
      s <- .get_sigma(model, verbose = verbose)
      if (insight::is_empty_object(s)) {
        s <- NULL
      }
      s
    })
  }

  # LOGLOSS ------------------
  if (("LOGLOSS" %in% metrics) && mi$is_binomial) {
    out$Log_loss <- .safe({
      .logloss <- performance_logloss(model, verbose = verbose)
      if (!is.na(.logloss)) {
        .logloss
      } else {
        NULL
      }
    })
  }

  # SCORE ------------------
  if (("SCORE" %in% metrics) && (mi$is_binomial || mi$is_count)) {
    .scoring_rules <- .safe(performance_score(model, verbose = verbose))
    if (!is.null(.scoring_rules)) {
      if (!is.na(.scoring_rules$logarithmic)) out$Score_log <- .scoring_rules$logarithmic
      if (!is.na(.scoring_rules$spherical)) out$Score_spherical <- .scoring_rules$spherical
    }
  }

  out <- as.data.frame(out)
  row.names(out) <- NULL
  out <- out[vapply(out, function(i) !all(is.na(i)), TRUE)]

  attributes(out) <- c(attributes(out), attri)
  class(out) <- c("performance_model", class(out))

  out
}


#' @export
model_performance.brmsfit <- model_performance.stanreg

#' @export
model_performance.stanmvreg <- model_performance.stanreg


#' @export
#' @inheritParams r2_bayes
#' @rdname model_performance.stanreg
model_performance.BFBayesFactor <- function(model,
                                            metrics = "all",
                                            verbose = TRUE,
                                            average = FALSE,
                                            prior_odds = NULL,
                                            ...) {
  all_metrics <- c("R2", "SIGMA")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  }

  # check for valid input
  metrics <- toupper(.check_bad_metrics(metrics, all_metrics, verbose))

  # check for valid BFBayesFactor object
  mi <- insight::model_info(model, verbose = FALSE)
  if (!mi$is_linear || mi$is_correlation || mi$is_ttest || mi$is_binomial || mi$is_meta) {
    if (isTRUE(verbose)) {
      insight::format_warning("This type of Bayes factor models is not supported.")
    }
    return(NULL)
  }

  out <- list()
  attri <- list()

  if ("R2" %in% c(metrics)) {
    r2 <- r2_bayes(model, average = average, prior_odds = prior_odds, verbose = verbose)
    attri$r2_bayes <- attributes(r2) # save attributes

    # Format to df then to list
    r2_df <- as.data.frame(t(as.numeric(r2)))
    names(r2_df) <- gsub("_Bayes", "", names(r2), fixed = TRUE)
    out <- append(out, as.list(r2_df))
  }


  if ("SIGMA" %in% toupper(metrics)) {
    sig <- suppressMessages(
      .get_sigma_bfbayesfactor(
        model,
        average = average,
        prior_odds = prior_odds,
        verbose = verbose
      )
    )
    out$Sigma <- bayestestR::point_estimate(sig, "median")[[1]]
  }


  out <- as.data.frame(out)
  row.names(out) <- NULL

  attributes(out) <- c(attributes(out), attri)
  class(out) <- c("performance_model", class(out))

  out
}



# helper -------------------


.get_sigma_bfbayesfactor <- function(model, average = FALSE, prior_odds = NULL, verbose = TRUE) {
  if (average) {
    return(.get_sigma_bfbayesfactor_model_average(model, prior_odds = prior_odds))
  }

  params <- insight::get_parameters(model, verbose = verbose)
  if (!"sig2" %in% colnames(params)) {
    insight::format_error("This is not a linear model.")
  }
  sqrt(params$sig2)
}


.get_sigma_bfbayesfactor_model_average <- function(model, prior_odds = NULL) {
  insight::check_if_installed("BayesFactor")

  BFMods <- bayestestR::bayesfactor_models(model, verbose = FALSE)

  if (!is.null(BFMods$log_BF)) {
    BFMods$BF <- exp(BFMods$log_BF)
  }

  # extract parameters
  intercept_only <- which(BFMods$Model == "1")
  params <- vector(mode = "list", length = nrow(BFMods))
  for (m in seq_along(params)) {
    if (length(intercept_only) && m == intercept_only) {
      y <- insight::get_response(model)
      params[[m]] <- rep(stats::sd(y), 4000)
    } else if (m == 1) {
      # If the model is the "den" model
      params[[m]] <- suppressMessages(.get_sigma_bfbayesfactor(1 / model[1]))
    } else {
      params[[m]] <- suppressMessages(.get_sigma_bfbayesfactor(model[m - 1]))
    }
  }

  params <- lapply(params, data.frame)


  # Compute posterior model probabilities
  if (!is.null(prior_odds)) {
    prior_odds <- c(1, prior_odds)
  } else {
    prior_odds <- rep(1, nrow(BFMods))
  }

  posterior_odds <- prior_odds * BFMods$BF
  posterior_odds <- posterior_odds[-1] / posterior_odds[1]

  do.call(
    bayestestR::weighted_posteriors,
    c(params, list(missing = 0, prior_odds = posterior_odds))
  )[[1]]
}
