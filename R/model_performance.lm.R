#' Performance of Regression Models
#'
#' Compute indices of model performance for regression models.
#'
#' @param model A model.
#' @param metrics Can be \code{"all"}, \code{"common"} or a character vector of metrics to be computed (some of \code{c("AIC", "BIC", "R2", "RMSE", "LOGLOSS", "PCP", "SCORE")}). \code{"common"} will compute AIC, BIC, R2 and RMSE.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame (with one row) and one column per "index" (see \code{metrics}).
#'
#' @details Depending on \code{model}, following indices are computed:
#' \itemize{
#'   \item{\strong{AIC}} {Akaike's Information Criterion, see \code{?stats::AIC}}
#'   \item{\strong{BIC}} {Bayesian Information Criterion, see \code{\link[stats:BIC]{?stat::BIC}}}
#'   \item{\strong{R2}} {r-squared value, see \code{\link{r2}}}
#'   \item{\strong{R2_adj}} {adjusted r-squared, see \code{\link{r2}}}
#'   \item{\strong{RMSE}} {root mean squared error, see \code{\link{performance_rmse}}}
#'   \item{\strong{LOGLOSS}} {Log-loss, see \code{\link{performance_logloss}}}
#'   \item{\strong{SCORE_LOG}} {score of logarithmic proper scoring rule, see \code{\link{performance_score}}}
#'   \item{\strong{SCORE_SPHERICAL}} {score of spherical proper scoring rule, see \code{\link{performance_score}}}
#'   \item{\strong{PCP}} {percentage of correct predictions, see \code{\link{performance_pcp}}}
#' }
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @importFrom insight model_info
#' @export
model_performance.lm <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (all(metrics == "all")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "LOGLOSS", "PCP", "SCORE")
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  info <- insight::model_info(model)

  ## TODO remove is.list() once insight 0.8.3 is on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown")
  }

  out <- list()
  attrib <- list()

  if ("AIC" %in% toupper(metrics)) {
    out$AIC <- performance_aic(model)
  }
  if ("BIC" %in% toupper(metrics)) {
    out$BIC <- .get_BIC(model)
  }
  if ("R2" %in% toupper(metrics)) {
    R2 <- r2(model)
    attrib$r2 <- attributes(R2)
    out <- c(out, R2)
  }
  if ("RMSE" %in% toupper(metrics)) {
    out$RMSE <- performance_rmse(model, verbose = verbose)
  }
  if (("LOGLOSS" %in% toupper(metrics)) && isTRUE(info$is_binomial)) {
    .logloss <- performance_logloss(model, verbose = verbose)
    if (!is.na(.logloss)) out$LOGLOSS <- .logloss
  }
  if (("SCORE" %in% toupper(metrics)) && (isTRUE(info$is_binomial) || isTRUE(info$is_count))) {
    .scoring_rules <- performance_score(model, verbose = verbose)
    if (!is.na(.scoring_rules$logarithmic)) out$SCORE_LOG <- .scoring_rules$logarithmic
    if (!is.na(.scoring_rules$spherical)) out$SCORE_SPHERICAL <- .scoring_rules$spherical
  }

  if (("PCP" %in% toupper(metrics)) && isTRUE(info$is_binomial) && !isTRUE(info$is_multinomial) && !isTRUE(info$is_ordinal)) {
    out$PCP <- performance_pcp(model, verbose = verbose)$pcp_model
  }

  # TODO: What with sigma and deviance?

  out <- as.data.frame(.compact_list(out, remove_na = TRUE))
  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  # Add attributes
  attributes(out) <- c(attributes(out), attrib)

  out
}

#' @export
model_performance.glm <- model_performance.lm

#' @export
model_performance.Arima <- model_performance.lm

#' @export
model_performance.glmx <- model_performance.lm

#' @export
model_performance.lmrob <- model_performance.lm

#' @export
model_performance.betareg <- model_performance.lm

#' @export
model_performance.censReg <- model_performance.lm

#' @export
model_performance.clm <- model_performance.lm

#' @export
model_performance.clm2 <- model_performance.lm

#' @export
model_performance.coxph <- model_performance.lm

#' @export
model_performance.felm <- model_performance.lm

#' @export
model_performance.iv_robust <- model_performance.lm

#' @export
model_performance.ivreg <- model_performance.lm

#' @export
model_performance.multinom <- model_performance.lm

#' @export
model_performance.plm <- model_performance.lm

#' @export
model_performance.polr <- model_performance.lm

#' @export
model_performance.bayesx <- model_performance.lm

#' @export
model_performance.survreg <- model_performance.lm

#' @export
model_performance.svyglm <- model_performance.lm

#' @export
model_performance.truncreg <- model_performance.lm

#' @export
model_performance.vglm <- model_performance.lm

#' @export
model_performance.fixest <- model_performance.lm

#' @export
model_performance.DirichletRegModel <- model_performance.lm

#' @export
model_performance.flexsurvreg <- model_performance.lm

#' @export
model_performance.hurdle <- model_performance.lm



# mfx models -------------------------------

#' @export
model_performance.logitor <- function(model, ...) {
  model_performance(model$fit, ...)
}

#' @export
model_performance.logitmfx <- model_performance.logitor

#' @export
model_performance.probitmfx <- model_performance.logitor

#' @export
model_performance.poissonirr <- model_performance.logitor

#' @export
model_performance.poissonmfx <- model_performance.logitor

#' @export
model_performance.negbinirr <- model_performance.logitor

#' @export
model_performance.negbinmfx <- model_performance.logitor

#' @export
model_performance.betaor <- model_performance.logitor

#' @export
model_performance.betamfx <- model_performance.logitor





# other models -------------------------------


#' @export
model_performance.mlogit <- function(model, metrics = "all", verbose = TRUE, ...) {
  if (requireNamespace("mlogit", quietly = TRUE)) {
    model_performance.lm(model = model, metrics = metrics, verbose = verbose, ...)
  } else {
    NULL
  }
}

