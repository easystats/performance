#' @title Bayesian R2
#' @name r2_bayes
#'
#' @description Compute R2 for Bayesian models. For mixed models (including a random part),
#' it additionally computes the R2 related to the fixed effects only (marginal R2).
#'
#' @param model A Bayesian regression model (from \code{brms}, \code{rstanarm},
#'   \code{BayesFactor}, etc).
#' @param robust Logical, if \code{TRUE}, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#' @param ci Value or vector of probability of the CI (between 0 and 1) to be estimated.
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
#' if (require(BayesFactor)) {
#'   data(mtcars)
#'
#'   BFM <- generalTestBF(mpg ~ cyl * gear, data = mtcars, progress = FALSE)
#'   FM <- lm(mpg ~ cyl * gear, data = mtcars)
#'
#'   r2(FM)
#'   r2(BFM[4])
#'
#'
#'   # with random effects:
#'   mtcars$gear <- factor(mtcars$gear)
#'   model <- lmBF(mpg ~ hp + cyl + gear + gear:wt, mtcars, progress = FALSE, whichRandom = c("gear", "gear:wt"))
#'   r2(model)
#' }
#'
#' \dontrun{
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
#' @importFrom insight find_algorithm is_multivariate find_response
#' @importFrom stats median mad sd
#' @importFrom bayestestR ci
r2_bayes <- function(model, robust = TRUE, ci = .89) {
  r2_bayesian <- .r2_posterior(model)

  if (is.null(r2_bayesian)) {
    return(NULL)
  }

  if (insight::is_multivariate(model)) {
    structure(
      class = "r2_bayes_mv",
      rapply(r2_bayesian, ifelse(robust, stats::median, mean)),
      "SE" = rapply(r2_bayesian, ifelse(robust, stats::mad, stats::sd)),
      "Estimates" = rapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      "CI" = rapply(r2_bayesian, bayestestR::hdi, ci = ci),
      "robust" = robust
    )
  } else {
    structure(
      class = "r2_bayes",
      lapply(r2_bayesian, ifelse(robust, stats::median, mean)),
      "SE" = lapply(r2_bayesian, ifelse(robust, stats::mad, stats::sd)),
      "Estimates" = lapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      "CI" = lapply(r2_bayesian, bayestestR::hdi, ci = ci),
      "robust" = robust
    )
  }
}


.r2_posterior <- function(model){
  UseMethod(".r2_posterior")
}


.r2_posterior.default <- function(model) {
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


#' @importFrom insight get_parameters get_response find_predictors
#' @importFrom stats median mad sd
#' @importFrom bayestestR point_estimate hdi
.r2_posterior.BFBayesFactor <- function(model){
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
  all(colnames(params) == colnames(mm))

  # Compute R2!
  y <- insight::get_response(model)
  yy <- as.matrix(params) %*% t(mm)
  r2s <- rstantools::bayes_R2(yy, y = y)
  r2_bayesian <- data.frame(R2_Bayes = r2s)

  rand <- insight::find_predictors(model, effects = "random", flatten = TRUE)
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
