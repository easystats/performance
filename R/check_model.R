#' @title Visual check of model assumptions
#' @name check_model
#'
#' @description Visual check of model various assumptions (normality of residuals,
#' normality of random effects, heteroscedasticity, homogeneity of variance,
#' multicollinearity).
#'
#' @param x A model object.
#' @param panel Logical, if \code{TRUE}, plots are arranged as panels; else,
#' single plots for each diagnostic are returned.
#' @param ... Currently not used.
#'
#' @return The data frame that is used for plotting.
#'
#' @note This function just prepares the data for plotting. To create the plots,
#' \CRANpkg{see} needs to be installed.
#'
#' @examples
#' \dontrun{
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_model(m)
#'
#' library(lme4)
#' m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' check_model(m, panels = FALSE)
#'
#' library(rstanarm)
#' m <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#' check_model(m)
#' }
#'
#' @export
check_model <- function(x, ...) {
  UseMethod("check_model")
}


#' @rdname check_model
#' @export
check_model.default <- function(x, panels = TRUE, ...) {
  minfo <- insight::model_info(x)

  if (minfo$is_bayesian) {
    ca <- .check_assumptions_stan(x)
  } else if (minfo$is_linear) {
    ca <- .check_assumptions_linear(x, minfo)
  } else if (minfo$is_mixed) {
    ca <- .check_assumptions_glm(x)
  } else {
    stop(paste0("`check_assumptions()` not implemented for models of class '", class(x)[1], "' yet."), call. = FALSE)
  }

  attr(ca, "panels") <- panels
  ca
}




.check_assumptions_linear <- function(model, model_info) {
  dat <- list()

  dat$VIF <- .diag_vif(model)
  dat$QQ <- .diag_qq(model)
  dat$REQQ <- .diag_reqq(model, level = .95, model_info = model_info)
  dat$NORM <- .diag_norm(model)
  dat$NCV <- .diag_ncv(model)
  dat$HOMOGENEITY <- .diag_homogeneity(model)

  dat <- .compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}




.check_assumptions_glm <- function(model) {
  dat <- list()
  dat$REQQ <- .diag_reqq(model, level = .95, model_info = model_info)
  dat <- .compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}




#' @importFrom stats reshape update
.check_assumptions_stan <- function(model) {
  if (inherits(model, "brmsfit")) {

    # check if brms can be loaded

    if (!requireNamespace("brms", quietly = TRUE))
      stop("Package `brms` needs to be loaded first!", call. = F)

    # check if prior sample are available

    d2 <- brms::prior_samples(model)

    if (is.null(d2))
      stop("No prior-samples found. Please use option `sample_prior = TRUE` when fitting the model.", call. = FALSE)

    d1 <- brms::posterior_samples(model)

    # get samples from posterior and prior

    d1 <- d1[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d1), perl = TRUE)]
    d2 <- d2[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d2), perl = TRUE)]

  } else if (inherits(model, c("stanreg", "stanfit"))) {

    # check if rstanarm can be loaded
    if (!requireNamespace("rstanarm", quietly = TRUE))
      stop("Package `rstanarm` needs to be loaded first!", call. = F)


    # get samples from posterior and prior

    prior <- suppressWarnings(
      stats::update(model, prior_PD = TRUE, refresh = -1, iter = 2000, chains = 2)
    )

    d1 <- as.data.frame(model)
    d2 <- as.data.frame(prior)


    # remove intercept from output for ridgeline plot.
    # this would increase the range of the scale too much

    if (.obj_has_name(d1, "(Intercept)"))
      d1 <- .remove_column(d1, "(Intercept)")

    if (.obj_has_name(d2, "(Intercept)"))
      d2 <- .remove_column(d2, "(Intercept)")

    if (.obj_has_name(d1, "sigma"))
      d1 <- .remove_column(d1, "sigma")

    if (.obj_has_name(d2, "sigma"))
      d2 <- .remove_column(d2, "sigma")

    d1 <- d1[, grepl(pattern = "^(?!(b\\[\\(Intercept\\)|Sigma\\[))(.*)", colnames(d1), perl = TRUE)]
    d2 <- d2[, grepl(pattern = "^(?!(b\\[\\(Intercept\\)|Sigma\\[))(.*)", colnames(d2), perl = TRUE)]
  }


  # grouping variable

  d1$group <- "Posterior"
  d2$group <- "Prior"

  gather.cols <- colnames(d1)[1:(ncol(d1) - 1)]

  dat <- stats::reshape(
    rbind(d1, d2),
    idvar = "id",
    times = gather.cols,
    timevar = "y",
    v.names = "x",
    varying = gather.cols,
    direction = "long"
  )

  class(dat) <- c("check_model", "see_check_model", "data.frame")
  dat
}
