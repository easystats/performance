#' @title Nakagawa's R2 for mixed models
#' @name r2_nakagawa
#'
#' @description Compute the marginal and conditional r-squared value for
#'  mixed effects models with complex random effects structures.
#'
#' @param model A mixed effects model.
#' @param by_group Logical, if `TRUE`, returns the explained variance
#'   at different levels (if there are multiple levels). This is essentially
#'   similar to the variance reduction approach by \cite{Hox (2010), pp. 69-78}.
#' @param tolerance Tolerance for singularity check of random effects, to decide
#'   whether to compute random effect variances for the conditional r-squared
#'   or not. Indicates up to which value the convergence result is accepted. When
#'   `r2_nakagawa()` returns a warning, stating that random effect variances
#'   can't be computed (and thus, the conditional r-squared is `NA`),
#'   decrease the tolerance-level. See also [check_singularity()].
#'
#' @return A list with the conditional and marginal R2 values.
#'
#' @details
#'
#' Marginal and conditional r-squared values for mixed models are calculated
#' based on \cite{Nakagawa et al. 2017}. For more details on the computation of
#' the variances, see `?insight::get_variance`.
#'  \cr \cr
#'  The marginal r-squared considers only the variance of the fixed effects,
#'  while the conditional r-squared takes both the fixed and random effects into
#'  account. The random effect variances are actually the mean random effect
#'  variances, thus the r-squared value is also appropriate for mixed models
#'  with random slopes or nested random effects (see \cite{Johnson 2014}).
#'
#' @references \itemize{
#'  \item Hox, J. J. (2010). Multilevel analysis: techniques and applications
#'  (2nd ed). New York: Routledge.
#'
#'  \item Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM
#'  to random slopes models. Methods in Ecology and Evolution, 5(9), 944–946.
#'  \doi{10.1111/2041-210X.12225}
#'
#'  \item Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#'  obtaining R2 from generalized linear mixed-effects models. Methods in
#'  Ecology and Evolution, 4(2), 133–142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'
#'  \item Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The
#'  coefficient of determination R2 and intra-class correlation coefficient from
#'  generalized linear mixed-effects models revisited and expanded. Journal of
#'  The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'  }
#'
#' @examples
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   r2_nakagawa(model)
#'   r2_nakagawa(model, by_group = TRUE)
#' }
#' @export
r2_nakagawa <- function(model, by_group = FALSE, tolerance = 1e-5) {
  vars <- tryCatch(
    {
      insight::get_variance(model,
        tolerance = tolerance,
        name_fun = "r2()",
        name_full = "r-squared"
      )
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )


  if (is.null(vars) || all(is.na(vars))) {
    return(NA)
  }

  # check if we have successfully computed all variance components...

  components <- c("var.fixed", "var.residual")
  check_elements <- sapply(components, function(.i) !is.null(vars[[.i]]))

  if (!all(check_elements)) {
    return(NA)
  }


  # compute R2 by group
  if (isTRUE(by_group)) {
    # with random slopes, explained variance is inaccurate
    if (!is.null(insight::find_random_slopes(model))) {
      warning(insight::format_message("Model contains random slopes. Explained variance by levels is not accurate."), call. = FALSE)
    }

    # null-model
    null_model <- insight::null_model(model)
    vars_null <- insight::get_variance(null_model, tolerance = tolerance)

    # names of group levels
    group_names <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

    # compute r2 by level
    r2_random <- 1 - (vars$var.intercept[group_names] / vars_null$var.intercept[group_names])
    r2_fixed <- 1 - (vars$var.residual / vars_null$var.residual)

    out <- data.frame(
      Level = c("Level 1", group_names),
      R2 = c(r2_fixed, r2_random),
      stringsAsFactors = FALSE
    )

    class(out) <- c("r2_nakagawa_by_group", "data.frame")
    out
  } else {
    # Calculate R2 values
    if (datawizard::is_empty_object(vars$var.random) || is.na(vars$var.random)) {
      # if no random effect variance, return simple R2
      insight::print_color("Random effect variances not available. Returned R2 does not account for random effects.\n", "red")
      r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.residual)
      r2_conditional <- NA
    } else {
      r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual)
      r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
    }


    names(r2_conditional) <- "Conditional R2"
    names(r2_marginal) <- "Marginal R2"

    structure(
      class = "r2_nakagawa",
      list(
        "R2_conditional" = r2_conditional,
        "R2_marginal" = r2_marginal
      )
    )
  }
}




#' @export
as.data.frame.r2_nakagawa <- function(x, row.names = NULL, optional = FALSE, ...) {
  data.frame(
    R2_conditional = x$R2_conditional,
    R2_marginal = x$R2_marginal,
    stringsAsFactors = FALSE,
    row.names = row.names,
    optional = optional,
    ...
  )
}
