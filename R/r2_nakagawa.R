#' @title Nakagawa's R2 for mixed models
#' @name r2_nakagawa
#'
#' @description Compute the marginal and conditional r-squared value for
#'  mixed effects models with complex random effects structures.
#'
#' @param model A mixed effects model.
#'
#' @return A list with the conditional and marginal R2 values.
#'
#' @details Marginal and conditional r-squared values for mixed models are calculated
#'  based on \cite{Nakagawa et al. 2017}. For more details on the computation
#'  of the variances, see \code{\link[insight]{get_variance}}.
#'  \cr \cr
#'  The marginal r-squared considers only the variance of the fixed effects, while the
#'  conditional r-squared takes both the fixed and random effects into account.
#'  The random effect variances are actually the mean random effect variances,
#'  thus the r-squared value is also appropriate for mixed models with random
#'  slopes or nested random effects (see \cite{Johnson 2014}).
#'
#' @references \itemize{
#'  \item Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s R2 GLMM to random slopes models. Methods in Ecology and Evolution, 5(9), 944–946. \doi{10.1111/2041-210X.12225}
#'  \item Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. \doi{10.1111/j.2041-210x.2012.00261.x}
#'  \item Nakagawa, S., Johnson, P. C. D., & Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of The Royal Society Interface, 14(134), 20170213. \doi{10.1098/rsif.2017.0213}
#'  }
#'
#' @examples
#' library(lme4)
#' model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' r2_nakagawa(model)
#'
#' @importFrom insight get_variance print_color
#' @export
r2_nakagawa <- function(model) {
  vars <- tryCatch(
    {
      insight::get_variance(model, name_fun = "r2()", name_full = "r-squared")
    },
    warning = function(e) {
      if (inherits(e, c("simpleWarning", "warning"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
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

  # Calculate R2 values

  r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual)
  r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)

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
