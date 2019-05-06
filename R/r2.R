#' @title Compute the model's R2
#' @name r2
#'
#' @description Calculate the R2 value for different model objects. Depending
#'   on the model, R2, pseudo-R2 or marginal / adjusted R2 values are returned.
#'
#' @param model A statistical model.
#' @param ... Currently not used.
#'
#' @return Returns a list containing values related to the most appropriate R2
#'   for the given model. See the list below:
#' \itemize{
#'   \item Logistic models: \link[=r2_tjur]{Tjur's R2}
#'   \item General linear models: \link[=r2_nagelkerke]{Nagelkerke's R2}
#'   \item Multinomial Logit: \link[=r2_mcfadden]{McFadden's R2}
#'   \item Models with zero-inflation: \link[=r2_zeroinflated]{R2 for zero-inflated models}
#'   \item Mixed models: \link[=r2_nakagawa]{Nakagawa's R2}
#'   \item Bayesian models: \link[=r2_bayes]{R2 bayes}
#' }
#'
#' @seealso \code{\link{r2_bayes}}, \code{\link{r2_coxsnell}}, \code{\link{r2_kl}},
#'   \code{\link{r2_loo}}, \code{\link{r2_mcfadden}}, \code{\link{r2_nagelkerke}},
#'   \code{\link{r2_nakagawa}}, \code{\link{r2_tjur}}, \code{\link{r2_xu}} and
#'   \code{\link{r2_zeroinflated}}.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2(model)
#'
#' library(lme4)
#' model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' r2(model)
#'
#' @export
r2 <- function(model, ...) {
  UseMethod("r2")
}


#' @importFrom insight print_color
#' @export
r2.default <- function(model, ...) {
  insight::print_color(sprintf("Objects of class \"%s\" are not supported yet.\n", class(model)[1]), "red")
  return(NA)
}



#' @export
r2.betareg <- function(model, ...) {
  list(
    R2 = c(`Pseudo R2` = model$pseudo.r.squared)
  )
}



#' @export
r2.brmsfit <- function(model, ...) {
  r2_bayes(model, ...)
}



#' @export
r2.censReg <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.clm <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.clm2 <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}


#' @export
r2.coxph <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.crch <- function(model, ...) {
  list("R2_CoxSnell" = r2_coxsnell(model))
}



#' @export
r2.feis <- function(model, ...) {
  out <- list(
    R2 = c(`R2` = model$r2),
    R2_adjusted = c(`adjusted R2` = model$adj.r2)
  )

  attr(out, "model_type") <- "Fixed Effects Individual Slope"
  structure(class = "r2_generic", out)
}



#' @export
r2.felm <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r2),
    R2_adjusted = c(`adjusted R2` = model_summary$r2adj)
  )

  attr(out, "model_type") <- "Fixed Effects"
  structure(class = "r2_generic", out)
}



#' @importFrom insight model_info
#' @export
r2.glm <- function(model, ...) {
  if (insight::model_info(model)$is_logit) {
    list("R2_Tjur" = r2_tjur(model))
  } else {
    list("R2_Nagelkerke" = r2_nagelkerke(model))
  }
}



#' @export
r2.glmmTMB <- function(model, ...) {
  r2_nakagawa(model)
}



#' @export
r2.hurdle <- function(model, ...) {
  list("R2" = r2_zeroinflated(model))
}



#' @export
r2.iv_robust <- function(model, ...) {
  out <- list(
    R2 = c(`R2` = model$r.squared),
    R2_adjusted = c(`adjusted R2` = model$adj.r.squared)
  )

  attr(out, "model_type") <- "Two-Stage Least Squares Instrumental-Variable"
  structure(class = "r2_generic", out)
}



#' @export
r2.ivreg <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r.squared),
    R2_adjusted = c(`adjusted R2` = model_summary$adj.r.squared)
  )

  attr(out, "model_type") <- "Instrumental-Variable"
  structure(class = "r2_generic", out)
}



#' @importFrom stats pf
#' @export
r2.lm <- function(model, ...) {
  model_summary <- summary(model)

  out <- list(
    R2 = model_summary$r.squared,
    R2_adjusted = model_summary$adj.r.squared
  )

  names(out$R2) <- "R2"
  names(out$R2_adjusted) <- "adjusted R2"

  f.stat <- model_summary$fstatistic[1]
  DoF <- model_summary$fstatistic[2]
  DoF_residual <- model_summary$fstatistic[3]

  if (!is.null(f.stat)) {
    attr(out, "p_value") <- stats::pf(f.stat, DoF, DoF_residual, lower.tail = FALSE)
    attr(out, "F_statistic") <- f.stat
    attr(out, "DoF") <- DoF
    attr(out, "DoF_residual") <- DoF_residual
  }

  attr(out, "model_type") <- "Linear"
  structure(class = "r2_generic", out)
}



#' @export
r2.lme <- function(model, ...) {
  r2_nakagawa(model)
}



#' @export
r2.lmrob <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r.squared),
    R2_adjusted = c(`adjusted R2` = model_summary$adj.r.squared)
  )

  attr(out, "model_type") <- "Robust Linear"
  structure(class = "r2_generic", out)
}



#' @export
r2.merMod <- function(model, ...) {
  r2_nakagawa(model)
}



#' @export
r2.mixed <- function(model, ...) {
  r2_nakagawa(model)
}



#' @export
r2.MixMod <- function(model, ...) {
  r2_nakagawa(model)
}



#' @export
r2.mlogit <- function(model, ...) {
  list("R2_McFadden" = r2_mcfadden(model))
}



#' @export
r2.multinom <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.plm <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    "R2" = c(`R2` = model_summary$r.squared[1]),
    "R2_adjusted" = c(`adjusted R2` = model_summary$r.squared[2])
  )

  attr(out, "model_type") <- "Panel Data"
  structure(class = "r2_generic", out)
}



#' @export
r2.polr <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.stanreg <- function(model, ...) {
  r2_bayes(model, ...)
}



#' @export
r2.survreg <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.svyglm <- function(model, ...) {
  rsq <- (model$null.deviance - model$deviance) / model$null.deviance
  rsq.adjust = 1 - ((1 - rsq) * (model$df.null / model$df.residual))

  out <- list(
    R2 = c(`R2` = rsq),
    R2_adjusted = c(`adjusted R2` = rsq.adjust)
  )

  attr(out, "model_type") <- "Survey"
  structure(class = "r2_generic", out)
}



#' @export
r2.truncreg <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.vglm <- function(model, ...) {
  list("R2_Nagelkerke" = r2_nagelkerke(model))
}



#' @export
r2.zerotrunc <- function(model, ...) {
  list("R2" = r2_zeroinflated(model))
}



#' @export
r2.zeroinfl <- function(model, ...) {
  list("R2" = r2_zeroinflated(model))
}
