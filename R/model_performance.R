#' @title Model Performance
#' @name model_performance
#'
#' @description See the documentation for your object's class:
#' \itemize{
#'   \item [Frequentist Regressions][model_performance.lm]
#'   \item [Instrumental Variables Regressions][model_performance.ivreg]
#'   \item [Mixed models][model_performance.merMod]
#'   \item [Bayesian models][model_performance.stanreg]
#'   \item [CFA / SEM lavaan models][model_performance.lavaan]
#'   \item [Meta-analysis models][model_performance.rma]
#' }
#'
#' @seealso [`compare_performance()`][compare_performance] to compare performance of many different models.
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods, resp. for
#'   `compare_performance()`, one or multiple model objects (also of
#'   different classes).
#'
#' @return A data frame (with one row) and one column per "index" (see `metrics`).
#'
#' @details `model_performance()` correctly detects transformed response and
#' returns the "corrected" AIC and BIC value on the original scale. To get back
#' to the original scale, the likelihood of the model is multiplied by the
#' Jacobian/derivative of the transformation.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_performance(model)
#'
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' model_performance(model)
#' @export
model_performance <- function(model, ...) {
  UseMethod("model_performance")
}


#' @rdname model_performance
#' @export
performance <- model_performance



# methods --------------------------------

#' @export
print.performance_model <- function(x, digits = 3, layout = "horizontal", ...) {
  layout <- match.arg(layout, choices = c("horizontal", "vertical"))
  formatted_table <- format(x = x, digits = digits, format = "text", ...)

  # switch to vertical layout
  if (layout == "vertical") {
    formatted_table <- datawizard::rownames_as_column(as.data.frame(t(formatted_table)), "Metric")
    colnames(formatted_table)[2] <- "Value"
  }

  cat(insight::export_table(x = formatted_table, digits = digits, format = "text", caption = c("# Indices of model performance", "blue"), ...))
  invisible(x)
}

#' @export
model_performance.optim  <- model_performance.default

 #' @export
model_performance.aareg  <- model_performance.default

 #' @export
model_performance.anova  <- model_performance.default

 #' @export
model_performance.aov  <- model_performance.default

 #' @export
model_performance.biglm  <- model_performance.default

 #' @export
model_performance.binDesign  <- model_performance.default

 #' @export
model_performance.cch  <- model_performance.default

 #' @export
model_performance.clmm  <- model_performance.default

 #' @export
model_performance.coeftest  <- model_performance.default

 #' @export
model_performance.crr  <- model_performance.default

 #' @export
model_performance.cv.glmnet  <- model_performance.default

 #' @export
model_performance.data.frame  <- model_performance.default

 #' @export
model_performance.drc  <- model_performance.default

 #' @export
model_performance.durbinWatsonTest  <- model_performance.default

 #' @export
model_performance.ergm  <- model_performance.default

 #' @export
model_performance.factanal  <- model_performance.default

 #' @export
model_performance.fitdistr  <- model_performance.default

 #' @export
model_performance.gam  <- model_performance.default

 #' @export
model_performance.Gam  <- model_performance.default

 #' @export
model_performance.garch  <- model_performance.default

 #' @export
model_performance.geeglm  <- model_performance.default

 #' @export
model_performance.glmnet  <- model_performance.default

 #' @export
model_performance.glmRob  <- model_performance.default

 #' @export
model_performance.gmm  <- model_performance.default

 #' @export
model_performance.htest  <- model_performance.default

 #' @export
model_performance.list  <- model_performance.default

 #' @export
model_performance.lmodel2  <- model_performance.default

 #' @export
model_performance.lmRob  <- model_performance.default

 #' @export
model_performance.Mclust  <- model_performance.default

 #' @export
model_performance.mfx  <- model_performance.default

 #' @export
model_performance.mjoint  <- model_performance.default

 #' @export
model_performance.muhaz  <- model_performance.default

 #' @export
model_performance.negbin  <- model_performance.default

 #' @export
model_performance.nlrq  <- model_performance.default

 #' @export
model_performance.nls  <- model_performance.default

 #' @export
model_performance.NULL  <- model_performance.default

 #' @export
model_performance.orcutt  <- model_performance.default

 #' @export
model_performance.pam  <- model_performance.default

 #' @export
model_performance.poLCA  <- model_performance.default

 #' @export
model_performance.pyears  <- model_performance.default

 #' @export
model_performance.ridgelm  <- model_performance.default

 #' @export
model_performance.rlm  <- model_performance.default

 #' @export
model_performance.rq  <- model_performance.default

 #' @export
model_performance.rqs  <- model_performance.default

 #' @export
model_performance.sarlm  <- model_performance.default

 #' @export
model_performance.Sarlm  <- model_performance.default

 #' @export
model_performance.smooth.spline  <- model_performance.default

 #' @export
model_performance.speedglm  <- model_performance.default

 #' @export
model_performance.speedlm  <- model_performance.default

 #' @export
model_performance.summary.lm  <- model_performance.default

 #' @export
model_performance.summaryDefault  <- model_performance.default

 #' @export
model_performance.survdiff  <- model_performance.default

 #' @export
model_performance.survexp  <- model_performance.default

 #' @export
model_performance.survfit  <- model_performance.default

 #' @export
model_performance.svyolr  <- model_performance.default

 #' @export
model_performance.tbl_df  <- model_performance.default

 #' @export
model_performance.varest  <- model_performance.default
