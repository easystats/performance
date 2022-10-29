#' @export
model_performance.default <- function(model, metrics = "all", verbose = TRUE, ...) {
  # check for valid input
  .is_model_valid(model)

  if (any(tolower(metrics) == "log_loss")) {
    metrics[tolower(metrics) == "log_loss"] <- "LOGLOSS"
  }

  # all available options...
  all_metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE", "SIGMA", "LOGLOSS", "PCP", "SCORE")

  if (all(metrics == "all")) {
    metrics <- all_metrics
  } else if (all(metrics == "common")) {
    metrics <- c("AIC", "BIC", "R2", "R2_adj", "RMSE")
  }

  # check for valid input
  metrics <- .check_bad_metrics(metrics, all_metrics, verbose)

  if (!insight::is_model(model) || !insight::is_model_supported(model)) {
    if (isTRUE(verbose)) {
      insight::format_warning(paste0("Objects of class `", class(model)[1], "` are not supported model objects."))
    }
    return(NULL)
  }

  model_performance.lm(model = model, metrics = metrics, verbose = verbose, ...)
}



.check_bad_metrics <- function(metrics, all_metrics, verbose = TRUE) {
  # check for valid input
  bad_metrics <- which(!metrics %in% all_metrics)
  if (length(bad_metrics)) {
    if (verbose) {
      insight::format_warning(paste0(
        "Following elements are no valid metric: ",
        datawizard::text_concatenate(metrics[bad_metrics], enclose = "`")
      ))
    }
    metrics <- metrics[-bad_metrics]
  }
  metrics
}

#' @export
model_performance.aareg  <- model_performance.default

 #' @export
model_performance.acf  <- model_performance.default

 #' @export
model_performance.anova  <- model_performance.default

 #' @export
model_performance.aov  <- model_performance.default

 #' @export
model_performance.aovlist  <- model_performance.default

 #' @export
model_performance.biglm  <- model_performance.default

 #' @export
model_performance.binDesign  <- model_performance.default

 #' @export
model_performance.binWidth  <- model_performance.default

 #' @export
model_performance.boot  <- model_performance.default

 #' @export
model_performance.btergm  <- model_performance.default

 #' @export
model_performance.cch  <- model_performance.default

 #' @export
model_performance.character  <- model_performance.default

 #' @export
model_performance.cld  <- model_performance.default

 #' @export
model_performance.clmm  <- model_performance.default

 #' @export
model_performance.coeftest  <- model_performance.default

 #' @export
model_performance.confint.glht  <- model_performance.default

 #' @export
model_performance.confusionMatrix  <- model_performance.default

 #' @export
model_performance.crr  <- model_performance.default

 #' @export
model_performance.cv.glmnet  <- model_performance.default

 #' @export
model_performance.data.frame  <- model_performance.default

 #' @export
model_performance.density  <- model_performance.default

 #' @export
model_performance.dist  <- model_performance.default

 #' @export
model_performance.drc  <- model_performance.default

 #' @export
model_performance.durbinWatsonTest  <- model_performance.default

 #' @export
model_performance.emmGrid  <- model_performance.default

 #' @export
model_performance.epi.2by2  <- model_performance.default

 #' @export
model_performance.ergm  <- model_performance.default

 #' @export
model_performance.factanal  <- model_performance.default

 #' @export
model_performance.fitdistr  <- model_performance.default

 #' @export
model_performance.ftable  <- model_performance.default

 #' @export
model_performance.gam  <- model_performance.default

 #' @export
model_performance.Gam  <- model_performance.default

 #' @export
model_performance.garch  <- model_performance.default

 #' @export
model_performance.geeglm  <- model_performance.default

 #' @export
model_performance.glht  <- model_performance.default

 #' @export
model_performance.glmnet  <- model_performance.default

 #' @export
model_performance.glmrob  <- model_performance.default

 #' @export
model_performance.glmRob  <- model_performance.default

 #' @export
model_performance.gmm  <- model_performance.default

 #' @export
model_performance.htest  <- model_performance.default

 #' @export
model_performance.kappa  <- model_performance.default

 #' @export
model_performance.kde  <- model_performance.default

 #' @export
model_performance.Kendall  <- model_performance.default

 #' @export
model_performance.leveneTest  <- model_performance.default

 #' @export
model_performance.Line  <- model_performance.default

 #' @export
model_performance.Lines  <- model_performance.default

 #' @export
model_performance.list  <- model_performance.default

 #' @export
model_performance.lm.beta  <- model_performance.default

 #' @export
model_performance.lmodel2  <- model_performance.default

 #' @export
model_performance.lmRob  <- model_performance.default

 #' @export
model_performance.logical  <- model_performance.default

 #' @export
model_performance.lsmobj  <- model_performance.default

 #' @export
model_performance.manova  <- model_performance.default

 #' @export
model_performance.map  <- model_performance.default

 #' @export
model_performance.Mclust  <- model_performance.default

 #' @export
model_performance.mediate  <- model_performance.default

 #' @export
model_performance.mfx  <- model_performance.default

 #' @export
model_performance.mjoint  <- model_performance.default

 #' @export
model_performance.mle2  <- model_performance.default

 #' @export
model_performance.mlm  <- model_performance.default

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
model_performance.numeric  <- model_performance.default

 #' @export
model_performance.orcutt  <- model_performance.default

 #' @export
model_performance.pairwise.htest  <- model_performance.default

 #' @export
model_performance.pam  <- model_performance.default

 #' @export
model_performance.poLCA  <- model_performance.default

 #' @export
model_performance.Polygon  <- model_performance.default

 #' @export
model_performance.Polygons  <- model_performance.default

 #' @export
model_performance.power.htest  <- model_performance.default

 #' @export
model_performance.prcomp  <- model_performance.default

 #' @export
model_performance.pyears  <- model_performance.default

 #' @export
model_performance.rcorr  <- model_performance.default

 #' @export
model_performance.ref.grid  <- model_performance.default

 #' @export
model_performance.regsubsets  <- model_performance.default

 #' @export
model_performance.ridgelm  <- model_performance.default

 #' @export
model_performance.rlm  <- model_performance.default

 #' @export
model_performance.roc  <- model_performance.default

 #' @export
model_performance.rq  <- model_performance.default

 #' @export
model_performance.rqs  <- model_performance.default

 #' @export
model_performance.sarlm  <- model_performance.default

 #' @export
model_performance.Sarlm  <- model_performance.default

 #' @export
model_performance.SpatialLinesDataFrame  <- model_performance.default

 #' @export
model_performance.SpatialPolygons  <- model_performance.default

 #' @export
model_performance.SpatialPolygonsDataFrame  <- model_performance.default

 #' @export
model_performance.spec  <- model_performance.default

 #' @export
model_performance.speedglm  <- model_performance.default

 #' @export
model_performance.speedlm  <- model_performance.default

 #' @export
model_performance.summary_emm  <- model_performance.default

 #' @export
model_performance.summary.glht  <- model_performance.default

 #' @export
model_performance.summary.lm  <- model_performance.default

 #' @export
model_performance.summary.plm  <- model_performance.default

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
model_performance.systemfit  <- model_performance.default

 #' @export
model_performance.table  <- model_performance.default

 #' @export
model_performance.tobit  <- model_performance.default

 #' @export
model_performance.ts  <- model_performance.default

 #' @export
model_performance.TukeyHSD  <- model_performance.default

 #' @export
model_performance.varest  <- model_performance.default

 #' @export
model_performance.zoo  <- model_performance.default


