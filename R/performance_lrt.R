#' @title Likelihood-Ratio-Test for Model Comparison
#' @name performance_lrt
#'
#' @description Compute Likelihood-Ratio-Test (LRT) for model comparison. For regression models, this is obtained through the \code{anova(..., test="LRT")} function. This test gives usually somewhat close results (if not equivalent) to the commonly used F-test (obtained by default with \code{anova(...)}). The LRT is a maximum likelihood test used to see which model is a better (more likely) explanation of the data. Maximum likelihood tests make stronger assumptions than method of moments tests like the F-test, and in return are more efficient.
#'
#' @param ... Multiple model objects, which should respond to \code{anova()}.
#'
#' @return A data frame, based on the results from \code{anova()}.
#'
#' @details This only makes statistical sense if the models are nested. It is conventional to list the models from smallest to largest, but this is up to the user. The output shows the tests of the models against one another in the order specified.
#'
#' @seealso \code{\link[=compare_performance]{compare_performance()}} to compare performance of many different models.
#'
#' @examples
#' m1 <- lm(mpg ~ wt + cyl, data = mtcars)
#' m2 <- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' m3 <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' performance_lrt(m1, m2, m3)
#'
#' if (require("lavaan")) {
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9 "
#'   m1 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   structure <- " visual  =~ x2 + x3
#'                  textual =~ x4 + x6
#'                  speed   =~ x8 + x9 "
#'   m2 <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'
#'   performance_lrt(m1, m2)
#' }
#' @export
performance_lrt <- function(...) {
  UseMethod("performance_lrt")
}


#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.default <- function(...) {
  objects <- list(...)

  if (!all(sapply(objects, insight::is_model))) {
    stop("All objects must be valid regression model objects.")
  }

  object_names <- match.call(expand.dots = FALSE)$`...`

  # LRT for model comparison
  if (length(objects) > 1) {
    # sd_mle <- function(model) sqrt(mean(residuals(model)^2))
    # ll <- function(model, sd) {
    #   sum(dnorm(insight::get_response(model), mean = fitted(model), sd = sd, log = TRUE))
    # }
    # -2 * (ll(m2, sd_mle(m2)) - ll(m3, sd_mle(m3)))
    lrt <- stats::anova(..., test = "LRT")

    # sort
    lrt_order <- order(order(rownames(lrt)))
    objects <- objects[lrt_order]
    object_names <- object_names[lrt_order]

    .performance_lrt(objects, object_names, lrt)
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }
}



#' @importFrom stats anova
#' @importFrom insight is_model
#' @export
performance_lrt.lavaan <- function(...) {
  objects <- list(...)

  if (!all(sapply(objects, insight::is_model))) {
    stop("All objects must be valid lavaan models.")
  }

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Package 'lavaan' required. Please install it.")
  }

  object_names <- match.call(expand.dots = FALSE)$`...`

  # LRT for model comparison
  if (length(objects) > 1) {
    lrt <- lavaan::anova(..., test = "LRT")
    .performance_lrt(objects, object_names, lrt)
  } else {
    warning("At least two models required for a Likelihood-Ratio-Test.", call. = FALSE)
  }
}






# helper --------------------------


.performance_lrt <- function(objects, object_names, lrt) {
  # create data frame with info about model name and class
  m <- mapply(function(.x, .y) {
    data.frame(Model = as.character(.y), Type = class(.x)[1], stringsAsFactors = FALSE)
  }, objects, object_names, SIMPLIFY = FALSE)

  # bind all data
  out <- cbind(do.call(rbind, m), lrt)

  # Drop cols
  out <- out[!names(out) %in% c("AIC", "BIC", "Df diff", "Chisq diff")]  # For regressions

  # Rename columns
  colnames(out)[names(out) == "Df"] <- "df"
  colnames(out)[names(out) == "Res.Df"] <- "df_error"
  colnames(out)[names(out) == "Chisq"] <- "Chi2"
  colnames(out)[names(out) == "Sum of Sq"] <- "Sum_Squares"
  colnames(out)[grepl("^Pr\\(>", names(out))] <- "p"
  rownames(out) <- NULL

  # Reorder columns
  cols <- colnames(out)
  if(all(c("df", "Chi2") %in% cols)) {
    cols[cols %in% c("df", "Chi2")] <- c("Chi2", "df")  # Move 'df' column after Chi2
  }
  if(all(c("df_error", "RSS", "df", "Sum_Squares") %in% cols)) {
    cols[cols %in% c("df_error", "RSS", "df", "Sum_Squares")] <- c("RSS", "Sum_Squares", "df", "df_error")  # 'df_error' after 'df'
  }
  out <- out[cols]

  class(out) <- c("performance_lrt", "see_performance_lrt", "data.frame")
  out
}
