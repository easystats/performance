#' Check model for homogeneity of variances
#'
#' Check model for homogeneity of variances between groups described
#' by independent variables in a model.
#'
#' @param x A linear model or an ANOVA object.
#' @param method Name of the method (underlying test) that should be performed
#'   to check the homogeneity of variances. May either be `"levene"` for
#'   Levene's Test for Homogeneity of Variance, `"bartlett"` for the
#'   Bartlett test (assuming normal distributed samples or groups),
#'   `"fligner"` for the Fligner-Killeen test (rank-based, non-parametric
#'   test), or `"auto"`. In the latter case, Bartlett test is used if the
#'   model response is normal distributed, else Fligner-Killeen test is used.
#' @param ... Arguments passed down to `car::leveneTest()`.
#'
#' @return Invisibly returns the p-value of the test statistics. A p-value <
#'   0.05 indicates a significant difference in the variance between the groups.
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/performance.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' model <<- lm(len ~ supp + dose, data = ToothGrowth)
#' check_homogeneity(model)
#'
#' # plot results
#' if (require("see")) {
#'   result <- check_homogeneity(model)
#'   plot(result)
#' }
#' @export
check_homogeneity <- function(x, method = c("bartlett", "fligner", "levene", "auto"), ...) {
  UseMethod("check_homogeneity")
}


#' @export
check_homogeneity.default <- function(x, method = c("bartlett", "fligner", "levene", "auto"), ...) {
  method <- match.arg(method)

  resp <- insight::find_response(x)
  pred <- insight::find_predictors(x, component = "conditional", flatten = TRUE)

  # edge case, whitespace in name, so surround with backticks
  ws_pred <- pred != make.names(pred)
  if (any(ws_pred)) {
    pred[ws_pred] <- paste0("`", pred[ws_pred], "`")
  }

  if (length(pred) > 1) {
    pred <- paste0("interaction(", paste0(pred, collapse = ", "), ")", collapse = "")
  }

  f <- stats::as.formula(sprintf("%s ~ %s", resp, pred))

  if (method == "auto") {
    check <- tryCatch(
      {
        utils::capture.output(p <- check_normality(x))
        p
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(check)) {
      insight::print_color("'check_homogeneity()' cannot perform check for normality. Please specify the 'method'-argument for the test of equal variances.\n", "red")
      return(NULL)
    }

    method <- ifelse(check < 0.05, "fligner", "bartlett")
  }

  if (method == "fligner") {
    r <- stats::fligner.test(f, data = insight::get_data(x))
    p.val <- r$p.value
  } else if (method == "bartlett") {
    r <- stats::bartlett.test(f, data = insight::get_data(x))
    p.val <- r$p.value
  } else if (method == "levene") {
    insight::check_if_installed("car")
    r <- car::leveneTest(x, ...)
    p.val <- r$`Pr(>F)`
  }


  method.string <- switch(method,
    "bartlett" = "Bartlett Test",
    "fligner" = "Fligner-Killeen Test",
    "levene" = "Levene's Test"
  )

  if (is.na(p.val)) {
    warning(paste0("Could not perform ", method.string, "."), call. = FALSE)
    invisible(NULL)
  } else if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Variances differ between groups (%s, p = %.3f).\n", method.string, p.val), "red")
  } else {
    insight::print_color(sprintf("OK: There is insufficient evidence to say that the distributions have different variances (%s, p = %.3f).\n", method.string, p.val), "green")
  }

  attr(p.val, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(p.val, "method") <- method.string
  class(p.val) <- unique(c("check_homogeneity", "see_check_homogeneity", class(p.val)))

  invisible(p.val)
}

#' @rdname check_homogeneity
#' @export
check_homogeneity.afex_aov <- function(x, method = "levene", ...) {
  if (!requireNamespace("car")) {
    stop("car required for this function to work.")
  }

  if (tolower(method) != "levene") {
    message("Only Levene's test for homogeneity supported for afex_aov")
  }

  if (length(attr(x, "between")) == 0) {
    stop("Levene test is only aplicable to ANOVAs with between-subjects factors.")
  }

  data <- x$data$long # Use this to also get id column
  dv <- attr(x, "dv")
  id <- attr(x, "id")
  between <- names(attr(x, "between"))
  is_covar <- sapply(attr(x, "between"), is.null)

  ag_data <- stats::aggregate(data[, dv], data[, c(between, id)], mean)
  colnames(ag_data)[length(c(between, id)) + 1] <- dv

  if (any(is_covar)) {
    warning(insight::format_message("Levene's test is not appropriate with quantitative explanatory variables. Testing assumption of homogeneity among factor groups only."), call. = FALSE)
    # ## TODO maybe add as option?
    # warning("Testing assumption of homogeneity on residualzied data among factor groups only.", call. = FALSE)
    # ag_data[dv] <- stats::residuals(stats::lm(ag_data[,dv] ~ as.matrix(ag_data[between[is_covar]])))
    between <- between[!is_covar]
  }

  form <- stats::formula(paste0(dv, "~", paste0(between, collapse = "*")))
  test <- car::leveneTest(form, ag_data, center = mean, ...)

  p.val <- test[1, "Pr(>F)"]

  method.string <- "Levene's Test"

  if (is.na(p.val)) {
    warning(paste0("Could not perform ", method.string, "."), call. = FALSE)
    invisible(NULL)
  } else if (p.val < 0.05) {
    insight::print_color(sprintf("Warning: Variances differ between groups (%s, p = %.3f).\n", method.string, p.val), "red")
  } else {
    insight::print_color(sprintf("OK: There is insufficient evidence to say that the distributions have different variances (%s, p = %.3f).\n", method.string, p.val), "green")
  }

  attr(p.val, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(p.val, "method") <- method.string
  class(p.val) <- unique(c("check_homogeneity", "see_check_homogeneity", class(p.val)))

  invisible(p.val)
}
