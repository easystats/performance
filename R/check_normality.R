#' @title Check model for (non-)normality of residuals.
#' @name check_normality
#'
#' @description Check model for (non-)normality of residuals.
#'
#' @param x A model object.
#' @param effects Should normality for residuals (`"fixed"`) or random
#'   effects (`"random"`) be tested? Only applies to mixed-effects models.
#'   May be abbreviated.
#' @param ... Currently not used.
#'
#' @return The p-value of the test statistics. A p-value < 0.05 indicates a
#'   significant deviation from normal distribution.
#'
#' @note For mixed-effects models, studentized residuals, and *not*
#' standardized residuals, are used for the test. There is also a
#' [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#'  implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @seealso [`see::plot.see_check_normality()`] for options to customize the plot.
#'
#' @details `check_normality()` calls `stats::shapiro.test` and checks the
#' standardized residuals (or studentized residuals for mixed models) for
#' normal distribution. Note that this formal test almost always yields
#' significant results for the distribution of residuals and visual inspection
#' (e.g. Q-Q plots) are preferable. For generalized linear models, no formal
#' statistical test is carried out. Rather, there's only a `plot()` method for
#' GLMs. This plot shows a half-normal Q-Q plot of the absolute value of the
#' standardized deviance residuals is shown (in line with changes in
#' `plot.lm()` for R 4.3+).
#'
#' @examplesIf insight::check_if_installed("see", minimum_version = "0.9.1", quietly = TRUE)
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_normality(m)
#'
#' # plot results
#' x <- check_normality(m)
#' plot(x)
#'
#' \donttest{
#' # QQ-plot
#' plot(check_normality(m), type = "qq")
#'
#' # PP-plot
#' plot(check_normality(m), type = "pp")
#' }
#' @export
check_normality <- function(x, ...) {
  UseMethod("check_normality")
}


# default -------------------------

#' @export
check_normality.default <- function(x, ...) {
  .is_model_valid(x)

  if (!insight::model_info(x)$is_linear) {
    insight::format_alert(
      "Checking normality of residuals is only appropriate for linear models.",
      "Instead, please use `simulate_residuals()` and `check_residuals()` to test whether quantile residuals follow a uniform distribution." # nolint
    )
    return(NULL)
  }

  # check for normality of residuals
  p.val <- .check_normality(stats::rstandard(x), x)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(p.val, "effects") <- "fixed"
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  p.val
}

# PCA / FA ---------------

#' @export
check_normality.parameters_efa <- function(x, ...) {
  # check for normality of residuals
  p.val <- .check_normality(insight::get_residuals(x), x)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(p.val, "effects") <- "fixed"
  attr(p.val, "is_fa") <- TRUE
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  p.val
}

#' @export
check_normality.fa <- check_normality.parameters_efa

#' @export
check_normality.principal <- check_normality.parameters_efa

# glm ---------------

#' @export
check_normality.glm <- function(x, ...) {
  out <- 1
  attr(out, "data") <- x
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "effects") <- "fixed"
  attr(out, "model_info") <- insight::model_info(x)
  class(out) <- unique(c("check_normality", "see_check_normality", class(out)))

  insight::format_alert(
    "Generalized linear model residuals are not expected to follow a normal distribution.",
    "Instead, please use `simulate_residuals()` and `check_residuals()` to test whether quantile residuals follow a uniform distribution." # nolint
  )
  invisible(out)
}

# simulated residuals ----------

#' @export
check_normality.performance_simres <- function(x, ...) {
  # check for normality of residuals
  res <- stats::residuals(x, quantile_function = stats::qnorm)
  p.val <- .check_normality(res[!is.infinite(res) & !is.na(res)], x)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(p.val, "effects") <- "fixed"
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  p.val
}


# numeric -------------------

#' @export
check_normality.numeric <- function(x, ...) {
  # check for normality of residuals
  p.val <- .check_normality(x, NULL, type = "raw")

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse(substitute(x))
  attr(p.val, "effects") <- "fixed"
  class(p.val) <- unique(c("check_normality", "see_check_normality", "check_normality_numeric", class(p.val)))

  p.val
}


# methods ----------------------


#' @importFrom stats residuals
#' @export
residuals.check_normality_numeric <- function(object, ...) {
  attr(object, "data")
}


#' @importFrom stats rstudent
#' @export
rstudent.check_normality_numeric <- function(model, ...) {
  attr(model, "data")
}


#' @export
plot.check_normality <- function(x, ...) {
  insight::check_if_installed("see", "for residual plots")
  NextMethod()
}


#' @export
print.check_normality <- function(x, ...) {
  pstring <- insight::format_p(x)
  type <- attributes(x)$type

  if (identical(attributes(x)$effects, "random")) {
    re_groups <- attributes(x)$re_groups
    for (i in seq_along(x)) {
      if (x[i] < 0.05) {
        insight::print_color(
          sprintf("Warning: Non-normality for random effects '%s' detected (%s).\n", re_groups[i], pstring[i]),
          "red"
        )
      } else {
        insight::print_color(
          sprintf("OK: Random effects '%s' appear as normally distributed (%s).\n", re_groups[i], pstring[i]),
          "green"
        )
      }
    }
  } else {
    if (length(x) > 1 && "units" %in% names(attributes(x))) type <- attributes(x)$units
    for (i in seq_along(x)) {
      if (x[i] < 0.05) {
        insight::print_color(sprintf("Warning: Non-normality of %s detected (%s).\n", type[i], pstring[i]), "red")
      } else {
        insight::print_color(sprintf("OK: %s appear as normally distributed (%s).\n", type[i], pstring[i]), "green")
      }
    }
  }

  # add FA / PCA information
  if (isTRUE(attributes(x)$is_fa)) {
    res <- insight::get_residuals(attributes(x)$data)
    lge_resid_tot <- sum(abs(res) > 0.05)
    lge_resid_pct <- lge_resid_tot / length(res)
    cat(paste0(
      "\nAbsolute residuals > 0.05 = ",
      lge_resid_tot,
      " (",
      insight::format_percent(lge_resid_pct),
      ")\n"
    ))
  }

  invisible(x)
}


# other classes --------------------

# mixed models ---------------------

#' @rdname check_normality
#' @export
check_normality.merMod <- function(x, effects = "fixed", ...) {
  # args
  effects <- insight::validate_argument(effects, c("fixed", "random"))
  info <- insight::model_info(x)

  # valid model?
  if (!info$is_linear && effects == "fixed") {
    insight::format_alert(
      "Checking normality of residuals is only appropriate for linear models.",
      "Instead, please use `simulate_residuals()` and `check_residuals()` to test whether quantile residuals follow a uniform distribution." # nolint
    )
    return(NULL)
  }

  if (effects == "random") {
    insight::check_if_installed("lme4")

    re <- tryCatch(
      {
        if (inherits(x, "glmmTMB")) {
          var_attr <- "condVar"
          .collapse_cond(lme4::ranef(x, condVar = TRUE))
        } else {
          var_attr <- "postVar"
          lme4::ranef(x, condVar = TRUE)
        }
      },
      error = function(e) {
        NULL
      }
    )

    p.val <- re_groups <- NULL

    if (!is.null(re)) {
      for (i in names(re)) {
        for (j in colnames(re[[i]])) {
          re_groups <- c(re_groups, paste0(i, ": ", j))
          p.val <- c(p.val, .check_normality(re[[i]][[j]], x, "random effects"))
        }
      }
      attr(p.val, "re_qq") <- .model_diagnostic_ranef_qq(x, level = 0.95, model_info = info)
      attr(p.val, "type") <- "random effects"
      attr(p.val, "re_groups") <- re_groups
    }
  } else if (inherits(x, "glmmTMB")) {
    p.val <- .check_normality(stats::residuals(x, type = "deviance"), x)
  } else {
    # check for normality of residuals
    p.val <- .check_normality(stats::rstudent(x), x)
  }

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(p.val, "effects") <- effects
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  p.val
}


#' @export
check_normality.glmmTMB <- check_normality.merMod


#' @export
check_normality.lmerModLmerTest <- check_normality.merMod


#' @export
check_normality.afex_aov <- function(x, ...) {
  r <- suppressMessages(stats::residuals(x, append = FALSE))
  p.val <- .check_normality(r, x)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  invisible(p.val)
}


#' @export
check_normality.BFBayesFactor <- check_normality.afex_aov


# helper ---------------------

.check_normality <- function(x, model, type = "residuals") {
  ts_result <- .safe({
    if (length(x) >= 5000) {
      suppressWarnings(stats::ks.test(x, y = "pnorm", alternative = "two.sided"))
    } else {
      stats::shapiro.test(x)
    }
  })

  if (is.null(ts_result)) {
    insight::print_color(
      sprintf("`check_normality()` does not support models of class `%s`.\n", class(model)[1]),
      "red"
    )
    return(NULL)
  }

  out <- ts_result$p.value
  attr(out, "type") <- type

  out
}
