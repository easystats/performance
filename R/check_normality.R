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
#'   standardized residuals, are used for the test. There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#'    implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details `check_normality()` calls `stats::shapiro.test` and checks the
#' standardized residuals (or Studentized residuals for mixed models) for
#' normal distribution. Note that this formal test almost always yields
#' significant results for the distribution of residuals and visual inspection
#' (e.g. Q-Q plots) are preferable.
#'
#' @examples
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_normality(m)
#'
#' # plot results
#' if (require("see")) {
#'   x <- check_normality(m)
#'   plot(x)
#' }
#' \dontrun{
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
  # valid model?
  if (!insight::model_info(x)$is_linear) {
    message(insight::format_message("Checking normality of residuals is only useful an appropriate assumption for linear models."))
    return(NULL)
  }

  # check for normality of residuals
  p.val <- .check_normality(stats::rstandard(x), x)

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse(substitute(x))
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

#' @rawNamespace if (getRversion() >= "3.5.0") {
#'   S3method(stats::residuals, check_normality_numeric)
#'   S3method(stats::rstudent, check_normality_numeric)
#' }

#' @export
residuals.check_normality_numeric <- function(object, ...) {
  attr(object, "data")
}

#' @export
rstudent.check_normality_numeric <- function(model, ...) {
  attr(model, "data")
}


# methods ----------------------

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
    for (i in 1:length(x)) {
      if (x[i] < 0.05) {
        insight::print_color(sprintf("Warning: Non-normality for random effects '%s' detected (%s).\n", re_groups[i], pstring[i]), "red")
      } else {
        insight::print_color(sprintf("OK: Random effects '%s' appear as normally distributed (%s).\n", re_groups[i], pstring[i]), "green")
      }
    }
  } else {
    if (x < 0.05) {
      insight::print_color(sprintf("Warning: Non-normality of %s detected (%s).\n", type, pstring), "red")
    } else {
      insight::print_color(sprintf("OK: %s appear as normally distributed (%s).\n", type, pstring), "green")
    }
  }
  invisible(x)
}



# other classes --------------------

# mixed models ---------------------

#' @rdname check_normality
#' @export
check_normality.merMod <- function(x, effects = c("fixed", "random"), ...) {
  # args
  effects <- match.arg(effects)
  info <- insight::model_info(x)

  # valid model?
  if (!info$is_linear && effects == "fixed") {
    message(insight::format_message("Checking normality of residuals is only useful an appropriate assumption for linear models."))
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
        return(NULL)
      }
    )

    p.val <- c()
    re_groups <- c()

    if (!is.null(re)) {
      for (i in names(re)) {
        for (j in colnames(re[[i]])) {
          re_groups <- c(re_groups, paste0(i, ": ", j))
          p.val <- c(p.val, .check_normality(re[[i]][[j]], x, "random effects"))
        }
      }
      attr(p.val, "re_qq") <- .diag_reqq(x, level = .95, model_info = info)
      attr(p.val, "type") <- "random effects"
      attr(p.val, "re_groups") <- re_groups
    }
  } else {
    # check for normality of residuals
    p.val <- .check_normality(stats::rstudent(x), x)
  }

  attr(p.val, "data") <- x
  attr(p.val, "object_name") <- insight::safe_deparse(substitute(x))
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
  attr(p.val, "object_name") <- insight::safe_deparse(substitute(x))
  class(p.val) <- unique(c("check_normality", "see_check_normality", class(p.val)))

  invisible(p.val)
}


#' @export
check_normality.BFBayesFactor <- check_normality.afex_aov



# helper ---------------------

.check_normality <- function(x, model, type = "residuals") {
  ts <- tryCatch(
    {
      if (length(x) >= 5000) {
        suppressWarnings(stats::ks.test(x, y = "pnorm", alternative = "two.sided"))
      } else {
        stats::shapiro.test(x)
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(ts)) {
    insight::print_color(sprintf("'check_normality()' does not support models of class '%s'.\n", class(model)[1]), "red")
    return(NULL)
  }

  out <- ts$p.value
  attr(out, "type") <- type

  out
}
