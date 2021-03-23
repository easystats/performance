.diag_vif <- function(model) {
  dat <- .compact_list(check_collinearity(model))
  if (is.null(dat)) {
    return(NULL)
  }
  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"

  if (ncol(dat) == 5) {
    colnames(dat) <- c("x", "y", "se", "facet", "group")
    dat[, c("x", "y", "facet", "group")]
  } else {
    colnames(dat) <- c("x", "y", "se", "group")
    dat[, c("x", "y", "group")]
  }
}


#' @importFrom stats residuals rstudent fitted rstandard
.diag_qq <- function(model) {
  if (inherits(model, c("lme", "lmerMod", "merMod", "glmmTMB"))) {
    res_ <- sort(stats::residuals(model), na.last = NA)
  } else if (inherits(model, "glm")) {
    res_ <- sort(stats::rstandard(model, type = "pearson"), na.last = NA)
  } else {
    res_ <- tryCatch(
      {
        sort(stats::rstudent(model), na.last = NA)
      },
      error = function(e) {
        NULL
      }
    )
    if (is.null(res_)) {
      res_ <- tryCatch(
        {
          sort(stats::residuals(model), na.last = NA)
        },
        error = function(e) {
          NULL
        }
      )
    }
  }

  if (is.null(res_)) {
    insight::print_color(sprintf("QQ plot could not be created. Cannot extract residuals from objects of class '%s'.\n", class(model)[1]), "red")
    return(NULL)
  }

  fitted_ <- sort(stats::fitted(model), na.last = NA)
  stats::na.omit(data.frame(x = fitted_, y = res_))
}



#' @importFrom stats qnorm ppoints
#' @importFrom insight print_color
.diag_reqq <- function(model, level = .95, model_info) {
  # check if we have mixed model
  if (!model_info$is_mixed) {
    return(NULL)
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it.", call. = FALSE)
  }

  tryCatch(
    {
      if (inherits(model, "glmmTMB")) {
        var_attr <- "condVar"
        re <- .collapse_cond(lme4::ranef(model, condVar = TRUE))
      } else {
        var_attr <- "postVar"
        re <- lme4::ranef(model, condVar = TRUE)
      }
    },
    error = function(e) {
      return(NULL)
    }
  )


  se <- tryCatch(
    {
      suppressWarnings(lapply(re, function(.x) {
        pv <- attr(.x, var_attr, exact = TRUE)
        cols <- seq_len(dim(pv)[1])
        unlist(lapply(cols, function(.y) sqrt(pv[.y, .y, ])))
      }))
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(se)) {
    insight::print_color("Could not compute standard errors from random effects for diagnostic plot.\n", "red")
    return(NULL)
  }


  mapply(function(.re, .se) {
    ord <- unlist(lapply(.re, order)) + rep((0:(ncol(.re) - 1)) * nrow(.re), each = nrow(.re))

    df.y <- unlist(.re)[ord]
    df.ci <- stats::qnorm((1 + level) / 2) * .se[ord]

    data.frame(
      x = rep(stats::qnorm(stats::ppoints(nrow(.re))), ncol(.re)),
      y = df.y,
      conf.low = df.y - df.ci,
      conf.high = df.y + df.ci,
      facet = gl(ncol(.re), nrow(.re), labels = names(.re)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }, re, se, SIMPLIFY = FALSE)
}




#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd
.diag_norm <- function(model) {
  r <- try(stats::residuals(model), silent = TRUE)

  if (inherits(r, "try-error")) {
    insight::print_color(sprintf("Non-normality of residuals could not be computed. Cannot extract residuals from objects of class '%s'.\n", class(model)[1]), "red")
    return(NULL)
  }

  dat <- as.data.frame(bayestestR::estimate_density(r))
  dat$curve <- stats::dnorm(seq(min(dat$x), max(dat$x), length.out = nrow(dat)), mean(r), stats::sd(r))
  dat
}




#' @importFrom stats residuals fitted
.diag_ncv <- function(model) {
  ncv <- tryCatch(
    {
      data.frame(
        x = stats::fitted(model),
        y = stats::residuals(model)
      )
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(ncv)) {
    insight::print_color(sprintf("Non-constant error variance could not be computed. Cannot extract residuals from objects of class '%s'.\n", class(model)[1]), "red")
    return(NULL)
  }

  ncv
}


#' @importFrom insight get_variance_residual
#' @importFrom stats rstandard fitted
.diag_homogeneity <- function(model) {
  faminfo <- insight::model_info(model)
  r <- tryCatch(
    {
      if (inherits(model, "merMod")) {
        stats::residuals(model, scaled = TRUE)
      } else if (inherits(model, c("glmmTMB", "MixMod"))) {
        sigma <- if (faminfo$is_mixed) {
          sqrt(insight::get_variance_residual(model))
        } else {
          .sigma_glmmTMB_nonmixed(model, faminfo)
        }
        stats::residuals(model) / sigma
      } else if (inherits(model, "glm")) {
        stats::rstandard(model, type = "pearson")
      } else {
        stats::rstandard(model)
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(r)) {
    insight::print_color(sprintf("Homogeneity of variance could not be computed. Cannot extract residual variance from objects of class '%s'.\n", class(model)[1]), "red")
    return(NULL)
  }

  data.frame(
    x = stats::fitted(model),
    y = sqrt(abs(r))
  )
}



#' @importFrom insight model_info
.sigma_glmmTMB_nonmixed <- function(model, faminfo) {
  if (!is.na(match(faminfo$family, c("binomial", "poisson", "truncated_poisson")))) {
    return(1)
  }
  betad <- model$fit$par["betad"]
  switch(faminfo$family,
    gaussian = exp(0.5 * betad),
    Gamma = exp(-0.5 * betad),
    exp(betad)
  )
}
