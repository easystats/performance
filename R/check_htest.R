#' @export
check_normality.htest <- function(x, ...) {
  model_data <- insight::get_data(x)
  if (is.null(model_data)) {
    insight::format_error(
      "Cannot check assumptions - Unable to retrieve data from `htest` object."
    )
  }
  method <- x[["method"]]


  if (grepl("Welch", method, fixed = TRUE) ||
    grepl("F test to compare two variances", method, fixed = TRUE)) {
    # sanity check
    if (!is.numeric(model_data[[2]])) {
      insight::format_error(
        "Discrete or character variables are not supported for this test. Please use a continuous variable for the second argument."
      )
    }
    m1 <- stats::lm(model_data[[1]] ~ 1)
    m2 <- stats::lm(model_data[[2]] ~ 1)

    out <- check_normality(m1)
    out[2] <- check_normality(m2)[1]
    attr(out, "units") <- c("Group1", "Group2")
  } else if (grepl("Two Sample t-test", method, fixed = TRUE)) {
    m <- stats::lm(
      formula = value ~ factor(name),
      data = datawizard::data_to_long(model_data)
    )

    out <- check_normality(m)
  } else if (grepl("One Sample t-test", method, fixed = TRUE)) {
    m <- stats::lm(model_data[[1]] ~ 1)

    out <- check_normality(m)
  } else if (grepl("Paired t-test", method, fixed = TRUE)) {
    if (!is.numeric(model_data[[2]])) {
      insight::format_error(
        "Discrete or character variables are not supported for this test. Please use a continuous variable for the second argument."
      )
    }
    d <- model_data[[1]] - model_data[[2]]
    m <- stats::lm(d ~ 1)

    out <- check_normality(m)
  } else if (grepl("One-way analysis of means (not assuming equal variances)", method, fixed = TRUE)) {
    model_data <- split(model_data, model_data[[2]])
    outs <- lapply(model_data, function(d) {
      check_normality(stats::lm(d[[1]] ~ 1))
    })

    out <- unlist(outs)
    attributes(out) <- attributes(outs[[1]])
    attr(out, "units") <- paste0("Group", seq_along(outs))
  } else if (grepl("One-way analysis of means", method, fixed = TRUE)) {
    m <- stats::aov(model_data[[1]] ~ factor(model_data[[2]]))

    out <- check_normality(m)
  } else if (grepl("Pearson's product-moment correlation", method, fixed = TRUE)) {
    out <- .MVN_hz(model_data)[["p value"]]
    class(out) <- c("check_normality", "see_check_normality", "numeric")
    attr(out, "type") <- "residuals"
  } else if (grepl("Pearson's Chi-squared test", method, fixed = TRUE) ||
    grepl("Chi-squared test for given probabilities", method, fixed = TRUE)) {
    out <- c(
      "5" = all(x$expected >= 5),
      "10" = all(x$expected >= 10)
    )
    class(out) <- c("check_normality_binom", "logical")
    attr(out, "object_name") <- substitute(x)
    return(out)
  } else {
    insight::format_error(
      "This `htest` is not supported (or this assumption is not required for this test)."
    )
  }

  attr(out, "object_name") <- substitute(x)
  attr(out, "data") <- NULL
  out
}


#' @export
check_homogeneity.htest <- function(x, ...) {
  model_data <- insight::get_data(x)
  if (is.null(model_data)) {
    insight::format_error(
      "Cannot check assumptions - Unable to retrieve data from `htest` object."
    )
  }
  method <- x[["method"]]

  if (grepl("(not assuming equal variances)", method, fixed = TRUE) ||
    grepl("Welch", method, fixed = TRUE)) {
    insight::format_error("Test does not assume homogeneity. No need to test this assumption.")
  }

  if (grepl("Two Sample t-test", method, fixed = TRUE)) {
    m <- stats::lm(
      formula = value ~ factor(name),
      data = datawizard::data_to_long(model_data)
    )
  } else if (grepl("One-way analysis of means", method, fixed = TRUE)) {
    m <- stats::aov(
      stats::reformulate(names(model_data)[2], response = names(model_data)[1]),
      data = model_data
    )
  } else {
    insight::format_error(
      "This `htest` is not supported (or this assumption is not required for this test)."
    )
  }

  out <- check_homogeneity(m, ...)

  attr(out, "object_name") <- substitute(x)
  attr(out, "data") <- NULL
  out
}


#' @export
check_symmetry.htest <- function(x, ...) {
  model_data <- insight::get_data(x)
  if (is.null(model_data)) {
    insight::format_error(
      "Cannot check assumptions - Unable to retrieve data from `htest` object."
    )
  }
  method <- x[["method"]]

  if (grepl("signed rank", method, fixed = TRUE)) {
    if (ncol(model_data) > 1) {
      out <- check_symmetry(model_data[[1]] - model_data[[2]])
    } else {
      out <- check_symmetry(model_data[[1]])
    }
  } else {
    insight::format_error(
      "This `htest` is not supported (or this assumption is not required for this test)."
    )
  }

  attr(out, "object_name") <- substitute(x)
  out
}


# check_model.htest <- function() {
#
# }


# Print -------------------------------------------------------------------

#' @export
print.check_normality_binom <- function(x, ...) {
  if (x["10"]) {
    insight::print_color(insight::format_message(
      "OK: All cells in the expected table have more than 10 observations.\n"
    ), "green")
  } else if (x["5"]) {
    insight::print_color(insight::format_message(
      "Warning: All cells in the expected table have more than 5 observations, but some have less than 10.\n"
    ), "yellow")
  } else {
    insight::print_color(insight::format_message(
      "Warning: Some cells in the expected table have less than 5 observations.\n"
    ), "red")
  }
  invisible(x)
}


# Utils -------------------------------------------------------------------

.MVN_hz <- function(data, cov = TRUE, tol = 1e-25) {
  # from MVN:::hz
  dataframe <- as.data.frame(data)
  dname <- insight::safe_deparse_symbol(data)
  data <- data[stats::complete.cases(data), ]
  data <- as.matrix(data)
  n <- dim(data)[1]
  p <- dim(data)[2]
  data.org <- data
  if (cov) {
    S <- ((n - 1) / n) * cov(data)
  } else {
    S <- cov(data)
  }
  dif <- scale(data, scale = FALSE)
  Dj <- diag(dif %*% solve(S, tol = tol) %*% t(dif))
  Y <- data %*% solve(S, tol = tol) %*% t(data)
  Djk <- -2 * t(Y) + matrix(diag(t(Y))) %*% matrix(rep(1, n), 1, n) + matrix(rep(1, n), n, 1) %*% diag(t(Y))
  b <- 1 / (sqrt(2)) * ((2 * p + 1) / 4)^(1 / (p + 4)) * (n^(1 / (p + 4)))
  if (qr(S)$rank == p) {
    HZ <- n * (1 / (n^2) * sum(sum(exp(-(b^2) / 2 * Djk))) - 2 * ((1 + (b^2))^(-p / 2)) * (1 / n) * (sum(exp(-((b^2) / (2 * (1 + (b^2)))) * Dj))) + ((1 + (2 * (b^2)))^(-p / 2)))
  } else {
    HZ <- n * 4
  }
  wb <- (1 + b^2) * (1 + 3 * b^2)
  a <- 1 + 2 * b^2
  mu <- 1 - a^(-p / 2) * (1 + p * b^2 / a + (p * (p + 2) * (b^4)) / (2 * a^2))
  si2 <- 2 * (1 + 4 * b^2)^(-p / 2) + 2 * a^(-p) * (1 + (2 * p * b^4) / a^2 + (3 * p * (p + 2) * b^8) / (4 * a^4)) -
    4 * wb^(-p / 2) * (1 + (3 * p * b^4) / (2 * wb) + (p * (p + 2) * b^8) / (2 * wb^2))
  pmu <- log(sqrt(mu^4 / (si2 + mu^2)))
  psi <- sqrt(log((si2 + mu^2) / mu^2))
  pValue <- 1 - stats::plnorm(HZ, pmu, psi)
  MVN <- ifelse(pValue > 0.05, "YES", "NO")
  cbind.data.frame(Test = "Henze-Zirkler", HZ = HZ, `p value` = pValue, MVN = MVN)
}
