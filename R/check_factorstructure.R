#' Check suitability of data for Factor Analysis (FA) with Bartlett's Test of Sphericity and KMO
#'
#' This checks whether the data is appropriate for Factor Analysis (FA) by
#' running the Bartlett's Test of Sphericity and the Kaiser, Meyer, Olkin (KMO)
#' Measure of Sampling Adequacy (MSA). See **details** below for more information
#' about the interpretation and meaning of each test.
#'
#'
#' @details
#' ### Bartlett's Test of Sphericity
#'
#' Bartlett's (1951) test of sphericity tests whether a matrix (of correlations)
#' is significantly different from an identity matrix (filled with 0). It tests
#' whether the correlation coefficients are all 0. The test computes the
#' probability that the correlation matrix has significant correlations among at
#' least some of the variables in a dataset, a prerequisite for factor analysis
#' to work.
#'
#' While it is often suggested to check whether Bartlett’s test of sphericity is
#' significant before starting with factor analysis, one needs to remember that
#' the test is testing a pretty extreme scenario (that all correlations are non-significant).
#' As the sample size increases, this test tends to be always significant, which
#' makes it not particularly useful or informative in well-powered studies.
#'
#' ### Kaiser, Meyer, Olkin (KMO)
#'
#' *(Measure of Sampling Adequacy (MSA) for Factor Analysis.)*
#'
#' Kaiser (1970) introduced a Measure of Sampling Adequacy (MSA), later modified
#' by Kaiser and Rice (1974). The Kaiser-Meyer-Olkin (KMO) statistic, which can
#' vary from 0 to 1, indicates the degree to which each variable in a set is
#' predicted without error by the other variables.
#'
#' A value of 0 indicates that the sum of partial correlations is large relative
#' to the sum correlations, indicating factor analysis is likely to be
#' inappropriate. A KMO value close to 1 indicates that the sum of partial
#' correlations is not large relative to the sum of correlations and so factor
#' analysis should yield distinct and reliable factors. It means that patterns
#' of correlations are relatively compact, and so factor analysis should yield
#' distinct and reliable factors. Values smaller than 0.5 suggest that you should
#' either collect more data or rethink which variables to include.
#'
#' Kaiser (1974) suggested that KMO > .9 were marvelous, in the .80s,
#' meritorious, in the .70s, middling, in the .60s, mediocre, in the .50s,
#' miserable, and less than .5, unacceptable. Hair et al. (2006) suggest
#' accepting a value > 0.5. Values between 0.5 and 0.7 are mediocre, and values
#' between 0.7 and 0.8 are good.
#'
#' Variables with individual KMO values below 0.5 could be considered for
#' exclusion them from the analysis (note that you would need to re-compute the
#' KMO indices as they are dependent on the whole dataset).
#'
#' @param x A dataframe or a correlation matrix. If the latter is passed, `n`
#'   must be provided.
#' @param n If a correlation matrix was passed, the number of observations must
#'   be specified.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(performance)
#'
#' check_factorstructure(mtcars)
#'
#' # One can also pass a correlation matrix
#' r <- cor(mtcars)
#' check_factorstructure(r, n = nrow(mtcars))
#'
#' @return A list of lists of indices related to sphericity and KMO.
#' @seealso [`check_clusterstructure()`].
#'
#' @references
#' This function is a wrapper around the `KMO` and the `cortest.bartlett()`
#' functions in the **psych** package (Revelle, 2016).
#'
#' - Revelle, W. (2016). How To: Use the psych package for Factor Analysis
#'   and data reduction.
#'
#' - Bartlett, M. S. (1951). The effect of standardization on a Chi-square
#'   approximation in factor analysis. Biometrika, 38(3/4), 337-344.

#' - Kaiser, H. F. (1970). A second generation little jiffy.
#'   Psychometrika, 35(4), 401-415.
#'
#' - Kaiser, H. F., & Rice, J. (1974). Little jiffy, mark IV. Educational
#'   and psychological measurement, 34(1), 111-117.
#'
#' - Kaiser, H. F. (1974). An index of factorial simplicity.
#'   Psychometrika, 39(1), 31-36.
#'
#' @export
check_factorstructure <- function(x, n = NULL, ...) {
  # TODO: detect (and remove?) factors

  kmo <- check_kmo(x, n, ...)
  sphericity <- check_sphericity_bartlett(x, n, ...)

  text <- paste0("\n  - Sphericity: ", attributes(sphericity)$text, "\n  - KMO: ", attributes(kmo)$text)

  if (attributes(kmo)$color == "red" || attributes(sphericity)$color == "red") {
    color <- "red"
  } else {
    color <- "green"
  }

  out <- list(KMO = kmo, sphericity = sphericity)

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Is the data suitable for Factor Analysis?"
  class(out) <- c("easystats_check", class(out))

  out
}





#' @rdname check_factorstructure
#' @export
check_kmo <- function(x, n = NULL, ...) {
  out <- .validate_factor_structure(x, n, ...)

  Q <- solve(out$r)

  Q <- stats::cov2cor(Q)
  diag(Q) <- 0
  diag(out$r) <- 0

  sumQ2 <- sum(Q^2)
  sumr2 <- sum(out$r^2)
  MSA <- sumr2 / (sumr2 + sumQ2)
  MSA_variable <- colSums(out$r^2) / (colSums(out$r^2) + colSums(Q^2))
  out <- list(MSA = MSA, MSA_variable = MSA_variable)

  # TODO: add interpret_kmo in effectsize and use that here for more fine-grained interpretation
  if (MSA < 0.5) {
    text <-
      sprintf(
        "The Kaiser, Meyer, Olkin (KMO) overall measure of sampling adequacy suggests that factor analysis is likely to be inappropriate (KMO = %.2f).",
        MSA
      )
    color <- "red"
  } else {
    text <-
      sprintf(
        "The Kaiser, Meyer, Olkin (KMO) overall measure of sampling adequacy suggests that data seems appropriate for factor analysis (KMO = %.2f).",
        MSA
      )
    color <- "green"
  }

  # Individual scores:
  text_ind <- toString(paste0(
    names(MSA_variable),
    " (",
    insight::format_value(MSA_variable),
    ifelse(MSA_variable < 0.5, "*", ""),
    ")"
  ))

  text <- paste0(text, " The individual KMO scores are: ", text_ind, ".")

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "KMO Measure of Sampling Adequacy"
  class(out) <- c("easystats_check", class(out))

  out
}





#' @rdname check_factorstructure
#' @export
check_sphericity_bartlett <- function(x, n = NULL, ...) {
  out <- .validate_factor_structure(x, n, ...)

  p <- dim(out$r)[2]

  detR <- det(out$r)
  statistic <- -log(detR) * (out$n - 1 - (2 * p + 5) / 6)
  dof <- p * (p - 1) / 2
  pval <- stats::pchisq(statistic, df = dof, lower.tail = FALSE)

  out <- list(chisq = statistic, p = pval, dof = dof)

  if (pval < 0.001) {
    msg_text <- sprintf(
      "Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analysis (Chisq(%i) = %.2f, %s).", # nolint
      dof,
      statistic,
      insight::format_p(pval)
    )
    color <- "green"
  } else {
    msg_text <- sprintf(
      "Bartlett's test of sphericity suggests that there is not enough significant correlation in the data for factor analysis (Chisq(%i) = %.2f, %s).", # nolint
      dof,
      statistic,
      insight::format_p(pval)
    )
    color <- "red"
  }

  attr(out, "text") <- text
  attr(out, "color") <- color
  attr(out, "title") <- "Test of Sphericity"
  class(out) <- c("easystats_check", class(out))

  out
}



# Helpers -----------------------------------------------------------------

#' @keywords internal
.validate_factor_structure <- function(x, n = NULL, ...) {
  if (is.null(n)) {
    r <- stats::cor(x, use = "pairwise.complete.obs", ...)
    n <- nrow(x)
  } else {
    r <- x
  }

  if (nrow(r) != ncol(r)) {
    insight::format_error("The correlation matrix is not square.")
  }

  return(list(n = n, r = r))
}
