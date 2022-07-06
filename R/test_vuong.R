#' @rdname test_performance
#' @export
test_vuong <- function(...) {
  UseMethod("test_vuong")
}


#' @export
test_vuong.default <- function(..., reference = 1) {
  # Attribute class to list and get names from the global environment
  objects <- insight::ellipsis_info(..., only_models = TRUE)

  # Sanity checks (will throw error if non-valid objects)
  .test_performance_checks(objects)

  # ensure proper object names
  objects <- .check_objectnames(objects, sapply(match.call(expand.dots = FALSE)$`...`, as.character))

  # If a suitable class is found, run the more specific method on it
  if (inherits(objects, c("ListNestedRegressions", "ListNonNestedRegressions", "ListLavaan"))) {
    test_vuong(objects, reference = reference)
  } else {
    stop("The models cannot be compared for some reason :/", call. = FALSE)
  }
}


# TODO: Add a nice printing method with some interpretation (see nonnest2's output)

#' @export
test_vuong.ListNestedRegressions <- function(objects, ...) {
  .test_vuong(objects, nested = TRUE, reference = NULL)
}


#' @export
test_vuong.ListNonNestedRegressions <- function(objects, reference = 1, ...) {
  .test_vuong(objects, nested = FALSE, reference = reference)
}





# -------------------------------------------------------------------------
# Utils -------------------------------------------------------------------
# -------------------------------------------------------------------------
# The code below is adapted from nonnest2: https://github.com/cran/nonnest2/blob/master/R/vuongtest.R
# All credits go to its author
# Testing snippets before each function were added for quick testing, but can be removed later

# TODO: Get rid of dependencies. Currently the functions require:
# - sandwich::estfun()
# - CompQuadForm::imhof()


.test_vuong <- function(objects, nested = FALSE, reference = NULL, ...) {
  out <- data.frame(Omega2 = NA, p_Omega2 = NA, LR = NA, p_LR = NA, stringsAsFactors = FALSE)

  for (i in 2:length(objects)) {
    if (is.null(reference)) {
      ref <- objects[[i - 1]]
    } else {
      ref <- objects[[reference]]
    }
    rez <- .test_vuong_pairs(ref, objects[[i]], nested = nested, adj = "none")
    out <- rbind(
      out,
      data.frame(
        Omega2 = rez$Omega2,
        p_Omega2 = rez$p_Omega2,
        LR = rez$LRTstat,
        p_LR = rez$p_LRT,
        stringsAsFactors = FALSE
      )
    )
  }

  out <- cbind(.test_performance_init(objects), out)

  attr(out, "is_nested") <- nested
  attr(out, "reference") <- reference
  class(out) <- c("test_performance", class(out))

  out
}


# Vuong test for two models -----------------------------------------------
# -------------------------------------------------------------------------


# m1 <- lm(mpg ~ disp, data=mtcars)
# m2 <- lm(mpg ~ drat, data=mtcars)
# ref <- nonnest2::vuongtest(m1, m2, nested=FALSE)
# rez <- .test_vuong(m1, m2, nested=FALSE)
# all(ref$omega == rez$Omega2)
# ref$p_omega == rez$p_Omega2
# ref$LRTstat == rez$LRTstat
#
# m1 <- lm(mpg ~ disp, data=mtcars)
# m2 <- lm(mpg ~ disp + drat, data=mtcars)
# object1 <- m1; object2 <- m2
# ref <- nonnest2::vuongtest(m1, m2, nested=TRUE)
# rez <- .test_vuong(m1, m2, nested=TRUE)
# all(ref$omega == rez$Omega2)
# ref$p_omega == rez$p_Omega2
# ref$LRTstat == rez$LRTstat
#
# ref <- nonnest2::vuongtest(m2, m1, nested=TRUE)
# rez <- .test_vuong(m2, m1, nested=TRUE)
# all(ref$omega == rez$Omega2)
# ref$p_omega == rez$p_Omega2
# ref$LRTstat == rez$LRTstat
.test_vuong_pairs <- function(object1, object2, nested = FALSE, adj = "none") {
  insight::check_if_installed("CompQuadForm")

  # If nested==TRUE, find the full model and reverse if necessary
  if (nested) {
    dfs <- c(insight::get_df(object1, type = "residual"), insight::get_df(object2), type = "residual")
    if (order(dfs)[1] == 2) {
      temp <- object2
      object2 <- object1
      object1 <- temp
    }
  }

  # Get individual log-likelihoods
  llA <- attributes(insight::get_loglikelihood(object1))$per_obs
  llB <- attributes(insight::get_loglikelihood(object2))$per_obs


  # DISTINGUISABILITY TEST --------
  # Eq (4.2)
  n <- insight::n_obs(object1)
  omega_hat_2 <- (n - 1) / n * stats::var(llA - llB, na.rm = TRUE)

  # Get p-value of weighted chi-square dist
  lamstar <- .test_vuong_lambda(object1, object2)

  # Note: dr package requires non-negative weights, which does not help when nested==TRUE
  # tmp <- dr::dr.pvalue(lamstar^2, n * omega_hat_2)
  # pOmega <- tmp[[4]]
  p <- suppressWarnings(CompQuadForm::imhof(n * omega_hat_2, lamstar^2)$Qq)

  # ROBUST LRTEST -----------
  # Calculate likelihood ratio; Eq (6.4)
  lr <- sum(llA - llB, na.rm = TRUE)

  # Adjustments to likelihood ratio
  if (adj != "none") {
    # FIXME lavaan equality constraints; use df instead?
    nparA <- insight::n_parameters(object1, effects = "fixed")
    nparB <- insight::n_parameters(object2, effects = "fixed")

    if (adj == "aic") {
      lr <- lr - (nparA - nparB)
    }
    if (adj == "bic") {
      lr <- lr - (nparA - nparB) * log(n) / 2
    }
  }


  # Null distribution and test stat depends on nested
  if (nested) {
    teststat <- 2 * lr
    p_LRTA <- CompQuadForm::imhof(teststat, -lamstar)[[1]]
    p_LRTB <- NA
  } else {
    teststat <- (1 / sqrt(n)) * lr / sqrt(omega_hat_2)

    ## Two 1-tailed p-values from a normal:
    p_LRTA <- stats::pnorm(teststat, lower.tail = FALSE)
    p_LRTB <- stats::pnorm(teststat)
  }

  rval <- list(
    Omega2 = omega_hat_2,
    p_Omega2 = p,
    LRTstat = teststat,
    p_LRT = min(c(p_LRTA, p_LRTB), na.rm = TRUE)
  )
  rval
}






# Compute lambda (Eq 3.6) -------------------------------------------------
# -------------------------------------------------------------------------

# m1 <- lm(Sepal.Length ~ Petal.Width, data=iris)
# m2 <- lm(Sepal.Length ~ Sepal.Width, data=iris)
# ref <- nonnest2:::calcLambda(m1, m2, n=150, score1=NULL, score2=NULL, vc1=stats::vcov, vc2=stats::vcov)
# rez <- .test_vuong_lambda(m1, m2)
# all(ref == rez)
.test_vuong_lambda <- function(model1, model2) {
  # Compute lambda (Eq 3.6)

  # Get AB
  AB1 <- .test_vuong_AB(model1)
  AB2 <- .test_vuong_AB(model2)

  # Eq (2.7)
  Bc <- crossprod(AB1$sc, AB2$sc) / AB1$n

  W <- cbind(
    rbind(
      -AB1$B %*% chol2inv(chol(AB1$A)),
      t(Bc) %*% chol2inv(chol(AB1$A))
    ),
    rbind(
      -Bc %*% chol2inv(chol(AB2$A)),
      AB2$B %*% chol2inv(chol(AB2$A))
    )
  )

  lamstar <- eigen(W, only.values = TRUE)$values
  # Discard imaginary part, as it only occurs for tiny eigenvalues?
  Re(lamstar)
}





# Compute AB (Eq 2.1 and 2.2) ---------------------------------------------
# -------------------------------------------------------------------------

# m <- lm(Sepal.Length ~ Petal.Width * Species, data=iris)
# ref <- nonnest2:::calcAB(m, n=150, scfun = NULL, vc = stats::vcov)
# rez <- .test_vuong_AB(m)
# all(ref$A == rez$A)
# all(ref$B == rez$B)
# all(ref$sc == rez$sc)
#
# structure <- " visual  =~ x1 + x2 + x3
#                  textual =~ x4 + x5 + x6
#                  speed   =~ x7 + x8 + x9
#
#                   visual ~~ textual + speed "
# model <- lavaan::sem(structure, data = lavaan::HolzingerSwineford1939)
# ref <- nonnest2:::calcAB(model, n=insight::n_obs(model), scfun = NULL, vc = lavaan::vcov)
# rez <- .test_vuong_AB(model)
# all(ref$A == rez$A)
# all(ref$B == rez$B)
# all(ref$sc == rez$sc)
.test_vuong_AB <- function(model) {
  # A, B as defined in Vuong Eq (2.1) and (2.2)

  n <- insight::n_obs(model)

  # Get A (Eq 2.1)
  if (inherits(model, "lavaan")) {
    insight::check_if_installed("lavaan")

    covmat <- lavaan::vcov(model) # model@vcov$vcov
    duplicates <- duplicated(colnames(covmat))
    covmat <- n * covmat[!duplicates, !duplicates]
    scaling <- 1
  } else {
    scaling <- insight::get_sigma(model, ci = NULL, verbose = FALSE)^2
    if (is.null(scaling) || is.na(scaling)) scaling <- 1
    covmat <- n * insight::get_varcov(model, component = "conditional")
  }

  A <- chol2inv(chol(covmat))

  # Get B (Eq 2.2)
  sc <- (1 / scaling) * .get_gradients(model)
  sc.cp <- crossprod(sc) / n
  B <- matrix(sc.cp, nrow(A), nrow(A))

  list(A = A, B = B, sc = sc, n = n)
}
