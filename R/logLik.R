#' @exportS3Method logLik ivreg
logLik.ivreg <- function(object, ...) {
  res <- object$residuals
  p <- object$rank
  w <- object$weights

  N <- length(res)

  if (is.null(w)) {
    w <- rep.int(1, N)
  } else {
    excl <- w == 0
    if (any(excl)) {
      res <- res[!excl]
      N <- length(res)
      w <- w[!excl]
    }
  }
  N0 <- N

  val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  attr(val, "nall") <- N0
  attr(val, "nobs") <- N
  attr(val, "df") <- p + 1
  class(val) <- "logLik"

  val
}


#' @exportS3Method logLik plm
logLik.plm <- function(object, ...) {
  res <- object$residuals
  w <- object$weights

  N <- length(res)

  if (is.null(w)) {
    w <- rep.int(1, N)
  } else {
    excl <- w == 0
    if (any(excl)) {
      res <- res[!excl]
      N <- length(res)
      w <- w[!excl]
    }
  }
  N0 <- N

  val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  attr(val, "nall") <- N0
  attr(val, "nobs") <- N
  attr(val, "df") <- insight::get_df(object, type = "model")
  class(val) <- "logLik"

  val
}

#' @exportS3Method logLik cpglm
logLik.cpglm <- logLik.plm



#' @exportS3Method logLik iv_robust
logLik.iv_robust <- function(object, ...) {
  res <- insight::get_residuals(object)
  p <- object$rank
  w <- object$weights

  N <- length(res)

  if (is.null(w)) {
    w <- rep.int(1, N)
  } else {
    excl <- w == 0
    if (any(excl)) {
      res <- res[!excl]
      N <- length(res)
      w <- w[!excl]
    }
  }
  N0 <- N

  val <- 0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) + log(sum(w * res^2))))

  attr(val, "nall") <- N0
  attr(val, "nobs") <- N
  attr(val, "df") <- p + 1
  class(val) <- "logLik"

  val
}




#' @exportS3Method logLik svycoxph
logLik.svycoxph <- function(object, ...) {
  val <- object$ll[2]
  attr(val, "nall") <- insight::n_obs(object)
  attr(val, "nobs") <- insight::n_obs(object)
  attr(val, "df") <- object$degf.resid
  class(val) <- "logLik"

  val
}
