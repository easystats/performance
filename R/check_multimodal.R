#' Check if a distribution is unimodal or multimodal
#'
#' For univariate distributions (one-dimensional vectors), this functions
#' performs a Ameijeiras-Alonso et al. (2018) excess mass test. For multivariate
#' distributions (data frames), it uses mixture modelling. However, it seems that
#' it always returns a significant result (suggesting that the distribution is
#' multimodal). A better method might be needed here.
#'
#'
#' @param x A numeric vector or a data frame.
#' @param ... Arguments passed to or from other methods.
#'
#' @examplesIf require("multimode") && require("mclust")
#' \donttest{
#' # Univariate
#' x <- rnorm(1000)
#' check_multimodal(x)
#'
#' x <- c(rnorm(1000), rnorm(1000, 2))
#' check_multimodal(x)
#'
#' # Multivariate
#' m <- data.frame(
#'   x = rnorm(200),
#'   y = rbeta(200, 2, 1)
#' )
#' plot(m$x, m$y)
#' check_multimodal(m)
#'
#' m <- data.frame(
#'   x = c(rnorm(100), rnorm(100, 4)),
#'   y = c(rbeta(100, 2, 1), rbeta(100, 1, 4))
#' )
#' plot(m$x, m$y)
#' check_multimodal(m)
#' }
#' @references
#' - Ameijeiras-Alonso, J., Crujeiras, R. M., and Rodríguez-Casal, A. (2019).
#' Mode testing, critical bandwidth and excess mass. Test, 28(3), 900-919.
#'
#' @export
check_multimodal <- function(x, ...) {
  UseMethod("check_multimodal")
}

#' @export
check_multimodal.data.frame <- function(x, ...) {
  insight::check_if_installed("mclust")

  mclustBIC <- mclust::mclustBIC # this is needed as it is internally required by the following functions
  model <- mclust::Mclust(x, verbose = FALSE)
  model_h0 <- mclust::Mclust(x, G = 1, verbose = FALSE)

  # Parametric
  loglik <- stats::logLik(model)
  loglik0 <- stats::logLik(model_h0)
  rez <- list(Chisq = as.numeric(loglik - loglik0), df = attributes(loglik)$df - 2)
  rez$p <- 1 - stats::pchisq(rez$Chisq, df = rez$df)

  # Text
  text <- "The parametric mixture modelling test suggests that "
  if (rez$p < 0.05) {
    text <- paste0(
      text,
      "the multivariate distribution is significantly multimodal (Chi2(",
      insight::format_value(rez$df, protect_integers = TRUE),
      ") = ",
      insight::format_value(rez$Chisq),
      ", ", insight::format_p(rez$p), ").\n"
    )
    color <- "green"
  } else {
    text <- paste0(
      text,
      "the hypothesis of a multimodal multivariate distribution cannot be rejected (Chi2(",
      insight::format_value(rez$df, protect_integers = TRUE),
      ") = ",
      insight::format_value(rez$Chisq),
      ", ", insight::format_p(rez$p), ").\n"
    )
    color <- "yellow"
  }


  attr(rez, "text") <- insight::format_message(text)
  attr(rez, "color") <- color
  attr(rez, "title") <- "Is the data multimodal?"
  class(rez) <- c("easystats_check", class(rez))

  rez
}



#' @export
check_multimodal.numeric <- function(x, ...) {
  insight::check_if_installed("multimode")

  rez <- multimode::modetest(x, mod0 = 1, method = "ACR")
  rez <- list(p = rez$p.value, excess_mass = rez$statistic)

  text <- "The Ameijeiras-Alonso et al. (2018) excess mass test suggests that "

  if (rez$p < 0.05) {
    text <- paste0(
      text,
      "the distribution is significantly multimodal (excess mass = ",
      insight::format_value(rez$excess_mass),
      ", ", insight::format_p(rez$p), ").\n"
    )
    color <- "green"
  } else {
    text <- paste0(
      text,
      "the hypothesis of a multimodal distribution cannot be rejected (excess mass = ",
      insight::format_value(rez$excess_mass),
      ", ", insight::format_p(rez$p), ").\n"
    )
    color <- "yellow"
  }

  attr(rez, "text") <- insight::format_message(text)
  attr(rez, "color") <- color
  attr(rez, "title") <- "Is the variable multimodal?"
  class(rez) <- c("easystats_check", class(rez))

  rez
}
