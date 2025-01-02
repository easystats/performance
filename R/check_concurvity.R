#' @rdname check_collinearity
#' @export
check_concurvity <- function(x, ...) {
  UseMethod("check_concurvity")
}


#' @export
check_concurvity.gam <- function(x, ...) {
  insight::check_if_installed("mgcv")
  conc <- as.data.frame(mgcv::concurvity(x))

  # only smooth terms
  smooth_terms <- colnames(conc)[grepl("s\\((.*)\\)", colnames(conc))]

  out <- data.frame(
    Term = c("Parametric", smooth_terms),
    VIF = as.vector(1 / (1 - as.numeric(conc[1, ]))),
    VIF_proportion = as.vector(as.numeric(conc[3, ])),
    stringsAsFactors = FALSE
  )

  class(out) <- c("check_concurvity", "see_check_concurvity", class(out))
  out
}


# methods ---------------------------------

#' @export
print.check_concurvity <- function(x, ...) {
  vifs <- x$VIF

  x$VIF <- sprintf("%.2f", x$VIF)
  x$VIF_proportion <- sprintf("%.2f", x$VIF_proportion)

  colnames(x)[3] <- "VIF %"

  low_corr <- which(vifs < 5)
  if (length(low_corr)) {
    cat("\n")
    insight::print_color("Low Concurvity\n\n", "green")
    print.data.frame(x[low_corr, ], row.names = FALSE)
  }

  mid_corr <- which(vifs >= 5 & vifs < 10)
  if (length(mid_corr)) {
    cat("\n")
    insight::print_color("Moderate Concurvity\n\n", "yellow")
    print.data.frame(x[mid_corr, ], row.names = FALSE)
  }

  high_corr <- which(vifs >= 10)
  if (length(high_corr)) {
    cat("\n")
    insight::print_color("High Concurvity\n\n", "red")
    print.data.frame(x[high_corr, ], row.names = FALSE)
  }
}
