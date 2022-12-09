#' \code{performance-package}
#'
#' @title performance: An R Package for Assessment, Comparison and Testing of
#' Statistical Models
#'
#' @description
#'
#' A crucial aspect when building regression models is to evaluate the
#' quality of modelfit. It is important to investigate how well models fit
#' to the data and which fit indices to report. Functions to create
#' diagnostic plots or to compute fit measures do exist, however, mostly
#' spread over different packages. There is no unique and consistent
#' approach to assess the model quality for different kind of models.
#'
#' The primary goal of the **performance** package is to fill this gap and
#' to provide utilities for computing **indices of model quality** and
#' **goodness of fit**. These include measures like r-squared (R2), root
#' mean squared error (RMSE) or intraclass correlation coefficient (ICC),
#' but also functions to check (mixed) models for overdispersion,
#' zero-inflation, convergence or singularity.
#'
#' References: Lüdecke et al. (2021) \doi{10.21105/joss.03139}
#'
#' @docType package
#' @name performance-package
#' @keywords internal
"_PACKAGE"