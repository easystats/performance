#' @title Describe Properties of Item Scales
#' @name check_itemscale
#'
#' @description Compute various measures of internal consistencies
#'   applied to (sub)scales, which items were extracted using
#'   `parameters::principal_components()`.
#'
#' @param x An object of class `parameters_pca`, as returned by
#'   [`parameters::principal_components()`].
#'
#' @return A list of data frames, with related measures of internal
#'   consistencies of each subscale.
#'
#' @details
#'
#' `check_itemscale()` calculates various measures of internal
#' consistencies, such as Cronbach's alpha, item difficulty or discrimination
#' etc. on subscales which were built from several items. Subscales are
#' retrieved from the results of [`parameters::principal_components()`], i.e.
#' based on how many components were extracted from the PCA,
#' `check_itemscale()` retrieves those variables that belong to a component
#' and calculates the above mentioned measures.
#'
#' @note
#' - *Item difficulty* should range between 0.2 and 0.8. Ideal value
#'   is `p+(1-p)/2` (which mostly is between 0.5 and 0.8). See
#'   [`item_difficulty()`] for details.
#'
#' - For *item discrimination*, acceptable values are 0.20 or higher;
#'   the closer to 1.00 the better. See [`item_reliability()`] for more
#'   details.
#'
#' - In case the total *Cronbach's alpha* value is below the acceptable
#'   cut-off of 0.7 (mostly if an index has few items), the
#'   *mean inter-item-correlation* is an alternative measure to indicate
#'   acceptability. Satisfactory range lies between 0.2 and 0.4. See also
#'   [`item_intercor()`].
#'
#' @references
#' - Briggs SR, Cheek JM (1986) The role of factor analysis in the development
#'   and evaluation of personality scales. Journal of Personality, 54(1),
#'   106-148. doi: 10.1111/j.1467-6494.1986.tb00391.x
#' - Trochim WMK (2008) Types of Reliability.
#'   ([web](https://conjointly.com/kb/types-of-reliability/))
#'
#' @examples
#' # data generation from '?prcomp', slightly modified
#' C <- chol(S <- toeplitz(.9^(0:15)))
#' set.seed(17)
#' X <- matrix(rnorm(1600), 100, 16)
#' Z <- X %*% C
#' if (require("parameters") && require("psych")) {
#'   pca <- principal_components(as.data.frame(Z), rotation = "varimax", n = 3)
#'   pca
#'   check_itemscale(pca)
#' }
#' @export
check_itemscale <- function(x) {
  if (!inherits(x, "parameters_pca")) {
    insight::format_error(
      "`x` must be an object of class `parameters_pca`, as returned by `parameters::principal_components()`."
    )
  }

  insight::check_if_installed("parameters")

  dataset <- attributes(x)$dataset

  ## TODO: remove once parameters 0.18.3 or higher on CRAN
  # backward compatibility to parameters 0.18.2
  if (is.null(dataset)) {
    dataset <- attributes(x)$data_set
  }

  subscales <- parameters::closest_component(x)

  out <- lapply(sort(unique(subscales)), function(.subscale) {
    columns <- names(subscales)[subscales == .subscale]
    items <- dataset[columns]
    reliability <- item_reliability(items)

    .item_discr <- reliability$item_discrimination
    if (is.null(.item_discr)) .item_discr <- NA
    .item_alpha <- reliability$alpha_if_deleted
    if (is.null(.item_alpha)) .item_alpha <- NA

    s_out <- data.frame(
      Item = columns,
      Missings = sapply(items, function(i) sum(is.na(i)) / nrow(items)),
      Mean = sapply(items, mean, na.rm = TRUE),
      SD = sapply(items, stats::sd, na.rm = TRUE),
      Skewness = sapply(items, function(i) as.numeric(datawizard::skewness(i))),
      "Difficulty" = item_difficulty(items)$difficulty,
      "Discrimination" = .item_discr,
      "alpha if deleted" = .item_alpha,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    attr(s_out, "item_intercorrelation") <- item_intercor(items)
    attr(s_out, "cronbachs_alpha") <- cronbachs_alpha(items)

    s_out
  })

  class(out) <- unique(c("check_itemscale", class(out)))
  out
}



# methods -------------------------------------

#' @export
print.check_itemscale <- function(x, digits = 2, ...) {
  insight::print_color("# Description of (Sub-)Scales", "blue")

  cat(insight::export_table(
    lapply(seq_along(x), function(i) {
      out <- x[[i]]
      attr(out, "table_caption") <- c(sprintf("\nComponent %i", i), "red")
      attr(out, "table_footer") <- c(sprintf(
        "\nMean inter-item-correlation = %.3f  Cronbach's alpha = %.3f",
        attributes(out)$item_intercorrelation,
        attributes(out)$cronbachs_alpha
      ), "yellow")

      out
    }),
    digits = digits,
    format = "text",
    missing = "<NA>",
    zap_small = TRUE
  ))
}
