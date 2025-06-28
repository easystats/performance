#' @title Describe Properties of Item Scales
#' @name check_itemscale
#'
#' @description Compute various measures of internal consistencies
#' applied to (sub)scales, which items were extracted using
#' `parameters::principal_components()` or `parameters::factor_analysis()`.
#'
#' @param x An object of class `parameters_pca`, as returned by
#' [`parameters::principal_components()`], of class `parameters_efa`, as
#' returned by [`parameters::factor_analysis()`], or a data frame.
#' @param factor_index If `x` is a data frame, `factor_index` must be specified.
#' It must be a numeric vector of same length as number of columns in `x`, where
#' each element is the index of the factor to which the respective column in `x`.
#' @inheritParams item_omega
#'
#' @return A list of data frames, with related measures of internal
#' consistencies of each subscale.
#'
#' @details
#'
#' `check_itemscale()` calculates various measures of internal consistencies,
#' such as Cronbach's alpha, item difficulty or discrimination etc. on subscales
#' which were built from several items. Subscales are retrieved from the results
#' of [`parameters::principal_components()`] or
#' [`parameters::factor_analysis()`], i.e. based on how many components were
#' extracted from the PCA, respectively how many factors were extracted from the
#' factor analysis. `check_itemscale()` retrieves those variables that belong to
#' a component and calculates the above mentioned measures.
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
#'
#' @examplesIf require("parameters") && require("psych")
#' # data generation from '?prcomp', slightly modified
#' C <- chol(S <- toeplitz(0.9^(0:15)))
#' set.seed(17)
#' X <- matrix(rnorm(1600), 100, 16)
#' Z <- X %*% C
#'
#' pca <- parameters::principal_components(
#'   as.data.frame(Z),
#'   rotation = "varimax",
#'   n = 3
#' )
#' pca
#' check_itemscale(pca)
#'
#' # as data frame
#' check_itemscale(
#'   as.data.frame(Z),
#'   factor_index = parameters::closest_component(pca)
#' )
#' @export
check_itemscale <- function(x,
                            factor_index = NULL,
                            reverse_items = NULL,
                            verbose = TRUE) {
  if (!inherits(x, c("parameters_pca", "parameters_efa", "data.frame"))) {
    insight::format_error(
      "`x` must be an object of class `parameters_pca`, as returned by `parameters::principal_components()`, an object of class `parameters_efa`, as returned by `parameters::factor_analysis()`, or a data frame." # nolint
    )
  }

  # save information
  is_pca_or_efa <- inherits(x, c("parameters_pca", "parameters_efa"))

  # if data frame, we need `factor_index`
  if (inherits(x, "data.frame") && !is_pca_or_efa) {
    if (is.null(factor_index)) {
      insight::format_error("If `x` is a data frame, `factor_index` must be specified.")
    }
    if (!is.numeric(factor_index)) {
      insight::format_error("`factor_index` must be numeric.")
    }
    if (length(factor_index) != ncol(x)) {
      insight::format_error(
        "`factor_index` must be of same length as number of columns in `x`.",
        "Each element of `factor_index` must be the index of the factor to which the respective column in `x` belongs to." # nolint
      )
    }
  }

  # factor_index must be a named vector (column names as names)
  if (!is.null(factor_index) && is.null(names(factor_index)) && !is_pca_or_efa) {
    factor_index <- stats::setNames(factor_index, colnames(x))
  }

  # assign data and factor index
  if (is_pca_or_efa) {
    insight::check_if_installed("parameters")
    dataset <- attributes(x)$dataset
    subscales <- parameters::closest_component(x)
  } else {
    dataset <- x
    subscales <- factor_index
  }

  # should some items be reversed?
  if (!is.null(reverse_items)) {
    # check if the object has an attribute "reverse_items" - if so, warn that
    # we don't want to "double reverse" items
    if ("reverse_items" %in% names(attributes(x))) {
      insight::format_warning(
        "It seem that items have already been reversed in this data set. Make sure that you do not reverse them again."
      )
    }
    # numeric indices should be replaced by their column names
    if (is.numeric(reverse_items)) {
      reverse_items <- colnames(dataset)[reverse_items]
    }
    if (verbose) {
      insight::format_alert("Reversing items: ", toString(reverse_items))
    }
    # reverse the items
    dataset <- datawizard::reverse_scale(dataset, reverse_items, verbose = verbose)
  }

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
      Missings = vapply(items, function(i) sum(is.na(i)) / nrow(items), numeric(1)),
      Mean = vapply(items, mean, numeric(1), na.rm = TRUE),
      SD = vapply(items, stats::sd, numeric(1), na.rm = TRUE),
      Skewness = vapply(items, function(i) as.numeric(datawizard::skewness(i)), numeric(1)),
      Difficulty = item_difficulty(items)$Difficulty,
      Discrimination = .item_discr,
      `alpha if deleted` = .item_alpha,
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
    zap_small = TRUE,
    ...
  ))
}


#' @export
print_html.check_itemscale <- function(x, digits = 2, ...) {
  x <- lapply(seq_along(x), function(i) {
    out <- x[[i]]
    attr(out, "table_caption") <- sprintf(
      "Component %i: Mean inter-item-correlation = %.3f, Cronbach's alpha = %.3f",
      i,
      attributes(out)$item_intercorrelation,
      attributes(out)$cronbachs_alpha
    )
    out
  })
  insight::export_table(
    x,
    caption = "Description of (Sub-)Scales",
    digits = digits,
    format = "html",
    missing = "<NA>",
    zap_small = TRUE
  )
}
