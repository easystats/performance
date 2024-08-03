#' @title Check correct model adjustment for identifying causal effects
#' @name check_dag
#'
#' @description Check correct model adjustment for identifying causal effects.
#'
#' @export
check_dag <- function(...,
                      outcome = NULL,
                      exposure = NULL,
                      adjusted = NULL,
                      effect = c("all", "total", "direct")) {
  insight::check_if_installed(
    c("ggdag", "dagitty"),
    reason = "to check correct adjustments for identifying causal effects."
  )

  effect <- match.arg(effect)

  # retrieve formulas
  formulas <- list(...)

  # check if first object is a model object, and convert to formula
  if (insight::is_regression_model(formulas[[1]])) {
    vars <- insight::find_variables(
      formulas[[1]],
      effects = "fixed",
      component = "conditional",
      flatten = FALSE
    )
    formulas[[1]] <- stats::as.formula(
      paste(vars$response, "~", paste(vars$conditional, collapse = "+"))
    )
  }

  # if outcome is not set, use first dependent variable
  if (is.null(outcome)) {
    outcome <- insight::find_response(formulas[[1]])
  }

  # if exposure is not set, use first independent variable
  if (is.null(exposure)) {
    exposure <- insight::find_variables(
      formulas[[1]],
      effects = "fixed",
      component = "conditional",
      flatten = FALSE
    )$conditional[1]
  }

  # convert to dag
  dag_args <- c(formulas, list(exposure = exposure, outcome = outcome))
  dag <- do.call(ggdag::dagify, dag_args)

  # add adjustments
  if (!is.null(adjusted)) {
    dag <- .adjust_dag(dag, adjusted)
  }

  # data for checking effects
  checks <- lapply(c("direct", "total"), function(x) {
    adjustment_set <- unlist(dagitty::adjustmentSets(dag, effect = x), use.names = FALSE)
    adjustment_nodes <- unlist(dagitty::adjustedNodes(dag), use.names = FALSE)
    list(
      adjustment_not_needed = is.null(adjustment_set) && is.null(adjustment_nodes),
      incorrectly_adjusted = is.null(adjustment_set) && !is.null(adjustment_nodes),
      current_adjustments = adjustment_nodes,
      minimal_adjustments = adjustment_set
    )
  })

  attr(dag, "effect") <- effect
  attr(dag, "outcome") <- outcome
  attr(dag, "exposure") <- exposure
  attr(dag, "check_direct") <- insight::compact_list(checks[[1]])
  attr(dag, "check_total") <- insight::compact_list(checks[[2]])

  class(dag) <- c(c("check_dag",  "see_check_dag"), class(dag))

  dag
}


# helper ----------------------------------------------------------------------

.adjust_dag <- function(dag, adjusted) {
  for (i in adjusted) {
    dag <- gsub(paste0("\n", i, "\n"), paste0("\n", i, " [adjusted]\n"), dag)
  }
  dag
}


# methods --------------------------------------------------------------------

#' @export
print.check_dag <- function(x, ...) {
  effect <- attributes(x)$effect

  for (i in c("direct", "total")) {
    if (i == "direct") {
      out <- attributes(x)$check_direct
    } else {
      out <- attributes(x)$check_total
    }

    exposure_outcome_text <- paste0(
      "\n- Outcome: ", attributes(x)$outcome,
      "\n- Exposure", ifelse(length(attributes(x)$exposure) > 1, "s", ""),
      ": ", datawizard::text_concatenate(attributes(x)$exposure)
    )

    # build message with check results for effects -----------------------

    if (isTRUE(out$adjustment_not_needed)) {
      # Scenario 1: no adjustment needed
      msg <- paste0(
        insight::color_text("Model is correctly specified.", "green"),
        exposure_outcome_text,
        "\n\nNo adjustment needed to estimate the ", i, " effect of ",
        datawizard::text_concatenate(attributes(x)$exposure),
        " on ",
        attributes(x)$outcome,
        "."
      )
    } else if (isTRUE(out$incorrectly_adjusted)) {
      # Scenario 2: incorrectly adjusted, adjustments where none is allowed
      msg <- paste0(
        insight::color_text("Incorrectly adjusted!", "red"),
        exposure_outcome_text,
        "\n\nTo estimate the ", i, " effect, do *not* adjust for: ",
        datawizard::text_concatenate(out$current_adjustments),
        "."
      )
    } else if (length(out$current_adjustments) != length(out$minimal_adjustment)) {
      # Scenario 3: missing adjustments
      msg <- paste0(
        insight::color_text("Incorrectly adjusted!", "red"),
        exposure_outcome_text,
        "\n\nTo estimate the ", i, " effect, *also* adjust for: ",
        insight::color_text(datawizard::text_concatenate(out$minimal_adjustments), "yellow"),
        "."
      )
      if (is.null(out$current_adjustments)) {
        msg <- paste0(msg, "\nCurrently, the model does not adjust for any variables.")
      } else {
        msg <- paste0(
          msg, "\nCurrently, the model currently only adjusts for ",
          insight::color_text(datawizard::text_concatenate(out$current_adjustments), "yellow"), "."
        )
      }
    } else {
      # Scenario 4: correct adjustment
      msg <- paste0(
        insight::color_text("Model is correctly specified.", "green"),
        exposure_outcome_text,
        "\n\nAll minimal sufficient adjustments to estimate the ", i, " effect were done."
      )
    }

    if (effect %in% c("all", i)) {
      cat(insight::print_color(insight::format_message(paste0("# Correct adjustments for identifying {.i ", i, "} effects\n\n")), "blue"))
      cat(msg)
      cat("\n\n")
    }
  }
}


#' @export
plot.check_dag <- function(x, size_point = 15, colors = NULL, which = "all", ...) {
  .data <- NULL
  insight::check_if_installed(c("ggdag", "ggplot2", "see"))
  which <- match.arg(which, choices = c("all", "current", "required"))

  p1 <- suppressWarnings(ggdag::ggdag_adjust(x, stylized = TRUE))
  p2 <- suppressWarnings(ggdag::ggdag_adjustment_set(x, shadow = TRUE, stylized = TRUE))

  # tweak data
  p1$data$type <- as.character(p1$data$adjusted)
  p1$data$type[p1$data$name == attributes(x)$outcome] <- "outcome"
  p1$data$type[p1$data$name %in% attributes(x)$exposure] <- "exposure"
  p1$data$type <- factor(p1$data$type, levels = c("outcome", "exposure", "adjusted", "unadjusted"))

  p2$data$type <- as.character(p2$data$adjusted)
  p2$data$type[p2$data$name == attributes(x)$outcome] <- "outcome"
  p2$data$type[p2$data$name %in% attributes(x)$exposure] <- "exposure"
  p2$data$type <- factor(p2$data$type, levels = c("outcome", "exposure", "adjusted", "unadjusted"))

  if (is.null(colors)) {
    point_colors <- see::see_colors(c("yellow", "cyan", "blue grey", "red"))
  } else if (length(colors) != 4) {
    insight::format_error("`colors` must be a character vector with four color-values.")
  } else {
    point_colors <- colors
  }
  names(point_colors) <- c("outcome", "exposure", "adjusted", "unadjusted")

  plot1 <- ggplot2::ggplot(p1$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    see::geom_point_borderless(ggplot2::aes(fill = .data$type), size = size_point) +
    ggdag::geom_dag_edges(
      ggplot2::aes(
        xend = .data$xend,
        yend = .data$yend,
        edge_alpha = .data$adjusted,
      )
    ) +
    ggdag::scale_adjusted() +
    ggdag::geom_dag_label(ggplot2::aes(label = .data$name)) +
    ggdag::theme_dag() +
    ggplot2::scale_fill_manual(values = point_colors) +
    ggplot2::ggtitle("Current model") +
    ggplot2::guides(edge_alpha = "none")

  plot2 <- ggplot2::ggplot(p2$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    see::geom_point_borderless(ggplot2::aes(fill = .data$type), size = size_point) +
    ggdag::geom_dag_edges(
      ggplot2::aes(
        xend = .data$xend,
        yend = .data$yend,
        edge_alpha = .data$adjusted,
      )
    ) +
    ggdag::scale_adjusted() +
    ggdag::geom_dag_label(ggplot2::aes(label = .data$name)) +
    ggdag::theme_dag() +
    ggplot2::scale_fill_manual(values = point_colors) +
    ggplot2::ggtitle("Required model") +
    ggplot2::guides(edge_alpha = "none")

  if (which == "all") {
    # fix legends
    plot2 <- plot2 + ggplot2::theme(legend.position = "none")
    # plot
    see::plots(plot1, plot2, n_rows = 1)
  } else if (which == "current") {
    plot1
  } else {
    plot2
  }
}
