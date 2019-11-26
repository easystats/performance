# trim leading / trailing whitespaces
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)





#' @importFrom stats AIC
.get_AIC <- function(x) {
  if (inherits(x, c("vgam", "vglm"))) {
    if (!requireNamespace("VGAM", quietly = TRUE)) {
      warning("Package 'VGAM' required for this function work. Please install it.", call. = FALSE)
      return(NULL)
    }
    VGAM::AIC(x)
  } else {
    stats::AIC(x)
  }
}




#' @importFrom stats BIC
.get_BIC <- function(x) {
  if (inherits(x, c("vgam", "vglm"))) {
    if (!requireNamespace("VGAM", quietly = TRUE)) {
      warning("Package 'VGAM' required for this function work. Please install it.", call. = FALSE)
      return(NULL)
    }
    VGAM::BIC(x)
  } else {
    stats::BIC(x)
  }
}




# safe deparse, works for very long strings
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = "")
}



# is string empty?
.is_empty_object <- function(x) {
  if (is.list(x)) {
    x <- tryCatch(
      {
        .compact_list(x)
      },
      error = function(x) {
        x
      }
    )
  }
  # this is an ugly fix because of ugly tibbles
  if (inherits(x, c("tbl_df", "tbl"))) x <- as.data.frame(x)
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}




# has object an element with given name?
.obj_has_name <- function(x, name) {
  name %in% names(x)
}




#' @importFrom stats na.omit sd
.std <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }

  # remove missings
  tmp <- stats::na.omit(x)

  # standardize
  tmp <- (tmp - mean(tmp)) / stats::sd(tmp)

  # and fill in values in original vector
  x[!is.na(x)] <- tmp

  x
}




# recode numeric vector, so lowest value stats with 0
# factors are coerced to numeric
.recode_to_zero <- function(x) {
  # check if factor
  if (is.factor(x) || is.character(x)) {
    # try to convert to numeric
    x <- .factor_to_numeric(x)
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}




# safe conversion from factor to numeric
#' @importFrom stats na.omit
.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}




# remove NULL elements from lists
.compact_list <- function(x, remove_na = FALSE) {
  if (remove_na) {
    x[!sapply(x, function(i) length(i) == 0 || is.null(i) || (length(i) == 1 & is.na(i)) || any(i == "NULL"))]
  } else {
    x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]
  }
}





# remove column
.remove_column <- function(data, variables) {
  data[, -which(colnames(data) %in% variables), drop = FALSE]
}



.remove_backticks_from_parameter_names <- function(x) {
  if (is.data.frame(x) && "Parameter" %in% colnames(x)) {
    x$Parameter <- gsub("`", "", x$Parameter, fixed = TRUE)
  }
  x
}
