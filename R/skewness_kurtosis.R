.skewness <- function(x) {
  if (is.matrix(x)) {
    apply(x, 2, .skewness_numeric)
  } else if (is.vector(x)) {
    .skewness_numeric(x)
  }
  else if (is.data.frame(x)) {
    sapply(x, .skewness_numeric)
  } else {
    .skewness_numeric(as.vector(x))
  }
}

.skewness_numeric <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2)
}


.kurtosis <- function(x) {
  if (is.matrix(x)) {
    apply(x, 2, .kurtosis_numeric)
  } else if (is.vector(x)) {
    .kurtosis_numeric(x)
  }
  else if (is.data.frame(x)) {
    sapply(x, .kurtosis_numeric)
  } else {
    .kurtosis_numeric(as.vector(x))
  }
}


.kurtosis_numeric <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
}


.normalize <- function(x) {
  as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))
}
