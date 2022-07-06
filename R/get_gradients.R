# Tests
# data(iris)
# iris$w <- rnorm(nrow(iris), 1, .3)

# x <- lm(Sepal.Length ~ Species * Sepal.Width, data=iris)
# all(sandwich::estfun(x) == .get_gradients(x))

# x <- lm(Sepal.Length ~ Species * Sepal.Width + Petal.Length * Petal.Width, weights = w, data=iris)
# all(sandwich::estfun(x) == .get_gradients(x))

# x <- glm(vs ~ mpg, data=mtcars, weights = wt)
# all(sandwich::estfun(x) == .get_gradients(x))

.get_gradients <- function(x, ...) {
  UseMethod(".get_gradients")
}


.get_gradients.default <- function(x, ...) {
  insight::check_if_installed("sandwich")
  tryCatch(
    {
      sandwich::estfun(x, ...)
    },
    error = function(e) {
      stop(insight::format_message(
        paste0("Could not compute gradients from a model object of class '", class(x)[1], "'."),
        "Please try a different test-function, or file an issue at https://github.com/easystats/performance/issues."
      ), call. = FALSE)
    }
  )
}


.get_gradients.lmerMod <- function(x, ...) {
  insight::get_residuals(x) * insight::get_weights(x, null_as_ones = TRUE) * insight::get_modelmatrix(x)
}


.get_gradients.glmerMod <- function(x, ...) {
  w <- as.vector(insight::get_residuals(x, "working")) * insight::get_weights(x, "working")
  w * insight::get_modelmatrix(x) / insight::get_auxiliary(x, type = "dispersion")
}


.get_gradients.glmmTMB <- function(x, ...) {
  if (insight::model_info(x)$is_linear) {
    insight::get_residuals(x) * insight::get_weights(x, null_as_ones = TRUE) * insight::get_modelmatrix(x)
  } else {
    w <- as.vector(insight::get_residuals(x)) * insight::get_weights(x, null_as_ones = TRUE)
    w * insight::get_modelmatrix(x) / insight::get_auxiliary(x, type = "dispersion")
  }
}


# .get_gradients.lm <- .get_gradients.lmer
#
# .get_gradients.glm <- function(x, ...) {
#   w <- as.vector(residuals(x, "working")) * weights(x, "working")
#   if (substr(x$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) {
#     dispersion <- 1
#   } else{
#     dispersion <- sum(w^2, na.rm = TRUE)/sum(weights(x, "working"), na.rm = TRUE)
#   }
#   rez <- w * insight::get_modelmatrix(x) / dispersion
#   rez
# }
#
