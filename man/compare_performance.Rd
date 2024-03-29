% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_performance.R
\name{compare_performance}
\alias{compare_performance}
\title{Compare performance of different models}
\usage{
compare_performance(
  ...,
  metrics = "all",
  rank = FALSE,
  estimator = "ML",
  verbose = TRUE
)
}
\arguments{
\item{...}{Multiple model objects (also of different classes).}

\item{metrics}{Can be \code{"all"}, \code{"common"} or a character vector of
metrics to be computed. See related
\code{\link[=model_performance]{documentation()}} of object's class for
details.}

\item{rank}{Logical, if \code{TRUE}, models are ranked according to 'best'
overall model performance. See 'Details'.}

\item{estimator}{Only for linear models. Corresponds to the different
estimators for the standard deviation of the errors. If \code{estimator = "ML"}
(default, except for \code{performance_aic()} when the model object is of class
\code{lmerMod}), the scaling is done by \code{n} (the biased ML estimator), which is
then equivalent to using \code{AIC(logLik())}. Setting it to \code{"REML"} will give
the same results as \code{AIC(logLik(..., REML = TRUE))}.}

\item{verbose}{Toggle warnings.}
}
\value{
A data frame with one row per model and one column per "index" (see
\code{metrics}).
}
\description{
\code{compare_performance()} computes indices of model
performance for different models at once and hence allows comparison of
indices across models.
}
\details{
\subsection{Model Weights}{
When information criteria (IC) are requested in \code{metrics} (i.e., any of \code{"all"},
\code{"common"}, \code{"AIC"}, \code{"AICc"}, \code{"BIC"}, \code{"WAIC"}, or \code{"LOOIC"}), model
weights based on these criteria are also computed. For all IC except LOOIC,
weights are computed as \code{w = exp(-0.5 * delta_ic) / sum(exp(-0.5 * delta_ic))},
where \code{delta_ic} is the difference between the model's IC value and the
smallest IC value in the model set (Burnham and Anderson, 2002).
For LOOIC, weights are computed as "stacking weights" using
\code{\link[loo:loo_model_weights]{loo::stacking_weights()}}.
}

\subsection{Ranking Models}{
When \code{rank = TRUE}, a new column \code{Performance_Score} is returned.
This score ranges from 0\\% to 100\\%, higher values indicating better model
performance. Note that all score value do not necessarily sum up to 100\\%.
Rather, calculation is based on normalizing all indices (i.e. rescaling
them to a range from 0 to 1), and taking the mean value of all indices for
each model. This is a rather quick heuristic, but might be helpful as
exploratory index.
\cr \cr
In particular when models are of different types (e.g. mixed models,
classical linear models, logistic regression, ...), not all indices will be
computed for each model. In case where an index can't be calculated for a
specific model type, this model gets an \code{NA} value. All indices that
have any \code{NA}s are excluded from calculating the performance score.
\cr \cr
There is a \code{plot()}-method for \code{compare_performance()},
which creates a "spiderweb" plot, where the different indices are
normalized and larger values indicate better model performance.
Hence, points closer to the center indicate worse fit indices
(see \href{https://easystats.github.io/see/articles/performance.html}{online-documentation}
for more details).
}

\subsection{REML versus ML estimator}{
By default, \code{estimator = "ML"}, which means that values from information
criteria (AIC, AICc, BIC) for specific model classes (like models from \emph{lme4})
are based on the ML-estimator, while the default behaviour of \code{AIC()} for
such classes is setting \code{REML = TRUE}. This default is intentional, because
comparing information criteria based on REML fits is usually not valid
(it might be useful, though, if all models share the same fixed effects -
however, this is usually not the case for nested models, which is a
prerequisite for the LRT). Set \code{estimator = "REML"} explicitly return the
same (AIC/...) values as from the defaults in \code{AIC.merMod()}.
}
}
\note{
There is also a \href{https://easystats.github.io/see/articles/performance.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
}
\examples{
\dontshow{if (require("lme4")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
compare_performance(lm1, lm2, lm3)
compare_performance(lm1, lm2, lm3, rank = TRUE)

m1 <- lm(mpg ~ wt + cyl, data = mtcars)
m2 <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
m3 <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
compare_performance(m1, m2, m3)
\dontshow{\}) # examplesIf}
}
\references{
Burnham, K. P., and Anderson, D. R. (2002).
\emph{Model selection and multimodel inference: A practical information-theoretic approach} (2nd ed.).
Springer-Verlag. \doi{10.1007/b97636}
}
