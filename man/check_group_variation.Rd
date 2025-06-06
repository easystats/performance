% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_group_variation.R
\name{check_group_variation}
\alias{check_group_variation}
\alias{check_group_variation.default}
\alias{check_group_variation.data.frame}
\alias{summary.check_group_variation}
\title{Check variables for within- and/or between-group variation}
\usage{
check_group_variation(x, ...)

\method{check_group_variation}{default}(x, ...)

\method{check_group_variation}{data.frame}(
  x,
  select = NULL,
  by = NULL,
  include_by = FALSE,
  numeric_as_factor = FALSE,
  tolerance_numeric = 1e-04,
  tolerance_factor = "crossed",
  ...
)

\method{summary}{check_group_variation}(object, flatten = FALSE, ...)
}
\arguments{
\item{x}{A data frame or a mixed model. See details and examples.}

\item{...}{Arguments passed to other methods}

\item{select}{Character vector (or formula) with names of variables to select
that should be checked. If \code{NULL}, selects all variables (except those in
\code{by}).}

\item{by}{Character vector (or formula) with the name of the variable that
indicates the group- or cluster-ID. For cross-classified or nested designs,
\code{by} can also identify two or more variables as group- or cluster-IDs.}

\item{include_by}{When there is more than one grouping variable, should they
be check against each other?}

\item{numeric_as_factor}{Should numeric variables be tested as factors?}

\item{tolerance_numeric}{The minimal percent of variation (observed \link{icc})
that is tolerated to indicate no within- or no between-effect.}

\item{tolerance_factor}{How should a non-numeric variable be identified as
varying \emph{only} "within" a grouping variable? Options are:
\itemize{
\item \code{"crossed"} - if all groups have all unique values of X.
\item \code{"balanced"} - if all groups have all unique values of X, \emph{with equal
frequency}.
}}

\item{object}{result from \code{check_group_variation()}}

\item{flatten}{Logical, if \code{TRUE}, the values are returned as character vector, not as list. Duplicated values are removed.}
}
\value{
A data frame with Group, Variable, Variation and Design columns.
}
\description{
Checks if variables vary within and/or between levels of grouping variables.
This function can be used to infer the hierarchical Design of a given
dataset, or detect any predictors that might cause heterogeneity bias (\emph{Bell
and Jones, 2015}). Use \code{summary()} on the output if you are mainly interested
if and which predictors are possibly affected by heterogeneity bias.
}
\details{
This function attempt to identify the variability of a set of variables
(\code{select}) with respect to one or more grouping variables (\code{by}). If \code{x} is a
(mixed effect) model, the variability of the fixed effects predictors are
checked with respect to the random grouping variables.
\cr\cr
Generally, a variable is considered to vary \emph{between} groups if is correlated
with those groups, and to vary \emph{within} groups if it not a constant within at
least one group.
\subsection{Numeric variables}{

Numeric variables are partitioned via \code{\link[datawizard:demean]{datawizard::demean()}} to their
within- and between-group components. Then, the variance for each of these
two component is calculated. Variables with within-group variance larger than
\code{tolerance_numeric} are labeled as \emph{within}, variables with a between-group
variance larger than \code{tolerance_numeric} are labeled as \emph{between}, and
variables with both variances larger than \code{tolerance_numeric} are labeled as
\emph{both}.

Setting \code{numeric_as_factor = TRUE} causes numeric variables to be tested
using the following criteria.
}

\subsection{Non-numeric variables}{

These variables can have one of the following three labels:
\itemize{
\item \emph{between} - the variable is correlated with the groups, \emph{and} is fixed
within each group (each group has exactly one unique, constant value)
\item \emph{within} - the variable is \emph{crossed} with the grouping variable, such that
all possible values appear within each group. The \code{tolerance_factor}
argument controls if full balance is also required.
\item \emph{both} - the variable is correlated with the groups, but also varies within
each group but is not fully crossed (or, when
\code{tolerance_factor = "balanced"} the variable is fully crossed, but not
perfectly balanced).
}

Additionally, the design of non-numeric variables is also checked to see if
they are \emph{nested} within the groups or is they are \emph{crossed}. This is
indicated by the \code{Design} column.
}

\subsection{Heterogeneity bias}{

Variables that vary both within and between groups can cause a heterogeneity
bias (\emph{Bell and Jones, 2015}). It is recommended to center (person-mean
centering) those variables to avoid this bias. See \code{\link[datawizard:demean]{datawizard::demean()}}
for further details. Use \code{summary()} to get a short text result that
indicates if and which predictors are possibly affected by heterogeneity
bias.
}
}
\examples{
data(npk)
check_group_variation(npk, by = "block")

data(iris)
check_group_variation(iris, by = "Species")

data(ChickWeight)
check_group_variation(ChickWeight, by = "Chick")

# A subset of mlmRev::egsingle
egsingle <- data.frame(
  schoolid = factor(rep(c("2020", "2820"), times = c(18, 6))),
  lowinc = rep(c(TRUE, FALSE), times = c(18, 6)),
  childid = factor(rep(
    c("288643371", "292020281", "292020361", "295341521"),
    each = 6
  )),
  female = rep(c(TRUE, FALSE), each = 12),
  year = rep(1:6, times = 4),
  math = c(
    -3.068, -1.13, -0.921, 0.463, 0.021, 2.035,
    -2.732, -2.097, -0.988, 0.227, 0.403, 1.623,
    -2.732, -1.898, -0.921, 0.587, 1.578, 2.3,
    -2.288, -2.162, -1.631, -1.555, -0.725, 0.097
  )
)

result <- check_group_variation(
  egsingle,
  by = c("schoolid", "childid"),
  include_by = TRUE
)
result

summary(result)

\dontshow{if (insight::check_if_installed("lme4", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

data(sleepstudy, package = "lme4")
check_group_variation(sleepstudy, select = "Days", by = "Subject")

# Or
mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
result <- check_group_variation(mod)
result

summary(result)
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
Modeling of Time-Series Cross-Sectional and Panel Data. Political Science
Research and Methods, 3(1), 133–153.
}
}
\seealso{
For further details, read the vignette
\url{https://easystats.github.io/parameters/articles/demean.html} and also
see documentation for \code{\link[datawizard:demean]{datawizard::demean()}}.
}
