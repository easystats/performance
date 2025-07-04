% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item_difficulty.R
\name{item_difficulty}
\alias{item_difficulty}
\title{Difficulty of Questionnaire Items}
\usage{
item_difficulty(x, maximum_value = NULL)
}
\arguments{
\item{x}{Depending on the function, \code{x} may be a \code{matrix} as returned by the
\code{cor()}-function, or a data frame with items (e.g. from a test or
questionnaire).}

\item{maximum_value}{Numeric value, indicating the maximum value of an item.
If \code{NULL} (default), the maximum is taken from the maximum value of all
columns in \code{x} (assuming that the maximum value at least appears once in the
data). If \code{NA}, each item's maximum value is taken as maximum. If the
required maximum value is not present in the data, specify the theoreritcal
maximum using \code{maximum_value}.}
}
\value{
A data frame with three columns: The name(s) of the item(s), the item
difficulties for each item, and the ideal item difficulty.
}
\description{
Compute various measures of internal consistencies for tests or
item-scales of questionnaires.
}
\details{
\emph{Item difficutly} of an item is defined as the quotient of the sum
actually achieved for this item of all and the maximum achievable score. This
function calculates the item difficulty, which should range between 0.2 and
0.8. Lower values are a signal for more difficult items, while higher values
close to one are a sign for easier items. The ideal value for item difficulty
is \code{p + (1 - p) / 2}, where \code{p = 1 / max(x)}. In most cases, the ideal item
difficulty lies between 0.5 and 0.8.
}
\examples{
data(mtcars)
x <- mtcars[, c("cyl", "gear", "carb", "hp")]
item_difficulty(x)
}
\references{
\itemize{
\item Bortz, J., and Döring, N. (2006). Quantitative Methoden der Datenerhebung.
In J. Bortz and N. Döring, Forschungsmethoden und Evaluation. Springer:
Berlin, Heidelberg: 137–293
\item Kelava A, Moosbrugger H (2020). Deskriptivstatistische Itemanalyse und
Testwertbestimmung. In: Moosbrugger H,  Kelava A, editors. Testtheorie und
Fragebogenkonstruktion. Berlin, Heidelberg: Springer, 143–158
}
}
