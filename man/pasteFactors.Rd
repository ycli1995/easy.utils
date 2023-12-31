% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_ops.R
\name{pasteFactors}
\alias{pasteFactors}
\title{Paste two factor vectors}
\usage{
pasteFactors(x, y, collapse = "_")
}
\arguments{
\item{x, y}{Factor vectors}

\item{collapse}{A character string to separate the \code{x} and \code{y}.}
}
\value{
A new factor vector
}
\description{
Paste two factors and re-assign the levels
}
\examples{
x <- factor(c(rep("A", 10), rep("B", 10)), levels = c("A", "B"))
y <- factor(c(rep("a", 5), rep("b", 15)), levels = c("a", "b"))
pasteFactors(x, y)

}
