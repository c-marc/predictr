% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{getPredValues}
\alias{getPredValues}
\title{Get predictive values}
\usage{
getPredValues(FPF, TPF, prior = NULL)
}
\arguments{
\item{FPF}{False Positive Fraction}

\item{TPF}{True Positive Fraction}

\item{prior}{a vector of prior probabilities. If NULL, will compute over (0,1)}
}
\value{
a tibble with:
\itemize{
\item prior
\item type: "negative" or "positive"
\item posterior: posterior probabilities
}
}
\description{
Get predictive values
}
\examples{
getPredValues(.4, .6, .5)
}
