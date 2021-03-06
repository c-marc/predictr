% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{getLRatios}
\alias{getLRatios}
\title{Get likelihood ratios}
\usage{
getLRatios(FPF, TPF)
}
\arguments{
\item{FPF}{False Positive Fraction}

\item{TPF}{True Positive Fraction}
}
\value{
a list with positive and negative likelihood ratios
}
\description{
Get likelihood ratios
}
\examples{
getLRatios(.4, .6)
}
