% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise.R
\name{summarise_vec}
\alias{summarise_vec}
\title{Summarise vector}
\usage{
summarise_vec(x, interval = 0.95, na.rm = FALSE)
}
\arguments{
\item{x}{A vector or matrix of numeric values}

\item{interval}{The density interval used to find the upper and lower intervals. Default is 0.95.}

\item{na.rm}{If \code{TRUE} (default is \code{FALSE}), missing values will be removed.}
}
\value{
a vector of numeric values summarising the vector \code{x}.
}
\description{
Handy function to summaries a vector of numeric values
}
