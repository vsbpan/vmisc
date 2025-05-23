% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{Hoover}
\alias{Hoover}
\title{Hoover Index}
\usage{
Hoover(x, na.rm = FALSE)
}
\arguments{
\item{x}{a vector of numeric data}

\item{na.rm}{a logical value indicating whether to drop \code{NA} values}
}
\value{
A numeric value
}
\description{
Calculate the Hoover index of a vector of numeric data
}
\details{
The Hoover index can be thought of the percentage of the total x that would have to be reallocated to make all individuals within a population have equal values of x. It is calculated as such:
\deqn{H(X) = \frac{\sum_i^n |x_i - E[X]|}{2 \sum_i^n x_i}}
}
