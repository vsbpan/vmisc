% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exports.R
\name{rbind.fill}
\alias{rbind.fill}
\alias{capture_output}
\alias{ad.test}
\alias{hessian}
\alias{KL}
\title{Other handy exported functions}
\usage{
\method{rbind}{fill}(...)

capture_output(code)

ad.test(...)

hessian(...)

KL(x, test.na = TRUE, unit = "log", est.prob = NULL)
}
\arguments{
\item{...}{additional arguments passed to the exported function}

\item{code}{see \code{purrr::capture_output}}

\item{test.na, unit, est.prob, x}{See \code{philentropy::KL()}.}
}
\value{
see documentation of exported functions.
}
\description{
Essentially a wrapper for certain useful functions exported form other packages.
}
