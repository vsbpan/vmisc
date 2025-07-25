% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis_utils.R
\name{tex}
\alias{tex}
\title{Latex label}
\usage{
tex(x, italic = FALSE, ...)
}
\arguments{
\item{x}{the latex command without the `$`.}

\item{italic}{if TRUE italicize the output}

\item{...}{additional arguments passed to `latex2exp::Tex()`}
}
\value{
a plotmath expression
}
\description{
a wrapper for `latex2exp::Tex()`
}
