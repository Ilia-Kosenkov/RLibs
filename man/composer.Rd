% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{\%>>\%}
\alias{\%>>\%}
\alias{\%<<\%}
\title{Compose functions}
\usage{
x \%>>\% y

x \%<<\% y
}
\arguments{
\item{x}{Lhs.}

\item{y}{Rhs.}
}
\value{
A composed function
}
\description{
Composes two functions using \code{purrr::compose}, in different directions.
Supports \code{rlang}-style lambdas (in parentheses).
}
\examples{
(~.x ^ 2) \%>>\% (~.x + 5)
}
