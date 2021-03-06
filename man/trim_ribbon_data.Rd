% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GGplot2Tools_TrimRibbonData.R
\name{trim_ribbon_data}
\alias{trim_ribbon_data}
\alias{TrimRibbonData}
\title{Trim ribbon data}
\usage{
trim_ribbon_data(
  .data,
  x,
  y,
  xlim,
  ylim,
  lwr,
  upp,
  ...,
  .exclude_outliers = FALSE
)

TrimRibbonData(
  .data,
  x,
  y,
  xlim,
  ylim,
  lwr,
  upp,
  ...,
  .exclude_outliers = FALSE
)
}
\arguments{
\item{.data}{Input table.}

\item{x}{\code{x} column.}

\item{y}{\code{y} column.}

\item{xlim}{\code{x} limits.}

\item{ylim}{\code{y} limits.}

\item{lwr}{Lower \code{y} limit of ribbon.}

\item{upp}{Upper \code{y} limit of ribbon}

\item{...}{Additional columns to preserve.}

\item{.exclude_outliers}{If \code{TRUE}, \code{y}-outliers are assigned NA values.}
}
\value{
Table ready to be plotted.
}
\description{
Trim ribbon data
}
