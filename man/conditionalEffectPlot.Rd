% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditionalEffectPlot.R
\name{conditionalEffectPlot}
\alias{conditionalEffectPlot}
\title{Make conditional effect plot}
\usage{
conditionalEffectPlot(
  semfit,
  values = NULL,
  data,
  no = 1,
  mod = NULL,
  color = c("black", "red"),
  lty = c(1, 3),
  linesize = 1
)
}
\arguments{
\item{semfit}{An object of class lavaan}

\item{values}{Optional value}

\item{data}{A data.frame}

\item{no}{Integer}

\item{mod}{Name of moderator variable}

\item{color}{character vector line color}

\item{lty}{numeric line type}

\item{linesize}{numeric linesize}
}
\description{
Make conditional effect plot
}
