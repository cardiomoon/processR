% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catMediation.R
\name{catMediation}
\alias{catMediation}
\title{Make Mediation Equation with one categorical variable}
\usage{
catMediation(
  X = NULL,
  M = NULL,
  Y = NULL,
  labels = list(),
  data,
  moderator = list(),
  covar = NULL,
  mode = 0,
  maxylev = 2,
  range = TRUE,
  rangemode = 1
)
}
\arguments{
\item{X}{Name of independent variable}

\item{M}{Name of mediator variable}

\item{Y}{Name of dependent variable}

\item{labels}{optional list}

\item{data}{A data.frame}

\item{moderator}{A list}

\item{covar}{A list of covariates}

\item{mode}{A numeric. 0: SEM equation, 1: regression equation}

\item{maxylev}{maximal unique length of categorical variable}

\item{range}{A logical}

\item{rangemode}{range mode}
}
\description{
Make Mediation Equation with one categorical variable
}
\examples{
labels=list(X="cyl",M="am",Y="mpg")
moderator=list(name=c("cyl","wt"),site=list(c("a","c"),c("c")))
covar=list(name=c("carb","disp"),label=c("carb","disp"),site=list(c("M","Y"),"Y","Y"))
cat(catMediation(labels=labels,data=mtcars))
cat(catMediation(X="am",Y="mpg",data=mtcars,moderator=moderator,covar=covar,maxylev=6))
cat(catMediation(X="am",Y="mpg",data=mtcars,moderator=moderator,covar=covar))
cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars))
cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars,moderator=moderator))
cat(catMediation(X="cyl",M="am",Y="mpg",data=mtcars,moderator=moderator))
cat(catMediation(X="am",M="hp",Y="mpg",data=mtcars,moderator=moderator,maxylev=6))
cat(catMediation(X="hp",M="am",Y="mpg",data=mtcars,maxylev=6))
cat(catMediation(X="am",M="hp",Y="mpg",data=mtcars,moderator=moderator,covar=covar))
}
