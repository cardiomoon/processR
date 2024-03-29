% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condPlot.R
\name{jnPlot}
\alias{jnPlot}
\title{Draw johnson_neyman plot}
\usage{
jnPlot(
  fit,
  pred = NULL,
  modx = NULL,
  digits = 3,
  plot = FALSE,
  mode = 1,
  xvar = "Z",
  addEq = FALSE,
  ...
)
}
\arguments{
\item{fit}{A regression model}

\item{pred}{name of predictor variable}

\item{modx}{name of moderator variable}

\item{digits}{integer indicating the number of decimal places}

\item{plot}{logical. Whether or not draw plot}

\item{mode}{integer 1 or 2}

\item{xvar}{Name of xvar}

\item{addEq}{logical}

\item{...}{Further arguments to be passed to interactions::johnson_neyman()}
}
\description{
Draw johnson_neyman plot
}
\examples{
fit=lm(mpg~hp*wt,data=mtcars)
jnPlot(fit)
\donttest{
fit=lm(justify~frame*skeptic,data=disaster)
res=jnPlot(fit)
res$plot
fit=lm(govact~negemot*sex*age+posemot+ideology,data=glbwarm)
jnPlot(fit,pred="negemot:sex",modx="age",mode=2,addEq=TRUE)
}
}
