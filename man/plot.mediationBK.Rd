% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediationBK.R
\name{plot.mediationBK}
\alias{plot.mediationBK}
\title{S3 method for class mediationBK}
\usage{
\method{plot}{mediationBK}(x, ...)
}
\arguments{
\item{x}{An object of class mediationBK}

\item{...}{Further arguments to be passed to plot()}
}
\description{
S3 method for class mediationBK
}
\examples{
labels=list(X="cond",M="pmi",Y="reaction")
result=mediationBK(labels=labels,data=pmi)
plot(result,type=1)
plot(result)
plot(result,type=1,whatLabel="label",arrowslabels="c",addprime=FALSE)
plot(result,whatLabel="label",arrowslabels=c("a","b","c"))
}
