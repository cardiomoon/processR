% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modmedSummary.R
\name{modmedSummary}
\alias{modmedSummary}
\title{Summarize the moderated mediation}
\usage{
modmedSummary(
  semfit,
  mod = NULL,
  values = NULL,
  boot.ci.type = "perc",
  add.range = TRUE
)
}
\arguments{
\item{semfit}{An object of class lavaan}

\item{mod}{name of moderator}

\item{values}{Optional. Numeric vector}

\item{boot.ci.type}{Type of bootstrapping interval. Choices are c("norm","basic","perc",bca.simple")}

\item{add.range}{logical Whether or not add range}
}
\value{
A data.frame and an object of class modmedSummary
}
\description{
Summarize the moderated mediation
}
\examples{
require(lavaan)
labels=list(X="frame",M="justify",Y="donate",W="skeptic")
moderator=list(name="skeptic",site=list(c("a","c")))
model=tripleEquation(labels=labels,moderator=moderator)
cat(model)
\donttest{
semfit=sem(model,data=disaster,se="boot",bootstrap=100)
modmedSummary(semfit)
conditionalEffectPlot(semfit,data=disaster)
labels=list(X="dysfunc",M="negtone",Y="perform",W="negexp")
moderator=list(name="negexp",site=list("b"))
model=tripleEquation(labels=labels,moderator=moderator,data=teams,rangemode=2)
cat(model)
semfit=sem(model,data=teams,se="boot",bootstrap=100)
summary(semfit)
modmedSummary(semfit)
conditionalEffectPlot(semfit,data=teams)
}
}
