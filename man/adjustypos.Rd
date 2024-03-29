% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawModel.R
\name{adjustypos}
\alias{adjustypos}
\title{Adjust y position}
\usage{
adjustypos(
  ypos,
  ymargin = 0.02,
  rady = 0.06,
  maxypos = 0.6,
  minypos = 0,
  totalOnly = FALSE
)
}
\arguments{
\item{ypos}{y position}

\item{ymargin}{vertical margin of plot}

\item{rady}{vertical radius of the box}

\item{maxypos}{maximal y position of X or W variables}

\item{minypos}{minimal y position of X or W variables}

\item{totalOnly}{logical if TRUE, arrange ypos with center 0.5}
}
\description{
Adjust y position
}
\examples{
ypos=c(0.5,0.9,1,1,2,3)
adjustypos(ypos)
adjustypos(ypos,totalOnly=TRUE)
}
