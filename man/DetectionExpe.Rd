\name{DetectionExpe}
\alias{DetectionExpe}
\docType{data}
\title{
  Detectability of Leks during Grid Cells Search
}
\description{
  This dataset contains the result of an experiment carried out to
  assess the detectability of leks during grid cell search.
}
\usage{data("DetectionExpe")}
\format{
  A data frame with 16 observations on the following 4 variables.
  \describe{
    \item{\code{observer}}{a character vector containing the type of
      observer that searched the grid cell (experienced/inexperienced).}
    \item{\code{Nquad}}{a numeric vectors indicating the number of leks
      in the grid cell searched by the observer.}
    \item{\code{Nsect}}{a numeric vector indicating how many leks were
      actually included in the sector defined by the observer, among
      those present within the
      grid cell. }
    \item{\code{Ndete}}{a numeric vector indicating how many leks were
      detected by the observers among those both present in the grid cell
      and included in the search sector.}
  }
}
\details{
  
  Our model requires to model the probability to detect an unknown lek
  during grid cell search.  Such searches are carried out in two steps:
  (i) a search sector is defined by the observer within a grid cell, so
  that their subjective assessment of the probability to include a lek
  is maximal, (ii) the observer will then search unknown leks within the
  sector during one morning.  Because of the subjective definition of
  the sector and the variability in detection probability, we carried
  out an experiment to model the detection process during the model.

  We carried out an experiment with two observers to fit such a
  model. Two observers, one highly experience and a new observer with no
  experienced were told to search for new leks in grid cells unknown to
  them, but known to us (some of them containing no leks). We recorded
  the sector delimited by the observer, as well as the leks detected by
  the observers.

}
\source{
  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.
}
\references{
  
  Calenge C., Menoni E., Milhau B., Montadert M. (in prep.). Building a
  Capercaillie Monitoring program in the Pyrenees Mountains France based
  on Participatory Research.

}
\examples{
## The data frame
DetectionExpe

## Total number of leks in grid cells
Nq <- tapply(DetectionExpe$Nquad, DetectionExpe$observer, sum)

## Total number of leks in delimited Sectors
Ns <- tapply(DetectionExpe$Nsect, DetectionExpe$observer, sum)

## Total number of detections
Nd <- tapply(DetectionExpe$Ndete, DetectionExpe$observer, sum)

## Proportion
data.frame(Nq,Ns,Nd)


}
\keyword{datasets}
