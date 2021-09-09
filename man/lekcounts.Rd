\name{lekcounts}
\alias{lekcounts}
\docType{data}
\title{
  Capercaillie Lek Counts
}
\description{
  
  This dataset contains the results of capercaillie lek counts in the
  Pyrenees mountains between 2010 and 2019.
  
}
\usage{data("lekcounts")}
\format{
  A data frame with 1459 observations (the 1459 count occasions) on the following 8 variables:
  \describe{
    \item{\code{lek}}{a numeric vector containing the lek label for each
      lek count in the dataset.  Must be numbered from 1 to L (where L
      is the total number of leks).}
    \item{\code{period}}{a numeric vector containing the period label
      for each lek count in the dataset.  Must be numbered from 1 to P
      (where P is the total number of periods)}
    \item{\code{nbobs}}{a numeric vector containing the number of
      observers for each lek count in the dataset.  We have subtracted 2
      to the number of observers, to improve the mixing of the MCMC chains.} 
    \item{\code{nbmales}}{a numeric vector containing the number of
      males counted for each lek count in the dataset.}
    \item{\code{gr}}{a numeric vector containing the geographic region label for each lek count
    in the dataset. Must be numbered from 1 to 5.}
    \item{\code{type}}{a numeric vector containing the type label for each lek count
    in the dataset. Must be numbered from 1 to 3 (1 = known active leks,
    2 = known indeterminate leks, 3 = unknown leks).}
    \item{\code{natun}}{a numeric vector containing the type label for each lek count
    in the dataset. Must be numbered from 1 to U (where U is the total
    number of natural units).}
    \item{\code{year}}{a numeric vector containing the period label for each lek count in the
    dataset.  Must be numbered from 1 to Y (where Y is the total number
    of years)}
  }
}
\details{
  This dataset is one of the three datasets used to fit the model in
  Calenge et al. in prep. (the other two being \code{gridSearch} and
  \code{DetectionExpe}).
}
\source{
  The original Dataset has been kindly provided by the Observatoire des
  Galliformes de Montagne:

  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.
}
\references{
  
  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.

}
\examples{

head(lekcounts)

}
\keyword{datasets}
