\name{estimateNmales}
\alias{estimateNmales}
\alias{print.estimateMales}
\alias{summary.estimateMales}
\alias{getTotal}
\alias{changeRate}
\alias{showChangeRate}
\alias{distritime}
\title{
  Estimation of the Number of Male Capercaillie in the Pyrenees Mountains
}
\description{
  The function \code{estimateNmales} can be used to obtain estimates of
  the number of male capercaillie in each geographic region of the
  Pyrenees mountains from a model of the lek count data and a model of
  the probability of presence of unknown leks.

  The function \code{getTotal} can be used to obtain estimate of the
  number of male capercaillie in the whole area.
  
  The function \code{changeRate} can be used to obtain estimate of the
  rate of change of the number of males between two periods.

  The functions \code{showChangeRate} and \code{distritime} can be used
  to plot the posterior distribution of rate of changes between two
  periods and the posterior distribution of the number of males for each
  period respectively.  They rely on the package ggplot2, and can be
  combined with other functions from this package (e.g. \code{ggtitle}
  to add a title, etc.).

  The function \code{summary} can be used to display summary statistics
  on the number of males in a region of interest (or on the whole
  area).
}
\usage{

estimateNmales(coefsCounts, coefsUL, gridFrame, NKAL, NKIL,
               what = c("tot", "kalc", "kilc", "ulc", "ul"))

\method{print}{estimateMales}(x, \dots)

getTotal(x, \dots)

changeRate(object, GR, changeRatePeriods = c(1, 5))

\method{summary}{estimateMales}(object, GR, CIlevel = 80,
                                changeRatePeriods = c(1, 5),
                                threshold = c(-10, 10), \dots)

showChangeRate(object, GR, changeRatePeriods = c(1, 5),
               quantiles = c(0.1, 0.5, 0.9))

distritime(x, xlab = "Period",
           ylab = "Number of cocks", trim = 0.95)

}
\arguments{
  \item{coefsCounts}{
    object of class \code{mcmc.list} containing the MCMC iterations of a
    registered model for lek count data.
  }
  \item{coefsUL}{
    object of class \code{mcmc.list} containing the MCMC iterations of a
    registered model for grid cell search data.
  }
  \item{gridFrame}{
    A data frame containing the list of grid cells present in the Pyrenees
    mountains (see \code{help("gridFrame")}), required to estimate the
    number of cocks in the whole mountains.
  }
  \item{NKAL}{
    A vector of length 5 (the 5 geographic regions) containing the
    number of known active leks in each geographic region.
  }
  \item{NKIL}{
    A vector of length 5 (the 5 geographic regions) containing the
    number of known indeterminate leks in each geographic region.
  }
  \item{what}{
    character string indicating in which compartment should the number
    of males be estimated: 
    \code{"tot"} (default) returns the estimation of the total number of
    males; \code{"kalc","kilc"} respectively return the estimation of
    the number of males on respectively known active and known
    indeterminate leks; \code{"ulc"} returns the estimation of the number
    of males on unknown leks; and \code{"ul"} returns the estimation of
    the number of unknown leks.
  }
  \item{object,x}{object of class \code{"estimateMales"}}
  \item{GR}{integer value indicating the number of the geographic region
    for which a summary is required.  If missing, the summary is shown
    for the whole area.}
  \item{CIlevel}{numeric value indicating the level for credible
    intervals}
  \item{changeRatePeriods}{vector of length two indicating the number of
    the two periods used to calculate the change rate (defaulting to the
    calculation of the rate between the first and fifth period).}
  \item{threshold}{vector of length two indicating the change rate thresholds
    (in percent) defining a decrease/stability/increase of the
    population size.}
  \item{quantiles}{vector of numeric values indicating which quantile
    should be shown on the plot.}
  \item{xlab}{character string indicating the label for the x axis}
  \item{ylab}{character string indicating the label for the y axis}
  \item{trim}{numeric value indicating how to trim the distribution to
    avoid outliers compressing the display. The \code{100*(1-trim)}\% of
    the largest MCMC simulations of the number of males averaged over
    all periods are removed prior to the plot.}
  \item{\dots}{additional arguments to be passed to other functions.}
}
\value{
  
  An object of class \code{"estimateMales"}, which is a list with 5
  components (the 5 regions), each component being a matrix with M rows
  (MCMC iterations) and P columns (periods), containing M simulations of
  the number of males in each region for each period under the model.

  The function \code{getTotal} returns a matrix with M rows (MCMC
  iterations) and P columns (periods), containing M simulations of
  the number of males on the whole area for each period under the model.

  The function \code{changeRate} returns a vector with M elements (MCMC
  iterations) containing the M simulations of the rate of change of the
  number of males in the region (or on the whole area) between the two
  periods indicated in the arguments.
  
  The summary returned by the function \code{summary} can be stored as a
  list. 
}
\references{

  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{getTotal}} for further functions to work with objects of
  class \code{"estimateMales"}
}
\examples{

## Number of known active and indeterminate leks
NKAL <- c(14L, 64L, 76L, 48L, 46L)
NKIL <- c(6L, 80L, 102L, 32L, 96L)


## The list of coefficients are stored as datasets
nm <- estimateNmales(coefModelCountDetectBinREY, coefModelULPresence,
                     gridFrame, NKAL, NKIL)

## Summary for the whole area
summary(nm)

## show the distribution of the number of males
## on the whole area
distritime(getTotal(nm))

## Rate of change between period 1 and 5
showChangeRate(nm)


}
\keyword{model}
