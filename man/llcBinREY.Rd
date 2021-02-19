\name{llcBinREY}
\alias{llcBinREY}
\alias{llcBin}
\alias{llcBetaBinREY}
\alias{llcBinPerYear}
\alias{llcBinREYObs2}
\docType{data}
\title{
  Log-Predictive Densities for Lek Count Data Under Different Models
}
\description{
  These datasets contain the Log-Predictive Densities (LPD)
  for each lek under various registered count models, calculated by
  cross-validation.
}
\usage{
data("llcBin")
data("llcBinREY")
data("llcBetaBinREY")
data("llcBinPerYear")
data("llcBinREYObs2")
}
\format{
  Each dataset is a matrix of class \code{LLCountsSim}, returned by the
  function \code{LLCount} of this package, with 330 rows (the 330 leks)
  and 4000 columns (the 4000 MCMC iterations), containing the LPD
  calculated for each lek and each MCMC iteration.
}
\details{
  Each dataset describes the predictive ability of a tested model, as
  returned by the function \code{LLCount} designed to estimate LPD with
  objects returned by the function \code{kfoldCVModelCount} (which
  performs K-fold cross-validation). We show in the examples how these
  datasets were obtained. The 
  models corresponding to these datasets are the following:

  \code{llcBinPerYear} contains the LPDs for model \code{modelCountDetectBin}
  (see \code{?modelCountDetectBin}), using every single year as a
  period.  
  
  \code{llcBin} contains the LPDs for model \code{modelCountDetectBin}
  (see \code{?modelCountDetectBin}), using as periods the two-years
  periods (2010-2011, 2012-2013, etc.)  

  \code{llcBinREY} contains the LPDs for model \code{modelCountDetectBinREY}
  (see \code{?modelCountDetectBinREY}), using as periods the two-years
  periods.

  \code{llcBetaBinREY} contains the LPDs for model
  \code{modelCountDetectBetaBinREY} (see \code{?modelCountDetectBetaBinREY}), using
  as periods the two-years periods.

  To test the predictive ability of a model, this approach consists in
  splitting the original lek counts dataset \code{lekcounts} in G
  subsets of leks.  In the dataset used by Calenge et al. (in prep.), we
  used 10 groups of 33 leks.  Thus, for each subset i, we can build a
  calibration dataset with all subsets except subset i and fit the model
  by MCMC with this calibration dataset.  The function
  \code{kfoldCVModelCount} performs this operation. We can then predict
  the count data of each lek using a model fitted without this lek, and
  calculate the log-probability density of all counts on each lek, for
  each MCMC vector of parameters simulated the model (avoiding the
  circularity consisting in using a model fit with a dataset to predict
  the same dataset).  The function \code{LLCount} performs this
  operation. Finally, the function \code{elpdLeks} calculates the
  expected log-probability for each lek.

  However, both the functions \code{kfoldCVModelCount} and
  \code{LLCount} take a very long time (several hours) so that we
  included these datasets to allow the readers of Calenge et al. (in
  prep.) to work directly with the results.
}
\source{
  The original Dataset has been kindly provided by the Observatoire des
  Galliformes de Montagne:

  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.
}
\seealso{
  See the vignette \code{vignette("caperpyogm")} for a more detailed
  description of the k-fold validation process.  See also
  \code{\link{kfoldCVModelCount}}.
}
\references{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\examples{
## We demonstrate how we calculated llcBinREY, but the process is the
## same for other datasets

## We work on the dataset lekcounts
head(lekcounts)

## We prepare the dataset to fit the model with JAGS
dataList <- dataCount2jags(lekcounts$lek, lekcounts$period,
                           lekcounts$nbobs, lekcounts$nbmales,
                           lekcounts$gr, as.numeric(factor(lekcounts$type)),
                           lekcounts$natun, lekcounts$year)
dataList

## We define 10 groups of 33 leks
set.seed(980)
ooo <- sample(c(rep(1:10,each=33)))

## Performs K-fold validation. WARNING!! THIS CALCULATION TAKES
## SEVERAL HOURS!!!
\dontrun{
listCoefsCVBinREY <- kfoldCVModelCount(ooo, dataList, "modelCountDetectBinREY")
}

## To save time for the user, we have stored the result of this
## command in the dataset listCoefsCVBinREY (for the model
## modelCountDetectBinREY only. We could not include the results of
## cross-validation for other models due to the large object size, but
## we can send them on request).
listCoefsCVBinREY

## Finally, we can use LLCount to calculate the LPD of each lek counts
## for each MCMC iteration, under a model that was not fit using these
## counts.
## WARNING!!! THIS CALCULATION ALSO TAKES MORE THAN ONE HOUR!!!
\dontrun{
llcBinREY <- LLCount(dataList, listCVCoefBinREY, oo)
}

## And the result is:
llcBinREY

## Expected LPD can be calculated with:
elpdLeks(llcBinREY)


}
\keyword{datasets}
