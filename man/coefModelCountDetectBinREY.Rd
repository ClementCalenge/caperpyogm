\name{coefModelCountDetectBinREY}
\alias{coefModelCountDetectBinREY}
\alias{coefModelCountDetectBin}
\alias{coefModelCountDetectBetaBinREY}
\alias{coefModelm281}
\docType{data}
\title{
  Datasets Containing MCMC iterations for Various Registered Count Models
}
\description{
  These datasets are objects of class \code{"mcmc.list"} containing the
  result of the fit of registered models (see Details) to our lek count
  dataset (\code{lekcounts}).
}
\usage{

data("coefModelCountDetectBinREY")

data("coefModelCountDetectBin")

data("coefModelCountDetectBetaBinREY")

data("coefModelm281")

}
\format{
  Objects of class \code{"mcmc.list"}.
}
\details{
  Each dataset contains the MCMC iterations obtained by fitting a
  registered model (see \code{help(modelCountDetectBinREY)} for a list
  of registered model) to our dataset \code{lekcount}.  See examples
  below for a description of the process that led to these
  datasets. More precisely:
  
  \code{coefModelCountDetectBin} contains the results of the fit of the
  model \code{modelCountDetectBin} (see \code{?modelCountDetectBin}),
  using as periods the two-years periods (2010-2011, 2012-2013, etc.)

  \code{coefModelCountDetectBinREY} contains the results of the fit of
  the model \code{modelCountDetectBinREY} (see
  \code{?modelCountDetectBinREY}), using as periods the two-years
  periods.

  \code{coefModelm281} contains the results of the fit of
  the same model \code{modelCountDetectBinREY} after having removed lek
  281 frome the dataset.  This lek is an outlier, and refitting the
  model without this lek allows to assess its effect on the estimates of
  the model (see the vignette).

  \code{coefModelCountDetectBetaBinREY} contains the results of the fit
  of the model \code{modelCountDetectBetaBinREY} (see
  \code{?modelCountDetectBetaBinREY}), using as periods the two-years
  periods.

}
\source{
  The original Dataset has been kindly provided by the Observatoire des
  Galliformes de Montagne:

  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.
}
\references{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\examples{
## We demonstrate how we calculated coefModelCountDetectBinREY,
## but the process is the same for other datasets

## We work on the dataset lekcounts
head(lekcounts)

## We prepare the dataset to fit the model with JAGS
dataList <- dataCount2jags(lekcounts$lek, lekcounts$period,
                           lekcounts$nbobs, lekcounts$nbmales,
                           lekcounts$gr, as.numeric(factor(lekcounts$type)),
                           lekcounts$natun, lekcounts$year)
dataList

## We then fit the model. WARNING!!! THIS COMMAND IS VERY SLOW AND
## CAN TAKE SEVERAL HOURS
\dontrun{
coefModelCountDetectBinREY <- fitModelCount(dataList, "modelCountDetectBinREY")
}

## To save time for the user, we have stored the result of this
## command in the dataset coefModelCountDetectBinREY.
## The same approach was used for other datasets.


}
\keyword{datasets}
