\name{simBinREY}
\alias{simBinREY}
\alias{simBin}
\alias{simBetaBinREY}
\docType{data}
\title{
  Simulated Count Datasets
}
\description{
  These datasets contain the results of simulated count datasets under
  various models fitted to the dataset \code{lekcounts}.
}
\usage{

data("simBinREY")
data("simBin")
data("simBetaBinREY")

}
\format{
  Each dataset is an object of class \code{"caperpySim"} (see
  \code{help(simulateModelCount)}).
}
\details{

  Each dataset contains count data simulated by a fitted registered
  model with the function \code{simulateModelCount}. The examples section
  illustrate the process that led to these datasets.  More specifically,
  these datasets are:
  
  \code{simBin} contains the simulated  datasets for the model
  \code{modelCountDetectBin} (see \code{?modelCountDetectBin}).  

  \code{simBinREY} contains the simulated  datasets for the model
  \code{modelCountDetectBinREY} (see \code{?modelCountDetectBinREY}).  

  \code{simBetaBinREY} contains the simulated  datasets for the model
  \code{modelCountDetectBetaBinREY} (see \code{?modelCountDetectBetaBinREY}).  


  
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

## We illustrate the process leading to simBinREY (very long). Other
## datasets have been simulated similarly.


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
## Note that the result is stored as a dataset in the package
\dontrun{
coefModelCountDetectBinREY <- fitModelCount(dataList, "modelCountDetectBinREY")
}


## We then simulate datasets based on these coefficients
## WARNING, THIS IS ALSO VERY SLOW (ABOUT 1H30)!!!!!
## Note that the result is also stored as a dataset in the package
\dontrun{
simBinREY <- simulateModelCount(coefModelCountDetectBinREY, dataList)
}

## Residuals vs prediction:
plot(predict(simBinREY), residuals(simBinREY), xlab="predictions",
     ylab="residuals")


## The same plot, but using the lek as a grouping factor
plot(predict(simBinREY, groupingFactor="lek"),
     residuals(simBinREY, groupingFactor="lek"),
     xlab="predictions",
     ylab="residuals")

}
\keyword{datasets}
