\name{listCoefsCVBinREY}
\alias{listCoefsCVBinREY}
\docType{data}
\title{
  Cross-Validation Data for the Count Model ModelCountDetectBin
}
\description{
  This dataset contains the MCMC iterations for the K-fold
  cross-validation of the model \code{"modelCountDetectBinREY"}
}
\usage{data("listCoefsCVBinREY")}
\format{
  An object of class \code{"CVModelCount"}
}
\details{

  Each dataset describes the K-fold cross validation results of the
  model \code{"modelCountDetectBinREY"}. We show in the examples how
  this datasets was obtained.
  
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

## We describe here the process to calculate the object
## listCoefCVBinREY

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

## And the result is stored in the dataset:
llcBinREY

## Expected LPD can be calculated with:
elpdLeks(llcBinREY)

}
\keyword{datasets}
