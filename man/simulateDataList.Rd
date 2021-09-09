\name{simulateDataList}
\alias{simulateDataList}
\alias{simulateN}
\alias{simulateDataList2}
\title{
  Simulate an object of class \code{caperpyData} based on a Fitted Model
  for Lek Counts
}
\description{
  The function \code{simulateDataList} simulate a data list of class
  \code{caperpyData} using the model \code{modelCountDetectBinREY} used in the
  paper.  This dataset can be used to test the effect of unaccounted
  variation and/or double counts in the detection process.

  The functions \code{simulateN} and \code{simulateDataList2} also
  simulate the model, but decompose the simulation of the state process
  (\code{simulateN}) and the simulation of the detection process
  (\code{simulateDataList2}).
}
\usage{

simulateDataList(coefs, dataList, betaBinDelta = NULL, doubleCountsp = NULL)

simulateN(coefs, dataList)

simulateDataList2(coefs, dataList, simn, betaBinDelta = NULL, doubleCountsp = NULL)


}
\arguments{
  \item{coefs}{
    An object of class \code{mcmc.list} returned by the function
    \code{fitModelCount}
  }
  \item{dataList}{
    An object of class \code{caperpyData} returned by the function
    \code{dataCount2jags}. 
  }
  \item{betaBinDelta}{
    numeric value of the parameter \code{delta2} controlling the amount
    of extra-binomial variation added to the dataset.
  }
  \item{doubleCountsp}{
    numeric value indicating the probability that a detected male is
    counted twice.
  }
  \item{simn}{
    numeric vector containing the values of the number of males
    simulated on the leks by the function \code{simulateN}.
  }
}
\details{
  
  This function can be used to simulate a dataset from the model used in
  the paper.  The parameters used to simulate the model are the median
  of the posterior distribution.  This function can be used to generate
  extra-binomial variation to the data, or to simulate double counts in
  the data.  This function has been used in the vignette to assess the
  sensitivity of our model to unaccounted variation in the detection
  probability and to double counts.

  The functions \code{simulateN} and \code{simulateDataList2} also
  simulate the model, but decompose the whole process: \code{simulateN}
  simulates the true numbers of males on the leks and
  \code{simulateDataList2} simulates the detected number of males, given
  the true numbers passed as arguments (see the vignette for an example
  of use).
  
}
\value{

  \code{simulateN} return a vector of length n, where n is the number of
  lek/periods in \code{dataList}.
  
  \code{simulateDataList} and \code{simulateDataList2} return an object
  of class \code{"caperpySim"}.
  
}
\references{
  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\examples{

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

## Example of use
dl <- simulateDataList(coefModelCountDetectBinREY, dataList,
                       betaBinDelta = 0.05, doubleCountsp = 0.4)


## Which is equivalent to:
ne <- simulateN(coefModelCountDetectBinREY, dataList)
dl2 <- simulateDataList2(coefModelCountDetectBinREY, dataList, ne,
                        betaBinDelta = 0.05, doubleCountsp = 0.4)


}
\keyword{model}
