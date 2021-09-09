\name{dataCount2jags}
\alias{dataCount2jags}
\alias{fitModelCount}
\alias{print.caperpyData}
\title{
  Functions Required for the Lek Count Model Fit
}
\description{
  
  \code{dataCount2jags} prepares the lek count dataset for model fit and
  \code{fitModelCount} fits the model.

}
\usage{

dataCount2jags(lek, period, nbobs, nbmales, gr, type, natun, year)

\method{print}{caperpyData}(x, ...)

fitModelCount(dataList,
              registeredModel=c("modelCountDetectBinREY",
                                "modelCountDetectBin",
                                "modelCountDetectBetaBinREY",
                                "modelCountDetectBinREYObs2"),
              parameters, inits, n.chains=4,
              n.iter=500000, thin=500)

}
\arguments{
  \item{lek}{
    a vector containing the lek label for each lek count in the
    dataset.  Must be numbered from 1 to L (where L is the total number
    of leks).
  }
  \item{period}{
    a vector containing the period label for each lek count in the
    dataset.  Must be numbered from 1 to P (where P is the total number
    of periods)
  }
  \item{nbobs}{
    a vector containing the number of observers for each lek count in the dataset.
  }
  \item{nbmales}{
    a vector containing the number of males counted for each lek count in the dataset.    
  }
  \item{gr}{
    a vector containing the geographic region label for each lek count
    in the dataset. Must be numbered from 1 to 5.
  }
  \item{type}{
    a vector containing the type label for each lek count
    in the dataset. Must be numbered from 1 to 3 (1 = known active leks,
    2 = known indeterminate leks, 3 = unknown leks).
  }
  \item{natun}{
    a vector containing the type label for each lek count
    in the dataset. Must be numbered from 1 to U (where U is the total
    number of natural units).
  }
  \item{year}{
    a vector containing the period label for each lek count in the
    dataset.  Must be numbered from 1 to Y (where Y is the total number
    of years)
  }
  \item{dataList}{
    object of class \code{"caperpyData"} returned by the function
    \code{dataCount2jags} containing the dataset used to fit the model.
  }
  \item{registeredModel}{
    character string containing the name of a registered count model
    (see \code{help("modelCountDetectBin")} for a list of registered
    model names).
  }
  \item{parameters}{
    vector of character string containing name of parameters to monitor
    during MCMC iterations.  Can be left unspecified.
  }
  \item{inits}{
    The starting values for the MCMC. Can be left unspecified.
  }
  \item{n.chains}{
    The number of MCMC chain to perform.
  }
  \item{n.iter}{
    The number of MCMC iterations to monitor.
  }
  \item{thin}{
    thinning intervals for monitors.
  }
  \item{x}{
    an object of class \code{"caperpyData"}
  }
  \item{\dots}{
    additional arguments to be passed from and to other functions.
  }
}
\value{

  \code{dataCount2jags} returns an object of class \code{"caperpyData"}

  \code{fitModelCount} returns an object of class \code{"mcmc.list"},
  returned by \code{rjags}.

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
  \code{\link{simulateModelCount}} to simulate datasets with the model,
  for residual and prediction analysis.
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
\dontrun{
coefModelCountDetectBinREY <- fitModelCount(dataList, "modelCountDetectBinREY")
}

## To save time for the user, we have stored the result of this
## command in the dataset coefModelCountDetectBinREY.
str(coefModelCountDetectBinREY)

}
\keyword{model}
