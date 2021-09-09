\name{kfoldCVModelCount}
\alias{kfoldCVModelCount}
\alias{restartCV}
\alias{LLCount}
\alias{elpdLeks}
\alias{print.CVModelCount}
\alias{print.LLCountsSim}
\title{
  K-fold Cross-Validation for Lek Count Models
}
\description{
  K-fold Cross validation for Lek Count Models.
}
\usage{

kfoldCVModelCount(lekGroup, dataList,
                  registeredModel = c("modelCountDetectBinREY",
                                      "modelCountDetectBin",
                                      "modelCountDetectBetaBinREY",
                                      "modelCountDetectBinREYObs2"),
                  parameters, n.chains = 4, n.iter = 30000, thin = 30,
                  backupFile = tempfile(pattern = "bckp",
                  tmpdir = getwd(), fileext = ".Rds"))

restartCV(filename)

\method{print}{CVModelCount}(x, \dots)

LLCount(dataList, listCVCoef, lekGroup, iterations,
        nrepint = 1000, verbose = TRUE)

\method{print}{LLCountsSim}(x, \dots)

elpdLeks(x)

}
\arguments{
  \item{lekGroup}{
    vector of N integers from 1 to K, where N is the number of Leks in
    the dataset and K is the number of subsets used in K-fold
    cross-validation.
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
  \item{n.chains}{
    The number of MCMC chain to perform.
  }
  \item{n.iter}{
    The number of MCMC iterations to monitor.
  }
  \item{thin}{
    thinning intervals for monitors.
  }
  \item{backupFile}{
    character string containing the name of a file that will be used to
    backup calculations.  If the function \code{kfoldCVModelCount} is
    stopped while the calculations are not finished, the calculations
    can be restarted with \code{restartCV}, passing this filename as
    \code{filename} argument.
  }
  \item{filename}{character string.  See argument \code{backupFile}
    above.}
  \item{listCVCoef}{
    object of class \code{"CVModelCount"} returned by the function
    \code{kfoldCVModelCount} or \code{restartCV} containing the K
    objects of class mcmc.list corresponding to the K subsets.
  }
  \item{iterations}{integer vector containing the indices of the MCMC
    iterations for which LPD is to be calculated.  Can be left
    unspecified (in which case all iterations are used).}
  \item{nrepint}{The number of Monte-Carlo simulation to perform for
    integration in the calculation of LPD (see the vignette for
    details).}
  \item{verbose}{logical value indicating if information should be printed}
  \item{x}{for \code{elpdLeks} and \code{print.LLCountsSim}, an object
    of class \code{"LLCountSim"}.  For \code{print.CVModelCount}, an
    object of class \code{"CVModelCount"}.}
  \item{\dots}{
    additional arguments to be passed from and to other functions.
  }  
}
\details{
  
  To test the predictive ability of a model, this approach consists in
  splitting the original lek counts dataset \code{lekcounts} in G
  subsets of leks.  In the dataset used by Calenge et al. (in prep.), we
  used 10 groups of 33 leks.  Thus, for each subset i, we can build a
  calibration dataset with all subsets except subset i and fit the model
  by MCMC with this calibration dataset.  The function
  \code{kfoldCVModelCount} performs this operation. We can then predict
  the count data of each lek using a model fitted without this lek, and
  calculate the log-probability density (LPD) of all counts on each lek,
  for each MCMC vector of parameters simulated the model (avoiding the
  circularity consisting in using a model fit with a dataset to predict
  the same dataset).  The function \code{LLCount} performs this
  operation. Finally, the function \code{elpdLeks} calculates the
  expected log-probability for each lek (mean over all iterations).

  Note that the functions \code{kfoldCVModelCount} and \code{LLCount}
  can take a very long time.  See the vignette
  \code{vignette("caperpyogm")} for a more detailed description of the
  k-fold validation process.

}
\value{

  The functions \code{kfoldCVModelCount} and \code{restartCV} return an
  object of class \code{"CVModelCount"}, which is a list with K
  components, the component i being an object of class
  \code{"mcmc.list"} corresponding to the model fitted to the dataset
  excluding lek group i.

  The function \code{LLCount} return a matrix of class
  \code{"LLCountSim"} with N rows (the N leks) and P columns (the P MCMC
  iterations) containing the LPD for all counts of each lek calculated
  using each MCMC iteration.
  
  The function \code{elpdLeks} returns a vector with N elements (the N
  leks) containing the estimated expected log-probability densities for
  all counts of each lek.
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
  
  \code{\link{llcBinREY}} contains datasets generated by this process
  for several models.  See the vignette \code{vignette("caperpyogm")}
  for a more detailed description of the k-fold validation process.

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
llcBinREY <- LLCount(dataList, listCoefsCVBinREY, ooo)
}

## And the result is stored in the dataset:
llcBinREY

## Expected LPD can be calculated with:
elpdLeks(llcBinREY)


}
\keyword{model}
