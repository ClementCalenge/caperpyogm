\name{simulateModelGrid}
\alias{simulateModelGrid}
\alias{predict.caperpyGridSim}
\alias{print.caperpyGridSim}
\title{
  Simulate Datasets based on a Fitted Model for Lek Counts
}
\description{
  The function \code{simulateModelCount} simulate M datasets from a
  model to lek counts datasets (where M is the number of MCMC iterations
  used to fit the model).  These datasets can be used to assess the
  goodness of fit of the model.  Residuals and prediction can be
  calculated with \code{residuals} and \code{predict}.
}
\usage{

simulateModelGrid(coefs, dataList, what=c("newUL","hasPreviousUL"))

\method{predict}{caperpyGridSim}(object,
                             groupingFactor = c("gr", "none"),
                             includeGrouping = FALSE, se.fit = FALSE, \dots)

\method{print}{caperpyGridSim}(x, \dots)

}
\arguments{
  \item{coefs}{
    An object of class \code{mcmc.list} returned by the function
    \code{fitModelGrid}
  }
  \item{dataList}{
    An object of class \code{caperpyGridData} returned by the function
    \code{datagrid2jags}. 
  }
  \item{what}{
    character string indicating the target of the simulation (should we
    simulate the new unknown leks discovered during grid cell searches,
    or the leks discovered prior to sampling?).
  }
  \item{object,x}{
    an object of class \code{"caperpyGridSim"}
  }
  \item{groupingFactor}{
    character string indicating how to calculate predictions (see Details)
  }
  \item{includeGrouping}{
    logical value indicating whether the grouping factor should be
    included in the output.
  }
  \item{se.fit}{
    logical value indicating whether the standard error should be
    calculated for each prediction
  }
  \item{\dots}{
    additional arguments that can be passed to the function \code{print}
  }
}
\details{
  
  The simulation of datasets can be used to assess the goodness of fit
  of the model to the data (using randomization tests). It can also be
  calculate predictions of a model for grid cell searches data.  In
  practice, we use the parameters generated by each MCMC iteration to
  simulate a new dataset (i.e., new unknown leks that could have been
  discovered under the model).  Then, the user can compare an observed
  summary statistics (e.g. total number of discovered unknown leks
  summed during the whole period) to the same statistics calculated with
  each simulated dataset.

  The function predict will calculate the mean simulated value for each
  cell search (by averaging the simulated values).  The function can
  also be used to predict the mean proportion of cells containing an
  unknown lek for each level of a grouping factor (for example, the
  proportion of cells searched containing an unknown lek in each
  geographical region).  For the moment, only the geographical region is
  allowed as a grouping factor. \code{"none"} indicate that a prediction
  will be returned for each cell search.

  Note that two variables can be the target of the simulations: either
  the new unknown leks discovered by a cell search, or the leks
  previously discovered outside the framework of our study (between 2010
  and 2017).

}
\value{

  \code{simulateModelGrid} returns an object of class
  \code{"caperpyGridSim"}, which is a list with two elements: (i) an
  element named \code{origData}, which is the original dataset formatted
  as a data.frame, which contains the results of the N original cell
  searches, as well as explanatory variables (regions, proportion of the
  cell with presence area, etc.), and (ii) an element named \code{sim},
  which is a matrix with N rows and S columns containing the S simulated
  lek presence datasets according to the model.

  \code{predict.caperpyGridSim} returns either a vector of numeric
  values (if both \code{se.fit} and \code{includeGrouping} are both
  FALSE) or a data.frame containing the predictions, and the SE and/or
  grouping factor.
  
}
\references{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{datagrid2jags}} for more information on how to fit a
  model to grid cell search data.
}
\examples{

## We work on the gridSearch dataset...
head(gridSearch)

## ...and on the experiment dataset:
head(DetectionExpe)

## We prepare the dataset to fit the model with JAGS
dataListQ <- datagrid2jags(gridSearch, DetectionExpe)

dataListQ

## We then fit the model. WARNING!!! THIS COMMAND IS VERY SLOW AND
## CAN TAKE SEVERAL HOURS
\dontrun{
coefModelULPresence <- fitModelGrid(dataListQ, "modelULPresence")
}


## We then simulate datasets based on these coefficients
simQuad <- simulateModelGrid(coefModelULPresence, dataListQ)

## prediction:
predict(simQuad)

## Observation for each geographical region
tapply(dataListQ$newUL, dataListQ$gr, mean)
       
}
\keyword{model}
