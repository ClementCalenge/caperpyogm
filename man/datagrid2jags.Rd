\name{datagrid2jags}
\alias{datagrid2jags}
\alias{fitModelGrid}
\alias{print.caperpyGridData}
\title{
  Functions Required for the Grid Cell Search Model Fit
}
\description{
  
  \code{datagrid2jags} prepares the grid cell search dataset for model fit
  and \code{fitModelGrid} fits the model.

}
\usage{

  datagrid2jags(searchData, experiment)

  \method{print}{caperpyGridData}(x, \dots)

  fitModelGrid(dataList, registeredModel = c("modelULPresence"),
               parameters, inits, n.chains = 4, n.iter = 5e+05, thin = 500)
}
\arguments{

  \item{searchData}{ A data.frame containing the results of grid cell
    searches carried out between 2010 and 2019, with the same format as
    the dataset \code{gridSearch} (must contain columns named
    \code{gr} indicating the geographic region where the searched
    cell is located, \code{presenceArea} indicating the proportion of
    the cell covered by area of presence of the capercaillie, \code{newUL}
    indicating whether an unknown lek was uncovered by the search,
    \code{previousSearch} indicating whether (1) or not (1) there was a
    possibility that the grid cell has been searched for new leks prior to
    the random selection in the program, \code{hasPreviousUL} indicating
    whether an unknown lek was already discovered between 2010 and the
    random selection of the cell, and \code{presenceKL} indicating
    whether a known active or indeterminate lek is present in the
    cell).  }
  
  \item{experiment}{ A data.frame containing the results of the
    experiment carried out to assess the probability of detection of an
    unknown lek in a grid cell with the search protocol, with the same
    format as the dataset \code{DetectionExpe} (must contain the 4
    variables: \code{observer} containing the type of observer that
    searched the grid cell [experienced/inexperienced], \code{Nquad}
    indicating the number of leks in the grid cell searched by the
    observer, \code{Nsect} indicating how many leks were actually
    included in the sector defined by the observer, among those present
    within the cell, and \code{Ndete} indicating how many leks were
    detected by the observers among those both present in the grid cell
    and included in the search sector).}



  \item{dataList}{
    object of class \code{"caperpyGridData"} returned by the function
    \code{datagrid2jags} containing the dataset used to fit the model.
  }

  \item{registeredModel}{
    character string containing the name of a registered count model
    (today, "modelULPresence" is the only available model, see
    \code{help("modelULPresence")}).
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
    an object of class \code{"caperpyGridData"}
  }
  \item{\dots}{
    additional arguments to be passed from and to other functions.
  }
  
}
\value{

  \code{datagrid2jags} returns an object of class \code{"caperpyGridData"}

  \code{fitModelGrid} returns an object of class \code{"mcmc.list"},
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
  \code{\link{modelULPresence}} for a description of the registered models.
}
\examples{

## We work on the gridSearch dataset...
head(gridSearch)

## ...and on the experiment dataset:
head(DetectionExpe)

## We prepare the dataset to fit the model with JAGS
dataListQ <- datagrid2jags(gridSearch, DetectionExpe)

## We then fit the model. WARNING!!! THIS COMMAND IS VERY SLOW AND
## CAN TAKE SEVERAL HOURS
\dontrun{
coefModelULPresence <- fitModelGrid(dataListQ, "modelULPresence")
}

## To save time for the user, we have stored the result of this
## command in the dataset coefModelULPresence.
str(coefModelULPresence)


}
\keyword{model }
