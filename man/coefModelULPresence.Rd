\name{coefModelULPresence}
\alias{coefModelULPresence}
\docType{data}
\title{
  Datasets Containing MCMC iterations for Registered Grid Cells Search Models
}
\description{
  This dataset is an object of class \code{"mcmc.list"} containing the
  result of the fit of registered models (see Details) to our grid cells search
  dataset (\code{gridSearch}).
}
\usage{

data("coefModelULPresence")

}
\format{
  Objects of class \code{"mcmc.list"}.
}
\details{

  This dataset contains the MCMC iterations obtained by fitting a
  registered model (see \code{help(modelULPresence)} for a list of
  registered model -- only one today) to our dataset
  \code{gridSearch}.  See examples below for a description of the
  process that led to this dataset.
  
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
\keyword{datasets}
