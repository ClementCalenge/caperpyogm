\name{caperpyogm-package}
\alias{caperpyogm-package}
\alias{caperpyogm}
\docType{package}
\title{
\packageTitle{caperpyogm}
}
\description{
\packageDescription{caperpyogm}
}
\details{

  This package is the companion package to the paper of Calenge et
  al. (in prep.) describing the monitoring of the capercaillie (Tetrao
  urogallus) in the Pyrenees mountains (France).  It contains all the
  data and functions to reproduce the modelling approach presented in
  this paper to estimate the number of male capercaillie in the five
  geographic regions of the Pyrenees mountains.  A vignette details the
  whole process and model, but we summarize it here.
  
  Three datasets allow this modelling approach: \code{lekcounts}
  contains the results of counts carried out on capercaillie leks,
  \code{gridSearch} contains the result of the search for unknown leks
  in grid cells randomly selected in the frame of grid cells (this frame
  is stored in \code{gridFrame}), and \code{DetectionExpe} contains the
  result of an experiment carried out to assess the detectability of
  unknown leks in these grid cells searches.

  The functions \code{fitModelCount} and \code{simulateModelCount} can
  be used to model the results of male counts carried out on leks during
  spring between 2010 and 2019 (the latter allowing to assess the
  goodness of fit).  Note that different fitted models (i.e., MCMC
  iterations) are available as dataset in the package (see the help page
  of \code{coefModelCountDetectBinREY}).  The JAGS models are described
  on the help page of \code{modelCountDetectBinREY} and the
  coefficients.  Note that the quality of prediction of the different
  models can be compared by cross-validation (see the help page of
  \code{kfoldCVModelCount}).

  The function \code{fitModelGrid} can be used to model the result of
  the searches of new, unknown, leks in grid cells randomly sampled in the
  Pyrenees (combining the datasets \code{gridSearch} and
  \code{DetectionExpe}).

  Finally, the number of males can be estimated from the results of
  these two models with the function \code{EstimateNmales}. Graphical
  display and summary functions are provided on the help page of these
  functions.
  
}
\author{
\packageAuthor{caperpyogm}

Maintainer: \packageMaintainer{caperpyogm}
}
\references{
  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.


  Data in the package were kindly provided by:
  
  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.

}
\keyword{ package }
\examples{

## All relevant examples are presented on the help pages
## of the functions described in the section details

}
