\name{modelCountDetectBin}
\alias{modelCountDetectBin}
\alias{modelCountDetectBinREY}
\alias{modelCountDetectBinREYObs2}
\alias{modelCountDetectBetaBinREY}
\alias{registeredModels}
\alias{registeredCountModels}
\docType{data}
\title{
  Registered Models for the Lek Count Data.
}
\description{
  These datasets (lazy loaded) contain the JAGS code for the alternative
  models 
}
\usage{
data("modelCountDetectBin")
data("modelCountDetectBinREY")
data("modelCountDetectBinREYObs2")
data("modelCountDetectBetaBinREY")
}
\format{
  The format is a character string containg the JAGS code, directly
  usable with JAGS.
}
\details{
  These datasets contain the JAGS code for the following models:
   
  \code{modelCountDetectBin}
  This model describes the most basic N-mixture model tested in the
  paper.  The number of detected males is the result of a binomial
  process, where the logit of the detection probability is modeled as a
  function of a linear effect of the number of observers.

  \code{modelCountDetectBinREY}
  This model describes the most basic N-mixture model tested in the
  paper.  The number of detected males is the result of a binomial
  process, where the logit of the detection probability is modeled as a
  function of a linear effect of the number of observers plus a Year
  random effect.

  \code{modelCountDetectBinREYObs2}
  This model describes the most basic N-mixture model tested in the
  paper.  The number of detected males is the result of a binomial
  process, where the logit of the detection probability is modeled as a
  function of a quadratic effect of the number of observers plus a Year
  random effect.

  \code{modelCountDetectBinREY} This model describes the most basic
  N-mixture model tested in the paper.  The number of detected males is
  the result of a beta-binomial process, where the logit of the mean
  detection probability is modeled as a function of a linear effect of
  the number of observers plus a Year random effect.

}
\source{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\references{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\examples{

## The model 
cat(modelCountDetectBinREY)
}
\keyword{datasets}
