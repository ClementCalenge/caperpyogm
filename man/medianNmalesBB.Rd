\name{medianNmalesBB}
\alias{medianNmalesBB}
\alias{medianNmalesBB2}
\alias{medianNmalesDC}
\alias{medianNmalesDC2}
\docType{data}
\title{
  Estimated Number of Males with Various Levels of Unaccounted
  Heterogeneity in Detection and Double Counting
}
\description{
  These datasets (lazy loaded) contain the JAGS code for the alternative
  models 
}
\usage{
data("medianNmalesBB")
data("medianNmalesBB2")
data("medianNmalesDC")
data("medianNmalesDC2")
}
\format{
  Each object is a matrix with n rows (see below) and 10 columns.
}
\details{
  These datasets store the estimated point estimates of the number of
  males on the whole mountain range in 2010-2011 for simulated
  situations where the simulated detection process is different from the
  process hypothesized by the model (see vignette for details). We
  simulated different levels of either heterogeneity in detection
  probability or accidental double counting. For each level, we
  simulated 10 times the process to generate 10 datasets, and for each
  one, we fitted the model \code{modelCountDetectBinREY} to obtain an
  estimate of the total number of males on the whole mountain range in
  2010-2011. More precisely:
   
  \code{medianNmalesBB} This dataset gives the 10 point estimates (columns;
  median of the posterior distribution) of the number of males for each
  one of the 7 values (rows) of the parameter delta2 controlling the
  level of unaccounted heterogeneity in the beta-binomial distribution
  used to describe the variation in detection probability (see
  vignette). The 7 simulated values of delta2 where: 0, 0.001, 0.005,
  0.01, 0.05, 0.1, and 0.3. Here, each dataset was simulated by the
  function \code{simulateDataList}, i.e. by first
  simulating the state process (i.e., the true number of males on
  sampled leks) and then simulating the detection process with added
  unaccounted heterogeneity.

  \code{medianNmalesBB2} This dataset gives the 10 point estimates
  (columns; median of the posterior distribution) of the number of males
  for each one of the 6 values (rows) of the parameter delta2
  controlling the level of unaccounted heterogeneity in the
  beta-binomial distribution used to describe the variation in detection
  probability (see vignette). The 6 simulated values of delta2 where: 0,
  0.004, 0.015, 0.05, 0.1, and 0.2. Here, the same realization of the
  state process was used for all simulations (obtained with the function
  \code{simulateN}).  Only the detection process varied across
  simulations, and the detected number of males given the true number
  was simulated by the function \code{simulateDataList2}.

  \code{medianNmalesDC} This dataset gives the 10 point estimates
  (columns; median of the posterior distribution) of the number of males
  for each one of the 5 values (rows) of the probability that a detected
  male was counted twice (see vignette). The 5 simulated values of
  delta2 where: 0.05, 0.1, 0.2, 0.3, and 0.5. Here, each
  dataset was simulated by the function \code{simulateDataList}, i.e. by
  first simulating the state process (i.e., the true number of males on
  sampled leks) and then simulating the detection process with added
  unaccounted double counting.

  \code{medianNmalesDC2} This dataset gives the 10 point estimates
  (columns; median of the posterior distribution) of the number of males
  for each one of the 5 values (rows) of the probability that a detected
  male was counted twice (see vignette). The 5 simulated values of
  delta2 where: 0.05, 0.1, 0.2, 0.3, and 0.5. Here, the same realization
  of the state process was used for all simulations (obtained with the
  function \code{simulateN}).  Only the detection process varied across
  simulations, and the detected number of males given the true number
  was simulated by the function \code{simulateDataList2}.
  

}
\source{
  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.
}
\references{
  Calenge C., Menoni E., Milhau B., Foulche K, Chiffard J., Marchandeau
  S. (in prep.). The participatory monitoring of the capercaillie in the
  French Pyrenees.
}
\examples{

## See the vignette for further details
\dontrun{
vignette("caperpyogm")
}
}
\keyword{datasets}
