\name{gridFrame}
\alias{gridFrame}
\docType{data}
\title{
  Frame of All Grid Cells Defined in the Pyrenees Mountains.
}
\description{
  This dataset contains the list of all grid cells partitioning the
  Pyrenees mountains, with the descriptive variables for each, allowing
  an inference for the whole area (see \code{help(estimateNmales)}.
}
\usage{data("gridFrame")}
\format{
  A data frame with 1803 observations (the 1803 grid cells) on the following 4 variables.
  \describe{
    \item{\code{presenceArea}}{a numeric vector indicating the proportion of the
      grid cell covered by capercaillie presence area}
    \item{\code{presenceKL}}{a numeric vector indicating
      whether a known active or indeterminate lek is present in the
      grid cell}
    \item{\code{gr}}{a numeric vector indicating the geographic region
      where the grid cell is located (numbered from 1 to 5, corresponding
      to the 5 defined geographic regions)}
    \item{\code{grqf}}{a numeric vector indicating the geographic region
      code used in the model (eithe 1 -- piemont -- or 2 -- High
      mountains).} 
  }
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
head(gridFrame)

}
\keyword{datasets}
