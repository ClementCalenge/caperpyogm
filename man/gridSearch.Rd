\name{gridSearch}
\alias{gridSearch}
\docType{data}
\title{
  Search for Unknown Capercaillie Leks in Grid Cells
}
\description{

  This dataset contains the results of the search in grid cells randomly
  sampled in the Pyrenees Mountains for unknown capercaillie leks
  between 2010 and 2019.

}
\usage{data("gridSearch")}
\format{
  A data frame with 496 observations on the following 6 variables.
  \describe{
    \item{\code{presenceArea}}{a numeric vector indicating the proportion of the
    grid cell covered by capercaillie presence area}
    \item{\code{newUL}}{a numeric vector indicating whether an unknown
      lek was uncovered by the search} 
    \item{\code{previousSearch}}{a numeric vector indicating whether (1)
      or not (1) there was a possibility that the grid cell has been
      searched for unknown leks prior to the random selection in the
      program (see Details)} 
    \item{\code{hasPreviousUL}}{a numeric vector indicating whether a
      unknown lek was already discovered between 2010 and the
      random selection of the grid cell}
    \item{\code{gr}}{a numeric vector indicating the geographic region
      where the searched cell is located}
    \item{\code{presenceKL}}{a numeric vector indicating
      whether a known active or indeterminate lek is present in the
      grid cell}
  }
}
\details{
  This dataset is one of the three datasets used to fit the model in
  Calenge et al. in prep. (the other two being \code{gridSearch} and
  \code{DetectionExpe}).   
  
  It contains the results of searches in randomly selected grid cell in
  the Pyrenees mountains, to estimate the proportion of grid cells
  containing unknown leks.  Note that the random sampling design changed
  in 2018, leading to the inclusion in the frame of new information
  concerning the presence of unknown leks in some grid cells, which were
  discovered outside the framework of our program between 2010 and 2017.
  \code{previousSearch} indicates if a cell could have been searched
  prior to its random sampling (in practice, takes the value 1 if the
  cell was sampled in 2018 or 2019).  The variable
  \code{hasPreviousUL} takes the value 1 if an unknown lek was
  discovered in a cell after 2010 but prior to its random sampling by
  the program.
  
}
\source{

  The original Dataset has been kindly provided by the Observatoire des
  Galliformes de Montagne:

  Observatoire des Galliformes de Montagne. 90 impasse des daudes
  74320 Sevrier, France.

}
\references{
  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.
}
\examples{

head(gridSearch)

}
\keyword{datasets}
