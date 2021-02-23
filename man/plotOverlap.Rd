\name{plotOverlap}
\alias{plotOverlap}
\alias{overlapPriorPost}
\title{
  Calculate the Overlap between the Prior and the Posterior distribution
}
\description{
  
  The function \code{overlapPriorPost} implements the diagnostic of
     Garrett and Zeger (2000) to assess the overlap between the prior
     and the posterior.

  The function \code{plotOverlap} displays the table summarizing the
  overlap between the prior and the posterior distributions of the
  parameters (result of the function \code{overlapPriorPost}.
  
}
\usage{

overlapPriorPost(x, param, prior = "dnorm(z,0,sqrt(10))", from = -10, to = 10, n = 1000)

plotOverlap(x, df, cex = 1.5)
}
\arguments{
  \item{x}{
    An object of class \code{mcmc.list}.
  }
  \item{param}{
    A character string containing the name of a parameter in \code{x}
  }
  \item{prior}{a character string indicating the form of the prior for the
    parameter.  Note that the parameter should be named \code{z} in
    this expression.}
  \item{from,to,n}{the values of \code{param} over which the prior and
    posterior should be compared (passed to the function
    \code{density}.}
  \item{df}{
    A data.frame (see details).
  }
  \item{cex}{
    the size of the text in the table
  }
}
\details{
  
  For the function \code{plotOverlap}, tqhe object \code{df} must
  contain the following variables: (i) \code{Parameter}: the name of the
  parameter in the model \code{i}, (ii) \code{tau}: the value of the
  overlap parameter tau between the prior and the posterior,(iii)
  \code{namo}: a character string containing a plotmath expression to be
  used in the first column of the resulting table, (iv) \code{prior}: a
  character string indicating the form of the prior for the parameter;
  note that the parameter should be named \code{z} in this expression,
  (v) three columns named \code{from,to,n} passed to the function
  \code{density}, and used to calculate the values of the parameter over
  which the prior and posterior should be compared.
  
}
\value{

  The function \code{overlapPriorPost} returns a numeric value
  corresponding to the diagnostic.

}
\references{

  Calenge C., Menoni E., Milhau B., Montadert M. (in
  prep.). Building a Capercaillie Monitoring program in the Pyrenees 
  Mountains France based on Participatory Research.

  Garrett, E. and Zeger, S. 2000. Latent class model diagnosis.
  Biometrics, 56, 1055-1067.

}
\author{
  Clement Calenge \email{clement.calenge@ofb.gouv.fr}
}
\seealso{
  \code{\link{overlapPriorPost}} to calculate the coefficient
  \code{tau}.
}
\examples{

\dontrun{
    
## Use the MCMC samples from the model coefModelCountDetectBinREY 
rs <- do.call(rbind, coefModelCountDetectBinREY)

## Detects the name of the parameters for which tau should be calculated
library(stringr)
nam <- c("interceptpd","pente1pd","mukappa","sigmaREY",
         "sigmaepsilon","sigmaeta","sigmakappa",
         "sigmanu")
nam <- unlist(lapply(nam, function(z) colnames(rs)[str_detect(colnames(rs),z)]))

## Remove the parameters not part of the model (only three regions
## for KILs, and only one region for ULs)
nam <- nam[!nam%in%c("mukappa[3,2]", "mukappa[4,2]", "mukappa[2,3]",
            "mukappa[3,3]", "mukappa[4,3]","mukappa[5,3]")]

## For each parameter, calculates the value of tau:
lb <- sapply(nam, function(na) {
    if (str_detect(na, "mukappa")|na%in%c("interceptpd","pente1pd"))
        tau <- overlapPriorPost(coefModelCountDetectBinREY, na)
    if (str_detect(na, "sigma"))
        tau <- overlapPriorPost(coefModelCountDetectBinREY, na,
                                prior="dgamma(den$x[j],0.01, 0.01)", from=0, to=1000)
    return(tau)
})


## Builds the data.frame required by plotOverlap
df <- data.frame(Parameter=nam, tau=lb)
namo <- paste0("expression(", c("alpha[d]","beta[d]", gsub("kappa","",gsub(",", "", nam[3:11])),
                               "sigma[e]", "sigma[epsilon]",
                               "sigma[eta]^(1)", "sigma[eta]^(2)",
                               "sigma[eta]^(3)",
                               "sigma[kappa]","sigma[nu]"),")")
df$namo <- namo
df$prior <- "dgamma(z,0.01,0.01)"
df$prior[1:11] <- "dnorm(z,0,sqrt(10))"
df$from <- 0.01
df$to <- 20
df$to[12] <- 100
df$to[13] <- 600
df$to[17] <- 600
df$from[1:11] <- -4
df$to[1:11] <- 4


plotOverlap(coefModelCountDetectBinREY, df, cex=1)

}


}
\keyword{ hplot }
