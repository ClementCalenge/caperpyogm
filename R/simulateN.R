simulateN <- function (coefs, dataList)
{
    if (!inherits(coefs, "mcmc.list"))
        stop("incorrect class for coefs")
    if (is.null(attr(coefs, "registeredModel")))
        stop("Not a registered model")
    registeredModel <- attr(coefs, "registeredModel")
    if (registeredModel != "modelCountDetectBinREY")
        stop("only implemented for model \"modelCountDetectBinREY\"")
    if (!inherits(dataList, "caperpyData"))
        stop("dataList should be of class caperpyData")
    na <- colnames(coefs[[1]])
    Nyears <- dataList$Nyears
    Nperiods <- dataList$Nperiods
    Ngr <- dataList$Ngr
    Ntypes <- dataList$Ntypes
    sigmanu <- median(1/sqrt(MCMCchains(coefs, params = "sigmanu")))
    sigmaepsilon <- median(1/sqrt(MCMCchains(coefs, params = "sigmaepsilon")))
    sigmaeta <- apply(1/sqrt(MCMCchains(coefs, params = "sigmaeta")),
        2, median)
    sigmakappa <- median(1/sqrt(MCMCchains(coefs, params = "sigmakappa")))
    kap <- apply(MCMCchains(coefs, params = "kappa"), 2, median)
    kappa <- array(kap, dim = c(Ngr, Ntypes, Nperiods))
    epsilon <- matrix(0, dataList$Nleks, dataList$Nperiods)
    loglambda <- matrix(0, dataList$Nleks, dataList$Nperiods)
    lambda <- matrix(0, dataList$Nleks, dataList$Nperiods)
    eta <- rep(0, dataList$Nleks)
    nu <- rnorm(max(dataList$natun), 0, sigmanu)
    for (i in 1:dataList$Nleks) {
        eta[i] <- rnorm(1, 0, sigmaeta[dataList$ellp[i]])
        for (j in 1:dataList$Nperiods) {
            epsilon[i, j] <- rnorm(1, 0, sigmaepsilon)
            loglambda[i, j] <- kappa[dataList$gr[i], dataList$ellp[i],
                j] + nu[dataList$natun[i]] * dataList$ell2[i] +
                eta[i] + epsilon[i, j]
            lambda[i, j] <- exp(loglambda[i, j])
        }
    }
    N <- rep(0, dataList$Nlekperiods)
    pd <- dataList$y
    lpd <- dataList$y
    y <- dataList$y
    for (i in 1:dataList$Nlekperiods) {
        N[i] <- rpois(1, lambda[dataList$lek[i], dataList$period[i]])
    }
    return(N)
}
