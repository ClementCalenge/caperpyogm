simulateDataList2 <- function (coefs, dataList, simn, betaBinDelta = NULL, doubleCountsp = NULL)
{
    if (!inherits(coefs, "mcmc.list"))
        stop("incorrect class for coefs")
    if (is.null(attr(coefs, "registeredModel")))
        stop("Not a registered model")
    registeredModel <- attr(coefs, "registeredModel")
    if (registeredModel != "modelCountDetectBinREY")
        stop("only implemented for model \"modelCountDetectBinREY\"")
    if (length(simn)!=nrow(dataList$y))
        stop("incorrect length for simn")
    if (!is.null(betaBinDelta)) {
        betaBin <- TRUE
        delta2 <- betaBinDelta
    }
    else {
        betaBin <- FALSE
    }
    if (!is.null(doubleCountsp)) {
        doubleCounts <- TRUE
    }
    else {
        doubleCounts <- FALSE
    }
    if (!inherits(dataList, "caperpyData"))
        stop("dataList should be of class caperpyData")
    na <- colnames(coefs[[1]])
    Nyears <- dataList$Nyears
    Nperiods <- dataList$Nperiods
    Ngr <- dataList$Ngr
    Ntypes <- dataList$Ntypes

    interceptpd <- median(MCMCchains(coefs, params = "interceptpd"))
    pente1pd <- median(MCMCchains(coefs, params = "pente1pd"))
    REY <- apply(MCMCchains(coefs, params = "REY"), 2, median)
    REYt <- array(REY, dim = c(Ngr, Nyears))

    N <- simn
    pd <- dataList$y
    lpd <- dataList$y
    y <- dataList$y
    for (i in 1:dataList$Nlekperiods) {
        for (k in 1:dataList$repetition[i]) {
            lpd[i, k] <- interceptpd + pente1pd * dataList$observers[i,
                k] + REYt[dataList$gr[dataList$lek[i]], dataList$year[i,
                k]]
            pd[i, k] <- exp(lpd[i, k])/(1 + exp(lpd[i, k]))
            if (betaBin) {
                pro <- rbeta(1, pd[i, k] * ((1 - delta2)/delta2),
                  (1 - pd[i, k]) * ((1 - delta2)/delta2))
            }
            else {
                pro <- pd[i, k]
            }
            y[i, k] <- rbinom(1, N[i], pro)
            if (doubleCounts) {
                y[i, k] <- y[i, k] + rbinom(1, y[i, k], doubleCountsp)
            }
        }
    }
    dataList$y <- y
    return(dataList)
}
