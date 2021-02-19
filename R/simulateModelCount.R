simulateModelCount <-
function(coefs, dataList, verbose=TRUE)
{

    if (!inherits(coefs,"mcmc.list"))
        stop("incorrect class for coefs")

    if (is.null(attr(coefs, "registeredModel")))
        stop("Not a registered model")

    registeredModel <- attr(coefs, "registeredModel")

    includeREY  <- grepl("REY",registeredModel)
    betaBin <- grepl("BetaBin", registeredModel)
    obs2 <- grepl("Obs2",registeredModel)

    if (!inherits(dataList,"caperpyData"))
        stop("dataList should be of class caperpyData")

    ## Automatic detection of the model fitted
    na <- colnames(coefs[[1]])

    if (verbose) {
        cat("Simulate model", registeredModel, "...\n")
    }


    Nyears <- dataList$Nyears
    Nperiods <- dataList$Nperiods
    Ngr <- dataList$Ngr
    Ntypes <- dataList$Ntypes


    ## Gets the sampled parameters
    sigmanu <- 1/sqrt(MCMCchains(coefs, params="sigmanu"))
    sigmaepsilon <- 1/sqrt(MCMCchains(coefs, params="sigmaepsilon"))
    sigmaeta <- 1/sqrt(MCMCchains(coefs, params="sigmaeta"))
    sigmakappa <- 1/sqrt(MCMCchains(coefs, params="sigmakappa"))
    interceptpd <- MCMCchains(coefs, params="interceptpd")
    pente1pd <- MCMCchains(coefs, params="pente1pd")
    if (betaBin)
        delta2 <- MCMCchains(coefs, params="delta2")
    kap <- MCMCchains(coefs, params="kappa")
    if (includeREY)
        REY <- MCMCchains(coefs, params="REY")
    if (obs2) {
        pente2pd <- MCMCchains(coefs, params="pente2pd")
    } else {
        pente2pd <- matrix(rep(0,nrow(kap)),ncol=1)
    }

    ## Repeat sampling
    sim <- lapply(1:nrow(kap), function(r) {
        if (verbose)
            cat(r, " on ", nrow(kap), "\r")

        kappa <- array(kap[r,], dim=c(Ngr,Ntypes,Nperiods))
        epsilon <- matrix(0, dataList$Nleks, dataList$Nperiods)

        if (includeREY) {
            REYt <- array(REY[r,], dim=c(Ngr, Nyears))
        } else {
            REYt <- array(0, dim=c(Ngr, Nyears))
        }
        loglambda <- matrix(0, dataList$Nleks, dataList$Nperiods)
        lambda <- matrix(0, dataList$Nleks, dataList$Nperiods)
        eta <- rep(0, dataList$Nleks)
        nu <- rnorm(max(dataList$natun), 0, sigmanu)

        for (i in 1:dataList$Nleks) {
            eta[i] <- rnorm(1, 0,sigmaeta[r,dataList$ellp[i]])

            for (j in 1:dataList$Nperiods) {
                epsilon[i,j] <- rnorm(1, 0, sigmaepsilon[r,1])
                loglambda[i,j] <- kappa[dataList$gr[i],
                                        dataList$ellp[i],j]+
                    nu[dataList$natun[i]]*dataList$ell2[i]+eta[i]+epsilon[i,j]
                lambda[i,j] <- exp(loglambda[i,j])
            }
        }

        N <- rep(0, dataList$Nlekperiods)
        pd <- dataList$y
        lpd <- dataList$y
        y <- dataList$y

        for (i in 1:dataList$Nlekperiods) {
            N[i] <- rpois(1, lambda[dataList$lek[i], dataList$period[i]]  )
            for (k in 1:dataList$repetition[i]) {
                lpd[i,k] <- interceptpd[r,1] +
                    pente1pd[r,1]*dataList$observers[i,k]+
                    pente2pd[r,1]*(dataList$observers[i,k]^2)+
                    REYt[dataList$gr[dataList$lek[i]],dataList$year[i,k]]
                pd[i,k] <- exp(lpd[i,k])/(1+exp(lpd[i,k]))
                if (betaBin) {
                    pro <- rbeta(1,pd[i,k]*((1-delta2)/delta2), (1-pd[i,k])*((1-delta2)/delta2))
                } else {
                    pro <- pd[i,k]
                }
                y[i,k] <- rbinom(1, N[i], pro)
            }
        }
        return(y)
    })

    ## Tableau des y simules pour le betabin
    if (verbose)
        cat("formatting data...\r")
    siobb <- sapply(1:length(sim), function(r) {
        unlist(lapply(1:nrow(sim[[r]]), function(i) sim[[r]][i,1:dataList$repetition[i]]))
    })


    ## Original data
    ry <- unlist(lapply(1:nrow(dataList$y), function(i) dataList$y[i,1:dataList$repetition[i]]))
    robs <- unlist(lapply(1:nrow(dataList$y), function(i)
        dataList$observers[i,1:dataList$repetition[i]]))
    rperiod <- unlist(lapply(1:nrow(dataList$y), function(i) rep(dataList$period[i],dataList$repetition[i])))
    ryear <- unlist(lapply(1:nrow(dataList$y), function(i)
        dataList$year[i,1:dataList$repetition[i]]))
    rlek <- unlist(lapply(1:nrow(dataList$y), function(i) rep(dataList$lek[i],dataList$repetition[i])))
    rellp <- unlist(lapply(1:nrow(dataList$y), function(i)
        rep(dataList$ellp[dataList$lek[i]],dataList$repetition[i])))
    rgr <- unlist(lapply(1:nrow(dataList$y), function(i)
        rep(dataList$gr[dataList$lek[i]],dataList$repetition[i])))

    ori <- data.frame(lek=rlek, year=ryear, period=rperiod, nbobs=robs, nbmales=ry, gr=rgr, type=rellp)

    ##
    res <- list(origData=ori,
                sim=siobb)

    class(res) <- "caperpySim"
    return(res)
}
