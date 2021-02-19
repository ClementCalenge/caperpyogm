fitModelCount <-
function(dataList,
                          registeredModel=c("modelCountDetectBinREY", "modelCountDetectBin",
                                            "modelCountDetectBetaBinREY","modelCountDetectBinREYObs2"),
                          parameters, inits, n.chains=4,
                          n.iter=500000, thin=500)
{

    if (!inherits(dataList,"caperpyData"))
        stop("dataList should be of class caperpyData")

    if (missing(inits))
        inits <- list(N=apply(dataList$y,1,max)+2)

    ## The model
    registeredModel <- match.arg(registeredModel)
    mod <- eval(parse(text=registeredModel))

    if (missing(parameters)) {

        ## For all models
        parameters <- c("mukappa","kappa","sigmakappa","sigmaepsilon","sigmaeta",
                        "sigmanu", "interceptpd","pente1pd")

        if (grepl("REY",registeredModel)) {
            parameters <- c(parameters, "sigmaREY","REY")
        }

        if (grepl("BetaBin", registeredModel)) {
            parameters <- c(parameters, "delta2")
        }

    }

    modb <- jags.model(textConnection(mod), data=dataList, n.chains=n.chains, inits = inits)
    cat("OK. Starting MCMC iterations...\n")
    st <- system.time(coefmod <- coda.samples(modb,
                                              variable.names=parameters,
                                              n.iter = n.iter, thin=thin))
    cat("OK. Time spent in main iterations:\n")
    print(st)
    attr(coefmod,"registeredModel") <- registeredModel
    return(coefmod)
}
