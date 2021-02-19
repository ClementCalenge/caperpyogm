fitModelGrid <-
function(dataList,
                            registeredModel=c("modelULPresence"),
                            parameters, inits, n.chains=4,
                            n.iter=500000, thin=500)
{

    if (!inherits(dataList,"caperpyGridData"))
        stop("dataList should be of class caperpyGridData")

    ## The model
    registeredModel <- match.arg(registeredModel)
    mod <- eval(parse(text=registeredModel))

    if (missing(parameters)) {
        parameters <- c("P_inclusion_inex","P_inclusion_expe","P_detection", "a","b","d",
                        "pdprev")
    }

    if (missing(inits)) {
        inits <- list(P_inclusion_inex=0.4, P_inclusion_expe=0.6, P_detection=0.99, pres=rep(1,dataList$Nq))
    }

    modb <- jags.model(textConnection(mod), data=dataList, n.chains=n.chains, inits = inits)

    cat("OK. Starting MCMC iterations...\\n")
    st <- system.time(coefmod <- coda.samples(modb,
                                              variable.names=parameters,
                                              n.iter = n.iter, thin=thin))
    cat("OK. Time spent in main iterations:\\n")
    print(st)
    attr(coefmod,"registeredModelQ") <- registeredModel
    return(coefmod)
}
