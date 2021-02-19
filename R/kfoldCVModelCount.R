kfoldCVModelCount <-
function(lekGroup, dataList,
                              registeredModel=c("modelCountDetectBinREY", "modelCountDetectBin",
                                                "modelCountDetectBetaBinREY","modelCountDetectBinREYObs2"),
                              parameters, n.chains=4,
                              n.iter=30000, thin=30,
                              backupFile=tempfile(pattern = "bckp", tmpdir=getwd(), fileext = ".Rds"))
{


    if (!inherits(dataList,"caperpyData"))
        stop("dataList should be of class caperpyData")

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


    ## Checks format factor
    chff <- function(v, nv)
    {
        if (!is.numeric(v))
            stop(paste0(nv," is not numeric"))
        unl <- sort(unique(v))
        msg <- paste0("The vector ", nv, " is not numbered from 1 to N")
        if (length(unl)!=length(1:max(unl)))
            stop(msg)
        if (!all(unl==c(1:max(unl))))
            stop(msg)
    }
    chff(lekGroup, "lekGroup")

    magr <- max(lekGroup)
    if (length(lekGroup)!=dataList$Nleks) {
        stop("lekGroup should have a length corresponding to the number of leks in dataList")
    }

    ldo <- lapply(1:magr, function(i) excludeLeks(dataList, lekGroup, i))

    listarg <- list(ldo=ldo, registeredModel=registeredModel, parameters=parameters,
                    n.chains=n.chains, n.iter=n.iter, thin=thin,
                    backupFile=backupFile)

    cat("Starting cross-validation...\n")

    reb <- list()
    for (i in 1:magr) {

        cat("#################################################\n##\n##\n")
        cat("## Leks Group", i,"\n\n")
        inits <- list(N=apply(ldo[[i]]$y,1,max)+2)
        reb[[i]] <- fitModelCount(ldo[[i]], registeredModel, parameters, inits, n.chains,
                                  n.iter, thin)
        attr(reb, "notyet") <- TRUE
        attr(reb, "listarg") <- listarg
        saveRDS(reb, backupFile)
        cat("Backup in", backupFile,"\n")
    }
    attr(reb, "notyet") <- NULL
    attr(reb, "listarg") <- NULL
    class(reb) <- "CVModelCount"
    attr(reb, "registeredModel") <- registeredModel
    cat("End. Saving results in backup file...\n")
    saveRDS(reb, backupFile)
    return(reb)
}
