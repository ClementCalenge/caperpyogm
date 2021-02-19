LLCount <-
function(dataList, listCVCoef, lekGroup, iterations, nrepint=1000, verbose=TRUE)
{

    if (!inherits(dataList,"caperpyData"))
        stop("dataList should be of class caperpyData")

    if (missing(iterations))
        iterations <- 1:nrow(MCMCchains(listCVCoef[[1]],"kappa"))

    if (!inherits(listCVCoef, "CVModelCount"))
        stop("listCVCoef should be of class CVModelCount")

    ## Automatic detection of the model fitted
    na <- colnames(listCVCoef[[1]][[1]])
    registeredModel <- attr(listCVCoef, "registeredModel")
    includeREY  <- grepl("REY",registeredModel)
    betaBin <- grepl("BetaBin", registeredModel)
    obs2 <- grepl("Obs2",registeredModel)

    if (verbose) {
        cat("Simulate model", registeredModel, "...\n")
    }

    ## To allow verbose information if required
    mait <- max(iterations)

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


    ## Split the data per lek group to calculate their likelihood
    ly <- lapply(split(as.data.frame(dataList$y), dataList$lek),as.matrix)
    lo <- lapply(split(as.data.frame(dataList$observers), dataList$lek),as.matrix)
    le <- lapply(split(as.data.frame(dataList$year), dataList$lek),as.matrix)
    lp <- split(dataList$lek, dataList$lek)
    la <- split(dataList$period, dataList$lek)
    lr <- split(dataList$repetition, dataList$lek)

    ## which leks
    ord <- unlist(lapply(1:max(lekGroup), function(kf) {
        c(1:dataList$Nleks)[lekGroup==kf]
    }))


    ## The dataset likelihood is calculated for each iteration
    results <- sapply(iterations, function(r) {

        if (verbose)
            cat("Iteration:", r, "on", mait, "\r")


        ##
        lob <- lapply(1:max(lekGroup), function(kf) {

            ## coef estimated once group kf is removed
            coe <- listCVCoef[[kf]]

            ## parameters of the model from the corresponding group
            sigmanu <- 1/sqrt(MCMCchains(coe, params="sigmanu"))[r,1]
            sigmaepsilon <- 1/sqrt(MCMCchains(coe, params="sigmaepsilon"))[r,1]
            sigmaeta <- 1/sqrt(MCMCchains(coe, params="sigmaeta"))[r,]
            sigmakappa <- 1/sqrt(MCMCchains(coe, params="sigmakappa"))[r,1]
            interceptpd <- MCMCchains(coe, params="interceptpd")[r,1]
            pente1pd <- MCMCchains(coe, params="pente1pd")[r,1]
            if (obs2) {
                pente2pd <- MCMCchains(coe, params="pente2pd")[r,1]
            } else {
                pente2pd <- 0
            }
            kap <- MCMCchains(coe, params="kappa")[r,]
            if (includeREY)
                REY <- MCMCchains(coe, params="REY")[r,]
            kappa <- array(kap, dim=c(5,3,10))
            if (betaBin)
                delta2 <- MCMCchains(coe, params="delta2")[r,1]


            ## Arrays
            if (includeREY) {
                REY <- array(REY, dim=c(dataList$Ngr, dataList$Nyears))
            } else {
                REY <- array(0, dim=c(dataList$Ngr, dataList$Nyears))
            }

            ## For each place
            proparind <- sapply(c(1:dataList$Nleks)[lekGroup==kf], function(i) {


                ## Year for this group of leks
                an <- la[[i]]

                kappala <- matrix(rep(kappa[dataList$gr[i],
                                            dataList$ellp[i],an],nrepint),
                                  nrow=nrepint, byrow = TRUE)

                epsilon <- matrix(rnorm(ncol(kappala)*nrepint, 0, sigmaepsilon),nrow=nrepint)
                effetsite <- matrix(rep(rnorm(nrepint, mean=0, sd=sigmaeta[dataList$ellp[i]]),
                                        length(an)), ncol=ncol(kappala))

                nu <- 0
                if (dataList$ellp[i]==2)
                    nu <- matrix(rep(rnorm(nrepint, 0, sigmanu),
                                     ncol(kappala)),nrow=nrepint)
                lambda <- exp(kappala+nu+effetsite+epsilon)
                lambda[lambda>300] <- 300 ## avoid overflow



                ## log-likelihood
                lvrai <- sum(sapply(1:length(an), function(a) {
                    no <- lo[[i]][a,1:lr[[i]][a]]
                    ny <- ly[[i]][a,1:lr[[i]][a]]
                    lpdem <- interceptpd+pente1pd*no+pente2pd*(no^2)+REY[dataList$gr[i],le[[i]][a,1:lr[[i]][a]]]
                    pdem <- exp(lpdem)/(1+exp(lpdem))
                    if (betaBin) {
                        pd <- sapply(1:length(pdem), function(e) rbeta(nrepint, pdem[e]*((1-delta2)/delta2),
                                                                 (1-pdem[e])*((1-delta2)/delta2)))
                    } else {
                        pd <- pdem
                    }
                    samn <- c(rpois(nrepint, lambda[,a]), max(ny))

                    po <- sum(log(sapply(1:length(pdem), function(e) {
                        if (betaBin) {
                            pde <- pd[,e]
                        } else {
                            pde <- pd[e]
                        }
                        mean(dbinom(ny[e], samn, pde))
                    })))
                    return(po)
                }))
                return(lvrai)
            })
            return(proparind)
        })
        return(unlist(lob))
    })

    results <- results[order(ord),]
    class(results) <- "LLCountsSim"

    return(results)

}
