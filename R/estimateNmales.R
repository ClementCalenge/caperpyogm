estimateNmales <-
function(coefsCounts, coefsUL, gridFrame, NKAL, NKIL,
                           what=c("tot", "kalc","kilc","ulc","ul"))
{

    ## Correct class ?
    if (!inherits(coefsCounts, "mcmc.list"))
        stop("coefsCounts is not the correct class")
        if (is.null(attr(coefsCounts, "registeredModel")))
        stop("coefsCounts is not a registered model")


    if (!inherits(coefsUL, "mcmc.list"))
        stop("coefsUL is not the correct class")
    if (is.null(attr(coefsUL, "registeredModelQ")))
        stop("coefsUL is not a registered model")

    what <- match.arg(what)

    ## Extract interesting parameters from both models
    kap <- MCMCchains(coefsCounts, params="kappa")
    sigmanu <- 1/MCMCchains(coefsCounts, params="sigmanu")
    sigmaeta <- 1/MCMCchains(coefsCounts, params="sigmaeta")
    sigmaepsilon <- 1/MCMCchains(coefsCounts, params="sigmaepsilon")
    a <- MCMCchains(coefsUL, params="a")
    b <- MCMCchains(coefsUL, params="b")
    d <- MCMCchains(coefsUL, params="d")


    ## The number of iterations should be the same in the two coef lists
    if (nrow(a)<nrow(kap)) {
        kap <- kap[1:nrow(a),]
        sigmanu <- sigmanu[1:nrow(a),,drop=FALSE]
        sigmaeta <- sigmaeta[1:nrow(a),,drop=FALSE]
        sigmaepsilon <- sigmaepsilon[1:nrow(a),,drop=FALSE]
        msg <- paste0("The number of iterations is larger in coefsCounts than in coefsUL.\n",
                      "Only the first ", nrow(a), " iterations were kept for coefsCounts")
        warning(msg)
    }
    if (nrow(kap) < nrow(a)) {
        a <- a[1:nrow(kap),,drop=FALSE]
        b <- b[1:nrow(kap),,drop=FALSE]
        d <- d[1:nrow(kap),,drop=FALSE]
        msg <- paste0("The number of iterations is larger in coefsUL than in coefsCounts.\n",
                      "Only the first ", nrow(a), " iterations were kept for coefsUL")
        warning(msg)
    }


    ## Number of GR and periods:
    ngr <- length(NKAL)
    nperiods <- ncol(kap)/(ngr*3)

    gk <- function(i)
    {
        array(kap[i,], c(ngr,3,nperiods))
    }


    ## Average number a known active lek
    lap <- do.call(rbind,lapply(1:nrow(kap), function(i) {
                             as.vector(exp(gk(i)[,1,]+sigmaeta[i,1]/2+
                                           sigmaepsilon[i,1]/2))
                         }))

    ## Average number on known indeterminate lek
    lip <- do.call(rbind,lapply(1:nrow(kap), function(i) {
                             ma <- (exp(gk(i)[,2,]+sigmanu[i,1]/2+sigmaeta[i,2]/2+sigmaepsilon[i,1]/2))
                             as.vector(rbind(ma[1,], ma[1,], ma[2,],ma[2,], ma[5,]))
                         }))


    ## Number of grid cells in each GR
    sapi <- t(sapply(1:nrow(a), function(i) {
        lp <- a[i,gridFrame$grqf]+b[i,gridFrame$grqf]*gridFrame$presenceArea+d[i,1]*gridFrame$presenceKL
        propi <- exp(lp)/(1+exp(lp))
        tapply(propi,gridFrame$gr,sum)
    }))

    ## expectation of number of males on UL
    lpi <- do.call(rbind,lapply(1:nrow(kap), function(i) {
                             exp(gk(i)[1,3,]+sigmaeta[i,3]/2+sigmaepsilon[i,1]/2)
                         }))


    ## Population size on UL
    npit <- rowSums(sapi)
    lnpi <- npit*lpi



    ## For each region
    pre <- t(sapply(1:nrow(lap), function(i) {
        if (what=="kalc")
            return(as.vector(matrix(NKAL*lap[i,],ngr,nperiods)))
        if (what=="kilc")
            return(as.vector(matrix(NKIL*lip[i,],ngr,nperiods)))
        if (what=="ulc")
            return(as.vector(outer(sapi[i,],lpi[i,])))
        if (what=="ul")
            return(sapi[i,])

        ## Case: tot
        parreg <- matrix(NKAL*lap[i,],ngr,nperiods)+
            matrix(NKIL*lip[i,],ngr,nperiods)+
            outer(sapi[i,],lpi[i,])
        return(as.vector(parreg))
    }))

    if (what!="ul") {
        lipre <- lapply(1:ngr, function(g) {
            pre[,sapply(1:nperiods, function(i) g+(i-1)*ngr)]
        })
    } else {
        lipre <- lapply(1:ngr, function(g) {
            pre[,g]
        })
    }


    results <- lipre
    class(results) <- "estimateMales"
    return(results)
}
