residuals.caperpySim <-
function(object,
                                 groupingFactor=c("none","lek","lekyear","lekperiod"),
                                 includeGrouping=FALSE, ...)
{
    if (!inherits(object, "caperpySim"))
        stop("x should be of class caperpySim")
    groupingFactor <- match.arg(groupingFactor)

    if (includeGrouping&(groupingFactor=="none")) {
        warning("cannot include groupingFactor when groupingFactor is none")
        includeGrouping <- FALSE
    }

    ry <- object$origData$nbmales
    sim <- object$sim

    if (groupingFactor!="none") {

        if (groupingFactor=="lek")
            index <- object$origData$lek

        if (groupingFactor=="lekyear")
            index <- paste(object$origData$lek,
                           object$origData$year)

        if (groupingFactor=="lekperiod")
            index <- paste(object$origData$lek,
                           object$origData$period)


        dfs <- as.data.frame(sim)
        na <- paste0("groumpf",1:ncol(dfs))
        names(dfs) <- na
        dfo <- data.frame(nbmo=object$origData$nbmales)
        dfso <- cbind(dfo,dfs)

        oo <- aggregate(dfso,by=list(index), mean)

        sim <- as.matrix(oo[,na])
        ry <- oo$nbmo

    }
    resi <- (ry-rowMeans(sim))/apply(sim,1,sd)
    names(resi) <- NULL
    if (includeGrouping) {
        resi <- data.frame(residual=resi)
        resi[[groupingFactor]] <- oo[,1]
    }
    return(resi)
}
