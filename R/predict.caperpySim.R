predict.caperpySim <-
function(object,
                               groupingFactor=c("none","lek","lekyear","lekperiod"),
                               includeGrouping=FALSE, se.fit=FALSE, ...)
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

    predi <- rowMeans(sim)
    if ((se.fit+includeGrouping)==0)
        return(predi)

    dfp <- data.frame(predicted=predi)
    if (se.fit) {
        sef <- apply(sim,1,sd)
        dfp$se <- sef
    }

    if (includeGrouping) {
        dfp[[groupingFactor]] <- oo[,1]
    }

    return(dfp)
}
