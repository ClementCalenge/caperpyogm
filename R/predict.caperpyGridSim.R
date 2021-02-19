predict.caperpyGridSim <- function(object, groupingFactor=c("gr","none"),
                                   includeGrouping=FALSE, se.fit=FALSE, ...)
{
    if (!inherits(object, "caperpyGridSim"))
        stop("x should be of class caperpyGridSim")
    groupingFactor <- match.arg(groupingFactor)
    if (includeGrouping & (groupingFactor == "none")) {
        warning("cannot include groupingFactor when groupingFactor is none")
        includeGrouping <- FALSE
    }
    sim <- object$sim

    what <- attr(object,"what")
    if (what=="newUL") {
        ry <- object$origData$newUL
    } else {
        ry <- object$origData$hasPreviousUL
    }

    if (groupingFactor=="gr") {
        index <- object$origData$gr
        dfs <- as.data.frame(sim)
        na <- paste0("groumpf", 1:ncol(dfs))
        names(dfs) <- na
        dfo <- data.frame(obs = ry)
        dfso <- cbind(dfo, dfs)
        oo <- aggregate(dfso, by = list(index), mean)
        sim <- as.matrix(oo[, na])
        ry <- oo$obs
    }

    predi <- rowMeans(sim)
    if ((se.fit + includeGrouping) == 0)
        return(predi)
    dfp <- data.frame(predicted = predi)
    if (se.fit) {
        sef <- apply(sim, 1, sd)
        dfp$se <- sef
    }
    if (includeGrouping) {
        dfp[[groupingFactor]] <- oo[, 1]
    }

    return(dfp)
}
