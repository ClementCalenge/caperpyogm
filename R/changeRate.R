changeRate <-
function(object, GR, changeRatePeriods=c(1,5))
{
    if (!inherits(object, "estimateMales")) {
        stop("object should be of class estimateMales")
    }

    if (length(changeRatePeriods)!=2)
        stop("changeRatePeriods should be a vector of length 2")

    if (!missing(GR)) {
        dis <- object[[GR]]
        grr <- GR
    } else {
        dis <- getTotal(object)
        grr <- 1
    }

    txvar <- 100*(dis[,changeRatePeriods[2]]-dis[,changeRatePeriods[1]])/dis[,changeRatePeriods[1]]

    return(txvar)
}
