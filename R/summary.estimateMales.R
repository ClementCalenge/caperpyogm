summary.estimateMales <-
function(object, GR, CIlevel=80, changeRatePeriods=c(1,5),
                                  threshold=c(-10,10), ...)
{
    if (!inherits(object, "estimateMales")) {
        stop("object should be of class estimateMales")
    }
    if (!(CIlevel<100&CIlevel>0))
        stop("CIlevel should be comprised between 0 and 100")

    if (length(changeRatePeriods)!=2)
        stop("changeRatePeriods should be a vector of length 2")

    if (!missing(GR)) {
        dis <- object[[GR]]
        grr <- GR
    } else {
        dis <- getTotal(object)
        grr <- 1
    }

    Nperiods <- ncol(object[[1]])
    NGR <- length(object)
    cat("Estimates of the number of males during the ",Nperiods, " periods",
        "\nin the ",c("whole Pyrenees Mountains ", paste0("geographic region ", grr, " "))[(!missing(GR))+1],
        " (median and ", CIlevel,"% CI):\n\n", sep="")


    df <- data.frame(Point.est=apply(dis,2,median),
                     CIlow=apply(dis,2,quantile,(1-CIlevel/100)/2),
                     CIup=apply(dis,2,quantile,1-(1-CIlevel/100)/2))
    print(df)

    cat("\nChange rate between period", changeRatePeriods[1], "and period",
        changeRatePeriods[2], "is: ")
    txvar <- 100*(dis[,changeRatePeriods[2]]-dis[,changeRatePeriods[1]])/dis[,changeRatePeriods[1]]
    cre <- c(median(txvar), quantile(txvar,(1-CIlevel/100)/2), quantile(txvar,1-(1-CIlevel/100)/2))

    cat(paste0(round(cre[1], 2),"% (", CIlevel,"% CI: ",
               round(cre[2], 2),"% -- ",
               round(cre[3], 2),"%)\n"))
    cat("\nProbability of scenarii (decrease:< ", threshold[1], "%; stability:",
        threshold[1],"--",threshold[2],"%; increase:>", threshold[2],"%):\n\n", sep="")
    df2 <- data.frame(scenario=c("decrease","stability","increase"),
                      probability=c(mean(txvar<threshold[1]),
                                    mean(txvar>=threshold[1]&txvar<threshold[2]),
                                    mean(txvar>=threshold[2])))
    print(df2)
    return(invisible(list(sizeEstimate=df, changeRate=cre, scenarii=df2)))
}
