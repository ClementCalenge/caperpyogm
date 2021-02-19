simulateModelGrid <- function(coefs, dataList, what=c("newUL","hasPreviousUL"))
{
    if (!inherits(coefs, "mcmc.list"))
        stop("incorrect class for coefs")
    if (is.null(attr(coefs, "registeredModelQ")))
        stop("Not a registered model")
    what <- match.arg(what)

    if (!inherits(dataList, "caperpyGridData"))
        stop("dataList should be of class caperpyGridData")

    na <- colnames(coefs[[1]])

    ch <- MCMCchains(coefs)

    a <- ch[,c("a[1]","a[2]")]
    b <- ch[,c("b[1]","b[2]")]
    mgr <- cbind(as.numeric(dataList$gr==1), as.numeric(dataList$gr==2))
    ao <- mgr%*%t(a)
    bo <- mgr%*%t(b)
    lp <- ao+bo*dataList$presenceArea + dataList$presenceKL%*%t(ch[,"d"])
    pr <- exp(lp)/(1+exp(lp))
    pres <- rbinom(length(pr), 1, as.vector(pr)) %>% matrix(nrow=nrow(pr))

    ## Previous UL
    prul <- rbinom(length(pr), 1,
                   as.vector(pres*dataList$previousSearch*matrix(ch[,"pdprev"],
                                                                 byrow=TRUE, ncol=nrow(ch),
                                                                 nrow=nrow(pres)))) %>%
        matrix(nrow=nrow(pr))

    if (what=="hasPreviousUL") {
        so <- prul
    } else {

    ## Detection probability
    mimax <- ch[,"P_detection"]*ch[,c("P_inclusion_inex","P_inclusion_expe")]
    pd <- runif(nrow(mimax), mimax[,1], mimax[,2])
    ## new UL
    newUL <- rbinom(length(pr), 1,
                    as.vector(pres*(1-prul)*matrix(pd,
                                                   byrow=TRUE, ncol=nrow(ch),
                                                   nrow=nrow(pres)))) %>%
        matrix(nrow=nrow(pr))
        so <- newUL
    }


    res <- list(origData=as.data.frame(dataList[c("presenceArea","newUL","previousSearch",
                                                  "hasPreviousUL","presenceKL","gr")]),
                sim=so)
    attr(res,"what") <- what
    class(res) <- "caperpyGridSim"

    return(res)
}
