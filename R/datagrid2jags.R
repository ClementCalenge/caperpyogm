datagrid2jags <-
function(searchData, experiment)
{
    if (!inherits(searchData, "data.frame"))
        stop("searchData should be a data.frame")
    if (!inherits(experiment, "data.frame"))
        stop("experiment should be a data.frame")


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

    chff(searchData$gr,"gr")

    dqua <- list(Nq=nrow(searchData),
                 gr=searchData$gr,
                 presenceArea=searchData$presenceArea-mean(searchData$presenceArea),
                 newUL=searchData$newUL,
                 previousSearch=searchData$previousSearch,
                 hasPreviousUL=searchData$hasPreviousUL,
                 presenceKL=searchData$presenceKL)


    debutant <- experiment[experiment$observer=="Inexperienced",]
    pro <- experiment[experiment$observer=="Experienced",]


    dqua$inex_Nquad <- debutant$Nquad
    dqua$expe_Nquad <- pro$Nquad
    dqua$inex_Nsect <- debutant$Nsect
    dqua$expe_Nsect <- pro$Nsect
    dqua$inex_Ndete <- debutant$Ndete
    dqua$expe_Ndete <- pro$Ndete

    dqua$Ninex <- length(dqua$inex_Nquad)
    dqua$Nexpe <- length(dqua$expe_Nquad)
    dqua$Ngr <- max(dqua$gr)


    class(dqua) <- "caperpyGridData"

    return(dqua)

}
