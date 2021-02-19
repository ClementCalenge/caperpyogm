excludeLeks <-
function(dataList, lekGroup, removedGroup)
{


    if (!inherits(dataList,"caperpyData"))
        stop("dataList should be of class caperpyData")

    if (length(lekGroup)!=dataList$Nleks)
        stop("The number of leks in lekGroup != dataList$Nleks")

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

    ld <- l <- dataList

    keptLeks <- sort(c(1:ld$Nleks)[lekGroup!=removedGroup])

    d <- data.frame(noupla=1:length(keptLeks), pla=keptLeks,
                    natun=as.numeric(factor(l$natun[keptLeks])),
                    gr=l$gr[keptLeks], ellp=l$ellp[keptLeks],
                    ell2=l$ell2[keptLeks])

    ld$y <- l$y[l$lek%in%keptLeks,]
    ld$observers <- l$observers[l$lek%in%keptLeks,]
    ld$year <- l$year[l$lek%in%keptLeks,]
    ld$lek <- l$lek[l$lek%in%keptLeks]
    ld$lek <- sapply(ld$lek, function(x) d$noupla[d$pla==x])
    ld$period <- l$period[l$lek%in%keptLeks]
    ld$repetition <- l$repetition[l$lek%in%keptLeks]
    ld$natun <- d$natun
    ld$gr <- d$gr
    ld$ellp <- d$ellp
    ld$ell2 <- d$ell2
    ld$Nperiods <- max(ld$period)
    ld$Nyears <- max(ld$year)
    ld$Nleks <- max(ld$lek)
    ld$Nlekperiods <- nrow(ld$y)
    ld$Ngr <- max(ld$gr)
    ld$Nnatun <- max(ld$natun)

    if (ld$Nperiods!=l$Nperiods) {
        msg <- paste0("Incorrect groups of Leks: by removing this group of leks,\n",
                      "some periods have also been removed from the data.\n",
                      "Use a different grouping")
        stop(msg)
    }
    if (ld$Ngr!=l$Ngr) {
        msg <- paste0("Incorrect groups of Leks: by removing this group of leks,\n",
                      "some geographic regions have also been removed from the data.\n",
                      "Use a different grouping")
        stop(msg)
    }
    if (length(unique(ld$ellp))!=l$Ntypes) {
        msg <- paste0("Incorrect groups of Leks: by removing this group of leks,\n",
                      "some lek types have also been removed from the data.\n",
                      "Use a different grouping")
        stop(msg)
    }
    class(ld) <- class(l)
    return(ld)
}
