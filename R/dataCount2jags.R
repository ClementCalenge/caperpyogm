dataCount2jags <-
function(lek, period, nbobs, nbmales, gr, type,natun, year)
{
    ## Checks length
    df <- list(lek,period,nbobs,nbmales, gr, type, natun, year)
    if(length(unique(sapply(df, length)))!=1)
        stop("Incorrect length of vectors")

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

    ## Checks that the leks are numbered from 1:Nplace
    chff(lek,"lek")
    chff(year, "year")
    chff(period, "period")
    chff(natun, "natun")
    chff(gr, "gr")
    chff(type, "type")

    ## recode
    recodegr <- cbind(1:5, c(1,1,2,2,5), rep(1,5))

    ## Build a list usable for jags to fit the model
    lekperiod <- paste(lek,period)
    idlekperiod <- as.numeric(factor(lekperiod))
    o <- split(nbmales, idlekperiod)
    oz <- split(nbobs, idlekperiod)
    oa <- split(year, idlekperiod)
    repetition <- o %>% map_int(length)
    mo <- max(repetition)
    o <- o %>% map(~c(.,rep(0,mo-length(.)))) %>% bind_rows %>% t
    oz <- oz %>% map(~c(.,rep(0,mo-length(.)))) %>% bind_rows %>% t
    oa <- oa %>% map(~c(.,rep(0,mo-length(.)))) %>% bind_rows %>% t


    idplan <- levels(factor(lekperiod))
    lekp <- strsplit(idplan, " ") %>% map_chr(1) %>% as.numeric
    periodp <- strsplit(idplan, " ") %>% map_chr(2) %>% as.numeric
    dun <- data.frame(lek, natun) %>% unique %>% arrange(lek)
    del <- data.frame(lek, type) %>% unique %>% arrange(lek)


    drg <- data.frame(lek, gr) %>% unique %>% arrange(lek)
    ## Les PI sont toutes de la meme RG
    grrc <- drg$gr
    for (i in 1:nrow(recodegr)) {
        for (j in 1:ncol(recodegr)) {
            grrc[del$type==j&drg$gr==i] <- recodegr[i,j]
        }
    }
    drg$gr <- grrc

    ## Donnees renvoyees
    datalist <- list()
    datalist$Nlekperiods <- nrow(o)
    datalist$Ngr <- max(gr)
    datalist$Nnatun <- max(natun)
    datalist$Nleks <- max(lek)
    datalist$Nperiods <- max(period)
    datalist$Nyears <- max(year)
    datalist$Ntypes <- max(type)
    datalist$y <- o
    datalist$observers <- oz
    datalist$year <- oa
    datalist$period <- periodp
    datalist$lek <- lekp
    datalist$repetition <- repetition
    datalist$natun <- dun[[2]]
    datalist$gr <- drg[[2]]
    datalist$ellp <- del[[2]]
    datalist$ell2 <- as.numeric(del[[2]]==2)

    class(datalist) <- "caperpyData"

    return(datalist)
}
