restartCV <-
function(filename)
{
    bckp <- readRDS(filename)
    if (!attr(bckp, "notyet"))
        stop("Not a correct cross-validation backup filename")
    listarg <- attr(bckp, "listarg")

    ldo <- listarg$ldo
    registeredModel <- listarg$registeredModel
    parameters <- listarg$parameters
    n.chains <- listarg$n.chains
    n.iter <- listarg$n.iter
    thin <- listarg$thin
    backupFile <- listarg$backupFile
    magr <- length(ldo)
    iter <- length(bckp)


    cat("Restarting cross-validation at group", iter+1, "...\n")

    reb <- bckp
    for (i in (iter+1):magr) {

        cat("#################################################\n##\n##\n")
        cat("## Leks Group", i,"\n\n")
        inits <- list(N=apply(ldo[[i]]$y,1,max)+2)
        reb[[i]] <- fitModelCount(ldo[[i]], registeredModel, parameters, inits, n.chains,
                                  n.iter, thin)
        attr(reb, "notyet") <- TRUE
        attr(reb, "listarg") <- listarg
        cat("Saving results in backup file...\n")
        saveRDS(reb, backupFile)
        cat("Backup in", backupFile,"\n")
    }
    attr(reb, "notyet") <- NULL
    attr(reb, "listarg") <- NULL
    class(reb) <- "CVModelCount"
    attr(reb, "registeredModel") <- registeredModel
    cat("End. Saving results in backup file...\n")
    saveRDS(reb, backupFile)
    return(reb)
}
