print.estimateMales <-
function(x, ...)
{
    if (!inherits(x,"estimateMales"))
        stop("x is not of class estimateMales")
    cat("*******************************\n")
    cat("** Object of class estimateMales\n\n")
    cat("This object is a list with ", length(x), " matrices (regions) with ",
        nrow(x[[1]]),
        " rows (MCMC iterations) and",
        "\n",ncol(x[[1]]), " columns (periods) containing simulations of population size in each region.\n\n",
        sep="")
}
