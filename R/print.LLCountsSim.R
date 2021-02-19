print.LLCountsSim <-
function(x,...)
{
    if (!inherits(x,"LLCountsSim"))
        stop("x should be of class LLCountsSim")

    cat("******************************\n")
    cat("** Object of class LLCountsSim\n")
    cat("\nMatrix with", nrow(x), "rows (leks) and", ncol(x), "columns (number of simulations)",
        "\ncontaining the log-likelihood of the lek counts \nfor each lek and each MCMC simulation\n")

}
