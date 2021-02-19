print.CVModelCount <-
function(x, ...)
{
    if (!inherits(x,"CVModelCount"))
        stop("x should be of class CVModelCount")
    cat("******************************************\n")
    cat("**  Object of class CVModelCount\n",
        "\nContains", length(x),"objects of class mcmc.list",
        "\ncontaining the cross-validation MCMC iterations \non the model",
        attr(x,"registeredModel"),"\n")
}
