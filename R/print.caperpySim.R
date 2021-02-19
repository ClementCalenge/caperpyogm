print.caperpySim <-
function(x,...)
{
    if (!inherits(x, "caperpySim"))
        stop("x should be of class caperpySim")

    N <- nrow(x$origData)
    S <- ncol(x$sim)
    cat("*********************************\n")
    cat("*** Object of class caperpySim\n\n")
    cat("This object is a list with two elements:\n\n")
    cat("$origData: original dataset (data.frame) containing the results of\n           the",
        N,"original counts, as well as explanatory variables\n          (regions, leks, period, nbobservers, etc.)\n\n")
    cat("$sim: matrix with", N, "rows and", S,"columns containing the\n",
        "   ",S,"simulated count datasets according to the model\n\n")
}
