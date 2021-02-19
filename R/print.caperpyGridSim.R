print.caperpyGridSim <- function (x, ...)
{
    if (!inherits(x, "caperpyGridSim"))
        stop("x should be of class caperpyGridSim")
    N <- nrow(x$origData)
    S <- ncol(x$sim)
    cat("*********************************\n")
    cat("*** Object of class caperpyGridSim\n\n")
    cat("This object is a list with two elements:\n\n")
    cat("$origData: original dataset (data.frame) containing the results of\n           the",
        N, "grid cell searches, as well as explanatory variables\n          (presence area, detection of newUL, previous searches, etc.)\n\n")
    cat("$sim: matrix with", N, "rows and", S, "columns containing the\n",
        "   ", S, "simulated UL presence datasets according to the model\n\n")
    cat("Focus of the simulations:", attr(x,"what"),"\n\n")
}
