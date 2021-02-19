print.caperpyGridData <-
function(x, ...)
{
    if (!inherits(x,"caperpyGridData"))
        stop("x is not of class caperpyGridData")
    cat("*******************************\n")
    cat("** Object of class caperpyGridData\n\n")
    cat("Structure of the object:\n\n")
    print(str(x))
}
