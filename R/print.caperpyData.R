print.caperpyData <-
function(x, ...)
{
    if (!inherits(x,"caperpyData"))
        stop("x is not of class caperpyData")
    cat("*******************************\n")
    cat("** Object of class caperpyData\n\n")
    cat("Structure of the object:\n\n")
    print(str(x))
}
