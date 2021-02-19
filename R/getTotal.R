getTotal <-
function(x, ...)
{
    if (!inherits(x,"estimateMales"))
        stop("x is not of class estimateMales")
    glu <- x[[1]]
    for (i in 2:5)
        glu <- glu+x[[i]]
    return(glu)
}
