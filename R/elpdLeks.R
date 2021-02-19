elpdLeks <-
function(x)
{
    if (!inherits(x,"LLCountsSim"))
        stop("x should be of class LLCountsSim")

    ui <- apply(x,1,function(y) {
        z <- y-max(y)
        log(mean(exp(z)))+max(y)
    })
    return(ui)
}
