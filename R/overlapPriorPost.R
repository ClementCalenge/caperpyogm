
overlapPriorPost <- function(x, param, prior="dnorm(z,0,sqrt(10))",
                             from=-10, to=10, n=1000)
{
    if (!inherits(x, "mcmc.list"))
        stop("x should be of class mcmc.list")
    if (!any(colnames(x[[1]])==param))
        stop("param is not a parameter in x")
    pri2 <- gsub("z","den$x[j]",prior)
    rs <- do.call(rbind, x)
    y <- rs[,param]

    den <- density(y, bw=1.06*sd(y)*(length(y)^(-1/5)), from=from, to=to, n=n)
    di <- diff(den$x)[1]
    tau <- sum(sapply(1:length(den$x), function(j) {
        di*min(c(eval(parse(text=pri2)),den$y[j]))
    }))
    return(tau)
}
