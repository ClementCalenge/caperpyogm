showChangeRate <-
function(object, GR, changeRatePeriods=c(1,5),
                           quantiles=c(0.1,0.5,0.9))
{

    x <- changeRate(object, GR, changeRatePeriods=c(1,5))
    if ((min(quantiles)<0)|(max(quantiles)>1))
        stop("incorrect quantiles")

    dens <- density(x)

    df <- data.frame(x=dens$x, y=dens$y)

    quantiles <- quantile(x, prob=quantiles)

    df$quant <- factor(findInterval(df$x,quantiles))

    plo <- ggplot(df, aes(.data$x,.data$y)) + geom_line() +
        geom_ribbon(aes(ymin=0, ymax=.data$y, fill=.data$quant)) +
        scale_fill_brewer(guide="none")+
        xlab("Change rate (%)")+
        ylab("Probability density")+
        geom_vline(xintercept=0, col="red",size=2)

    return(plo)

}
