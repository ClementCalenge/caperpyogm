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

    plo <- ggplot2::ggplot(df, ggplot2::aes(.data$x,.data$y)) + ggplot2::geom_line() +
        ggplot2::geom_ribbon(ggplot2::aes(ymin=0, ymax=.data$y, fill=.data$quant)) +
        ggplot2::scale_fill_brewer(guide="none")+
        ggplot2::xlab("Change rate (%)")+
        ggplot2::ylab("Probability density")+
        ggplot2::geom_vline(xintercept=0, col="red",size=2)

    return(plo)

}
