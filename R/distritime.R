distritime <-
function(x, xlab="Period",ylab="Number of cocks", trim=0.95)
{
    if (!inherits(x,"matrix"))
        stop("x should be a matrix")

    xs <- rowMeans(x)
    to2 <- x[-which(xs>quantile(xs,trim)),]

    to2 <- to2 %>% as.data.frame %>% gather
    to2$key <- substr(to2$key,2,nchar(to2$key))
    names(to2)[names(to2)=="key"] <- "Period"
    names(to2)[names(to2)=="value"] <- "Size"

    efm <- data.frame(an=1:ncol(x), ef=tapply(to2$Size, to2$Period, median))

    plo <- ggplot2::ggplot(to2, ggplot2::aes(y=.data$Size, x=.data$Period))+
        ggplot2::geom_violin(draw_quantiles=c(0.1,0.9))+
        ggplot2::geom_boxplot(width=0.2, fill="grey", outlier.shape=NA)+
        ggplot2::geom_point(data=efm, ggplot2::aes(x=.data$an,y=.data$ef), col="red")+
        ggplot2::geom_line(data=efm, ggplot2::aes(x=.data$an,y=.data$ef), col="red")+
        ggplot2::ylab(ylab)+ggplot2::xlab(xlab)

    return(plo)

}
