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

    efm <- data.frame(an=1:5, ef=tapply(to2$Size, to2$Period, median))

    plo <- ggplot(to2, aes(y=.data$Size, x=.data$Period))+
        geom_violin(draw_quantiles=c(0.1,0.9))+
        geom_boxplot(width=0.2, fill="grey", outlier.shape=NA)+
        geom_point(data=efm, aes(x=.data$an,y=.data$ef), col="red")+
        geom_line(data=efm, aes(x=.data$an,y=.data$ef), col="red")+
        ylab(ylab)+xlab(xlab)

    return(plo)

}
