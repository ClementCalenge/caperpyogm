plotOverlap <- function(x, df, cex=1.5)
{
    if (!inherits(df, "data.frame"))
        stop("df should inherits the class \'data.frame\'")

    if (!all(c("Parameter", "tau", "namo", "prior", "from", "to")%in%names(df)))
        stop("Incorrect column names")

    if (!inherits(x, "mcmc.list"))
        stop("x should inherits the class mcmc.list")

    rs <- do.call(rbind, x)

    if (!all(df$Parameter%in%colnames(rs)))
        stop("Some parameters in df are not available in x")
    six <- 0.9
    siy <- 0.9

    plotRowT <- function(x, y, pt, vp, yy, name, stat, prior="dnorm(z,0,sqrt(10))", from = -10,
                         to = 10, n = 10000)
    {
        seekViewport(vp)
        pri2 <- gsub("z", "den$x", prior)

        vpp <- viewport(x=unit(x,"native"),y=unit(y,"native"),
                        width=unit(six,"native"),
                        height=(unit(siy, "native")),
                        name=paste("identif3.",pt,".vp", sep=""),
                        clip="on")
        pushViewport(vpp)

        ## Density of the MCMC samples
        den <- density(yy, bw=1.06*sd(yy)*(length(yy)^(-1/5)), from=from, to=to, n=n)
        pri <- list()
        pri$x <- den$x
        pri$y <- eval(parse(text = pri2))

        ## parameters
        yra <- range(den$y)##range(c(pri$y,den$y))
        yra[1] <- 0
        yra[2] <- diff(yra)+0.1*diff(yra)
        yra[1] <- -0.05*diff(yra)
        xra <- range(den$x)

        x1 <- (pri$x-min(pri$x))/diff(xra)
        y1 <- (den$y-yra[1])/diff(yra)
        y2 <- (pri$y-yra[1])/diff(yra)

        grid.rect(gp=gpar(fill="lightgrey", col=NA), name=paste0("rect.", pt))
        grid.lines(unit(x1, "npc"),unit(y1,"npc"),
                   name=paste0("posterior.", pt), gp=gpar(lwd=2))

        grid.lines(unit(x1, "npc"),unit(y2,"npc"),
                   name=paste0("prior.", pt), gp=gpar(lwd=2, lty=2))

        ##
        seekViewport(vp)

        vpp <- viewport(x=unit(x-1,"native"),y=unit(y,"native"),
                        width=unit(six,"native"),
                        height=(unit(siy, "native")),
                        name=paste("identif2.",pt,".vp", sep=""))
        pushViewport(vpp)

        grid.rect(gp=gpar(fill="lightgrey", col=NA), name=paste0("rect1.", pt))
        grid.text(stat, gp=gpar(cex=cex), name=paste0("stat.", pt))

        seekViewport(vp)

        vpp <- viewport(x=unit(x-2,"native"),y=unit(y,"native"), width=unit(six,"native"),
                        height=(unit(siy, "native")),
                        name=paste("identif1.",pt,".vp", sep=""))
        pushViewport(vpp)

        grid.rect(gp=gpar(fill="lightgrey", col=NA))
        grid.text(eval(parse(text=name)), gp=gpar(cex=cex))

        seekViewport(vp)
    }


    df <- df[rev(1:nrow(df)),]


    ## 3 columns a nr rows (+ title)
    nr <- nrow(df)

    ## starts the plot
    grid.newpage()

    ## Parent viewport
    vp <- dataViewport(xscale=c(0,3), yscale=c(0,nr+1), extension=0,
                       name="vpr")
    pushViewport(vp)

    for (i in 1:nr) {
        plotRowT(x=2.5, y=i-0.5, pt=paste0("aa",i), yy=rs[,df$Parameter[i]],vp="vpr",
                 stat=round(df$tau[i],3), name=df$namo[i],
                 prior=df$prior[i], from=df$from[i], to=df$to[i])
    }

    vpp <- viewport(x=unit(2.5,"native"),y=unit(nr+0.5,"native"),
                    width=unit(six,"native"),
                    height=(unit(siy, "native")),
                    name="header1..vp")
    pushViewport(vpp)
    grid.text("Distributions", gp=gpar(cex=cex), name="namhead1")

    seekViewport("vpr")
    vpp <- viewport(x=unit(1.5,"native"),y=unit(nr+0.5,"native"),
                    width=unit(six,"native"),
                    height=(unit(siy, "native")),
                    name="header2..vp")
    pushViewport(vpp)
    grid.text(expression(tau), gp=gpar(cex=cex), name="namhead2")
    seekViewport("vpr")
    vpp <- viewport(x=unit(0.5,"native"),y=unit(nr+0.5,"native"),
                    width=unit(six,"native"),
                    height=(unit(siy, "native")),
                    name="header3..vp")
    pushViewport(vpp)
    grid.text("Parameter", gp=gpar(cex=cex), name="namhead3")

}
