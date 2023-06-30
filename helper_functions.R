# helper functions

panel.cor <- function(x, y, digits=2, prefix="", cex.cor )
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = 1.5 )
    text(.7, .8, Signif, cex=cex, col=2)
}

panel.smooth <- function( x, y, col=gray(0.5,0.5), bg=NA, pch=19, 
                         cex=1, col.smooth="red", span=2/3, iter=3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines( stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, lwd=2, ...)
}

# pairs( dat, lower.panel=panel.smooth, upper.panel=panel.cor)


fit.function <- function( b12, b13, b23, b14, b24, b34 )
{

    x1 <- e1
    x2 <- b12*x1 + e2
    x3 <- b13*x1 + b23*x2 + e3
    x4 <- b14*x1 + b24*x2 + b34*x3 + e4

    abs( cor(x1,x2) - r12 ) +
    abs( cor(x1,x3) - r13 ) +
    abs( cor(x2,x3) - r23 ) +
    abs( cor(x1,x4) - r14 ) +
    abs( cor(x2,x4) - r24 ) +
    abs( cor(x3,x4) - r34 ) 
}
