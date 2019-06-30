create_graph <- function()
{

    par( mfrow=c(1,2) )
      
   #if( slowSim==T ) { par(ask=TRUE) }   # HAVE TO CLICK ON GRAPH FOR NEXT ONE
            
   #if( slowSim==F ) { par(ask=FALSE) }

    ## Only showing 100 points on the left side
    
    plot( x, z, xlim= c(0,100), ylim=c(-600, 600 ), pch=19, col="gray", cex=1.5, bty="n", 
          axes=F, xlab="", ylab="" )
      axis( side=1 )
   abline( a=0, b=1 )
   r1 <- lm( z ~ x )
   abline( a= r1$coefficients[1], b= r1$coefficients[2], col="steelblue", lwd=2 )

   slopes1[i] <- r1$coefficients[2]

    ## Coef interval plot

    samp.size = length(z)
    
    stan.error[i] <- sqrt( sum( r1$residuals^2 ) / (samp.size-2) ) / sqrt( (samp.size-1)*var(x) )
      
   ci.lower[i] <- slopes1[i] + qt(0.025, samp.size )*stan.error[i]
      
   ci.upper[i] <- slopes1[i] + qt(0.975, samp.size )*stan.error[i]

    colour <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )

    plot( x=0, y=0, ylim=c(0,num.trials), xlim=c(-2.5, +3), col="white", 
      xlab="TRUE SLOPE = 1", ylab="", col.main="darkgray", col.lab="darkgray", 
      main="Confidence Intervals for Slope Estimates", axes=F, cex.lab=2, cex.main=2 )
    axis( side=1 )

    col2 <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )
    points( slopes1, 1:i, pch=20, col=col2  )
   
    segments( x0=ci.lower, y0=1:i, x1 = ci.upper, col=colour )
    
    polygon( x = c(-3, 4, 4, -3), 
        y = c( which(ci.lower < 0 )[1], which( ci.lower <0)[1], num.trials+10, num.trials+10), 
        border = NA, col = adjustcolor( "gray", alpha.f = 0.4 ))

    abline( v=0, col="darkgray", lty=3 )        
    abline( v=1, lty=1 )
    
    col3 <- "white"
    if( ci.lower[i] < 0 ) col3 <- "firebrick4"
    text( -1.5, 95, "Type II Errors", col=col3, cex=2, font=2  )

    segments( x0=-1.65, y0=10, x1= -1.35, col="steelblue" )
    points( x=-1.5, y=10, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=7, "HIGH Statistical Power", cex=1.5, col="darkgray" )

    segments( x0=-2.0, y0=20, x1= -1.0, col="steelblue" )
    points( x=-1.5, y=20, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=17, "LOW Statistical Power", cex=1.5, col="darkgray" )

}



sim_me_in_dv <- function( num.trials = 100 )
{


## MEASUREMENT ERROR IN THE DV

slopes1 <- NULL

x <- 1:100

stan.error <- NULL
   
ci.lower <- NULL

ci.upper <- NULL

# num.trials = 100

for( i in 1:num.trials )
{
    png( paste0( "image-", i, ".png" ), width=1600, height=800 )

    z <- x + rnorm(100, 0, i)

    par( mfrow=c(1,2) )
      
   #if( slowSim==T ) { par(ask=TRUE) }   # HAVE TO CLICK ON GRAPH FOR NEXT ONE
            
   #if( slowSim==F ) { par(ask=FALSE) }

    ## Only showing 100 points on the left side
    
    plot( x, z, xlim= c(0,100), ylim=c(-600, 600 ), pch=19, col="gray", cex=1.5, bty="n", 
          axes=F, xlab="", ylab="" )
      axis( side=1 )
   abline( a=0, b=1 )
   r1 <- lm( z ~ x )
   abline( a= r1$coefficients[1], b= r1$coefficients[2], col="steelblue", lwd=2 )

   slopes1[i] <- r1$coefficients[2]

    ## Coef interval plot

    samp.size = length(z)
    
    stan.error[i] <- sqrt( sum( r1$residuals^2 ) / (samp.size-2) ) / sqrt( (samp.size-1)*var(x) )
      
   ci.lower[i] <- slopes1[i] + qt(0.025, samp.size )*stan.error[i]
      
   ci.upper[i] <- slopes1[i] + qt(0.975, samp.size )*stan.error[i]

    colour <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )

    plot( x=0, y=0, ylim=c(0,num.trials), xlim=c(-2.5, +3), col="white", 
      xlab="TRUE SLOPE = 1", ylab="", col.main="darkgray", col.lab="darkgray", 
      main="Confidence Intervals for Slope Estimates", axes=F, cex.lab=2, cex.main=2 )
    axis( side=1 )

    col2 <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )
    points( slopes1, 1:i, pch=20, col=col2  )
   
    segments( x0=ci.lower, y0=1:i, x1 = ci.upper, col=colour )
    
    polygon( x = c(-3, 4, 4, -3), 
        y = c( which(ci.lower < 0 )[1], which( ci.lower <0)[1], num.trials+10, num.trials+10), 
        border = NA, col = adjustcolor( "gray", alpha.f = 0.4 ))

    abline( v=0, col="darkgray", lty=3 )        
    abline( v=1, lty=1 )
    
    col3 <- "white"
    if( ci.lower[i] < 0 ){ col3 <- "firebrick4" }
    text( -1.5, 95, "Type II Errors", col=col3, cex=2, font=2  )

    segments( x0=-1.65, y0=10, x1= -1.35, col="steelblue" )
    points( x=-1.5, y=10, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=7, "HIGH Statistical Power", cex=1.5, col="darkgray" )

    segments( x0=-2.0, y0=20, x1= -1.0, col="steelblue" )
    points( x=-1.5, y=20, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=17, "LOW Statistical Power", cex=1.5, col="darkgray" )

    # Sys.sleep( time=0.05 )

    dev.off()

    
}

}



sim_me_in_dv()



for( j in 1:10 )
{
  png( paste0( "image-", 100+j, ".png" ), width=1600, height=800 )
  create_graph()
  dev.off()
}






function (filename = "Rplot%03d.png", width = 480, height = 480, 
    units = "px", pointsize = 12, bg = "white", res = NA, family = "sans", 
    restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), 
    antialias = c("default", "none", "cleartype", "gray", "subpixel")) 


png( paste0( "image-", i, ".png" ), width=1600, height=800 )


   z <- x + rnorm(100, 0, i)

    par( mfrow=c(1,2) )
      
   #if( slowSim==T ) { par(ask=TRUE) }   # HAVE TO CLICK ON GRAPH FOR NEXT ONE
            
   #if( slowSim==F ) { par(ask=FALSE) }

    ## Only showing 100 points on the left side
    
    plot( x, z, xlim= c(0,100), ylim=c(-600, 600 ), pch=19, col="gray", cex=1.5, bty="n", 
          axes=F, xlab="", ylab="" )
      axis( side=1 )
   abline( a=0, b=1 )
   r1 <- lm( z ~ x )
   abline( a= r1$coefficients[1], b= r1$coefficients[2], col="steelblue", lwd=2 )

   slopes1[i] <- r1$coefficients[2]

    ## Coef interval plot

    samp.size = length(z)
    
    stan.error[i] <- sqrt( sum( r1$residuals^2 ) / (samp.size-2) ) / sqrt( (samp.size-1)*var(x) )
      
   ci.lower[i] <- slopes1[i] + qt(0.025, samp.size )*stan.error[i]
      
   ci.upper[i] <- slopes1[i] + qt(0.975, samp.size )*stan.error[i]

    colour <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )

    plot( x=0, y=0, ylim=c(0,num.trials), xlim=c(-2.5, +3), col="white", 
      xlab="True Slope = 1", ylab="", col.main="darkgray", col.lab="darkgray", 
      main="Confidence Intervals for Slope Estimates", axes=F )
    axis( side=1 )

    col2 <- ifelse( ci.lower < 0, "firebrick4", "steelblue" )
    points( slopes1, 1:i, pch=20, col=col2  )
   
    segments( x0=ci.lower, y0=1:i, x1 = ci.upper, col=colour )
    
    polygon( x = c(-3, 4, 4, -3), 
        y = c( which(ci.lower < 0 )[1], which( ci.lower <0)[1], num.trials+10, num.trials+10), 
        border = NA, col = adjustcolor( "gray", alpha.f = 0.4 ))

    abline( v=0, col="darkgray", lty=3 )        
    abline( v=1, lty=1 )
    text( -1.5, 95, "Type II Errors", col="firebrick4", cex=1.5, font=2 )

    segments( x0=-1.65, y0=10, x1= -1.35, col="steelblue" )
    points( x=-1.5, y=10, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=7, "HIGH Statistical Power", cex=0.8, col="darkgray" )

    segments( x0=-2.0, y0=20, x1= -1.0, col="steelblue" )
    points( x=-1.5, y=20, col="steelblue", pch=19, cex=1.2 )
    text( x=-1.5, y=17, "LOW Statistical Power", cex=0.8, col="darkgray" )

dev.off()



