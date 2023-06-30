# BLACK AND WHITE

# COMPARITIVE

# d = data set
# DV = name of DV


# m.male = m.24
# d.male=mdat.m.ed.salary
# baseline.group.1="M"
# 
# m.female=m.04
# d.female=mdat.f.ed.salary
# 
# baseline.group.2="F"
# DV="ED_salary"
# m.title=""
# sal.pos.1=c(3,3,1,1)
# sal.pos.2=c(3,3,1,1)
# par.mar=c(2,0,4,0)
# offset.C1=0
# offset.T1=0 



diff_n_diff_graph4 <- function( m.male, m.female, d.male, d.female,
                                DV, m.title,
                                baseline.group.1, baseline.group.2,
                                sal.pos.1=c(3,3,1,1), sal.pos.2=c(3,3,1,1),
                                par.mar=c(2,0,4,2),
                                offset.C1=0, offset.T1=0,
                                offset.C2=0, offset.T2=0,
                                tip1=0.99, tip2=0.99, tip3=0.99, tip4=0.99,
                                min.y="detect", max.y="detect" )
{
  
  par( mfrow=c(1,2), mar=par.mar, oma=c(0,0,2,0) )
  
  d <- d.male
  m <- m.male
  
  d$dv <- getElement( d, DV ) 
  
  C1.1 <- d %>% filter( treat==0, post==0 ) %>% summarize( mean( dv, na.rm=T) ) %>% as.numeric()
  
  C1.1 <- exp( coefficients(m)["(Intercept)"] )
  # C2 <- C1  + coefficients(m)["post"]
  # CF <- C1  + coefficients(m)["post"] + coefficients(m)["treat"] 
  # T1 <- C1  + coefficients(m)["treat"]
  # T2 <- C1  + coefficients(m)["post"] + coefficients(m)["treat"] + coefficients(m)["treat.post"]  
  
  C2.1 <- exp( log(C1.1)  + coefficients(m)["post"] )
  CF.1 <- exp( log(C1.1)  + coefficients(m)["post"] + coefficients(m)["treat"]  )
  T1.1 <- exp( log(C1.1)  + coefficients(m)["treat"] )
  T2.1 <- exp( log(C1.1)  + coefficients(m)["post"] + 
                 coefficients(m)["treat"] + coefficients(m)["treat.post"]  )
  
  mean.y1 <- mean( c(C1.1,C2.1,CF.1,T1.1,T2.1) ) 
  min.y1 <- min( c(C1.1,C2.1,CF.1,T1.1,T2.1) ) - 0.07*mean.y1
  max.y1 <- max( c(C1.1,C2.1,CF.1,T1.1,T2.1) ) + 0.05*mean.y1
  
  
  
  d <- d.female
  m <- m.female
  
  d$dv <- getElement( d, DV ) 
  
  C1.2 <- d %>% filter( treat==0, post==0 ) %>% summarize( mean( dv, na.rm=T) ) %>% as.numeric()
  
  C1.2 <- exp( coefficients(m)["(Intercept)"] )
  # C2 <- C1  + coefficients(m)["post"]
  # CF <- C1  + coefficients(m)["post"] + coefficients(m)["treat"] 
  # T1 <- C1  + coefficients(m)["treat"]
  # T2 <- C1  + coefficients(m)["post"] + coefficients(m)["treat"] + coefficients(m)["treat.post"]  
  
  C2.2 <- exp( log(C1.2)  + coefficients(m)["post"] )
  CF.2 <- exp( log(C1.2)  + coefficients(m)["post"] + coefficients(m)["treat"]  )
  T1.2 <- exp( log(C1.2)  + coefficients(m)["treat"] )
  T2.2 <- exp( log(C1.2)  + coefficients(m)["post"] + 
                 coefficients(m)["treat"] + coefficients(m)["treat.post"]  )
  
  mean.y2 <- mean( c(C1.2,C2.2,CF.2,T1.2,T2.2) ) 
  min.y2 <- min( c(C1.2,C2.2,CF.2,T1.2,T2.2) ) - 0.07*mean.y2
  max.y2 <- max( c(C1.2,C2.2,CF.2,T1.2,T2.2) ) + 0.05*mean.y2
  
  if( min.y=="detect"){ min.y <- min( min.y1, min.y2 ) }
  if( max.y=="detect"){ max.y <- max( max.y1, max.y2 ) }
  
  grids <- seq( round(min.y,-4)-10000, round(max.y,-4)+10000, by=10000 )
  
  
  T1 <- T1.1
  T2 <- T2.1
  C1 <- C1.1
  C2 <- C2.1
  CF <- CF.1
  
  # par( mar=par.mar )
  
  plot( c(0,1,0,1), c(T1,T2,C1,C2), bty="n",
        xlim=c(-0.3,1.3), ylim=c( min.y, max.y ),
        col="black", pch=19, cex=10, 
        xaxt="n", yaxt="n",
        xlab="", ylab="", 
        main="", cex.main=1.7, col.main="gray40" )
  
  
  abline( h=grids, col="gray50", lty=3 )
  
  arrows( x0=0, x1=0.8, y0=C1, y1=(tip1*C2), col="black", lwd=3.5, length=0.12 )
  arrows( x0=0, x1=0.8, y0=T1, y1=(tip2*T2), col="black", lwd=3.5, length=0.12 )
  #points( 0, C1, pch=19, cex=8, col="black" )
  
  points( c(0,1,0,1), c(T1,T2,C1,C2), col="black", pch=19, cex=10 )
  
  C1.pretty <- paste0("$", format( round( C1/1000, 0 ), big.mark="," ), "k" )
  C2.pretty <- paste0("$", format( round( C2/1000, 0 ), big.mark="," ), "k" )
  CF.pretty <- paste0("$", format( round( CF/1000, 0 ), big.mark="," ), "k" ) 
  T1.pretty <- paste0("$", format( round( T1/1000, 0 ), big.mark="," ), "k" )
  T2.pretty <- paste0("$", format( round( T2/1000, 0 ), big.mark="," ), "k" )
  
  text( c(0,1,0,1), c((T1+offset.T1),(T2+offset.T2),(C1+offset.C1),(C2+offset.C2)), 
        c( T1.pretty,T2.pretty,C1.pretty,C2.pretty), 
        pos=sal.pos.1, cex=3, offset = 3.5, col="gray45" )
  
  
  # axis( side=1, at=c(0,1), labels=c("Pre-Transition","Post-Transition" ) )
  
  text( 0, T1, baseline.group.1, col="white", cex=2, font=2 )  
  text( 0, C1, baseline.group.1, col="white", cex=2, font=2 )    
  text( 1, T2, "M", col="white", cex=2, font=2 )  # treatment group
  text( 1, C2, "F", col="white", cex=2, font=2 )  # control group
  
  
  
  
  
  
  
  T1 <- T1.2
  T2 <- T2.2
  C1 <- C1.2
  C2 <- C2.2
  CF <- CF.2
  
  # par( mar=par.mar )
  
  plot( c(1,0,1,0), c(T1,T2,C1,C2), bty="n",
        xlim=c(-0.3,1.3), ylim=c(min.y, max.y),
        col="black", pch=19, cex=10, 
        xaxt="n", yaxt="n",
        xlab="", ylab="", 
        main="", cex.main=1.7, col.main="gray40" )
  
  abline( h=grids, col="gray50", lty=3 )
  
  # segments( x0=1, x1=0, y0=C1, y1=C2, col="black", lwd=2 )
  # segments( x0=1, x1=0, y0=T1, y1=T2, col="black", lwd=2 )
  # points( 1, C1, pch=19, cex=8, col="black" )
  arrows( x0=1, x1=0.2, y0=C1, y1=(tip3*C2), col="black", lwd=3.5, length=0.12 )
  arrows( x0=1, x1=0.2, y0=T1, y1=(tip4*T2), col="black", lwd=3.5, length=0.12 )
  
  points( c(1,0,1,0), c(T1,T2,C1,C2), col="black", pch=19, cex=10 )
  
  C1.pretty <- paste0("$", format( round( C1/1000, 0 ), big.mark="," ), "k" )
  C2.pretty <- paste0("$", format( round( C2/1000, 0 ), big.mark="," ), "k" )
  CF.pretty <- paste0("$", format( round( CF/1000, 0 ), big.mark="," ), "k" ) 
  T1.pretty <- paste0("$", format( round( T1/1000, 0 ), big.mark="," ), "k" )
  T2.pretty <- paste0("$", format( round( T2/1000, 0 ), big.mark="," ), "k" )
  
  text( c(1,0,1,0), c((T1+offset.T1),T2,(C1+offset.C1),C2), 
        c( T1.pretty,T2.pretty,C1.pretty,C2.pretty), 
        pos=sal.pos.2, cex=3, offset = 3.5, col="gray45" )
  
  
  # axis( side=1, at=c(0,1), labels=c("Pre-Transition","Post-Transition" ) )
  
  text( 1, T1, baseline.group.2, col="white", cex=2, font=2 )  
  text( 1, C1, baseline.group.2, col="white", cex=2, font=2 )    
  text( 0, T2, "M", col="white", cex=2, font=2 )  # treatment group
  text( 0, C2, "F", col="white", cex=2, font=2 )  # control group  
  
  mtext( m.title, side=3, line=0, cex=3, outer=TRUE )
  
  # return( NULL )
  
}

