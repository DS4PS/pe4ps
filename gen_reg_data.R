library( scales )



effect.size= 0 to 1+

reg.discontinuity <- function( seed=100, n = 1000, score.min = 20, score.max = 100, cutoff = 60, err.sd = 100, y.min = 0, y.max = 100 )
{

score <- NULL
treatment <- NULL
y <- NULL
err <- NULL

{

set.seed(seed)
score = rbeta( n, shape1 = 7, shape2 = 2,  ncp = 0 )
score = rescale( score, to = c(score.min,score.max) )

if(missing(cutoff)) cutoff=median(score);

treatment = score
treatment = ifelse(treatment>cutoff, 0, 1)

score_c = score - cutoff

err = rnorm(n, 600, err.sd)

y = 40 + 120*treatment + 20*score + err

y = rescale(y, to = c(y.min,y.max))

data = as.data.frame(cbind(y, treatment, score, score_c))

}
  return( data )
 # return( write.csv(data, "data_RD.csv"))

}

 x <- reg.discontinuity( )


plot( x$score, x$y )



x1 <- 1:100
x2 <- x1 + rnorm(100,0,50)
plot( x1, x2 )
cor( x1, x2 )

y <- 50 + 2*x1 - 0.5*x2 + rnorm(100,0,10)

plot( x1, y )
plot( x2, y )

summary( lm( y ~ x1 + x2 ) )

x1.s <- scale( x1 ) 
x2.s <- scale( x2 )
y.s <- scale( y )

summary( lm( y.s ~ x1.s + x2.s ) )

m1 <- lm( y ~ x2 )
m2 <- lm( x1 ~ x2 )
cor( m1$residuals, m2$residuals )


x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2*x1 + 2*x2 + rnorm(100)

plot( x1, y )

cor( x1, y )

m1 <- lm( y ~ x2 )
m2 <- lm( x1 ~ x2 )
cor( m1$residuals, m2$residuals )

par( mfrow=c(1,2) )
plot( x1, y, pch=19, col="gray", bty="n", cex=2 )
plot( m2$residuals, m1$residuals, col="gray", pch=19, bty="n", cex=2  )


summary( lm( y ~ x1 ) )
summary( lm( y ~ x1 + x2 ) )

x1.s <- scale( x1 ) 
x2.s <- scale( x2 )
y.s <- scale( y )
summary( lm( y.s ~ x1.s + x2.s ) )



summary( lm( y ~ x1 + x2 ) )

m <- lm( y ~ x1 + x2 )

b0 <- 0.103
b1 <- 1.915
x1.hat <- b0 + b1*(x1/(sd(y)))
var( x1.hat )

se <- sqrt(diag(vcov(m)))["x1"]

( ( sd(x1) / sd(y) )^2 ) * se^2



e1 <- rnorm(10000)
e2 <- rnorm(10000)
e3 <- rnorm(10000)
e4 <- rnorm(10000)
e5 <- rnorm(10000)

c12 <- 0.2
c14 <- 0.2
c35 <- 0.2
c15 <- 0.2
c25 <- 0.2

z1 <- e1
z2 <- 0.2*z1 + e2
cor( z1, z2 )

z3 <- e3
z4 <- 0.2*z1 + 0.2*z3 + e4
cor( z1, z4 )
cor( z3, z4 )

z5 <- 0.2*z1 + 0.2*z2 + 0.2*z3 + e5
cor( z1, z5 )
cor( z2, z5 )
cor( z3, z5 )

dat <- data.frame( z1, z2, z3, z4, z5 )
pairs( dat )
cor( dat )



z1.s <- rescale( z1, to = c(0,100) )
z2.s <- rescale( z2, to=c(-50,150) )

cor( z1, z2 )
cor( z1.s, z2.s )


