
# correlation of A and rA+B when both are rnorm() is m below
#
# see: https://math.stackexchange.com/questions/2865621/correlation-between-two-linear-combinations-of-random-variables

rn1 <- rnorm(100000)
rn2 <- rnorm(100000)
rt <- 0.3  # correlation across time
m <- rt / sqrt( 1 - rt^2 ) 
rn3 <- m*rn1 + rn2
cor( rn1, rn3 )


for( i in seq(0,0.95,0.05) )
{

  print(i)
  rt <- i
  m <- rt / sqrt( 1 - rt^2 ) 
  rn3 <- m*rn1 + rn2
  print( cor( rn1, rn3 ) )

}





# group means:

b0 <- 10  # c1 
b1 <- 10  # diff between c1 and t1
b2 <- 20  # diff between c1 and c2
b3 <- 15  # treat effect

# error terms for each group, all standard normals
e0 <- rnorm(10000)
e1 <- rnorm(10000)
e2 <- rnorm(10000)
e3 <- rnorm(10000)

# within-group across-time correlation coefficient
rt <- 0.8  
m <- rt / sqrt( 1 - rt^2 ) 

# truncate these outliers ???
sum( e1 < -3.5 )
sum( e1 > 3.5 )
plot( e1, pch=19, cex=ifelse( abs(e1)>3.5, 2, 0.3), bty="n" )
abline( h=c(3.5,-3.5), lty=2, col="red" )

e1[ e1 > 3.5 ] <- 3.5
e1[ e1 < -3.5 ] <- 3.5
e2[ e2 > 3.5 ] <- 3.5
e2[ e2 < -3.5 ] <- 3.5



#### DEFINE ERROR STRUCTURE FOR POST-TREAT PERIOD

e2.1 <- m*e1 + e2
summary(e1)
summary(e2.1)
sd(e1)
sd(e2.1)
cor(e1,e2)
cor(e1,e2.1)

# maintains std.norm properties and across-time correlation
e2.s <- as.numeric( scale( e2.1 ) )
mean(e2.s)
sd(e2.s)
cor(e1,e2.s)



### GEN DATA PROCESS

c1 <- b0 + e0
sd(c1)

t1 <- b0 + b1 + e1
sd( t1 )


e2.s <- as.numeric( scale( m*e0 + e2 ) )
c2 <- b0 + b2 + e2.s 
mean(c2)
sd(c1)
sd(c2)
cor( c1, c2 )
plot( c1, c2, pch=19, col=gray(0.5,0.5), bty="n" )

e3.s <- as.numeric( scale( m*e1 + e3 ) )
t2 <- b0 + b1 + b2 + b3 + e3.s 
mean(t2)
sd(t2)
cor( t1, t2 )
plot( t1, t2, pch=19, col=gray(0.5,0.5), bty="n" )


p1 <- c(c1,t1)
p2 <- c(c2,t2)
cor( p1, p2 )
plot( p1, p2, pch=19, col=gray(0.5,0.5), bty="n" )


y <- c(c1,t1,c2,t2)
d.t <- c( rep( c(0,1,0,1), each=10000) )
d.p <- c( rep( c(0,0,1,1), each=10000) )
d.tp <- d.t * d.p

sd(y) # will not be sd(e) because this measures across-group variance

mod <- lm( y ~ d.t + d.p + d.tp )
summary( mod )


# note we can parameterize the model using natural units and the residual standard error
# (the standard deviation of the e's in our model)
# or using a standard normal then scaling the residuals (not the Y's).
# 
# If it works our ceiling and floor Y's should  
# approximately match the actual data min.y and max.y ?
#
# See: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
# for an explanation of residual standard error. 



# rescale y after data generation ?

# or rescale( e1, to( -3.5*residual_standard_error, 3.5*residual_standard_error ) )
# repeat for all e's  ?
# assumes max and min of all e's will be close to 3.5, valid for large samples
# but maybe use actual min(e) and max(e) ?