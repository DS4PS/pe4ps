library( scales )



effect.size= 0 to 1+

reg.discontinuity <- function( seed=100, n=100, score.min=20, score.max=100, cutoff=60, err.sd=100, y.min = 0, y.max = 100 )
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


