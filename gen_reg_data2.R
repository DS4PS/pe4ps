#' Generate a dataset with a given covariance structure.
#' 
#' \code{gen_reg_data} returns a synthetic dataset by matching parameters from your original dataset.
#'
#' @param dat A data frame that contains the true data.
#' @param y The dependent variable in the model. 
#' @return The sum of \code{x} and \code{y}.
#'
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#'
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @export
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
#' 
#' dat <- gen_reg_data()
#' pairs( dat, lower.panel=panel.smooth, upper.panel=panel.cor )
gen_reg_data <- function( )
{
	

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

	# variance term of x1 to x4
	e1 <- rnorm(100)
	e2 <- rnorm(100)
	e3 <- rnorm(100)
	e4 <- rnorm(100)

	# correlation matrix
	b12 <- r12 <- 0.2
	b13 <- r13 <- 0.4
	b23 <- r23 <- 0.2
	b14 <- r14 <- 0.2
	b24 <- r24 <- 0.3
	b34 <- r34 <- 0.2

	# data generation equations
	x1 <- e1
	x2 <- b12*x1 + e2
	x3 <- b13*x1 + b23*x2 + e3
	x4 <- b14*x1 + b24*x2 + b34*x3 + e4

	dat <- data.frame( x1, x2, x3, x4 )

	pairs( dat, 
			 lower.panel=panel.smooth, 
			 upper.panel=panel.cor)


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


	best.fit <- fit.function( b12=b12, b13=b13, b23=b23,
									  b14=b14, b24=b24, b34=b34 )

	# optimization heuristic
	for( i in 1:1000 )
	{
	  b12.temp <- b12 + rnorm(1,0,0.05)
	  b13.temp <- b13 + rnorm(1,0,0.05)
	  b23.temp <- b23 + rnorm(1,0,0.05)
	  b14.temp <- b14 + rnorm(1,0,0.05)
	  b24.temp <- b24 + rnorm(1,0,0.05)
	  b34.temp <- b34 + rnorm(1,0,0.05)

	  fit <- fit.function( b12=b12.temp, b13=b13.temp, 
								  b23=b23.temp, b14=b14.temp, 
								  b24=b24.temp, b34=b34.temp )

	  if( fit < best.fit )
	  {
		 b12 <- b12.temp
		 b13 <- b13.temp 
		 b23 <- b23.temp
		 b14 <- b14.temp
		 b24 <- b24.temp 
		 b34 <- b34.temp
		 best.fit <- fit
		 print( best.fit )
	  }

	}



	b12
	b13 
	b23
	b14
	b24
	b34 

	x1 <- e1
	x2 <- b12*x1 + e2
	x3 <- b13*x1 + b23*x2 + e3
	x4 <- b14*x1 + b24*x2 + b34*x3 + e4

	dat <- data.frame( x1, x2, x3, x4 )

	pairs( dat, 
			 lower.panel=panel.smooth, 
			 upper.panel=panel.cor)


	# rescale variables
	x1.s <- scales::rescale( x1, to=c( 0, 100 ) )
	x2.s <- scales::rescale( x2, to=c( -100, 0 ) )
	x3.s <- scales::rescale( x3, to=c( -20, 50 ) )
	x4.s <- scales::rescale( x4, to=c( 0.2, 0.9 ) )

	# doesn't change covariance structure

	d2 <- data.frame( x1.s, x2.s, x3.s, x4.s )

	pairs( d2, 
			 lower.panel=panel.smooth, 
			 upper.panel=panel.cor)

  return( d2 )
  
}


