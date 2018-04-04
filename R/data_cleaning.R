#' Iteratively remove outliers
#'
#' Remove outliers above or below d standard deviations. Repeat for niter iterations
#'
#' @param x Variable
#' @param niter Number of iterations
#' @param d Number of standard deviations from mean
#'
#' @export
#' @return Variable
remove_outliers_iterative <- function(x, niter=5, d=3)
{
	for(i in 1:niter)
	{
		s <- sd(x, na.rm=T)
		m <- mean(x, na.rm=T)
		index <- x > m + d*s | x < m - d*s
		x[index] <- NA
	}
	return(x)
}

#' Rank transform variable
#'
#' Convert variable into theoretical normal quantiles
#'
#' @param x Numeric variable
#' @param s Outcome standard deviation. Optional
#' @param m Outcome mean. Optional
#'
#' @export
#' @return Variable
rank_transform <- function(x, s=NULL, m=NULL)
{
	if((is.null(s) & !is.null(m)) | (is.null(m) & !is.null(s)))
	{
		stop("s and m must both be null or not null")
	}
	out <- rank(x) - 0.5
	out[is.na(x)] <- NA
	mP <- 0.5/max(out, na.rm = T)
	out <- out/(max(out, na.rm = T) + 0.5)
	out <- qnorm(out)
	if(!is.null(s) & !is.null(m))
	{
		out <- out * s + m
	}
	out
}

#' Adjust a variable to have equal mean and variance across all levels of another variable
#'
#' @param y Dependent variable
#' @param x Independent variable
#' @param keep.scale Default = TRUE. Keep the original mean and sd
#'
#' @export
#' @return Variable
adjust_mean_variance <- function(y, x, keep.scale=TRUE)
{
	d <- data.frame(y=y, x=x, index=1:length(y))

	d <- ddply(d, .(x), mutate, y1 = scale(y))
	if(keep.scale)
	{
		s <- sd(y, na.rm=TRUE)
		m <- mean(y, na.rm=TRUE)
		d$y1 <- scale(d$y1) * s + m
	} else {
		d$y1 <- scale(d$y1)
	}
	d <- d[order(d$index), ]
	return(d$y1)
}

#' Get residuals and keep original scale
#'
#' @param formula Formula
#' @param data Data frame
#' @param keep.scale Keep original mean and sd of y variable. Default = TRUE
#' @param ... Extra variables to pass to lm
#'
#' @export
#' @return Variable adjusted for independent variables
get_residuals <- function(formula, data, keep.scale=TRUE, ...)
{
	y <- model.response(model.frame(formula, data))
	m <- mean(y, na.rm=TRUE)
	s <- sd(y, na.rm=TRUE)
	res <- scale(residuals(lm(formula, data, na.action=na.exclude, ...)))
	if(keep.scale)
	{
		res <- res * s + m
	}
	return(res)
}


#' Winsorize a vector
#'
#' For a numeric vector, move values below and above the q and 1-q
#'   quantiles to those quantiles.
#'
#' @param x Numeric vector
#' @param q Lower quantile to use
#' @export
#' @return
#' A vector like the input \code{x}, but with extreme values moved in to
#'   the \code{q} and \code{1-q} quantiles.
winsorize <- function(x, q=0.006)
{
	extr <- stats::quantile(x, c(q, 1-q), na.rm=TRUE)
	if(diff(extr) < 0) extr <- rev(extr)
	x[!is.na(x) & x < extr[1]] <- extr[1]
	x[!is.na(x) & x > extr[2]] <- extr[2]
	return(x)
}
