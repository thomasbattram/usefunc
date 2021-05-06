# ------------------------------------------------------------------
# Functions that help tidy data for publication
# ------------------------------------------------------------------

#' Format a set of numbers to have a certain number of sig figs
#'
#' Format a set of numbers to have a certain number of sig figs
#'
#' @param num numeric vector
#' @param digits number of digits
#'
#' @export
#' @return character vector with all numbers formated
make_pretty <- function (num, digits) {
	.Deprecated("comma")
 	as.numeric(formatC(signif(num, digits), digits = digits, format = "fg", flag = "#"))
}

#' Format a set of numbers to have a certain number of sig figs
#'
#' Format a set of numbers to have a certain number of sig figs
#'
#' @param dat data
#' @param voi variable of interest
#' @param est effect estimate variable 
#' @param se SE variable
#' @param upper_CI upper confidence interval variable name
#' @param lower_CI lower confidence interval variable name
#' @param p pvalue variable name
#' @param arrange_by variable to arrange output by
#' @param output either SE or CI or both in output
#' @importFrom magrittr %>%
#'
#' @export
#' @return character vector with all numbers formated
make_pretty_lm_table <- function(dat, voi, est = "Estimate", se = "StdErr", upper_CI = "97.5 %", lower_CI = "2.5 %", p = "Pr(>|t|)", arrange_by = p, output = c("se", "CI")) {
	out <- dplyr::arrange_(dat, arrange_by) %>%
		dplyr::mutate(SE = make_pretty(.[[se]], 3)) %>%
		dplyr::mutate(P = make_pretty(.[[p]], 3)) %>%
		dplyr::mutate(low_CI = make_pretty(.[[lower_CI]], 3)) %>%
		dplyr::mutate(up_CI = make_pretty(.[[upper_CI]], 3)) %>%
		dplyr::mutate(`Estimate (95% CI)` = paste0(make_pretty(.[[est]], 3), " (", low_CI, ", ", up_CI, ")"))

	if ("se" %in% output && !("CI" %in% output)) {
		out <- dplyr::select_(out, voi, est, "SE", "P")
	} else if ("CI" %in% output && !("se" %in% output)) {
		out <- dplyr::select_(out, voi, "`Estimate (95% CI)`", "P")
	} else if ("se" %in% output && "CI" %in% output) {
		out <- dplyr::select_(out, voi, est, "SE", "low_CI", "up_CI", "P")
		colnames(out) <- c(voi, "Estimate", "SE", "Lower CI", "Upper CI", "P")
	} else {
		stop("Need to have se or CI in output")	
	} 

	return(out)
}

#' Format numbers
#'
#' Format numbers to being 2 digits
#'
#' @param x numeric vector
#'
#' @export
#' @return character vector with all numbers formated
comma <- function(x) format(x, digits = 2, big.mark = ",")
