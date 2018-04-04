# ------------------------------------------------------------------
# Functions that help tidy data for publication
# ------------------------------------------------------------------

# Formats a set of numbers to have a certain number of sig figs
make_pretty <- function (num, digits) {
  as.numeric(formatC(signif(num, digits), digits = digits, format = "fg", flag = "#"))
}

# Makes a table from lm stats - need to use the linear regression function first or something similar
make_pretty_lm_table <- function(dat, voi, est = "Estimate", se = "StdErr", upper_CI = "97.5 %", lower_CI = "2.5 %", p = "Pr(>|t|)", arrange_by = p, output = c("se", "CI")) {
	require(tidyverse)

	out <- arrange_(dat, arrange_by) %>%
		mutate(SE = make_pretty(.[[se]], 3)) %>%
		mutate(P = make_pretty(.[[p]], 3)) %>%
		mutate(low_CI = make_pretty(.[[lower_CI]], 3)) %>%
		mutate(up_CI = make_pretty(.[[upper_CI]], 3)) %>%
		mutate(`Estimate (95% CI)` = paste0(make_pretty(.[[est]], 3), " (", low_CI, ", ", up_CI, ")"))

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

