#' Extract mode from a numeric vector
#' 
#' @param v numeric vector
#' @export
#' @return mode of numeric vector
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Extract summary statistics from numeric vector
#' 
#' @param dat numeric vector
#' @param min extract minimum value
#' @param first_quartile extract first quartile
#' @param med extract median
#' @param third_quartile extract third quartile
#' @param max extract maximum value
#' @param na extract number of NA values
#' @export
#' @return data.frame of summary stats selected
extract_sum_stats <- function(dat, min = T, first_quartile = T, med = T, mean = T, third_quartile = T, max = T, na = T) {
  sum_tab <- data.frame(min = NA, first_quartile = NA, med = NA, mean = NA, third_quartile = NA, max = NA, na = NA)

  y <- c(sum(min), sum(first_quartile), sum(med), sum(mean), sum(third_quartile), sum(max), sum(na))
  names(y) <- 1:7
  y <- y[y == 1]

  for (i in names(y)) {
    j <- as.numeric(i)
    sum_tab[1, j] <- summary(dat)[[j]]
  }
  
  sum_tab <- sum_tab[, !is.na(sum_tab)]

  return(sum_tab)
}

#' Check if a vector is binary
#' 
#' @param v vector
#' @export
#' @return TRUE or FALSE depending on whether the vector is binary
is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}

#' Check if a vector is monomorphic
#' 
#' @param v vector
#' @export
#' @return TRUE or FALSE depending on whether the vector is monomorphic
is.monomorphic <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 1L
}


#' Extract summary stats from lm()
#'  
#' @param fit regression output from lm() function
#' @param outcome the outcome variable
#' @param exposure the exposure variable 
#' @export 
#' @return data.frame containing outcomes, estimate, se, p and CIs, residuals and the input
summarise_lm <- function(fit, outcome, exposure) {
  stopifnot(class(fit) == "lm")
  summ <- as.matrix(c(summary(fit)$coef[exposure, ], confint(fit)[exposure, ], summary(fit)$adj.r.squared))
  summ <- as.data.frame(t(summ))
  sum_tab <- summ %>%
    mutate(outcome = outcome) %>%
    dplyr::select(-`t value`) %>%
    dplyr::select(outcome, everything())

  colnames(sum_tab) <- c("outcome", "estimate", "se", "p", "CI_low", "CI_up", "adj_r2") 
  
  res <- resid(fit)
  covars <- names(fit$coef)[!(names(fit$coef) %in% c("(Intercept)", exposure))]

  out <- list(sum_tab, res, covars, fit)
  names(out) <- c("summary_data", "residuals", "covars", "fit")
  return(out)
}
