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


#' linear regression function
#' 
#' @param outcome names of outcomes
#' @param exposure names of exposures
#' @param dataset dataset containing the outcome and exposure data
#' @param covariate names of covariates
#' @export
#' @return data.frame containing outcomes, estimate, se, p and CIs from lm()
linear_regress <- function(outcome, exposure, dataset, covariate = NULL) 
{ 
  
  tx <- dataset

  dat <- numeric()
  fom <- formula(paste("out", paste(c(exposure, covariate), collapse = '+'))) 
  
  for (i in 1:length(outcome))  
  { 
    out <- [[outcome[i]]]
    fit <- lm(fom, data = tx) 
    temp <- c(summary(fit)$coef[exposure, ], confint(fit)[exposure, ]); 
    add <- rbind(add, temp);  
  }   
  add <- data.frame(add, check.names = F) %>%
    mutate(outcome = outcome) %>%
    dplyr::select(-`t value`) %>%
    dplyr::select(outcome, everything())
  colnames(add) <- c("outcome", "estimate", "se", "p", "CI_low", "CI_up") 
  return(add)
} 