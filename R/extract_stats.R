#' Extract mode from a numeric vector
#' 
#' @param v numeric vector
#' @export
#' @return mode of numeric vector
getmode <- function(v) {
    uniqv <- unique(v)
    tabv <- tabulate(match(v, uniqv))
    modev <- uniqv[which(tabv == max(tabv))]
    if (length(modev) > 1) {
        warning("There are multiple modes")
    }
    return(modev)
}

#' Extract summary statistics from numeric vector
#' 
#' @param dat numeric vector
#' @param min extract minimum value
#' @param q1 extract first quartile
#' @param med extract median
#' @param q3 extract third quartile
#' @param max extract maximum value
#' @param na extract number of NA values
#' @export
#' @return data.frame of summary stats selected
extract_sum_stats <- function(dat, min = T, q1 = T, med = T, mean = T, q3 = T, max = T, na = T) {
  sum_tab <- data.frame(min = NA, q1 = NA, med = NA, mean = NA, q3 = NA, max = NA, na = NA)

  y <- c(sum(min), sum(q1), sum(med), sum(mean), sum(q3), sum(max), sum(na))
  names(y) <- 1:7
  y <- y[y == 1]

  for (i in names(y)) {
    j <- as.numeric(i)
    sum_tab[1, j] <- summary(dat)[j]
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
  sum_tab <- dplyr::mutate(summ, outcome = outcome)
  sum_tab <-  dplyr::select(sum_tab, outcome, everything(), -`t value`)

  colnames(sum_tab) <- c("outcome", "estimate", "se", "p", "CI_low", "CI_up", "adj_r2") 
  
  res <- resid(fit)
  covars <- names(fit$coef)[!(names(fit$coef) %in% c("(Intercept)", exposure))]

  out <- list(sum_tab, res, covars, fit)
  names(out) <- c("summary_data", "residuals", "covars", "fit")
  return(out)
}

#' Extract summary stats from glm() when doing logistic regression
#'  
#' @param fit regression output from glm() function
#' @param outcome the outcome variable
#' @param exposure the exposure variable 
#' 
#' @export 
#' @return data.frame containing outcomes, estimate, se, p and CIs, residuals and the input
summarise_glm <- function(fit, outcome, exposure) {
  stopifnot(class(fit)[1] == "glm")
  summ <- as.matrix(c(summary(fit)$coef[exposure, ], confint(fit)[exposure, ], summary(fit)$adj.r.squared))
  summ <- as.data.frame(t(summ))
  sum_tab <- dplyr::mutate(summ, outcome = outcome)
  sum_tab <- dplyr::select(sum_tab, outcome, everything(), -`z value`)

  colnames(sum_tab) <- c("outcome", "estimate", "se", "p", "CI_low", "CI_up") 
  
  res <- resid(fit)
  covars <- names(fit$coef)[!(names(fit$coef) %in% c("(Intercept)", exposure))]

  out <- list(sum_tab, res, covars, fit)
  names(out) <- c("summary_data", "residuals", "covars", "fit")
  return(out)
}

#' Function for dealing with error messages when using tryCatch
#' 
#' @param e error message from function 
#' @param r_msg print the error message given by the function
#' @param user_msg a message given by the user
#' @param to_return what should be returned if there is an error 
#' 
#' @export
#' @return what is chosen by the user to be returned, default = NA 
err_msg <- function(e, r_msg = TRUE, user_msg = NULL, to_return = NA) {
  if (r_msg) print(e)
  if (!is.null(user_msg)) print(user_msg)
  return(to_return)
}

