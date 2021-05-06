#' Tidy numbers
#' 
#' Tidy numbers in a table for a rmarkdown report
#'
#' @param df data.frame 
#'
#' @export
#' @return data.frame with all numbers formated based on the comma function
tidy_nums <- function(df) 
{
    df[] <- lapply(df, comma)
    return(df)
}

#' Tidy column names
#'
#' Swap underscores for dashes in column names of tables for rmarkdown reports
#'
#' @param df data.frame 
#'
#' @export
#' @return character vector with all numbers formated
tidy_colnames <- function(df) 
{
    colnames(df) <- gsub("_", "-", colnames(df))
    return(df)
}

#' Convert numbers to text
#' 
#' Convert numbers to text when needed in rmarkdown reports
#'
#' @param x a single integer to have converted into text if needs be 
#' @param start_of_sentence TRUE or FALSE to indicate if the number is at the start of a sentence. Default = FALSE
#'
#' @export
#' @return Number written in text or number format
num_to_text <- function(x, start_of_sentence = FALSE)
{
    if (!x%%1 == 0) warning("X is not an integer")
    if (start_of_sentence) {
    	out <- numbers_to_words(x)
    	out <- stringr::str_to_sentence(out)
    } else {
	    if (x < 11 & x > -1) {
	        out <- numbers_to_words(x)
	    } else {
	        out <- x
	    }
    }
    return(out)
}