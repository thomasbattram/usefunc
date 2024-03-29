% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dnam-array-info.R
\name{get_zhou_recs}
\alias{get_zhou_recs}
\title{get_zhou_recs}
\usage{
get_zhou_recs(
  outpath,
  listonly = TRUE,
  array = c("hm450", "epic"),
  overwrite = FALSE
)
}
\arguments{
\item{outpath}{file path to output the data}

\item{listonly}{a logical value. If 'TRUE', list of CpGs recommended to exlude are returned. If "FALSE", full table of CpGs with reasons for potentially removing them returned.}

\item{array}{string. Array type being used, must be either "hm450" or "epic".}

\item{overwrite}{a logical value. If 'TRUE' outpath is overwritten. If 'FALSE' it is not overwritten.}
}
\value{
Either a character vector of CpG sites to exclude from an array or a tibble with reasons to exlcude certain sites.
}
\description{
Extract CpGs to exclude from zhou et al. 2017 (http://zwdzwd.github.io/InfiniumAnnotation)
Can extract just CpGs or the table with reasons for exclusion
}
