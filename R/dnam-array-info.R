#' get_zhou_recs
#' 
#' Extract CpGs to exclude from zhou et al. 2017 (http://zwdzwd.github.io/InfiniumAnnotation)
#' Can extract just CpGs or the table with reasons for exclusion
#' 
#' @param outpath file path to output the data
#' @param listonly a logical value. If 'TRUE', list of CpGs recommended to exlude are returned. If "FALSE", full table of CpGs with reasons for potentially removing them returned.
#' @param array string. Array type being used, must be either "hm450" or "epic".
#' @param overwrite a logical value. If 'TRUE' outpath is overwritten. If 'FALSE' it is not overwritten.
#' @export
#' @return Either a character vector of CpG sites to exclude from an array or a tibble with reasons to exlcude certain sites.
get_zhou_recs <- function(outpath, listonly = TRUE, array = c("hm450", "epic"), overwrite = FALSE) {
    ## Check output path
    if (base::file.exists(outpath) && !overwrite) {
        base::message(outpath, " is already present. Reading in from that file.")
        if (listonly) return(base::readLines(outpath))
        if (!listonly) return(readr::read_tsv(outpath))
    } else {
        ## Set download path
        if (array %in% "hm450") {
            download_path <- "https://zhouserver.research.chop.edu/InfiniumAnnotation/20180909/HM450/HM450.hg38.manifest.tsv.gz"
            base::message("Downloading from ", download_path)
        } else if (array == "epic") {
            download_path <- "https://zhouserver.research.chop.edu/InfiniumAnnotation/20180909/EPIC/EPIC.hg38.manifest.tsv.gz"
            base::message("Downloading from ", download_path)
        } else {
            stop("array must be either 'hm450' or 'epic'")
        } 
        ## Download the file using wget
        download_status <- utils::download.file(url = download_path, 
                                                destfile = outpath,
                                                method = "wget")
        if (download_status != 0) stop("Failed download. Try again or check the download path")
        message("File downloaded to ", outpath)
        ## return the results
        res <- readr::read_tsv(outpath)
        if (listonly) {
            ret <- res[res$MASK_general, "probeID", drop = TRUE]
            message("Writing list of CpGs to file at: ", outpath)
            writeLines(ret, con = outpath)
        } else {
            ret <- res
            message("Writing CpGs and exclusion recommendations to tsv file at: ", outpath)
            write.table(ret, file = outpath, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
        }
        return(ret)
    }
}
