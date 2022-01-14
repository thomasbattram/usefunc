#' extract_alspac_labels
#' 
#' Extract Stata labels from ALSPAC .dta files. These are needed to interpret numeric values assigned to categorical variables.
#' 
#' @param dat data.frame with columns "path", "obj", name", that correspond to the path within the ALSPAC directory to find the files, the file names of interest and the variables of interest.
#' @param alsp_dir string. Path to ALSPAC directory on local machine.
#' @export
#' @return list of variable names and their labels
extract_alspac_labels <- function(dat, alsp_dir)
{
	if (!all(c("path", "obj", "name") %in% colnames(dat))) stop("'dat' requires the columns 'path', 'obj', 'name' in columns")
	if (!file.exists(alsp_dir)) stop("ALSPAC directory not found.")
	
	uniq_obj <- unique(dat$obj)
	message("Extracting labels in ", length(uniq_obj), " ALSPAC files.")
	list_out <- lapply(uniq_obj, function(ob) {
		df <- dat[dat$obj == ob, ]
		full_path <- file.path(alsp_dir, unique(df$path), unique(df$obj))
		message("Reading in file: ", full_path)
		alsp_res <- haven::read_dta(full_path) %>%
			dplyr::select(all_of(df$name))
		val_labs <- lapply(df$name, function(nam) {labelled::val_labels(alsp_res[[nam]])})
		names(val_labs) <- df$name
		return(val_labs)
	})
	out <- unlist(list_out, recursive = FALSE)

	return(out)
}
