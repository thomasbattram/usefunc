#' Read binary GCTA GRM
#'
#' <full description>
#'
#' @param rootname Name of files. e.g. if the GRM files are data.grm.bin, data.grm.id and data.grm.N.bin then supply "data" as the argument
#' @param n.file Write out grm.N.bin file - not always needed 
#' 
#' @export
#' @return List with elements:
#'         grm: data frame of GRM in long format
#'         id: data frame of IDs
read_grm <- function(rootname, n.file = FALSE)
{
	if (!n.file) {
		print(".grm.N.bin file not being read - likely unnecessary")
	} else {
		n.file.name <- paste(rootname, ".grm.N.bin", sep="")
		cat("Reading N\n")
		n.file <- file(n.file.name, "rb")
		N <- readBin(n.file, n=n*(n+1)/2, what=numeric(0), size=4)
		close(n.file)
	}

	bin.file.name <- paste(rootname, ".grm.bin", sep="")
	id.file.name <- paste(rootname, ".grm.id", sep="")

	cat("Reading IDs\n")
	id <- read.table(id.file.name)
	n <- dim(id)[1]
	cat("Reading GRM\n")
	bin.file <- file(bin.file.name, "rb")
	grm <- readBin(bin.file, n=n*(n+1)/2, what=numeric(0), size=4)
	close(bin.file)

	cat("Creating data frame\n")
	l <- list()
	for(i in 1:n)
	{
		l[[i]] <- 1:i
	}
	col1 <- rep(1:n, 1:n)
	col2 <- unlist(l)
	if (n.file) {
		grm <- data.frame(id1=col1, id2=col2, N=N, grm=grm)	
	} else {
		grm <- data.frame(id1=col1, id2=col2, grm=grm)	
	}
	ret <- list()
	ret$grm <- grm
	ret$id <- id
	return(ret)
}


#' Convert long GRM format to matrix format
#'
#' @param grm Result from read_grm
#' @export
#' @return Matrix of n x n
make_grm_matrix <- function(grm)
{
	mat <- diag(nrow(grm$id))
	mat[upper.tri(mat, diag=TRUE)] <- grm$grm$grm
	mat <- t(mat)
	nsnpvec <- subset(grm$grm, id1 != id2)$N
	mat[upper.tri(mat, diag=FALSE)] <- nsnpvec
	rownames(mat) <- grm$id$V2
	colnames(mat) <- grm$id$V2
	return(mat)
}



#' Write read_grm style output back to binary GRM for use with GCTA
#'
#' @param grm Output from \link{read_grm}
#' @param rootname rootname
#' @param n.file Write out grm.N.bin file - not always needed
#' @export
write_grm <- function(grm, rootname, n.file = FALSE)
{
	if (!n.file) {
		print(".grm.N.bin file not being written - likely unnecessary")
	} else {
		n.file.name <- paste(rootname, ".grm.N.bin", sep="")
		n.file <- file(n.file.name, "wb")
		writeBin(grm$grm$N, n.file, size=4)
		close(n.file)
	}
	bin.file.name <- paste(rootname, ".grm.bin", sep="")
	n.file.name <- paste(rootname, ".grm.N.bin", sep="")
	id.file.name <- paste(rootname, ".grm.id", sep="")
	write.table(grm$id, id.file.name, row=F, col=F, qu=F)
	n <- dim(grm$id)[1]
	bin.file <- file(bin.file.name, "wb")
	writeBin(grm$grm$grm, bin.file, size=4)
	close(bin.file)
}

#' Check if files exist
#'
#' Either checks of all files for a rootname exist, or returns a specific suffix
#'
#' @param rootname rootname
#' @param software Either "plink" or "gcta"
#' @param suffix Either "all" or a specific suffic
#'
#' @export
#' @return Character string
check_rootname <- function(rootname, software="plink", suffix="all")
{
	sfs <- list(
		gcta = c("all", "grm.bin", "grm.N.bin", "grm.id"),
		plink = c("all", "bim", "fam", "bed")
	)

	stopifnot(software %in% names(sfs))
	stopifnot(suffix %in% sfs[[software]])

	if(suffix=="all")
	{
		nom <- paste(rootname, sfs[[software]][-1], sep=".")
		a <- file.exists(nom)
		if(!any(a))
		{
			stop(paste(nom[!a], collapse=" "))
		} else {
			return(rootname)
		}
	} else {
		if(file.exists(rootname))
		{
			return(rootname)
		} else if (file.exists(paste(rootname, suffix, sep="."))) {
			return(paste(rootname, suffix, sep="."))
		} else {
			stop(paste(suffix, "file does not exist"))
		}
	}
}



#' Read bim file
#' 
#' @param rootname rootname
#' @export
#' @return Data frame
read_bim <- function(rootname)
{
	nom <- check_rootname(rootname, "plink", "bim")
	bim <- read.table(nom, colClasses=c("character", "character", "numeric", "numeric", "character", "character"))
	names(bim) <- c("CHR", "SNP", "GD", "BP", "A1", "A2")
	return(bim)	
}


#' Read fam file
#' 
#' @param rootname rootname
#' @export
#' @return Data frame
read_fam <- function(rootname)
{
	nom <- check_rootname(rootname, "plink", "fam")
	fam <- read.table(nom, colClasses=c("character", "character", "character", "character", "character", "character"))
	names(fam) <- c("FID", "IID", "FATHER", "MOTHER", "SEX", "PHEN")
	return(fam)	
}


#' Read output from plink --linear
#'
#' @param filename filename
#' @param h Is there a header
#'
#' @export
#' @return Data.frame
read_plink_linear <- function(filename, h=TRUE)
{
	a <- read.table(filename, header=h, colClass=c("character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric"))
	if(!h)
	{
		names(a) <- c("CHR", "SNP", "BP", "A1", "TEST", "NMISS", "BETA", "STAT", "P")
	}
	return(a)
}


#' Read GCTA xmat file
#'
#' @param filename
#'
#' @export
#' @return List with the following elements:
#'         xmat: Matrix of nsnp x nid 0/1/2
#'         snps: SNP IDs (array)
#'         ids: FID and IID
read_gcta_xmat <- function(filename)
{
		a <- read.table(filename, colClass="character")
		snps <- as.character(a[1,-c(1:2)])
		ids <- a[-c(1:2), 1:2]
		alleles <- as.character(a[2,-c(1:2)])
		xmat <- matrix(as.numeric(as.matrix(a[-c(1,2), -c(1,2)])), nrow(a)-2, ncol(a)-2)
		ids <- data.frame(a)
		names(ids) <- c("FID", "IID")
		snps <- data.frame(snp=snps, allele=alleles)
		return(list(xmat=xmat, snps=snps, ids=ids))
}


#' Read plink raw format as exported from PLINK2 using --recode A
#'
#' @param filename Filename of exported data
#'
#' @export
#' @return List with the following elements:
#'         xmat: Matrix of nsnp x nid 0/1/2
#'         snps: SNP information (rsid and ref allele)
#'         ids: id information (ped file)
read_plink_raw <- function(filename)
{
	a <- read.table(filename, he=T, colClass="character", stringsAsFactors=FALSE)
	snps <- names(a[-c(1:6)])
	snps <- data.frame(do.call(rbind, strsplit(snps, split="_")))
	names(snps) <- c("SNP", "REF_ALLELE")
	ids <- a[, 1:6]
	xmat <- matrix(as.numeric(as.matrix(a[, -c(1:6)])), nrow(a))	
	return(list(xmat=xmat, snps=snps, ids=ids))
}


#' Read in GCTA .hsq file in long format
#'
#' @param filename .hsq file
#' @param label Optional value for labelling the output
#'
#' @export
#' @return Data frame of REML results
read_hsq <- function(filename, label=NULL)
{
		if(!file.exists(filename))
		{
				filename <- paste(filename, ".hsq", sep="")
				stopifnot(file.exists(filename))
		}
		a <- readLines(filename)
		ncomp <- (length(a) - 5) / 2
		b <- read.table(filename, header=T, nrows=ncomp*2+2, stringsAsFactors=FALSE)
		variances <- reshape2::melt(b[1:(ncomp),], measure.vars=c("Variance", "SE"))
		variances$label <- label
		variances$type <- "vg"
		vp <- reshape2::melt(b[(ncomp+2),], measure.vars=c("Variance", "SE"))
		vp$label <- label
		vp$type <- "vp"
		ve <- reshape2::melt(b[(ncomp+1),], measure.vars=c("Variance", "SE"))
		ve$label <- label
		ve$type <- "ve"
		hsq <- reshape2::melt(b[(ncomp+3):nrow(b), ], measure.vars=c("Variance", "SE"))
		hsq$label <- label
		hsq$type <- "hsq"
		c <- read.table(filename, h=F, skip=ncomp*2+3, stringsAsFactors=FALSE)
		names(c) <- c("variable", "value")
		c$Source <- "details"
		c$label <- label
		c <- rbind(c, c[1,])
		c$variable[1] <- "ncomp"
		c$value[1] <- ncomp
		c$type <- "details"
		return(plyr::rbind.fill(list(variances, vp, ve=ve, hsq, c)))
}

#' Read in LDAK .reml file in long format
#' 
#' @param input .reml file or the filename excluding .reml
#' @param version version of ldak used to run REML analyses. Default = 5.1. 5 is acceptable.
#' 
#' @export
#' @return list of all results in .reml file
read_ldak <- function(input, version = 5.1) {

	if (!grepl(".reml", input)) input <- paste0(input, ".reml")
	if (!version %in% c(5, 5.1)) stop("This function only works with version 5.1 or 5 of LDAK")

	# read in the data in two parts
	if (version == 5) skip_lines <- 13
	if (version == 5.1) skip_lines <- 16
	dat <- readr::read_delim(input, delim = " ", n_max = skip_lines, col_names = FALSE)
	her_dat <- readr::read_delim(input, delim = " ", skip = skip_lines)

	if (version == 5.1) colnames(her_dat)[grep("SD", colnames(her_dat))] <- c("sd, mi_sd")
	colnames(dat) <- c("variable", "value")
	# extract the meta-data
	meta_dat_vars <- c("Num_Kinships", 
						"Num_Regions", 
						"Num_Tops", 
						"Num_Covars", 
						"Total_Samples", 
						"With_Phenotypes")
	meta_dat <- dat %>%
		dplyr::filter(variable %in% meta_dat_vars)

	# extract the file names
	file_dat <- dat %>%
		dplyr::filter(variable %in% grep("file", variable, value = TRUE))

	# extract the likelihood estimates
	ll_vars <- c("Null_Likelihood", "Alt_Likelihood", "LRT_Stat", "LRT_P")
	l_dat <- dat %>%
		dplyr::filter(variable %in% ll_vars) %>%
		dplyr::mutate(value = as.numeric(value))

	# put it all in a list
	out_dat <- list(her_estimates = her_dat,
					likelihood_estimates = l_dat,
					meta_data = meta_dat, 
					other_file_names = file_dat)	
	return(out_dat)
}

#' Read in files with rownames using read_delim() from the readr package
#' 
#' @param input file name
#' @param delim delimiter
#' @param ... any parameters for read_delim()
#' 
#' @export
#' @return tibble of data that has rownames 
read_delim_rownames <- function(input, delim, ...) {
	temp <- readr::read_delim(input, n_max = 0, delim = delim) 
	cols <- c("row", colnames(temp))
	dat <- readr::read_delim(input, delim = delim, col_names = cols, skip = 1, ...)
	return(dat)
}

#' Use the base R load function but name the object(s) being loaded in
#' 
#' @param file file name
#' 
#' @export
#' @return data loaded in
new_load <- function(file) {
	temp_space <- new.env()
	var <- load(file, temp_space)
	out <- mget(var, temp_space)
	if (length(out) == 1) out <- out[[1]]
	rm(temp_space)
	return(out)
}

