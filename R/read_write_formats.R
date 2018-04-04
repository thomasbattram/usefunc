#' Read binary GCTA GRM
#'
#' <full description>
#'
#' @param rootname Name of files. e.g. if the GRM files are data.grm.bin, data.grm.id and data.grm.N.bin then supply "data" as the argument
#'
#' @export
#' @return List with elements:
#'         grm: data frame of GRM in long format
#'         id: data frame of IDs
read_grm <- function(rootname)
{
	bin.file.name <- paste(rootname, ".grm.bin", sep="")
	n.file.name <- paste(rootname, ".grm.N.bin", sep="")
	id.file.name <- paste(rootname, ".grm.id", sep="")

	cat("Reading IDs\n")
	id <- read.table(id.file.name)
	n <- dim(id)[1]
	cat("Reading GRM\n")
	bin.file <- file(bin.file.name, "rb")
	grm <- readBin(bin.file, n=n*(n+1)/2, what=numeric(0), size=4)
	close(bin.file)
	cat("Reading N\n")
	n.file <- file(n.file.name, "rb")
	N <- readBin(n.file, n=n*(n+1)/2, what=numeric(0), size=4)
	close(n.file)

	cat("Creating data frame\n")
	l <- list()
	for(i in 1:n)
	{
		l[[i]] <- 1:i
	}
	col1 <- rep(1:n, 1:n)
	col2 <- unlist(l)
	grm <- data.frame(id1=col1, id2=col2, N=N, grm=grm)	

	ret <- list()
	ret$grm <- grm
	ret$id <- id
	return(ret)
}


#' Convert long GRM format to matrix format
#'
#' @param grm Result from readGRM
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



#' Write readGRM style output back to binary GRM for use with GCTA
#'
#' @param grm Output from \link{readGRM}
#' @param rootname
#' @export
write_grm <- function(grm, rootname)
{
	bin.file.name <- paste(rootname, ".grm.bin", sep="")
	n.file.name <- paste(rootname, ".grm.N.bin", sep="")
	id.file.name <- paste(rootname, ".grm.id", sep="")
	write.table(grm$id, id.file.name, row=F, col=F, qu=F)
	n <- dim(grm$id)[1]
	bin.file <- file(bin.file.name, "wb")
	writeBin(grm$grm$grm, bin.file, size=4)
	close(bin.file)
	n.file <- file(n.file.name, "wb")
	writeBin(grm$grm$N, n.file, size=4)
	close(n.file)
}

#' Check if files exist
#'
#' Either checks of all files for a rootname exist, or returns a specific suffix
#'
#' @param rootname
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
#' @param rootname
#' @export
#' @return Data frame
read_bim <- function(rootname)
{
	nom <- checkRootname(rootname, "plink", "bim")
	bim <- read.table(nom, colClasses=c("character", "character", "numeric", "numeric", "character", "character"))
	names(bim) <- c("CHR", "SNP", "GD", "BP", "A1", "A2")
	return(bim)	
}


#' Read fam file
#' 
#' @param rootname
#' @export
#' @return Data frame
read_fam <- function(rootname)
{
	nom <- checkRootname(rootname, "plink", "fam")
	fam <- read.table(nom, colClasses=c("character", "character", "character", "character", "character", "character"))
	names(fam) <- c("FID", "IID", "FATHER", "MOTHER", "SEX", "PHEN")
	return(fam)	
}


#' Read output from plink --linear
#'
#' @param filename
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
		require(reshape2)
		require(plyr)
		if(!file.exists(filename))
		{
				filename <- paste(filename, ".hsq", sep="")
				stopifnot(file.exists(filename))
		}
		a <- readLines(filename)
		ncomp <- (length(a) - 5) / 2
		b <- read.table(filename, header=T, nrows=ncomp*2+2, stringsAsFactors=FALSE)
		variances <- melt(b[1:(ncomp),], measure.vars=c("Variance", "SE"))
		variances$label <- label
		variances$type <- "vg"
		vp <- melt(b[(ncomp+2),], measure.vars=c("Variance", "SE"))
		vp$label <- label
		vp$type <- "vp"
		ve <- melt(b[(ncomp+1),], measure.vars=c("Variance", "SE"))
		ve$label <- label
		ve$type <- "ve"
		hsq <- melt(b[(ncomp+3):nrow(b), ], measure.vars=c("Variance", "SE"))
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
		return(rbind.fill(list(variances, vp, ve=ve, hsq, c)))
}
