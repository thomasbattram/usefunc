#' Generate a boxplot within a violin plot
#' 
#' @param p a plot with the class "ggplot"
#' @param width width of the boxplot within the violin plot. Default = 0.2
#' @param ... other parameters that can be parsed to geom_violin and geom_boxplot
#' 
#' @export
#' @return boxplot within a violin plot
geom_vio_box <- function(p, width = 0.2, ...)
{
	p + 
		ggplot2::geom_violin(...) +
		ggplot2::geom_boxplot(position=position_dodge(0.9), width = width, ...)
}

#' Get colours for plotting that are better for colour blind people
#' 
#' Generates a vector of colour IDs that can be used for plots
#' 
#' @export
#' @return a vector of colour IDs
get_cb_palette <- function()
{
	c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
}

#' Make facets for forest plots
#' 
#' Generates a vector that can be added to a data.frame and split forest plots up
#' 
#' @param dat a data.frame that will be used for the forest plot
#' @param col_num number of columns the plot will be
#' @param group grouping variable 
#' 
#' @export
#' @return a vector used to facet plot made in forest_plot
facet_var_gen <- function (dat, col_num, group = NA) {
	group_num <- length(unique(dat[[group]]))
	if (group_num == 0) group_num <- 1
	x <- nrow(dat) / group_num
	if (x %% 1 != 0) stop("Make sure there is the same number of things in each group - put NA rows if needed")
	if (col_num == 1) {
		facet_var <- rep(1, times = nrow(dat))
	} else {
		if (nrow(dat) %% 2 != 0) {
			facet_var <- rep(
				c(rep(1:(col_num-1), each = round(x / col_num)),
				rep(col_num, times = x - (round(x / col_num) * (col_num - 1)))),
				times = group_num)
		} else {
			fv <- rep(1:(col_num - 1), each = round(x / col_num))
			facet_var <- rep(c(fv, rep(col_num, times = (nrow(dat) - length(fv) * group_num)/group_num)), times = group_num)
		}
	}
	return(facet_var)
}

#' Generate a forest plot
#' 
#' Generate a forest plot using ggplot2. This function has capability to be split into multiple forests and was generated to display Mendelian randomization results with a binary outcome.
#' 
#' @param dat a data.frame containing the data
#' @param group group
#' @param y_axis y-axis
#' @param units units
#' @param title title
#' @param scale scale
#' @param null_at null at
#' @param text_size text size
#' @param meta meta-analysis?
#' @param f_var facet variable name
#' @import ggplot2
#' 
#' @export
#' @return forest plot
forest_plot <- function(dat, col_num, group = NA, y_axis, units = NULL, title = NULL, scale = 1, null_at = 1, text_size = "norm", meta = FALSE, f_var = "facet_var") {
# Format of data for plot (doesn't matter where in the data frame these things are and can have extra columns)
# y_axis_var Estimate 2.5 % 97.5 % 
# ----------  ----     --    ---
# ----------  ----     --    --- 
# ----------  ----     --    ---

requireNamespace("magrittr")
requireNamespace("ggplot2")


if (text_size == "norm") {
	text_size <- NULL
}

if (meta) {
	if (!("meta" %in% colnames(dat))) {
		stop("Please include a column called meta in your data.frame")
	}
	shape <- "meta" 
} else {
	shape <- NULL
}

group_num <- length(unique(dat[[group]]))
if (group_num == 0) {
	group_num <- 1
}

met_num <- nrow(dat)/group_num/col_num

#
	#Produce a data frame that describes how the variables will be shaded
	if ((nrow(dat) / group_num) %% 2 != 0) {
		shading <- data.frame(
				min = 
					rep(c(rep(seq(from = 0.5, to = (met_num * col_num + 1)/col_num, by = 1), times = col_num - 1),
					seq(from = 0.5, to = ((nrow(dat) - (ceiling(met_num) * group_num * (col_num - 1))) / group_num), by = 1)),
					times = group_num),
	            max = 
	            	rep(c(rep(seq(from = 1.5, to = (met_num * col_num + 1)/col_num + 1, by = 1), times = col_num - 1),
					seq(from = 1.5, to = ((nrow(dat) - (ceiling(met_num) * group_num * (col_num - 1))) / group_num) + 1, by = 1)),
					times = group_num),
	            col = rep(c(0,1), length.out = nrow(dat)),
	            facet_var = dat[[f_var]])
	} else {
		shading <- data.frame(min = rep(seq(from = 0.5, to = met_num, by = 1), times = col_num * group_num),
	           	max = rep(seq(from = 1.5, to = met_num + 1, by = 1), times = col_num * group_num),
	           	col = rep(c(0,1), length.out = nrow(dat)),
	            facet_var = dat[[f_var]])
	}

	# Minimum and maximum x values
	if (scale == 1) {
		min_val <- floor(min(dat[["2.5 %"]], na.rm = T)) 
		max_val <- ceiling(max(dat[["97.5 %"]], na.rm = T))
	} else {
		min_val <- round(min(dat[["2.5 %"]], na.rm = T), digits = 6) - scale
		max_val <- round(max(dat[["97.5 %"]], na.rm = T), digits = 6) + scale
	}


	if (null_at <= min_val) {
		min_val <- null_at
	} else if (null_at >= max_val) {
		max_val <- null_at
	} else {
		min_val <- min_val
		max_val <- max_val
	}
	#Produce all the plots separately
	y_axis_var <- y_axis[1]

	plots <- list()

	for (i in 1:col_num) {
		test_forest_dat <- dplyr::filter(dat, facet_var == i)
		test_shading_dat <- dplyr::filter(shading, facet_var == i)
		
		test_forest_dat[[y_axis_var]] <- factor(test_forest_dat[[y_axis_var]], levels = unique(test_forest_dat[[y_axis_var]]))

		labels <- levels(test_forest_dat[[y_axis[1]]])
		if (length(y_axis) > 1) {
			for (j in 2:length(y_axis)) {
				labels <- paste(labels, test_forest_dat[[y_axis[j]]], sep = "\n")
			}
		}

		plots[[i]] <- ggplot(test_forest_dat) +
			geom_point(aes_string(x = y_axis_var, y = "Estimate", ymin = "`2.5 %`", ymax = "`97.5 %`", colour = group, group = group, shape = shape, size = shape), position=position_dodge(width = 0.9)) +
			geom_rect(data = test_shading_dat, aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, fill = factor(col)) ) +
			scale_fill_manual(values = c("white", "gray80")) +
			geom_pointrange(aes_string(x = y_axis_var, y = "Estimate", ymin = "`2.5 %`", ymax = "`97.5 %`", colour = group, group = group, shape = shape, size = shape), position=position_dodge(width = 0.9)) +
			geom_hline(yintercept = null_at) +
			theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "none", text = element_text(size = text_size)) +
			scale_y_continuous(limit = c(min_val, max_val)) +
			scale_x_discrete(labels = labels) +
			scale_size_manual(values = c(0.75, 1.5)) +
			coord_flip()
	}

	#Produce the legend for the plot
	legend <- ggplot(dat) +
		geom_point(aes_string(x = y_axis_var, y = "Estimate", ymin = "`2.5 %`", ymax = "`97.5 %`", colour = group, group = group), position=position_dodge(width = 0.9)) +
		scale_fill_manual(values = c("white", "gray80")) +
		geom_pointrange(aes_string(x = y_axis_var, y = "Estimate", ymin = "`2.5 %`", ymax = "`97.5 %`", colour = group, group = group), position=position_dodge(width = 0.9)) +
		scale_size_manual(values = c(0.75, 1.5)) +
		theme(legend.direction = "vertical", legend.justification = "center", legend.title = element_blank(), text = element_text(size = text_size))

	if (group_num > 1) {
		if (!is.factor(dat[[group]])) {
			stop("The group needs to be a factor")
		}
		legend <- legend + ggplot2::scale_colour_hue(breaks = rev(levels(dat[[group]]))) 
	}
	legend <- cowplot::get_legend(legend)

	p <- gridExtra::marrangeGrob(plots, right = legend, bottom = units, top = title, ncol = col_num, nrow = 1)
	return(p)
}

#' Remove points from a scatter plot where density is really high
#' 
#' @param x x-coordinates vector
#' @param y y-coordinates vector
#' @param resolution number of partitions for the x and y-dimensions.
#' @param max.per.cell maximum number of points per x-y partition.
#' @return index into the points that omits points from x-y partitions
#' so that each has at most \code{max.per.cell} points.
scatter.thinning <- function(x,y,resolution=100,max.per.cell=100) {
    x.cell <- floor((resolution-1)*(x - min(x,na.rm=T))/diff(range(x,na.rm=T))) + 1
    y.cell <- floor((resolution-1)*(y - min(y,na.rm=T))/diff(range(y,na.rm=T))) + 1
    z.cell <- x.cell * resolution + y.cell
    frequency.table <- table(z.cell)
    frequency <- rep(0,max(z.cell, na.rm=T))
    frequency[as.integer(names(frequency.table))] <- frequency.table
    f.cell <- frequency[z.cell]
    
    big.cells <- length(which(frequency > max.per.cell))
    sort(c(which(f.cell <= max.per.cell),
           sample(which(f.cell > max.per.cell),
                  size=big.cells * max.per.cell, replace=F)),
         decreasing=F)
}


#' Manhattan plot function for gwas (stolen from pcgoddard)
#' 
#' Generates a Manhattan plot using gwas data
#' 
#' @param df data.frame containing your results
#' @param hlight a vector containing any SNPs that need highlighting
#' @param col colours for the plot
#' @param title title of the plot
#' @param SNP name of the column corresponding to SNPs
#' @param CHR name of the column corresponding to chromosome number
#' @param BP name of the column corresponding to base pair
#' @param P name of the column corresponding to p value
#' @param sig the "significance threshold"
#' @param sugg the "suggestive significance threshold"
#' @param lab whether or not to add labels to the sites on the plot
#' @param colour coloured plot?
#' 
#' @export
#' @return manhattan plot
gg.manhattan <- function(df, hlight, col = brewer.pal(9, "Greys")[c(4,7)],
						 title, SNP = "SNP", CHR = "CHR", BP = "BP", P = "P",
						 sig = 1e-8, sugg = 1e-6, lab = FALSE, colour = TRUE){
  # format df
  df.tmp <- df %>% 
    
    # Compute chromosome size
    dplyr::group_by(!! as.name(CHR)) %>% 
    dplyr::summarise(chr_len = as.numeric(max(!! as.name(BP), na.rm = TRUE))) %>% 
    
    # Calculate cumulative position of each chromosome
    dplyr::mutate(tot = cumsum(chr_len) - chr_len) %>%
    dplyr::select(-chr_len) %>%
    
    # Add this info to the initial dataset
    dplyr::left_join(df, ., by=setNames(CHR, CHR)) %>%
    
    # Add a cumulative position of each SNP
    dplyr::arrange({{ CHR }}, {{ BP }}) %>%
    dplyr::mutate( BPcum = !! as.name(BP) + tot) %>%
    
    # Add highlight and annotation information
    dplyr::mutate( is_highlight := ifelse(!! as.name(SNP) %in% hlight, "yes", "no")) %>%
    dplyr::mutate( is_annotate := ifelse(!! as.name(SNP) %in% hlight, "yes", "no"))

    # change CHR to a factor
    df.tmp[[CHR]] <- as.factor(df.tmp[[CHR]])

  # thin the scatter plot
  selection.idx <- scatter.thinning(x = df.tmp$BPcum, y = -log10(df.tmp[[P]]), resolution=100, max.per.cell=100)
  df.select <- df.tmp[selection.idx, ]
  
  df.select$stat <- -log10(df.select[[P]])

  # for the colour later on
  chr_n <- length(unique(df.select[[CHR]]))

  # get chromosome center positions for x-axis
  axisdf <- df.select %>% dplyr::group_by_at(CHR) %>% dplyr::summarize(center=( max(BPcum) + min(BPcum) ) / 2 )

  p <- ggplot2::ggplot(df.select, aes(x = BPcum, y = stat)) +
    # Show all points
    ggplot2::geom_point(aes_string(color = CHR), alpha=0.8, size=2) +
    ggplot2::scale_color_manual(values = rep_len(col, length.out = chr_n)) +

    # custom axes:
    ggplot2::scale_x_continuous( label = axisdf[[CHR]], breaks= axisdf$center ) +
    ggplot2::scale_y_continuous(expand = c(0, 1)) + # expand=c(0,1)removes space between plot area and x axis 
    
    # add plot and axis titles
    ggplot2::ggtitle(paste0(title)) +
    ggplot2::labs(x = "Chromosome", y = "-log10(P)") +
    
    # add genome-wide sig and sugg lines
    ggplot2::geom_hline(yintercept = -log10(sig)) +
    ggplot2::geom_hline(yintercept = -log10(sugg), linetype="dashed")# +
    
    if (colour) {
    	p <- p + 
    		ggplot2::geom_point(data=subset(df.select, is_highlight=="yes"), color="orange", size=2)
    } else {
    	p <- p +
    		ggplot2::geom_point(data=subset(df.select, is_highlight=="yes"), shape=2, size=2)
    }
    # Add highlighted points
    # geom_point(data=subset(df.tmp, is_highlight=="yes"), color="orange", size=2) +
    p <- p +    
    # Custom the theme:
    ggplot2::theme_bw(base_size = 22) +
    ggplot2::theme( 
      plot.title = element_text(hjust = 0.5),
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
   if (lab && length(hlight) > 0) p <- p + ggrepel::geom_label_repel(data=df.select[df.select$is_annotate=="yes",],
   									  ggplot2::aes_string(label=SNP, alpha=0.7), size=5, force=1.3)
   return(p)
}





