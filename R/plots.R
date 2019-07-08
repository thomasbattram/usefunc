#' Make facets for forest plots
#' 
#' Generates a vector that can be added to a data.frame and split forest plots up
#' 
#' @param dat a data.frame that will be used for the forest plot
#' @param col_num number of columns the plot will be
#' @param group grouping variable 
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
}

forest_plot <- function(dat, col_num, group = NA, y_axis, units = NULL, title = NULL, scale = 1, null_at = 1, text_size = "norm", meta = FALSE, f_var = "facet_var") {
# Format of data for plot (doesn't matter where in the data frame these things are and can have extra columns)
# y_axis_var Estimate 2.5 % 97.5 % 
# ----------  ----     --    ---
# ----------  ----     --    --- 
# ----------  ----     --    ---

require("tidyverse")
require("gridExtra")
require("ggplot2")

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
		legend <- legend + scale_colour_hue(breaks = rev(levels(dat[[group]]))) 
	}
	legend <- cowplot::get_legend(legend)

	p <- marrangeGrob(plots, right = legend, bottom = units, top = title, ncol = col_num, nrow = 1)
	return(p)
}


#' Manhattan plot function for gwas (stolen from pcgoddard)
#' 
#' Generates a Manhattan plot using gwas data
#' 
#' @param df data.frame containing your results
#' @param threshold the P value threshold
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
gg.manhattan <- function(df, threshold, hlight, col = brewer.pal(9, "Greys")[c(4,7)],
						 title, SNP = "SNP", CHR = "CHR", BP = "BP", P = "P",
						 sig = 1e-8, sugg = 1e-6, lab = FALSE, colour = TRUE){
  # format df
  df.tmp <- df %>% 
    
    # Compute chromosome size
    group_by(!! as.name(CHR)) %>% 
    summarise(chr_len = max(!! as.name(BP), na.rm = TRUE)) %>% 
    
    # Calculate cumulative position of each chromosome
    mutate(tot = cumsum(chr_len) - chr_len) %>%
    dplyr::select(-chr_len) %>%
    
    # Add this info to the initial dataset
    left_join(df, ., by=setNames(CHR, CHR)) %>%
    
    # Add a cumulative position of each SNP
    arrange_(CHR, BP) %>%
    mutate( BPcum = !! as.name(BP) + tot) %>%
    
    # Add highlight and annotation information
    mutate( is_highlight := ifelse(!! as.name(SNP) %in% hlight, "yes", "no")) %>%
    mutate( is_annotate := ifelse(!! as.name(P) < threshold, "yes", "no"))

    # change CHR to a factor
    df.tmp[[CHR]] <- as.factor(df.tmp[[CHR]])

  
  # for the colour later on
  chr_n <- length(unique(df.tmp[[CHR]]))
  col_n <- length(col)

  # get chromosome center positions for x-axis
  axisdf <- df.tmp %>% group_by_(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )

  p <- ggplot(df.tmp, aes(x = BPcum, y = -log10(get(P)))) +
    # Show all points
    geom_point(aes_string(color = CHR), alpha=0.8, size=2) +
    scale_color_manual(values = rep(col, chr_n/col_n)) +

    # custom axes:
    scale_x_continuous( label = axisdf[[CHR]], breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 1)) + # expand=c(0,1)removes space between plot area and x axis 
    
    # add plot and axis titles
    ggtitle(paste0(title)) +
    labs(x = "Chromosome", y = "-log10(P)") +
    
    # add genome-wide sig and sugg lines
    geom_hline(yintercept = -log10(sig)) +
    geom_hline(yintercept = -log10(sugg), linetype="dashed")# +
    
    if (colour) {
    	p <- p + 
    		geom_point(data=subset(df.tmp, is_highlight=="yes"), color="orange", size=2)
    } else {
    	p <- p +
    		geom_point(data=subset(df.tmp, is_highlight=="yes"), shape=2, size=2)
    }
    # Add highlighted points
    # geom_point(data=subset(df.tmp, is_highlight=="yes"), color="orange", size=2) +
    p <- p +    
    # Custom the theme:
    theme_bw(base_size = 22) +
    theme( 
      plot.title = element_text(hjust = 0.5),
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
   if (lab) p <- p + geom_label_repel(data=df.tmp[df.tmp$is_annotate=="yes",],
   									  aes_string(label=SNP, alpha=0.7), size=5, force=1.3)
   return(p)
}





