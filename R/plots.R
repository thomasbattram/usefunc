# ----------------------------------------------------------------------------
# Plotting functions 
# ----------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Forest plot functions 
# ----------------------------------------------------------------------------
facet_var_gen <- function (dat, col_num, group) {
	#Generates a vector that can be added to a dataframe and used to split plots up 
	#group_num is more than 1 if you have multiple data points per categorical variable
	group_num <- length(unique(dat[[group]]))
	x <- nrow(dat) / group_num
	if (nrow(dat) %% 2 != 0) {
		facet_var <- rep(
			c(rep(1:(col_num-1), each = ceiling(x / col_num)),
			rep(col_num, times = x - (ceiling(x / col_num) * (col_num - 1)))),
			times = group_num)
	} else {
		facet_var <- rep(rep(1:col_num, each = ceiling(x / col_num)), times = group_num)
	}
}


forest_plot <- function(dat, col_num, group = NA, y_axis, units = NULL, title = NULL, scale = 1, null_at = 1, text_size = "norm", meta = FALSE) {
#Format of data for plot (doesn't matter where in the data frame these things are and can have extra columns)
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
	            facet_var = facet_var)
	} else {
		shading <- data.frame(min = rep(seq(from = 0.5, to = met_num, by = 1), times = col_num * group_num),
	           	max = rep(seq(from = 1.5, to = met_num + 1, by = 1), times = col_num * group_num),
	           	col = rep(c(0,1), length.out = nrow(dat)),
	            facet_var = facet_var)
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
		test_forest_dat <- filter(dat, facet_var == i)
		test_shading_dat <- filter(shading, facet_var == i)
		
		test_forest_dat[[y_axis_var]] <- factor(test_forest_dat[[y_axis_var]], levels = test_forest_dat[[y_axis_var]][1:(nrow(test_forest_dat) / group_num)])

		labels <- test_forest_dat[[y_axis[1]]]
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





