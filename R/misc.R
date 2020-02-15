#' Plot ggplot graphs inline with imgcat for iterm2
#' 
#' Takes a plot, saves it as a temporary file and plots it inline
#' 
#' @param plot a ggplot object
#' @param path path to where the temporary plot will be saved
#' @param save_plot keep the plot saved
#' @param filename filename
gg_imgcat <- function(plot, path = getwd(), save_plot = FALSE, 
				   	  filename = "temp.png") 
{	
	tempfile <- paste0(path, "/", filename)
	ggsave(tempfile, plot = plot)
	system(paste("~/.iterm2/imgcat", tempfile))
	system(paste("rm", tempfile))
}